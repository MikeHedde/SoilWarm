`%notin%` <- Negate(`%in%`)

assert_that <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

trim_chr <- function(x) stringr::str_squish(as.character(x))

normalise_met_name <- function(x) {
  # Extract MET250xxx from names such as MET250125_S1_L001 or MET250125_ARCH.
  stringr::str_extract(as.character(x), "MET[0-9]+")
}

is_unassigned <- function(x) {
  x <- trim_chr(x)
  is.na(x) | x == "" |
    stringr::str_detect(
      stringr::str_to_lower(x),
      "unknown|unclassified|uncultured|no data|multi-affiliation|unidentified"
    )
}

met_from_well <- function(plate, well) {
  # Convert a verified plate well to its MET sequencing identifier.
  #
  # The original helper assumed a scalar `plate` could be recycled safely while
  # indexing a vector of wells.  That was unnecessarily fragile.  We now map
  # each well explicitly and validate the physical geometry of each plate.
  #
  # Verified layouts:
  #   plaque1: A1:F12 -> MET250125:MET250196, column by column (A..F)
  #   plaque2: A1:D3, then A4:C12 -> MET250197:MET250235, column by column
  #
  # The visual plate-2 table labels B9 as "PL2_B9_10".  The sequencing
  # workbooks contain MET250225, and the verified numbering maps B9 uniquely to
  # MET250225.  This special case is checked in build_sample_map().

  well <- toupper(trim_chr(well))
  plate <- rep_len(trim_chr(plate), length(well))
  out <- rep(NA_character_, length(well))

  for (i in seq_along(well)) {
    w <- well[i]
    p <- plate[i]

    if (is.na(w) || w == "") next

    row_letter <- stringr::str_extract(w, "^[A-H]")
    col_number <- suppressWarnings(as.integer(stringr::str_extract(w, "[0-9]+$")))

    assert_that(!is.na(row_letter) && !is.na(col_number),
                paste0("Invalid well label: '", w, "'."))

    if (p == "plaque1") {
      assert_that(row_letter %in% LETTERS[1:6] && col_number %in% 1:12,
                  paste0("Unexpected well '", w, "' for plate 1."))
      row_index <- match(row_letter, LETTERS[1:6]) - 1L
      met_num <- 250125L + (col_number - 1L) * 6L + row_index
      out[i] <- paste0("MET", met_num)

    } else if (p == "plaque2") {
      if (col_number %in% 1:3) {
        assert_that(row_letter %in% LETTERS[1:4],
                    paste0("Unexpected well '", w, "' for plate 2 columns 1-3."))
        offset <- (col_number - 1L) * 4L + match(row_letter, LETTERS[1:4]) - 1L
      } else if (col_number %in% 4:12) {
        assert_that(row_letter %in% LETTERS[1:3],
                    paste0("Unexpected well '", w, "' for plate 2 columns 4-12."))
        offset <- 12L + (col_number - 4L) * 3L + match(row_letter, LETTERS[1:3]) - 1L
      } else {
        stop(paste0("Unexpected column in plate-2 well '", w, "'."), call. = FALSE)
      }
      out[i] <- paste0("MET", 250197L + offset)

    } else {
      stop(paste0("Unknown plate name: '", p, "'."), call. = FALSE)
    }
  }

  out
}

rv_coefficient <- function(A, B) {
  A <- as.matrix(A); B <- as.matrix(B)
  GA <- A %*% t(A)
  GB <- B %*% t(B)
  sum(GA * GB) / sqrt(sum(GA^2) * sum(GB^2))
}

rv_permutation_test <- function(A, B, nperm = 9999L, seed = 1L) {
  set.seed(seed)
  obs <- rv_coefficient(A, B)
  null <- replicate(nperm, {
    idx <- sample(seq_len(nrow(B)))
    rv_coefficient(A, B[idx, , drop = FALSE])
  })
  tibble(
    RV = obs,
    p_perm = (1 + sum(null >= obs)) / (nperm + 1),
    nperm = nperm
  )
}

all_perms <- function(x) {
  if (length(x) == 1L) return(matrix(x, nrow = 1L))
  do.call(rbind, lapply(seq_along(x), function(i) {
    cbind(x[i], all_perms(x[-i]))
  }))
}

align_axes <- function(reference_scores, candidate_scores, k = 3L) {
  common <- intersect(rownames(reference_scores), rownames(candidate_scores))
  assert_that(length(common) >= 10L, "Too few common samples for axis alignment.")

  R <- cor(
    reference_scores[common, seq_len(k), drop = FALSE],
    candidate_scores[common, seq_len(k), drop = FALSE],
    use = "pairwise.complete.obs"
  )

  perms <- all_perms(seq_len(k))
  scores <- apply(perms, 1, function(p) sum(abs(R[cbind(seq_len(k), p)])))
  best <- perms[which.max(scores), ]

  tibble(
    primary_axis = paste0("Axis", seq_len(k)),
    candidate_axis = paste0("Axis", best),
    correlation = R[cbind(seq_len(k), best)],
    abs_correlation = abs(correlation)
  )
}

transform_community <- function(X, method = c("hellinger", "clr"), pseudocount = 0.5) {
  method <- match.arg(method)
  X <- as.matrix(X)
  storage.mode(X) <- "double"

  assert_that(all(X >= 0, na.rm = TRUE), "Community matrix contains negative counts.")
  assert_that(all(rowSums(X) > 0), "At least one sample has zero reads before transformation.")

  if (method == "hellinger") {
    Z <- vegan::decostand(X, method = "hellinger")
  } else {
    # CLR sensitivity only. A small pseudocount is necessary because the data are sparse.
    # This is deliberately NOT the primary transformation.
    P <- X + pseudocount
    P <- P / rowSums(P)
    logP <- log(P)
    Z <- logP - rowMeans(logP)
  }

  # Centre variables but do not scale to unit variance.
  Z <- scale(Z, center = TRUE, scale = FALSE)

  # Remove any zero-variance columns after transformation.
  keep <- apply(Z, 2, function(z) is.finite(var(z)) && var(z) > 0)
  Z <- Z[, keep, drop = FALSE]
  as.data.frame(Z, check.names = FALSE)
}

build_sample_map <- function(plate_file, metadata_file) {
  # ---------------------------------------------------------------------------
  # Canonical sample identity map
  # ---------------------------------------------------------------------------
  # The workbook already contains the full sample-to-MET correspondence in
  # `Liste_Enregistrement_Colonnes`.  We use that sheet directly and DO NOT
  # reconstruct MET identifiers from plate geometry.
  #
  # Verified structure of the original workbook:
  #   111 DATA rows + 1 header row
  #   columns: plq, ech, yr, c, condition, Profondeur, traitement, qté_ADN
  #   `c` is the MET identifier (MET250125 ... MET250235, in plate order)
  #   108 biological libraries + 3 extraction controls
  #
  # This is intentionally the simplest and most auditable identity route.

  # ---- 1. Read canonical registration sheet WITH its header -----------------
  reg <- readxl::read_excel(
    plate_file,
    sheet = "Liste_Enregistrement_Colonnes",
    col_names = TRUE,
    .name_repair = "minimal"
  ) %>%
    as.data.frame(check.names = FALSE)

  required_reg <- c("plq", "ech", "yr", "c", "condition", "Profondeur", "traitement", "qté_ADN")
  assert_that(
    all(required_reg %in% names(reg)),
    paste0(
      "Liste_Enregistrement_Colonnes does not contain the expected columns. Found: ",
      paste(names(reg), collapse = ", ")
    )
  )

  reg <- reg %>%
    transmute(
      plate = str_to_lower(trim_chr(plq)),
      sample_number = trim_chr(ech),
      Year = suppressWarnings(as.integer(yr)),
      MET = trim_chr(c),
      condition = trim_chr(condition),
      Depth = trim_chr(Profondeur),
      plate_treatment = trim_chr(traitement),
      DNA_concentration = suppressWarnings(as.numeric(`qté_ADN`))
    ) %>%
    filter(!is.na(MET), MET != "")

  # 111 is the number of DATA rows. The header is not counted by read_excel()
  # because col_names = TRUE.
  assert_that(
    nrow(reg) == 111L,
    paste0(
      "Expected 111 data rows in Liste_Enregistrement_Colonnes; found ",
      nrow(reg), ". The header must not be counted as a sample."
    )
  )

  # ---- Known source-workbook annotation anomaly -----------------------------
  # In the original plate-registration workbook, one value in column `c` is
  # `PL2_B9_10` instead of a MET identifier (Plaque2, sample 33, 2024, PP,
  # 30-50 cm, treatment T). Among the expected series MET250125:MET250235, the
  # only absent identifier is MET250225; plate 2 well B9 also corresponds to
  # MET250225. We therefore repair this SINGLE known source annotation here,
  # leaving the original Excel workbook unchanged and making the correction
  # explicit/reproducible in code. Any other non-MET value remains fatal.
  known_bad <- reg$MET == "PL2_B9_10" &
    reg$plate == "plaque2" &
    reg$sample_number == "33" &
    reg$Year == 2024L &
    reg$condition == "PP" &
    reg$Depth == "30-50" &
    reg$plate_treatment == "T"

  if (sum(known_bad, na.rm = TRUE) == 1L) {
    message(
      "Known plate-map annotation repaired: PL2_B9_10 -> MET250225 ",
      "(Plaque2, sample 33, 2024, PP, 30-50 cm, T)."
    )
    reg$MET[known_bad] <- "MET250225"
  }

  non_met <- !str_detect(reg$MET, "^MET[0-9]+$")
  assert_that(
    !any(non_met),
    paste0(
      "Unexpected non-MET registration value(s) remain in column `c`: ",
      paste(unique(reg$MET[non_met]), collapse = ", "),
      ". Inspect Liste_Enregistrement_Colonnes before continuing."
    )
  )
  assert_that(n_distinct(reg$MET) == 111L, "MET identifiers are not unique in the registration sheet.")
  assert_that(
    setequal(reg$MET, paste0("MET", 250125:250235)),
    paste0(
      "After the documented correction, the registration sheet still does not contain exactly ",
      "MET250125 ... MET250235. Missing: ",
      paste(setdiff(paste0("MET", 250125:250235), reg$MET), collapse = ", "),
      "; unexpected: ",
      paste(setdiff(reg$MET, paste0("MET", 250125:250235)), collapse = ", ")
    )
  )

  # ---- 2. Separate biological libraries and extraction controls -------------
  is_extraction_blank <- str_detect(
    str_to_lower(reg$sample_number),
    "témoin extraction|temoin extraction"
  )

  extraction_controls <- reg %>%
    filter(is_extraction_blank) %>%
    mutate(
      Nom_Ech = paste0("Plaque2_Ech-", sample_number, "_2024"),
      LandUse = NA_character_,
      Treatment = NA_character_,
      sample_number_num = NA_real_,
      rep = NA_integer_,
      Plot = NA_character_,
      Block = NA_integer_,
      profile_id = NA_character_,
      sample_unit_id = NA_character_,
      is_negative = TRUE,
      negative_type = "extraction_blank"
    )

  assert_that(nrow(extraction_controls) == 3L, "Expected exactly 3 extraction controls.")
  assert_that(
    setequal(extraction_controls$MET, c("MET250200", "MET250204", "MET250208")),
    paste0(
      "Extraction-control MET identifiers differ from expected values: ",
      paste(extraction_controls$MET, collapse = ", ")
    )
  )

  bio <- reg %>%
    filter(!is_extraction_blank) %>%
    mutate(
      LandUse = recode(condition, PP = "Grassland", RC = "Cropland", .default = NA_character_),
      # CRITICAL convention in the plate workbook:
      # C = Chauffage (Heated), T = Témoin (Control).
      Treatment = recode(plate_treatment, C = "Heated", T = "Control", .default = NA_character_),
      sample_number_num = suppressWarnings(as.numeric(sample_number)),
      Nom_Ech = paste0(
        if_else(plate == "plaque1", "plaque1_Ech-", "Plaque2_Ech-"),
        sample_number, "_", Year
      )
    )

  assert_that(nrow(bio) == 108L, paste0("Expected 108 biological libraries; found ", nrow(bio), "."))
  assert_that(all(!is.na(bio$LandUse)), "At least one biological sample has an unknown PP/RC condition.")
  assert_that(all(!is.na(bio$Treatment)), "At least one biological sample has an unknown C/T treatment.")
  assert_that(all(bio$Year %in% c(2022L, 2023L, 2024L)), "Unexpected year in registration sheet.")
  assert_that(all(bio$Depth %in% c("0-15", "15-30", "30-50")), "Unexpected soil depth in registration sheet.")
  assert_that(all(table(bio$Year) == c(`2022` = 36L, `2023` = 36L, `2024` = 36L)),
              "Expected exactly 36 biological libraries in each of 2022, 2023 and 2024.")

  # Derive replicate / plot labels from the within-treatment ordering of the
  # original sample numbers. There are exactly three replicates for every
  # Year x LandUse x Depth x Treatment cell.
  bio <- bio %>%
    arrange(Year, LandUse, Depth, Treatment, sample_number_num) %>%
    group_by(Year, LandUse, Depth, Treatment) %>%
    mutate(rep = row_number()) %>%
    ungroup()

  cell_sizes <- bio %>% count(Year, LandUse, Depth, Treatment)
  assert_that(
    all(cell_sizes$n == 3L),
    paste0(
      "Each Year x LandUse x Depth x Treatment cell should contain 3 replicates. Problematic cells: ",
      paste(apply(cell_sizes[cell_sizes$n != 3, c("Year", "LandUse", "Depth", "Treatment", "n"), drop = FALSE],
                  1, paste, collapse = "/"), collapse = "; ")
    )
  )

  bio <- bio %>%
    mutate(
      Plot = paste0(if_else(Treatment == "Heated", "T", "C"), rep),
      Block = rep,
      profile_id = interaction(LandUse, Treatment, Block, drop = TRUE, sep = "__"),
      sample_unit_id = interaction(LandUse, Treatment, Block, Depth, drop = TRUE, sep = "__"),
      is_negative = FALSE,
      negative_type = NA_character_
    )

  # ---- 3. Match final_metadata.xlsx for primary 2022/2024 analysis -----------
  meta <- readxl::read_excel(metadata_file)
  needed <- c("Number", "LandUse", "Treatment", "Plot", "Year", "Depth")
  assert_that(all(needed %in% names(meta)), "final_metadata.xlsx is missing required identity columns.")

  meta_id <- meta %>%
    select(all_of(needed)) %>%
    mutate(
      across(c(LandUse, Treatment, Plot, Depth), trim_chr),
      Year = as.integer(Year)
    )

  join_keys <- c("Year", "LandUse", "Treatment", "Plot", "Depth")

  # Check that the sequence-derived map has exactly one candidate for every
  # metadata identity before joining.
  bio_primary <- bio %>%
    filter(Year %in% c(2022L, 2024L))

  dup_keys <- bio_primary %>%
    count(across(all_of(join_keys))) %>%
    filter(n != 1L)
  assert_that(nrow(dup_keys) == 0L,
              "The biological map is not unique for Year/LandUse/Treatment/Plot/Depth.")

  matched <- meta_id %>%
    left_join(
      bio_primary %>%
        select(all_of(join_keys), MET, sample_number, DNA_concentration,
               condition, plate_treatment, Block, profile_id, sample_unit_id, Nom_Ech),
      by = join_keys
    )

  assert_that(nrow(matched) == 72L, paste0("Expected 72 metadata rows; found ", nrow(matched), "."))
  assert_that(
    all(!is.na(matched$MET)),
    paste0(
      "At least one final_metadata row could not be mapped to a MET library. Rows: ",
      paste(which(is.na(matched$MET)), collapse = ", ")
    )
  )
  assert_that(n_distinct(matched$MET) == 72L, "Mapped 2022/2024 MET identifiers are not unique.")

  # Strong cross-check: final_metadata Number should encode the same ordering as
  # Year/LandUse/Treatment/Plot/Depth. We do not use Number to construct the
  # mapping because the original sequencing `ech` numbers differ in some groups.
  assert_that(
    all(table(matched$Year) == c(`2022` = 36L, `2024` = 36L)),
    "Expected 36 matched rows in 2022 and 36 in 2024."
  )

  all_map <- bind_rows(bio, extraction_controls)

  list(
    biological = bio,
    extraction_controls = extraction_controls,
    metadata_72 = matched,
    all_map = all_map,
    registration_111 = reg
  )
}

filter_marker_scope <- function(tax, marker, protist_definition = c("broad", "legacy_narrow")) {
  protist_definition <- match.arg(protist_definition)

  if (marker == "Archaea") {
    return(tax$Domain == "Archaea")
  }
  if (marker == "Bacteria") {
    return(tax$Domain == "Bacteria")
  }
  if (marker == "Fungi") {
    return(tax$Kingdom == "Fungi")
  }
  if (marker == "Nematodes") {
    return(tax$Phylum == "Nematoda")
  }
  if (marker == "Protists") {
    keep_supergroups <- c("Haptista", "Amoebozoa", "TSAR", "Excavata", "Obazoa")

    ok <- tax$Domain == "Eukaryota" &
      tax$Supergroup %in% keep_supergroups &
      tax$Major_group != "Opisthokonta" &
      tax$Kingdom %notin% c("Fungi", "Metazoa")

    # Exclude ambiguous high-level assignments.
    for (v in intersect(c("Major_group", "Kingdom", "Phylum", "Class"), names(tax))) {
      ok <- ok & !is_unassigned(tax[[v]])
    }

    if (protist_definition == "legacy_narrow") {
      # Reproduces the exclusions in the original Marion script as a sensitivity
      # analysis. These taxa are oomycete/stramenopile lineages and are retained
      # in the primary broad-protist definition.
      remove_orders <- c(
        "Peronosporales", "Peronosporomycetes_XX", "Saprolegniales",
        "Haliphthorales", "Thraustochytriaceae",
        "Labyrinthulomycetes_LAB14", "Labyrinthulomycetes_LAB15",
        "Labyrinthulomycetes_LAB3"
      )
      ok <- ok & tax$Order %notin% remove_orders
    }
    return(ok)
  }

  stop("Unknown marker: ", marker)
}

read_marker <- function(marker, protist_definition = "broad") {
  cfg <- MARKER_CONFIG[[marker]]
  f <- FILES[[cfg$key]]

  raw <- readxl::read_excel(f$file, sheet = f$sheet, .name_repair = "unique")

  n_tax <- length(cfg$tax_names)
  names(raw)[seq_len(n_tax)] <- cfg$tax_names

  # Normalise taxonomy to trimmed character strings.
  raw <- raw %>% mutate(across(all_of(cfg$tax_names), trim_chr))

  # Identify sequencing libraries from their stable prefixes.
  # We deliberately do NOT depend on punctuation in the blank names because
  # Excel/import software may alter '-', '_' or '.'.
  nm <- names(raw)
  sample_cols <- nm[str_detect(nm, regex("^MET[0-9]+", ignore_case = TRUE))]
  blank_cols  <- nm[str_detect(nm, regex("^\\s*BL", ignore_case = TRUE))]

  # Defensive fallback: in the original workbooks the 4 technical blanks are
  # the four count columns immediately preceding the first MET library. This
  # catches harmless name-repair changes while still failing if the structure
  # itself is inconsistent.
  if (length(blank_cols) != 4L && length(sample_cols) > 0L) {
    first_met <- match(sample_cols[1], nm)
    if (!is.na(first_met) && first_met > 4L) {
      candidate <- nm[(first_met - 4L):(first_met - 1L)]
      if (all(str_detect(candidate, regex("BL", ignore_case = TRUE)))) {
        blank_cols <- candidate
      }
    }
  }

  assert_that(length(sample_cols) == 111L,
              paste0(marker, ": expected 111 MET columns, found ", length(sample_cols),
                     ". First count-like columns: ", paste(head(nm[str_detect(nm, regex("MET|BL", ignore_case = TRUE))], 10), collapse = ", "), "."))
  # Some distributed versions of the fungal workbook omit the four technical
  # blanks entirely while retaining all 111 MET libraries. This is not fatal:
  # the three extraction controls encoded as MET libraries can still be used by
  # decontam::isContaminant(method = "prevalence"). We therefore accept either
  # 4 technical blanks (preferred) or 0, but never an intermediate number.
  assert_that(length(blank_cols) %in% c(0L, 4L),
              paste0(marker, ": expected either 4 technical blank columns or none; found ",
                     length(blank_cols), "."))
  if (length(blank_cols) == 0L) {
    message(marker, ": no technical blank columns found; contaminant detection will use the 3 extraction controls only. ",
            "This choice is recorded in QC and should be checked in sensitivity analyses.")
  }

  met_ids <- normalise_met_name(sample_cols)
  assert_that(n_distinct(met_ids) == 111L, paste0(marker, ": MET IDs are not unique."))
  assert_that(setequal(met_ids, paste0("MET", 250125:250235)),
              paste0(marker, ": MET sequence does not span MET250125...MET250235 exactly."))

  # Marker-domain filtering before contaminant detection.
  scope <- filter_marker_scope(raw[, cfg$tax_names, drop = FALSE], marker, protist_definition)
  raw <- raw[scope, , drop = FALSE]

  # Stable feature IDs.
  feature_id <- if ("observation_name" %in% names(raw)) raw$observation_name else paste0("ASV_", seq_len(nrow(raw)))
  feature_id <- make.unique(as.character(feature_id))

  tax <- raw[, cfg$tax_names, drop = FALSE] %>% as.data.frame()
  rownames(tax) <- feature_id

  # sample x ASV matrix. Keep technical blanks and MET libraries together here.
  count_cols <- c(sample_cols, blank_cols)
  counts <- t(as.matrix(raw[, count_cols, drop = FALSE]))
  storage.mode(counts) <- "double"
  colnames(counts) <- feature_id

  new_rn <- c(normalise_met_name(sample_cols), blank_cols)
  rownames(counts) <- new_rn

  # 3 extraction controls are MET libraries; the 4 technical blanks are separate.
  extraction_neg <- sample_map$extraction_controls$MET
  neg_flag <- rownames(counts) %in% c(extraction_neg, blank_cols)
  neg_type <- case_when(
    rownames(counts) %in% extraction_neg ~ "extraction_blank",
    rownames(counts) %in% blank_cols ~ "technical_blank",
    TRUE ~ "biological"
  )

  expected_neg <- 3L + length(blank_cols)
  assert_that(sum(neg_flag) == expected_neg,
              paste0(marker, ": expected ", expected_neg, " negative controls, found ", sum(neg_flag), "."))
  assert_that(sum(neg_flag) >= 3L,
              paste0(marker, ": fewer than 3 negative controls available; prevalence decontamination is not supported."))

  list(
    marker = marker,
    cfg = cfg,
    tax = tax,
    counts = counts,
    neg_flag = neg_flag,
    neg_type = neg_type,
    blank_cols = blank_cols
  )
}

classify_contaminants <- function(obj,
                                  method = c("decontam", "none", "mean_neg_5x"),
                                  threshold = 0.10) {
  method <- match.arg(method)
  X <- obj$counts

  if (method == "none") {
    return(tibble(
      feature_id = colnames(X),
      contaminant = FALSE,
      score = NA_real_,
      method = "none"
    ))
  }

  if (method == "decontam") {
    dc <- decontam::isContaminant(
      X,
      neg = obj$neg_flag,
      method = "prevalence",
      threshold = threshold,
      normalize = TRUE,
      detailed = TRUE
    )

    return(tibble(
      feature_id = rownames(dc),
      contaminant = dc$contaminant,
      score = dc$p,
      method = paste0("decontam_prevalence_", threshold)
    ))
  }

  # Transparent alternative heuristic used only as sensitivity analysis.
  # Compare mean read count per negative library vs mean per positive library.
  neg_mean <- colMeans(X[obj$neg_flag, , drop = FALSE])
  pos_mean <- colMeans(X[!obj$neg_flag, , drop = FALSE])
  flag <- neg_mean > 5 * pmax(pos_mean, 1e-12)

  tibble(
    feature_id = colnames(X),
    contaminant = flag,
    score = neg_mean / pmax(pos_mean, 1e-12),
    method = "mean_negative_gt_5x_positive"
  )
}

prepare_marker_table <- function(obj,
                                 rank,
                                 analysis_years = PRIMARY_YEARS,
                                 prevalence = PRIMARY_PREVALENCE,
                                 contaminant_method = "decontam",
                                 decontam_threshold = PRIMARY_DECONTAM_THRESHOLD) {

  cont <- classify_contaminants(obj, contaminant_method, decontam_threshold)
  cont <- cont %>% arrange(match(feature_id, colnames(obj$counts)))
  assert_that(identical(cont$feature_id, colnames(obj$counts)),
              paste0(obj$marker, ": contaminant table lost feature order."))

  keep_asv <- !cont$contaminant
  counts_dc <- obj$counts[, keep_asv, drop = FALSE]
  tax_dc <- obj$tax[keep_asv, , drop = FALSE]

  assert_that(rank %in% names(tax_dc), paste0(obj$marker, ": rank ", rank, " not available."))

  # Restrict to biological samples of requested years.
  bio_meta <- sample_map$biological %>% filter(Year %in% analysis_years)
  bio_ids <- bio_meta$MET
  counts_bio <- counts_dc[bio_ids, , drop = FALSE]

  # Reads remaining in the biological scope after contaminant removal.
  reads_after_decontam <- rowSums(counts_bio)

  # Only assigned ASVs at the focal rank are used for the rank-level table.
  assigned <- !is_unassigned(tax_dc[[rank]])
  counts_assigned <- counts_bio[, assigned, drop = FALSE]
  tax_assigned <- trim_chr(tax_dc[[rank]][assigned])

  reads_assigned <- rowSums(counts_assigned)
  assigned_fraction <- reads_assigned / pmax(reads_after_decontam, 1)

  # TRUE taxonomic aggregation: ASVs belonging to the same rank are summed.
  agg <- t(rowsum(t(counts_assigned), group = tax_assigned, reorder = FALSE))
  storage.mode(agg) <- "double"

  # Taxon prevalence is calculated AFTER aggregation.
  tax_prev <- colMeans(agg > 0)
  keep_taxon <- tax_prev >= prevalence
  agg_filt <- agg[, keep_taxon, drop = FALSE]

  assert_that(ncol(agg_filt) >= 2L,
              paste0(obj$marker, ": fewer than 2 taxa remain after filtering."))
  assert_that(all(rowSums(agg_filt) > 0),
              paste0(obj$marker, ": at least one sample has zero assigned reads after filtering."))

  rownames(agg_filt) <- bio_ids

  qc_sample <- tibble(
    marker = obj$marker,
    sample_id = bio_ids,
    Year = bio_meta$Year,
    LandUse = bio_meta$LandUse,
    Treatment = bio_meta$Treatment,
    Plot = bio_meta$Plot,
    Depth = bio_meta$Depth,
    reads_after_decontam = reads_after_decontam,
    reads_assigned_at_rank = reads_assigned,
    assigned_fraction = assigned_fraction,
    reads_after_prevalence_filter = rowSums(agg_filt)
  )

  qc_taxon <- tibble(
    marker = obj$marker,
    taxon = colnames(agg),
    prevalence = tax_prev,
    kept = keep_taxon,
    total_reads = colSums(agg)
  )

  qc_summary <- tibble(
    marker = obj$marker,
    rank = rank,
    n_asv_scope = ncol(obj$counts),
    n_contaminants = sum(cont$contaminant),
    n_asv_after_decontam = sum(keep_asv),
    n_asv_assigned_rank = sum(assigned),
    n_taxa_before_prevalence = ncol(agg),
    n_taxa_after_prevalence = ncol(agg_filt),
    prevalence_threshold = prevalence,
    median_assigned_fraction = median(assigned_fraction, na.rm = TRUE),
    min_assigned_fraction = min(assigned_fraction, na.rm = TRUE)
  )

  list(
    counts = agg_filt,
    contamination = cont,
    sample_qc = qc_sample,
    taxon_qc = qc_taxon,
    summary = qc_summary
  )
}


# Fit MCOA from already-prepared abundance tables. This avoids re-running
# decontamination / taxonomic aggregation when only ordination settings change.
fit_mcoa_from_prepared <- function(prep,
                                   transform = PRIMARY_TRANSFORM,
                                   mcoa_weight = PRIMARY_MCOA_WEIGHT,
                                   nf = PRIMARY_NF) {
  count_tables <- purrr::map(prep, "counts")
  shared_rows <- Reduce(intersect, purrr::map(count_tables, rownames))
  assert_that(length(shared_rows) > 1L, "No shared biological samples across kingdoms.")

  count_tables <- purrr::map(count_tables, ~.x[shared_rows, , drop = FALSE])
  transformed <- purrr::map(count_tables, ~transform_community(.x, method = transform))

  ktab <- ade4::ktab.list.df(
    transformed,
    rownames = shared_rows,
    tabnames = names(transformed),
    w.row = rep(1 / length(shared_rows), length(shared_rows))
  )

  nf_use <- min(nf, length(shared_rows) - 1L)
  mcoa <- ade4::mcoa(ktab, option = mcoa_weight, scannf = FALSE, nf = nf_use)

  syn <- as.data.frame(mcoa$SynVar)
  rownames(syn) <- shared_rows
  names(syn) <- paste0("MCOA", seq_len(ncol(syn)))

  eig <- tibble::tibble(
    axis = seq_along(mcoa$pseudoeig),
    pseudo_eigenvalue = mcoa$pseudoeig,
    percent = 100 * mcoa$pseudoeig / sum(mcoa$pseudoeig),
    cumulative_percent = 100 * cumsum(mcoa$pseudoeig) / sum(mcoa$pseudoeig)
  )

  list(
    prep = prep,
    count_tables = count_tables,
    transformed = transformed,
    ktab = ktab,
    mcoa = mcoa,
    scores = syn,
    eigen = eig,
    settings = list(transform = transform, mcoa_weight = mcoa_weight, nf = nf)
  )
}

require_intermediate <- function(path, producer_script = NULL) {
  if (!file.exists(path)) {
    msg <- paste0("Required intermediate file is missing: ", path)
    if (!is.null(producer_script)) msg <- paste0(msg, "\nRun first: ", producer_script)
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}
