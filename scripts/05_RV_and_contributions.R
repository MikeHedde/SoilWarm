# ==============================================================================
# 05_RV_and_contributions.R
# SOILWARM — inter-kingdom coherence and MCOA contributions
# 3-year version: exact whole-profile restricted permutations
# ==============================================================================

source("R/config.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(readxl)
  library(vegan)
  library(ade4)
})

source("R/functions_soilwarm.R")

# ------------------------------------------------------------------------------
# 1. Inputs
# ------------------------------------------------------------------------------

require_intermediate(
  file.path(INT_DIR, "04_primary_MCOA.rds"),
  "scripts/04_primary_MCOA.R"
)
require_intermediate(
  file.path(INT_DIR, "04_primary_scores_with_metadata.rds"),
  "scripts/04_primary_MCOA.R"
)

primary <- readRDS(file.path(INT_DIR, "04_primary_MCOA.rds"))
mcoa_meta <- readRDS(file.path(INT_DIR, "04_primary_scores_with_metadata.rds"))

if (is.null(primary$transformed)) stop("primary$transformed is missing.")
if (length(primary$transformed) < 2) stop("At least two kingdom tables are required.")

# ------------------------------------------------------------------------------
# 2. Align metadata to transformed-table row order
# ------------------------------------------------------------------------------

row_ids <- rownames(primary$transformed[[1]])
if (is.null(row_ids)) stop("Transformed tables must have sample IDs as row names.")

same_rows <- vapply(
  primary$transformed,
  function(x) identical(rownames(x), row_ids),
  logical(1)
)
if (!all(same_rows)) {
  stop("Transformed tables do not share exactly the same row order: ",
       paste(names(same_rows)[!same_rows], collapse = ", "))
}

if (!"MET" %in% names(mcoa_meta)) {
  stop("04_primary_scores_with_metadata.rds must contain MET.")
}

mcoa_meta <- mcoa_meta %>%
  mutate(
    MET = as.character(MET),
    profile_id = as.character(profile_id),
    Year = as.integer(as.character(Year)),
    Depth = as.character(Depth),
    LandUse = as.character(LandUse),
    Treatment = as.character(Treatment),
    Block = as.character(Block)
  )


if (nrow(mcoa_meta) != 108L) {
  stop("Script 05 expects 108 MCOA rows; found ", nrow(mcoa_meta), ".")
}

if (!identical(sort(unique(mcoa_meta$Year)), c(2022L, 2023L, 2024L))) {
  stop(
    "Script 05 expects years 2022, 2023 and 2024; found: ",
    paste(sort(unique(mcoa_meta$Year)), collapse = ", ")
  )
}

profile_layout_qc <- mcoa_meta %>%
  count(profile_id, name = "n_rows")

if (any(profile_layout_qc$n_rows != 9L)) {
  stop(
    "Each profile must contain 9 rows (3 years x 3 depths). Problematic profiles: ",
    paste(profile_layout_qc$profile_id[profile_layout_qc$n_rows != 9L], collapse = ", ")
  )
}

idx_meta <- match(as.character(row_ids), mcoa_meta$MET)
if (anyNA(idx_meta)) stop("Could not match all transformed-table rows to MET metadata.")

meta_aligned <- mcoa_meta[idx_meta, , drop = FALSE]
stopifnot(identical(as.character(meta_aligned$MET), as.character(row_ids)))

required_design <- c("profile_id", "Year", "Depth", "LandUse", "Treatment", "Block")
missing_design <- setdiff(required_design, names(meta_aligned))
if (length(missing_design) > 0) {
  stop("Missing design variables: ", paste(missing_design, collapse = ", "))
}

# ------------------------------------------------------------------------------
# 3. RV coefficient
# ------------------------------------------------------------------------------

rv_coefficient_05 <- function(A, B) {
  A <- scale(as.matrix(A), center = TRUE, scale = FALSE)
  B <- scale(as.matrix(B), center = TRUE, scale = FALSE)
  GA <- A %*% t(A)
  GB <- B %*% t(B)
  den <- sqrt(sum(GA * GA) * sum(GB * GB))
  if (!is.finite(den) || den == 0) return(NA_real_)
  sum(GA * GB) / den
}

# ------------------------------------------------------------------------------
# 4. Exact whole-profile permutations within LandUse x Treatment
#    All 3 years x 3 depths remain together and are matched exactly.
# ------------------------------------------------------------------------------

all_permutations_05 <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1) return(matrix(x, nrow = 1))
  do.call(rbind, lapply(seq_along(x), function(i) {
    cbind(x[i], all_permutations_05(x[-i]))
  }))
}

build_profile_permutations_05 <- function(
  metadata,
  strata_vars = c("LandUse", "Treatment"),
  match_vars = c("Year", "Depth"),
  max_exact = 20000L,
  n_monte_carlo = 9999L,
  seed = 505
) {
  required <- c("profile_id", strata_vars, match_vars)
  missing <- setdiff(required, names(metadata))
  if (length(missing) > 0) {
    stop("Missing permutation variables: ", paste(missing, collapse = ", "))
  }

  meta <- metadata %>%
    mutate(
      profile_id = as.character(profile_id),
      across(all_of(c(strata_vars, match_vars)), as.character)
    )

  layout <- meta %>%
    count(profile_id, across(all_of(match_vars)), name = "n")
  if (any(layout$n != 1L)) {
    stop("Each profile must contain exactly one row per Year x Depth combination.")
  }

  profile_info <- meta %>%
    distinct(profile_id, across(all_of(strata_vars)))
  if (anyDuplicated(profile_info$profile_id)) {
    stop("A profile belongs to more than one permutation stratum.")
  }

  profile_info$.stratum <- do.call(
    interaction,
    c(
      profile_info[strata_vars],
      list(drop = TRUE, lex.order = TRUE)
    )
  )
  profiles_by_stratum <- split(profile_info$profile_id, profile_info$.stratum)
  profiles_by_stratum <- lapply(profiles_by_stratum, as.character)
  n_per_stratum <- vapply(profiles_by_stratum, length, integer(1))
  n_possible <- prod(factorial(n_per_stratum))

  make_key <- function(profile_vec, dat) {
    do.call(paste, c(list(profile_vec), dat[match_vars], sep = "|||"))
  }
  source_key <- make_key(meta$profile_id, meta)
  if (anyDuplicated(source_key)) {
    stop("profile_id x Year x Depth does not uniquely identify rows.")
  }

  build_index <- function(maps) {
    target_profile <- meta$profile_id
    for (s in names(maps)) {
      mp <- maps[[s]]
      for (p in names(mp)) {
        target_profile[meta$profile_id == p] <- unname(mp[p])
      }
    }
    idx <- match(make_key(target_profile, meta), source_key)
    if (anyNA(idx)) stop("Could not map a profile permutation back to rows.")
    if (length(unique(idx)) != nrow(meta)) stop("Permutation mapping is not one-to-one.")
    idx
  }

  if (n_possible <= max_exact) {
    perm_mats <- lapply(profiles_by_stratum, all_permutations_05)
    combo_grid <- expand.grid(
      lapply(perm_mats, function(x) seq_len(nrow(x))),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    perm_idx <- matrix(NA_integer_, nrow = nrow(combo_grid), ncol = nrow(meta))

    for (i in seq_len(nrow(combo_grid))) {
      maps <- vector("list", length(profiles_by_stratum))
      names(maps) <- names(profiles_by_stratum)
      for (j in seq_along(profiles_by_stratum)) {
        src <- profiles_by_stratum[[j]]
        tgt <- perm_mats[[j]][combo_grid[i, j], , drop = TRUE]
        maps[[j]] <- setNames(as.character(tgt), src)
      }
      perm_idx[i, ] <- build_index(maps)
    }
    method <- "exact"
  } else {
    set.seed(seed)
    perm_idx <- matrix(NA_integer_, nrow = n_monte_carlo, ncol = nrow(meta))
    for (i in seq_len(n_monte_carlo)) {
      maps <- lapply(profiles_by_stratum, function(p) {
        setNames(sample(p, length(p), replace = FALSE), p)
      })
      perm_idx[i, ] <- build_index(maps)
    }
    method <- "monte_carlo"
  }

  list(
    indices = perm_idx,
    qc = tibble(
      method = method,
      n_rows = nrow(meta),
      n_profiles = n_distinct(meta$profile_id),
      n_strata = length(profiles_by_stratum),
      profiles_per_stratum = paste(n_per_stratum, collapse = ";"),
      theoretical_permutations = n_possible,
      generated_permutations = nrow(perm_idx),
      strata = paste(strata_vars, collapse = " x "),
      matched_within_profile = paste(match_vars, collapse = " x ")
    )
  )
}

perm_obj <- build_profile_permutations_05(
  meta_aligned,
  strata_vars = c("LandUse", "Treatment"),
  match_vars = c("Year", "Depth"),
  max_exact = 20000L,
  n_monte_carlo = if (exists("N_RV_PERM")) N_RV_PERM else 9999L,
  seed = 505
)
perm_idx <- perm_obj$indices

message(
  "Script 05 permutation scheme: ", perm_obj$qc$method,
  "; ", perm_obj$qc$generated_permutations,
  " profile-level permutations."
)

# ------------------------------------------------------------------------------
# 5. Restricted RV test
# ------------------------------------------------------------------------------

rv_test_restricted_05 <- function(A, B, permutation_indices) {
  obs <- rv_coefficient_05(A, B)
  null <- apply(permutation_indices, 1, function(idx) {
    rv_coefficient_05(A, B[idx, , drop = FALSE])
  })
  tibble(
    RV = obs,
    p_perm = mean(null >= obs - 1e-12, na.rm = TRUE),
    n_perm = length(null)
  )
}

# ------------------------------------------------------------------------------
# 6. Design-adjusted sensitivity analysis
# ------------------------------------------------------------------------------

residualize_community_05 <- function(Y, metadata) {
  design <- metadata %>%
    transmute(
      Year = factor(Year),
      Treatment = factor(Treatment),
      LandUse = factor(LandUse),
      Depth = factor(Depth),
      Block = factor(Block)
    )
  X_design <- model.matrix(
    ~ Year * Treatment * LandUse * Depth + Block,
    data = design
  )
  Y <- as.matrix(Y)
  out <- apply(Y, 2, function(y) lm.fit(x = X_design, y = y)$residuals)
  if (is.vector(out)) out <- matrix(out, ncol = 1)
  colnames(out) <- colnames(Y)
  rownames(out) <- rownames(Y)
  out
}

transformed_residual <- lapply(
  primary$transformed,
  residualize_community_05,
  metadata = meta_aligned
)

# ------------------------------------------------------------------------------
# 7. Pairwise RV tests
# ------------------------------------------------------------------------------

marker_pairs <- combn(names(primary$transformed), 2, simplify = FALSE)

rv_raw <- purrr::map_dfr(marker_pairs, function(p) {
  rv_test_restricted_05(
    primary$transformed[[p[1]]],
    primary$transformed[[p[2]]],
    perm_idx
  ) %>%
    mutate(
      marker1 = p[1],
      marker2 = p[2],
      adjustment = "raw",
      permutation = "whole_profile_within_LandUse_x_Treatment_matched_Year_x_Depth",
      .before = 1
    )
}) %>%
  mutate(p_FDR = p.adjust(p_perm, method = "BH"))

rv_adjusted <- purrr::map_dfr(marker_pairs, function(p) {
  rv_test_restricted_05(
    transformed_residual[[p[1]]],
    transformed_residual[[p[2]]],
    perm_idx
  ) %>%
    mutate(
      marker1 = p[1],
      marker2 = p[2],
      adjustment = "residual_Year_x_Treatment_x_LandUse_x_Depth_plus_Block",
      permutation = "whole_profile_within_LandUse_x_Treatment_matched_Year_x_Depth",
      .before = 1
    )
}) %>%
  mutate(p_FDR = p.adjust(p_perm, method = "BH"))

rv_results <- bind_rows(rv_raw, rv_adjusted)

# ------------------------------------------------------------------------------
# 8. MCOA contributions
# ------------------------------------------------------------------------------

cov2 <- as.data.frame(primary$mcoa$cov2)
cov2$marker <- rownames(cov2)
cov2 <- cov2 %>% relocate(marker)

taxon_coordinates <- as.data.frame(primary$mcoa$Tco)
expected_taxon_rows <- sum(vapply(primary$transformed, ncol, integer(1)))

if (nrow(taxon_coordinates) == expected_taxon_rows) {
  marker_vec <- rep(
    names(primary$transformed),
    times = vapply(primary$transformed, ncol, integer(1))
  )
  taxon_vec <- unlist(
    lapply(primary$transformed, colnames),
    use.names = FALSE
  )
  taxon_coordinates <- taxon_coordinates %>%
    mutate(marker = marker_vec, taxon = taxon_vec, .before = 1)
} else {
  warning("Could not reconstruct marker/taxon labels from Tco dimensions.")
  taxon_coordinates <- taxon_coordinates %>%
    rownames_to_column("taxon_key")
}

# ------------------------------------------------------------------------------
# 9. Exports
# ------------------------------------------------------------------------------

dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(INT_DIR, recursive = TRUE, showWarnings = FALSE)

write.csv(
  perm_obj$qc,
  file.path(TABLE_DIR, "05_permutation_scheme_QC.csv"),
  row.names = FALSE
)
write.csv(
  rv_results,
  file.path(TABLE_DIR, "05_pairwise_RV_permutation_tests.csv"),
  row.names = FALSE
)
write.csv(
  cov2,
  file.path(TABLE_DIR, "05_MCOA_table_covariances.csv"),
  row.names = FALSE
)
write.csv(
  taxon_coordinates,
  file.path(TABLE_DIR, "05_MCOA_taxon_coordinates.csv"),
  row.names = FALSE
)

analysis_05 <- list(
  permutation_QC = perm_obj$qc,
  permutation_indices = perm_idx,
  metadata_aligned = meta_aligned,
  pairwise_RV = rv_results,
  transformed_residual = transformed_residual,
  table_covariances = cov2,
  taxon_coordinates = taxon_coordinates
)

saveRDS(
  analysis_05,
  file.path(INT_DIR, "05_RV_and_contributions.rds")
)

message("Script 05 completed with profile-level restricted permutations.")
message("Raw pairwise RV tests:")
print(rv_raw %>% select(marker1, marker2, RV, p_perm, p_FDR))
message("Design-adjusted pairwise RV tests:")
print(rv_adjusted %>% select(marker1, marker2, RV, p_perm, p_FDR))

# ==============================================================================
# END
# ==============================================================================
