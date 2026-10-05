# ==============================================================================
# 01b_build_metadata_final.R
# SOILWARM — audited reconstruction of the 3-year analysis metadata
# ==============================================================================
# AIM
# ---
# Attach laboratory N-cycle measurements to the canonical sample identity map.
#
# CANONICAL IDENTITY
# ------------------
# intermediate/01_sample_map.rds is the ONLY source of MET, Year, LandUse,
# Treatment, Plot, Block, Depth, profile_id and sample_unit_id.
#
# LABORATORY SOURCE RULES
# -----------------------
# qPCR:
#   - its experimental descriptor grid is matched to the canonical sample map;
#   - raw C = Chauffage/Heated, T = Témoin/Control.
#
# DEA:
#   - values are attached by Year + Lab_Ech;
#   - supplied DEA Rep labels contain a known shift in part of 2022 and are NOT
#     used to define experimental identity;
#   - other DEA descriptors are checked against qPCR and must agree.
#
# NEA:
#   - values are attached by the full experimental descriptor key;
#   - NEA Ech is NOT used as identity because it is shifted in part of 2023/2024.
#
# IMPORTANT
# ---------
# - azote.csv and azotelm.csv are deliberately not used.
# - corrected laboratory columns are used for the analysis table.
# - no missing value is silently imputed.
# - Nitrate and nosZ2 are not included because no complete 3-year source has
#   yet been supplied.
#
# INPUTS
# ------
# intermediate/01_sample_map.rds
# data/raw/qPCR_2022_2023_2024.csv  [or duplicate-name variant "(1)"]
# data/raw/DEA_2022_2023_2024.csv
# data/raw/NEA_2022_2023_2024.csv
#
# OUTPUTS
# -------
# intermediate/01b_metadata_final.rds
# results/tables/metadata_final.csv
# results/tables/01b_metadata_qc.csv
# results/tables/01b_DEA_rep_mismatches.csv
# ==============================================================================

source("R/config.R")
source("R/functions_soilwarm.R")

require_intermediate(
  file.path(INT_DIR, "01_sample_map.rds"),
  "scripts/01_build_sample_map.R"
)

sample_map <- readRDS(
  file.path(INT_DIR, "01_sample_map.rds")
)

check <- function(ok, msg) {
  if (!isTRUE(ok)) stop(msg, call. = FALSE)
}

# ---- 1. Raw source paths ------------------------------------------------------
resolve_qpcr_file <- function() {
  candidates <- file.path(
    RAW_DIR,
    c("qPCR_2022_2023_2024.csv", "qPCR_2022_2023_2024(1).csv")
  )
  present <- candidates[file.exists(candidates)]

  if (length(present) == 0L) {
    stop(
      "Missing qPCR source. Expected one of:\n - ",
      paste(candidates, collapse = "\n - "),
      call. = FALSE
    )
  }

  if (length(present) > 1L) {
    warning(
      "Both qPCR source filenames are present. Using: ",
      present[[1]],
      call. = FALSE
    )
  }

  present[[1]]
}

qpcr_file <- resolve_qpcr_file()
dea_file <- file.path(RAW_DIR, "DEA_2022_2023_2024.csv")
nea_file <- file.path(RAW_DIR, "NEA_2022_2023_2024.csv")

missing_lab <- c(dea_file, nea_file)[!file.exists(c(dea_file, nea_file))]
if (length(missing_lab) > 0L) {
  stop(
    "Missing laboratory source file(s):\n - ",
    paste(missing_lab, collapse = "\n - "),
    call. = FALSE
  )
}

depth_keep <- c("0-15", "15-30", "30-50")
design_key <- c("Year", "LandUse", "Treatment", "Depth", "Block")

bio <- sample_map$biological |>
  dplyr::select(
    Year, LandUse, Treatment, Depth, Block, Plot,
    MET, sample_number_num, DNA_concentration,
    profile_id, sample_unit_id
  ) |>
  dplyr::rename(Plate_Ech = sample_number_num)

check(nrow(bio) == 108L, "01_sample_map.rds does not contain 108 biological rows.")

# ---- 2. qPCR -----------------------------------------------------------------
qpcr <- readr::read_delim(
  qpcr_file,
  delim = ";",
  show_col_types = FALSE,
  name_repair = "minimal"
) |>
  dplyr::filter(Profondeur %in% depth_keep) |>
  dplyr::mutate(
    Year = as.integer(Annee),
    Lab_Ech = as.integer(Ech),
    LandUse = dplyr::recode(Type_prairie, PP = "Grassland", RC = "Cropland"),
    Treatment = dplyr::recode(Temperature, C = "Heated", T = "Control"),
    Depth = Profondeur,
    Block = as.integer(stringr::str_extract(Rep, "[0-9]+$")),
    Plot_lab = paste0(
      dplyr::if_else(Treatment == "Heated", "T", "C"),
      Block
    )
  )

check(nrow(qpcr) == 108L, paste0("Expected 108 qPCR rows at 0-50 cm; found ", nrow(qpcr), "."))
check(
  nrow(dplyr::distinct(qpcr, dplyr::across(dplyr::all_of(design_key)))) == 108L,
  "qPCR design keys are not unique."
)

missing_in_qpcr <- dplyr::anti_join(
  bio |> dplyr::select(dplyr::all_of(design_key)),
  qpcr |> dplyr::select(dplyr::all_of(design_key)),
  by = design_key
)
extra_in_qpcr <- dplyr::anti_join(
  qpcr |> dplyr::select(dplyr::all_of(design_key)),
  bio |> dplyr::select(dplyr::all_of(design_key)),
  by = design_key
)

check(
  nrow(missing_in_qpcr) == 0L && nrow(extra_in_qpcr) == 0L,
  "The qPCR 0-50 cm experimental grid does not match the canonical 108-sample map."
)

meta <- qpcr |>
  dplyr::left_join(
    bio,
    by = design_key
  )

check(
  nrow(meta) == 108L &&
    all(!is.na(meta$MET)) &&
    dplyr::n_distinct(meta$MET) == 108L,
  "qPCR rows could not be mapped one-to-one to the canonical MET identities."
)

# ---- 3. DEA ------------------------------------------------------------------
dea_raw <- readr::read_delim(
  dea_file,
  delim = ";",
  show_col_types = FALSE,
  name_repair = "minimal"
) |>
  dplyr::mutate(
    Year = as.integer(Annee),
    Lab_Ech = as.integer(Ech)
  )

check(
  nrow(dplyr::distinct(dea_raw, Year, Lab_Ech)) == nrow(dea_raw),
  "DEA Year + Ech keys are not unique."
)

# Identity audit: all descriptors except Rep must agree with qPCR for the same
# Year + Lab_Ech. Rep is retained only for auditing because part of 2022 is
# shifted in the supplied DEA file.
dea_identity_qc <- qpcr |>
  dplyr::select(
    Year, Lab_Ech,
    qpcr_Type = Type_prairie,
    qpcr_Depth = Profondeur,
    qpcr_Temperature = Temperature,
    qpcr_Acronyme = Acronyme,
    qpcr_Rep = Rep
  ) |>
  dplyr::left_join(
    dea_raw |>
      dplyr::select(
        Year, Lab_Ech,
        dea_Type = Type_prairie,
        dea_Depth = Profondeur,
        dea_Temperature = Temperature,
        dea_Acronyme = Acronyme,
        dea_Rep = Rep
      ),
    by = c("Year", "Lab_Ech")
  ) |>
  dplyr::mutate(
    core_descriptor_match =
      qpcr_Type == dea_Type &
      qpcr_Depth == dea_Depth &
      qpcr_Temperature == dea_Temperature &
      qpcr_Acronyme == dea_Acronyme,
    rep_match = qpcr_Rep == dea_Rep
  )

check(
  all(dea_identity_qc$core_descriptor_match),
  paste0(
    "DEA contains descriptor mismatches beyond the known Rep-label issue. ",
    "Inspect results before using DEA values."
  )
)

dea_rep_mismatches <- dea_identity_qc |>
  dplyr::filter(!rep_match)

dea <- dea_raw |>
  dplyr::transmute(
    Year,
    Lab_Ech,
    PGNP_raw = PGNP,
    PGNP = PGNP_corrige,
    PNNP_raw = PNNP,
    PNNP = PNNP_corrige,
    N2O_reduction_raw = N2O_reduction,
    N2O_reduction = N2O_reduction_corrige,
    PNNP_PGNP_raw = `PNNP/PGNP`,
    PNNP_PGNP = `PNNP/PGNP_corrige`
  )

meta <- meta |>
  dplyr::left_join(
    dea,
    by = c("Year", "Lab_Ech")
  )

# ---- 4. NEA ------------------------------------------------------------------
nea <- readr::read_delim(
  nea_file,
  delim = ";",
  show_col_types = FALSE,
  name_repair = "minimal"
) |>
  dplyr::filter(Profondeur %in% depth_keep) |>
  dplyr::transmute(
    Year = as.integer(Annee),
    LandUse = dplyr::recode(Type_prairie, PP = "Grassland", RC = "Cropland"),
    Treatment = dplyr::recode(Temperature, C = "Heated", T = "Control"),
    Depth = Profondeur,
    Block = as.integer(stringr::str_extract(Rep, "[0-9]+$")),
    NEA_Ech_source = as.integer(Ech),
    NEA_raw = NEA_50,
    NEA = NEA_50_corrige
  )

check(nrow(nea) == 108L, paste0("Expected 108 NEA rows at 0-50 cm; found ", nrow(nea), "."))
check(
  nrow(dplyr::distinct(nea, dplyr::across(dplyr::all_of(design_key)))) == 108L,
  "NEA experimental descriptor keys are not unique."
)

missing_in_nea <- dplyr::anti_join(
  bio |> dplyr::select(dplyr::all_of(design_key)),
  nea |> dplyr::select(dplyr::all_of(design_key)),
  by = design_key
)
extra_in_nea <- dplyr::anti_join(
  nea |> dplyr::select(dplyr::all_of(design_key)),
  bio |> dplyr::select(dplyr::all_of(design_key)),
  by = design_key
)

check(
  nrow(missing_in_nea) == 0L && nrow(extra_in_nea) == 0L,
  "The NEA 0-50 cm descriptor grid does not match the canonical 108-sample map."
)

meta <- meta |>
  dplyr::left_join(
    nea,
    by = design_key
  )

# ---- 5. Final analysis metadata -----------------------------------------------
metadata_final <- meta |>
  dplyr::arrange(Year, Lab_Ech) |>
  dplyr::mutate(Number = dplyr::row_number(), .before = 1) |>
  dplyr::transmute(
    Number,
    MET,
    Year,
    LandUse,
    Treatment,
    Plot,
    Block,
    Depth,
    profile_id,
    sample_unit_id,
    Lab_Ech,
    Plate_Ech,
    DNA_concentration,

    # Realised N-process block
    PGNP,
    PNNP,
    N2O_reduction,
    PNNP_PGNP,
    NEA,

    # Molecular functional-potential block
    AOA = AOA_corrige,
    AOB = AOB_corrige,
    ComA = ComA_corrige,
    ComB = ComB_corrige,
    nirS = nirS_corrige,
    nirK = nirK_corrige,

    # Source column is named NoZI in the laboratory file.
    # Imported here under the analysis name nosZ1; nomenclature should remain
    # documented in the manuscript/source-data notes.
    nosZ1 = NoZI_corrige
  ) |>
  dplyr::mutate(
    # Explicit land-use trajectory. The legacy LandUse column is retained for
    # backward compatibility with scripts already built on the two experimental
    # groups, but it must not be interpreted as the annual land-use state.
    LandUseTrajectory = dplyr::case_when(
      LandUse == "Grassland" ~ "Stable_PP",
      LandUse == "Cropland" ~ "TP_to_Crop",
      TRUE ~ NA_character_
    ),

    # Actual state observed in each year.
    LandUseState = dplyr::case_when(
      LandUseTrajectory == "Stable_PP" ~ "PermanentPasture",
      LandUseTrajectory == "TP_to_Crop" & Year == 2022L ~ "TemporaryPasture",
      LandUseTrajectory == "TP_to_Crop" & Year %in% c(2023L, 2024L) ~ "Crop",
      TRUE ~ NA_character_
    ),
    .after = LandUse
  )

# ---- 6. Structural and missing-data QC ----------------------------------------
check(nrow(metadata_final) == 108L, "metadata_final does not contain 108 rows.")
check(
  all(!is.na(metadata_final$LandUseTrajectory)) &&
    all(!is.na(metadata_final$LandUseState)),
  "LandUseTrajectory/LandUseState reconstruction contains missing values."
)

expected_landuse_states <- metadata_final |>
  dplyr::distinct(LandUseTrajectory, Year, LandUseState) |>
  dplyr::arrange(LandUseTrajectory, Year)

check(
  nrow(expected_landuse_states) == 6L,
  "Expected 6 LandUseTrajectory x Year state combinations."
)

check(
  identical(
    as.integer(table(metadata_final$Year)[c("2022", "2023", "2024")]),
    c(36L, 36L, 36L)
  ),
  "metadata_final does not contain 36 observations in each year."
)
check(
  dplyr::n_distinct(metadata_final$MET) == 108L,
  "metadata_final MET identifiers are not unique."
)
check(
  dplyr::n_distinct(metadata_final$profile_id) == 12L,
  "metadata_final should contain 12 repeated profiles."
)
check(
  dplyr::n_distinct(metadata_final$sample_unit_id) == 36L,
  "metadata_final should contain 36 repeated depth-specific units."
)

analysis_vars <- c(
  "PGNP", "PNNP", "N2O_reduction", "PNNP_PGNP", "NEA",
  "AOA", "AOB", "ComA", "ComB", "nirS", "nirK", "nosZ1"
)

metadata_qc <- purrr::map_dfr(
  analysis_vars,
  function(v) {
    x <- metadata_final[[v]]
    tibble::tibble(
      variable = v,
      n_total = length(x),
      n_nonmissing = sum(!is.na(x)),
      n_missing = sum(is.na(x)),
      min = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
      median = if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE),
      max = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    )
  }
)

# No silent imputation: the currently known missing ComB remains NA.
if (any(metadata_qc$n_missing > 0L)) {
  message(
    "Missing laboratory values retained as NA: ",
    paste(
      paste0(
        metadata_qc$variable[metadata_qc$n_missing > 0L],
        "=",
        metadata_qc$n_missing[metadata_qc$n_missing > 0L]
      ),
      collapse = ", "
    )
  )
}

# ---- 7. Save -----------------------------------------------------------------
saveRDS(
  metadata_final,
  file.path(INT_DIR, "01b_metadata_final.rds")
)

readr::write_csv(
  metadata_final,
  file.path(TABLE_DIR, "metadata_final.csv"),
  na = ""
)

readr::write_csv(
  metadata_qc,
  file.path(TABLE_DIR, "01b_metadata_qc.csv"),
  na = ""
)

readr::write_csv(
  dea_rep_mismatches,
  file.path(TABLE_DIR, "01b_DEA_rep_mismatches.csv"),
  na = ""
)

message("metadata_final saved: ", nrow(metadata_final), " rows (36/year).")
message("DEA Rep-label mismatches documented: ", nrow(dea_rep_mismatches), ".")
message("qPCR source: ", basename(qpcr_file))
message("Next script: scripts/02_import_raw_markers.R")
