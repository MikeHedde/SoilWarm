# ==============================================================================
# 01_build_sample_map.R
# SOILWARM — canonical sequencing/sample identity map
# ==============================================================================
# AIM
# ---
# Build the canonical correspondence between the 111 sequenced MET libraries
# and experimental identities using ONLY the original plate-registration file.
#
# The sample map contains:
#   - 108 biological libraries: 36 in each of 2022, 2023 and 2024
#   - 3 extraction controls
#
# CRITICAL VERIFIED FACT
# ----------------------
# In the plate workbook:
#   C = Chauffage -> Heated
#   T = Témoin    -> Control
#
# KNOWN SOURCE-WORKBOOK ANOMALY
# -----------------------------
# One registration value is PL2_B9_10 instead of a MET identifier.
# It corresponds uniquely to the missing MET250225 and is repaired explicitly
# here; the original workbook is never edited.
#
# INPUT
# -----
# data/raw/plan de plaque ADN envoyé_correspondance code ADNid.xlsx
#
# OUTPUTS
# -------
# intermediate/01_sample_map.rds
# results/tables/01_sample_map_biological.csv
# results/tables/01_extraction_controls.csv
# results/tables/01_registration_111.csv
# ==============================================================================

source("R/config.R")
source("R/functions_soilwarm.R")


# ---- 0. Remove obsolete outputs from the previous 2-year workflow -------------
legacy_01_outputs <- c(
  file.path(TABLE_DIR, "01_metadata_72_with_MET_ids.csv"),
  file.path(TABLE_DIR, "01_sample_map_108_biological.csv")
)

invisible(
  file.remove(
    legacy_01_outputs[file.exists(legacy_01_outputs)]
  )
)

check <- function(ok, msg) {
  if (!isTRUE(ok)) stop(msg, call. = FALSE)
}

# ---- 1. Canonical registration sheet -----------------------------------------
reg <- readxl::read_excel(
  FILES$plate_map,
  sheet = "Liste_Enregistrement_Colonnes",
  col_names = TRUE,
  .name_repair = "minimal"
) |>
  as.data.frame(check.names = FALSE)

required_cols <- c(
  "plq", "ech", "yr", "c", "condition",
  "Profondeur", "traitement", "qté_ADN"
)

check(
  all(required_cols %in% names(reg)),
  paste0(
    "Liste_Enregistrement_Colonnes is missing expected columns. Found: ",
    paste(names(reg), collapse = ", ")
  )
)

reg <- reg |>
  dplyr::transmute(
    plate = stringr::str_to_lower(stringr::str_trim(as.character(plq))),
    sample_number = stringr::str_trim(as.character(ech)),
    Year = suppressWarnings(as.integer(yr)),
    MET = stringr::str_trim(as.character(c)),
    condition = stringr::str_trim(as.character(condition)),
    Depth = stringr::str_trim(as.character(Profondeur)),
    plate_treatment = stringr::str_trim(as.character(traitement)),
    DNA_concentration = suppressWarnings(as.numeric(`qté_ADN`))
  ) |>
  dplyr::filter(!is.na(MET), MET != "")

check(
  nrow(reg) == 111L,
  paste0("Expected 111 registered libraries; found ", nrow(reg), ".")
)

# ---- 2. Explicit repair of the single known MET annotation error -------------
bad_met <- reg |>
  dplyr::filter(!stringr::str_detect(MET, "^MET[0-9]+$"))

check(
  nrow(bad_met) == 1L &&
    identical(bad_met$MET[[1]], "PL2_B9_10") &&
    bad_met$Year[[1]] == 2024L &&
    bad_met$sample_number[[1]] == "33",
  paste0(
    "Unexpected non-MET registration value(s): ",
    paste(bad_met$MET, collapse = ", "),
    ". Only the verified PL2_B9_10 -> MET250225 repair is authorised."
  )
)

reg <- reg |>
  dplyr::mutate(
    MET = dplyr::if_else(MET == "PL2_B9_10", "MET250225", MET)
  )

check(
  dplyr::n_distinct(reg$MET) == 111L,
  "MET identifiers are not unique after the explicit repair."
)
check(
  all(stringr::str_detect(reg$MET, "^MET[0-9]+$")),
  "At least one registration value is still not a MET identifier."
)
check(
  setequal(reg$MET, paste0("MET", 250125:250235)),
  "The registration sheet does not contain exactly MET250125 ... MET250235."
)

# ---- 3. Extraction controls vs biological libraries --------------------------
reg <- reg |>
  dplyr::mutate(
    is_extraction_blank = stringr::str_detect(
      stringr::str_to_lower(sample_number),
      "témoin extraction|temoin extraction"
    )
  )

extraction_controls <- reg |>
  dplyr::filter(is_extraction_blank) |>
  dplyr::mutate(
    LandUse = NA_character_,
    Treatment = NA_character_,
    sample_number_num = NA_real_,
    rep = NA_integer_,
    Plot = NA_character_,
    Block = NA_integer_,
    profile_id = NA_character_,
    sample_unit_id = NA_character_,
    is_negative = TRUE,
    negative_type = "extraction_blank",
    Nom_Ech = paste0("Plaque2_Ech-", sample_number, "_", Year)
  )

check(
  nrow(extraction_controls) == 3L,
  paste0("Expected 3 extraction controls; found ", nrow(extraction_controls), ".")
)
check(
  setequal(
    extraction_controls$MET,
    c("MET250200", "MET250204", "MET250208")
  ),
  paste0(
    "Extraction-control MET identifiers differ from the verified values: ",
    paste(extraction_controls$MET, collapse = ", ")
  )
)

biological <- reg |>
  dplyr::filter(!is_extraction_blank)

check(
  nrow(biological) == 108L,
  paste0("Expected 108 biological libraries; found ", nrow(biological), ".")
)
check(
  all(biological$condition %in% c("PP", "RC")),
  "At least one biological library has an unexpected PP/RC condition."
)
check(
  all(biological$Depth %in% c("0-15", "15-30", "30-50")),
  "At least one biological library has an unexpected soil depth."
)
check(
  all(biological$plate_treatment %in% c("C", "T")),
  "At least one biological library has an unexpected C/T treatment code."
)

# ---- 4. Standardised experimental descriptors --------------------------------
biological <- biological |>
  dplyr::mutate(
    LandUse = dplyr::recode(condition, PP = "Grassland", RC = "Cropland"),
    Treatment = dplyr::recode(
      plate_treatment,
      C = "Heated",
      T = "Control"
    ),
    sample_number_num = suppressWarnings(as.numeric(sample_number))
  ) |>
  dplyr::arrange(Year, LandUse, Depth, Treatment, sample_number_num) |>
  dplyr::group_by(Year, LandUse, Depth, Treatment) |>
  dplyr::mutate(rep = dplyr::row_number()) |>
  dplyr::ungroup()

cell_sizes <- biological |>
  dplyr::count(Year, LandUse, Depth, Treatment)

check(
  all(cell_sizes$n == 3L),
  paste0(
    "Each Year x LandUse x Depth x Treatment cell must contain 3 biological ",
    "replicates. Problematic cells: ",
    paste(
      apply(
        cell_sizes[cell_sizes$n != 3L, , drop = FALSE],
        1,
        paste,
        collapse = "/"
      ),
      collapse = "; "
    )
  )
)

biological <- biological |>
  dplyr::mutate(
    Plot = paste0(dplyr::if_else(Treatment == "Heated", "T", "C"), rep),
    Block = rep,
    profile_id = paste(LandUse, Treatment, Block, sep = "__"),
    sample_unit_id = paste(LandUse, Treatment, Block, Depth, sep = "__"),
    is_negative = FALSE,
    negative_type = NA_character_,
    Nom_Ech = paste0(
      dplyr::if_else(plate == "plaque1", "plaque1_Ech-", "Plaque2_Ech-"),
      sample_number, "_", Year
    )
  )

# ---- 5. Structural QC ---------------------------------------------------------
check(
  identical(
    as.integer(table(biological$Year)[c("2022", "2023", "2024")]),
    c(36L, 36L, 36L)
  ),
  "Expected exactly 36 biological libraries in each of 2022, 2023 and 2024."
)
check(
  dplyr::n_distinct(biological$MET) == 108L,
  "Biological MET identifiers are not unique."
)
check(
  dplyr::n_distinct(biological$profile_id) == 12L,
  "Expected 12 repeated profiles."
)
check(
  dplyr::n_distinct(biological$sample_unit_id) == 36L,
  "Expected 36 repeated depth-specific sampling units."
)

# Keep a common object for marker-level joins/QC.
all_map <- dplyr::bind_rows(
  biological,
  extraction_controls
)

sample_map <- list(
  biological = biological,
  extraction_controls = extraction_controls,
  all_map = all_map,
  registration_111 = reg
)

# ---- 6. Save -----------------------------------------------------------------
saveRDS(
  sample_map,
  file.path(INT_DIR, "01_sample_map.rds")
)

write.csv(
  biological,
  file.path(TABLE_DIR, "01_sample_map_biological.csv"),
  row.names = FALSE
)
write.csv(
  extraction_controls,
  file.path(TABLE_DIR, "01_extraction_controls.csv"),
  row.names = FALSE
)
write.csv(
  reg,
  file.path(TABLE_DIR, "01_registration_111.csv"),
  row.names = FALSE
)

message("Sample map saved.")
message("Biological libraries: ", nrow(biological), " (36/year).")
message("Extraction controls: ", nrow(extraction_controls), ".")
message("Repeated profiles: ", dplyr::n_distinct(biological$profile_id), ".")
message("Next script: scripts/01b_build_metadata_final.R")
