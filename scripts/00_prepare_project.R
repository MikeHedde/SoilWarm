# ==============================================================================
# 00_prepare_project.R
# SOILWARM — multi-kingdom taxonomy × nitrogen functioning
# ==============================================================================
# ROLE
# ----
# One-time project check. Creates/checks the directory tree, checks the core R
# packages, and verifies the raw files required by the 3-year workflow.
#
# IMPORTANT
# ---------
# final_metadata.xlsx is no longer a raw input of the workflow.
# metadata_final.csv is rebuilt reproducibly by scripts/01b_build_metadata_final.R
# from the sample map + laboratory source files.
#
# RUN BEFORE: all other scripts.
# OUTPUTS: directory tree only; no biological data are modified.
# ==============================================================================

source("R/config.R")
source("R/functions_soilwarm.R")

# ---- 1. Core packages ---------------------------------------------------------
required_packages <- c(
  "readxl", "readr", "dplyr", "tidyr", "purrr", "stringr",
  "tibble", "ggplot2", "vegan", "ade4", "decontam"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required R packages:\n - ",
    paste(missing_packages, collapse = "\n - "),
    call. = FALSE
  )
}

# ---- 2. Resolve the qPCR source name ------------------------------------------
# The '(1)' suffix may have been added by a browser/download operation.
# Prefer the canonical filename, but accept the duplicate-name variant.
resolve_qpcr_file <- function() {
  candidates <- file.path(
    RAW_DIR,
    c("qPCR_2022_2023_2024.csv", "qPCR_2022_2023_2024(1).csv")
  )
  present <- candidates[file.exists(candidates)]

  if (length(present) == 0L) {
    return(candidates[[1]])
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

# ---- 3. Required raw inputs ---------------------------------------------------
expected <- c(
  FILES$plate_map,
  FILES$archaea$file,
  FILES$bacteria$file,
  FILES$fungi$file,
  FILES$nematodes$file,
  FILES$protists$file,
  qpcr_file,
  file.path(RAW_DIR, "DEA_2022_2023_2024.csv"),
  file.path(RAW_DIR, "NEA_2022_2023_2024.csv")
)

missing_files <- expected[!file.exists(expected)]

if (length(missing_files) > 0L) {
  stop(
    "Raw input files are missing from data/raw/:\n - ",
    paste(missing_files, collapse = "\n - "),
    "\n\nThe old final_metadata.xlsx is intentionally NOT required.",
    call. = FALSE
  )
}

message("Project structure OK.")
message("Raw files found: ", length(expected), "/", length(expected))
message("qPCR source: ", basename(qpcr_file))
message("Next script: scripts/01_build_sample_map.R")
