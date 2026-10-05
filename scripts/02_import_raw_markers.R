# ==============================================================================
# 02_import_raw_markers.R
# SOILWARM — raw metabarcoding import / sequencing-depth QC
# ==============================================================================
# AIM
# ---
# Import the five original marker workbooks at raw sequence-feature level,
# retain the biological scope of each marker, identify the 4 technical blanks
# + 3 extraction blanks, and perform sequencing-depth QC BEFORE decontamination.
#
# TERMINOLOGY
# -----------
# The source tables use "cluster" terminology and were produced through FROGS.
# Until the exact denoising/clustering settings are documented, these units are
# treated generically as sequence features rather than asserted to be ASVs.
#
# IMPORTANT
# ---------
# No sequence feature is removed here for prevalence or taxonomic resolution.
# The raw object is kept auditable so alternative decontamination/taxonomic
# choices can be rerun without re-reading the source workbooks.
#
# INPUTS
# ------
# intermediate/01_sample_map.rds
# intermediate/01b_metadata_final.rds
# five raw metabarcoding workbooks
#
# OUTPUTS
# -------
# intermediate/02_raw_markers_broad.rds
# intermediate/02_raw_protists_legacy.rds
# results/tables/02_raw_library_sizes.csv
# results/figures/02_library_sizes_by_marker.pdf
# ==============================================================================

source("R/config.R")

# functions_soilwarm.R contains several helper functions written with the
# magrittr pipe and unqualified dplyr/stringr verbs.  Attach the packages
# explicitly before sourcing/calling those helpers.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(readxl)
})

source("R/functions_soilwarm.R")

require_intermediate(
  file.path(INT_DIR, "01_sample_map.rds"),
  "scripts/01_build_sample_map.R"
)
require_intermediate(
  file.path(INT_DIR, "01b_metadata_final.rds"),
  "scripts/01b_build_metadata_final.R"
)

sample_map <- readRDS(
  file.path(INT_DIR, "01_sample_map.rds")
)
metadata_final <- readRDS(
  file.path(INT_DIR, "01b_metadata_final.rds")
)

analysis_years <- sort(unique(as.integer(metadata_final$Year)))

if (!identical(analysis_years, c(2022L, 2023L, 2024L))) {
  stop(
    "Expected metadata_final to contain exactly 2022, 2023 and 2024; found: ",
    paste(analysis_years, collapse = ", "),
    call. = FALSE
  )
}

if (!setequal(metadata_final$MET, sample_map$biological$MET)) {
  stop(
    "metadata_final and 01_sample_map.rds do not contain the same 108 biological MET IDs.",
    call. = FALSE
  )
}

# read_marker() is defined in R/functions_soilwarm.R and uses the sample_map
# object loaded above to identify the 3 extraction controls.
raw_markers_broad <- purrr::map(
  names(MARKER_CONFIG),
  ~ read_marker(.x, protist_definition = "broad")
)
names(raw_markers_broad) <- names(MARKER_CONFIG)

raw_protists_legacy <- read_marker(
  "Protists",
  protist_definition = "legacy_narrow"
)

saveRDS(
  raw_markers_broad,
  file.path(INT_DIR, "02_raw_markers_broad.rds")
)
saveRDS(
  raw_protists_legacy,
  file.path(INT_DIR, "02_raw_protists_legacy.rds")
)

# ---- Sequencing-depth QC before decontamination -------------------------------
raw_library_qc <- purrr::imap_dfr(
  raw_markers_broad,
  function(obj, marker) {
    tibble::tibble(
      marker = marker,
      sample_id = rownames(obj$counts),
      negative_type = obj$neg_type,
      reads_scope = rowSums(obj$counts)
    ) |>
      dplyr::left_join(
        sample_map$all_map |>
          dplyr::select(MET, Year, LandUse, Treatment, Plot, Depth),
        by = c("sample_id" = "MET")
      )
  }
)

write.csv(
  raw_library_qc,
  file.path(TABLE_DIR, "02_raw_library_sizes.csv"),
  row.names = FALSE
)

p <- raw_library_qc |>
  dplyr::filter(
    negative_type == "biological",
    Year %in% analysis_years
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(x = marker, y = reads_scope)
  ) +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::geom_jitter(width = 0.15, alpha = 0.45, size = 1.3) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    x = NULL,
    y = "Reads in target biological scope (log10 scale)"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
  )

ggplot2::ggsave(
  file.path(FIG_DIR, "02_library_sizes_by_marker.pdf"),
  p,
  width = 8,
  height = 5
)

message("Raw marker objects saved for 2022-2024.")
message("No prevalence filtering has been applied.")
message("Next script: scripts/03_decontaminate_and_aggregate.R")
