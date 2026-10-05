# ==============================================================================
# 13_optional_2023_MCOA.R
# ==============================================================================
# AIM
# ---
# Supplementary community-only analysis including 2022, 2023 and 2024.
# 2023 is deliberately excluded from the primary integrative analysis because
# final_metadata.xlsx contains the LEM variables only for 2022 and 2024.
#
# QUESTION
# --------
# Are the multi-kingdom synthetic gradients stable when the intermediate 2023
# eDNA survey is included?
# ============================================================================== 

source("R/config.R")
source("R/functions_soilwarm.R")
require_intermediate(file.path(INT_DIR, "01_sample_map.rds"), "scripts/01_build_sample_map.R")
require_intermediate(file.path(INT_DIR, "02_raw_markers_broad.rds"), "scripts/02_import_raw_markers.R")
require_intermediate(file.path(INT_DIR, "04_primary_MCOA.rds"), "scripts/04_primary_MCOA.R")
sample_map <- readRDS(file.path(INT_DIR, "01_sample_map.rds"))
raw_markers_broad <- readRDS(file.path(INT_DIR, "02_raw_markers_broad.rds"))
primary <- readRDS(file.path(INT_DIR, "04_primary_MCOA.rds"))

prep_3y <- purrr::imap(raw_markers_broad, function(obj, marker) {
  prepare_marker_table(obj, rank = PRIMARY_RANKS[[marker]],
                       analysis_years = c(2022L, 2023L, 2024L),
                       prevalence = PRIMARY_PREVALENCE,
                       contaminant_method = "decontam",
                       decontam_threshold = PRIMARY_DECONTAM_THRESHOLD)
})
fit_3y <- fit_mcoa_from_prepared(prep_3y, PRIMARY_TRANSFORM, PRIMARY_MCOA_WEIGHT, PRIMARY_NF)
saveRDS(fit_3y, file.path(INT_DIR, "13_MCOA_all_2022_2023_2024.rds"))

alignment <- align_axes(primary$scores, fit_3y$scores, k = 3) %>%
  mutate(scenario = "all_sequenced_years_2022_2023_2024", .before = 1)
write.csv(alignment, file.path(TABLE_DIR, "13_all_years_vs_primary_axis_stability.csv"), row.names = FALSE)

message("Optional 2023 sensitivity complete.")
