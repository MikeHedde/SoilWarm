# ==============================================================================
# 14_export_article_bundle.R
# ==============================================================================
# AIM
# ---
# Convenience export for collaborators / manuscript preparation. This script
# performs NO analysis. It only collects result tables that already exist and
# records the R session used for reproducibility.
# ============================================================================== 

source("R/config.R")
source("R/functions_soilwarm.R")

read_if_exists <- function(path) {
  if (!file.exists(path)) return(NULL)
  read.csv(path, check.names = FALSE)
}

candidates <- list(
  MCOA_scores = file.path(TABLE_DIR, "04_MAIN_MCOA_sample_scores.csv"),
  preprocessing = file.path(TABLE_DIR, "03_preprocessing_summary.csv"),
  MCOA_eigen = file.path(TABLE_DIR, "04_MAIN_MCOA_eigenvalues.csv"),
  interkingdom_RV = file.path(TABLE_DIR, "05_pairwise_RV_permutation_tests.csv"),
  N_process_PCA = file.path(TABLE_DIR, "06_N_process_PCA_scores.csv"),
  Tax_N_RV = file.path(TABLE_DIR, "07_taxonomy_Nprocess_RV_tests.csv"),
  Tax_N_RDA = file.path(TABLE_DIR, "07_taxonomy_Nprocess_RDA_R2.csv"),
  Kingdom_N_RV = file.path(TABLE_DIR, "07_kingdom_specific_Nprocess_RV.csv"),
  Axis_N_cor = file.path(TABLE_DIR, "07_MCOA_axes_vs_N_process_correlations.csv"),
  Delta_RV = file.path(TABLE_DIR, "08_delta_taxonomy_Nprocess_RV.csv"),
  Delta_RDA = file.path(TABLE_DIR, "08_delta_RDA_R2.csv"),
  Delta_models = file.path(TABLE_DIR, "08_delta_function_specific_models.csv"),
  Turnover_N = file.path(TABLE_DIR, "08_kingdom_turnover_Nfunction_magnitude_correlations.csv"),
  Decoupling = file.path(TABLE_DIR, "08_exploratory_decoupling_index.csv"),
  Sensitivity = file.path(TABLE_DIR, "09_sensitivity_axis_stability.csv"),
  Sens_status = file.path(TABLE_DIR, "09_sensitivity_status_and_warnings.csv"),
  Leave_one_out = file.path(TABLE_DIR, "10_leave_one_kingdom_out_stability.csv"),
  Mixed_ANOVA = file.path(TABLE_DIR, "11_MAIN_MCOA_axis_mixed_model_ANOVA.csv"),
  DiD = file.path(TABLE_DIR, "11_MAIN_MCOA_axis_DiD_contrasts.csv"),
  PERMDISP = file.path(TABLE_DIR, "12_kingdom_PERMDISP_checks.csv"),
  PERMANOVA = file.path(TABLE_DIR, "12_repeated_measures_PERMANOVA_temporal_terms.csv"),
  All_years = file.path(TABLE_DIR, "13_all_years_vs_primary_axis_stability.csv")
)

sheets <- lapply(candidates, read_if_exists)
sheets <- sheets[!vapply(sheets, is.null, logical(1))]
if (length(sheets) == 0L) stop("No result tables found. Run the analysis scripts first.")

writexl::write_xlsx(sheets, file.path(OUT_DIR, "SOILWARM_multikingdom_Ncoupling_article_bundle.xlsx"))
capture.output(sessionInfo(), file = file.path(OUT_DIR, "sessionInfo.txt"))

message("Article bundle written with ", length(sheets), " sheets.")
message("Session information: results/sessionInfo.txt")
