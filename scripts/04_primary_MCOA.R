# ==============================================================================
# 04_primary_MCOA.R
# SOILWARM — primary 3-year multi-kingdom MCOA
# ==============================================================================
# AIM
# ---
# Fit ONE common MCOA space to the cleaned/aggregated community tables from all
# three years (2022, 2023, 2024). This common space is essential for meaningful
# temporal trajectories; separate ordinations by year must not be used.
#
# PRIMARY TRANSFORMATION / WEIGHTING
# ----------------------------------
# Hellinger transformation; columns centred but not scaled;
# MCOA option = PRIMARY_MCOA_WEIGHT (normally "inertia").
#
# OUTPUTS
# -------
# intermediate/04_primary_MCOA.rds
# intermediate/04_primary_scores_with_metadata.rds
# results/tables/04_MAIN_MCOA_sample_scores.csv
# results/tables/04_MAIN_MCOA_eigenvalues.csv
# ==============================================================================

source("R/config.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(stringr)
  library(readxl)
  library(vegan)
  library(ade4)
})

source("R/functions_soilwarm.R")

require_intermediate(
  file.path(INT_DIR, "01b_metadata_final.rds"),
  "scripts/01b_build_metadata_final.R"
)
require_intermediate(
  file.path(INT_DIR, "03_primary_prepared_tables.rds"),
  "scripts/03_decontaminate_and_aggregate.R"
)

metadata_final <- readRDS(file.path(INT_DIR, "01b_metadata_final.rds"))
prep <- readRDS(file.path(INT_DIR, "03_primary_prepared_tables.rds"))

if (nrow(metadata_final) != EXPECTED_PRIMARY_N) {
  stop(
    "Expected ", EXPECTED_PRIMARY_N,
    " metadata rows; found ", nrow(metadata_final), ".",
    call. = FALSE
  )
}

years <- sort(unique(as.integer(metadata_final$Year)))
if (!identical(years, sort(PRIMARY_YEARS))) {
  stop(
    "MCOA years disagree with PRIMARY_YEARS. Found: ",
    paste(years, collapse = ", "),
    "; configured: ",
    paste(PRIMARY_YEARS, collapse = ", "),
    ".",
    call. = FALSE
  )
}

primary <- fit_mcoa_from_prepared(
  prep,
  transform = PRIMARY_TRANSFORM,
  mcoa_weight = PRIMARY_MCOA_WEIGHT,
  nf = PRIMARY_NF
)

saveRDS(
  primary,
  file.path(INT_DIR, "04_primary_MCOA.rds")
)

score_df <- primary$scores |>
  as.data.frame() |>
  tibble::rownames_to_column("MET")

primary_scores <- metadata_final |>
  mutate(MET = as.character(MET)) |>
  left_join(
    score_df,
    by = "MET"
  ) |>
  mutate(
    Year = factor(Year, levels = PRIMARY_YEARS),
    Treatment = factor(Treatment, levels = c("Control", "Heated")),
    LandUse = factor(LandUse),
    Depth = factor(Depth, levels = c("0-15", "15-30", "30-50")),
    Block = factor(Block),
    profile_id = factor(profile_id),
    sample_unit_id = factor(sample_unit_id)
  )

score_cols <- grep("^MCOA", names(primary_scores), value = TRUE)

if (length(score_cols) == 0L) {
  stop("No MCOA score columns were produced.", call. = FALSE)
}

if (!all(complete.cases(primary_scores[, score_cols, drop = FALSE]))) {
  stop("At least one primary MCOA score failed to match metadata.", call. = FALSE)
}

if (nrow(primary_scores) != EXPECTED_PRIMARY_N) {
  stop(
    "Expected ", EXPECTED_PRIMARY_N,
    " MCOA score rows; found ", nrow(primary_scores), ".",
    call. = FALSE
  )
}

if (dplyr::n_distinct(primary_scores$MET) != EXPECTED_PRIMARY_N) {
  stop("MCOA score MET identifiers are not unique.", call. = FALSE)
}

saveRDS(
  primary_scores,
  file.path(INT_DIR, "04_primary_scores_with_metadata.rds")
)

write.csv(
  primary_scores,
  file.path(TABLE_DIR, "04_MAIN_MCOA_sample_scores.csv"),
  row.names = FALSE
)
write.csv(
  primary$eigen,
  file.path(TABLE_DIR, "04_MAIN_MCOA_eigenvalues.csv"),
  row.names = FALSE
)

p_scree <- primary$eigen |>
  filter(axis <= min(10, max(axis))) |>
  ggplot(aes(axis, percent)) +
  geom_col() +
  scale_x_continuous(breaks = seq_len(min(10, max(primary$eigen$axis)))) +
  labs(x = "MCOA axis", y = "Pseudo-inertia (%)") +
  theme_bw()

ggsave(
  file.path(FIG_DIR, "04_MAIN_MCOA_scree.pdf"),
  p_scree,
  width = 7,
  height = 4.5
)

p_scores <- primary_scores |>
  ggplot(aes(MCOA1, MCOA2, shape = Treatment, group = sample_unit_id)) +
  geom_point(size = 2.2, alpha = 0.8) +
  facet_grid(Depth ~ Year) +
  labs(
    x = "MCOA synthetic axis 1",
    y = "MCOA synthetic axis 2"
  ) +
  theme_bw()

ggsave(
  file.path(FIG_DIR, "04_MAIN_MCOA_scores_axis1_axis2.pdf"),
  p_scores,
  width = 9.5,
  height = 8
)

message("Primary MCOA saved in ONE common 2022-2024 ordination space.")
message("Do not interpret axis signs; they are arbitrary.")
message("Next: scripts/05_RV_and_contributions.R, then scripts/06_build_N_function_block.R")
