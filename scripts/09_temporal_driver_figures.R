# ==============================================================================
# 09b_temporal_driver_figures.R
# SOILWARM — integrated temporal figures after scripts 08, 08c and 08d
# ==============================================================================
#
# PURPOSE
# -------
# Reorganise the temporal results around three components:
#   1. shared interannual variation / temporal asynchrony,
#   2. land-use-associated divergence relative to Stable_PP,
#   3. warming-associated divergence (Heated - Control).
#
# This script does NOT recompute statistics.
#
# INPUT TABLES
# ------------
# 08_relative_interval_movement_summary.csv
# 08_timing_shift_test.csv
# 08_timing_shift_pairwise.csv
# 08c_multivariate_DiD.csv
# 08c_process_changes_by_trajectory.csv
# 08c_process_DiD.csv
# 08d_warming_multivariate.csv
# 08d_warming_processes.csv
# 08d_landuse_x_warming_multivariate.csv
# 08d_landuse_x_warming_processes.csv
#
# OUTPUTS
# -------
# results/figures_main/Figure4_temporal_drivers.pdf/png
# results/figures_main/Figure5_N_activity_drivers.pdf/png
# results/figures_main/FigureS_landuse_x_warming.pdf/png
# results/figures_main/FigureS4_N_gene_potential_timing.pdf/png
# ==============================================================================

source("R/config.R")

required_pkgs <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "patchwork",
  "scales"
)

missing_pkgs <- required_pkgs[
  !vapply(
    required_pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_pkgs) > 0L) {
  stop(
    "Missing packages required by script 09b: ",
    paste(
      missing_pkgs,
      collapse = ", "
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(patchwork)
  library(scales)
})

FIG_MAIN_DIR <- file.path(
  OUT_DIR,
  "figures_main"
)

dir.create(
  FIG_MAIN_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

read_required_csv <- function(filename) {
  path <- file.path(
    TABLE_DIR,
    filename
  )

  if (!file.exists(path)) {
    stop(
      "Required file not found: ",
      path,
      call. = FALSE
    )
  }

  readr::read_csv(
    path,
    show_col_types = FALSE
  )
}

save_plot <- function(
  p,
  stem,
  width,
  height
) {
  ggsave(
    file.path(
      FIG_MAIN_DIR,
      paste0(
        stem,
        ".pdf"
      )
    ),
    p,
    width = width,
    height = height,
    device = cairo_pdf
  )

  ggsave(
    file.path(
      FIG_MAIN_DIR,
      paste0(
        stem,
        ".png"
      )
    ),
    p,
    width = width,
    height = height,
    dpi = 300
  )
}

theme_soilwarm <- function(
  base_size = 10
) {
  theme_bw(
    base_size = base_size
  ) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(
        face = "bold"
      ),
      axis.title = element_text(
        face = "bold"
      ),
      plot.title = element_text(
        face = "bold"
      ),
      plot.subtitle = element_text(
        colour = "grey30"
      ),
      legend.title = element_text(
        face = "bold"
      )
    )
}

format_p <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(
      p < 0.001,
      "p < 0.001",
      paste0(
        "p = ",
        formatC(
          p,
          format = "f",
          digits = 3
        )
      )
    )
  )
}

format_pq <- function(p, q) {
  p_txt <- ifelse(
    is.na(p),
    "p = NA",
    ifelse(
      p < 0.001,
      "p < 0.001",
      paste0(
        "p = ",
        formatC(
          p,
          format = "f",
          digits = 3
        )
      )
    )
  )

  q_txt <- ifelse(
    is.na(q),
    "q = NA",
    ifelse(
      q < 0.001,
      "q < 0.001",
      paste0(
        "q = ",
        formatC(
          q,
          format = "f",
          digits = 3
        )
      )
    )
  )

  paste0(
    p_txt,
    "\n",
    q_txt
  )
}

format_pq_inline <- function(p, q) {
  p_txt <- ifelse(
    is.na(p),
    "p=NA",
    ifelse(
      p < 0.001,
      "p<0.001",
      paste0(
        "p=",
        formatC(
          p,
          format = "f",
          digits = 3
        )
      )
    )
  )

  q_txt <- ifelse(
    is.na(q),
    "q=NA",
    ifelse(
      q < 0.001,
      "q<0.001",
      paste0(
        "q=",
        formatC(
          q,
          format = "f",
          digits = 3
        )
      )
    )
  )

  paste0(
    p_txt,
    "; ",
    q_txt
  )
}

PROCESS_LEVELS <- c(
  "PGNP",
  "PNNP",
  "N2O_reduction",
  "NEA"
)

PROCESS_LABELS <- c(
  "PGNP" = "PGNP",
  "PNNP" = "PNNP",
  "N2O_reduction" = "N2O reduction",
  "NEA" = "NEA"
)

INTERVAL_LEVELS <- c(
  "2022-2023",
  "2023-2024"
)

# ==============================================================================
# FIGURE 4 — THREE COMPONENTS OF TEMPORAL CHANGE
# ==============================================================================

relative_summary <- read_required_csv(
  "08_relative_interval_movement_summary.csv"
)

timing_test <- read_required_csv(
  "08_timing_shift_test.csv"
)

timing_pairwise <- read_required_csv(
  "08_timing_shift_pairwise.csv"
)

landuse_multi <- read_required_csv(
  "08c_multivariate_DiD.csv"
)

warming_multi <- read_required_csv(
  "08d_warming_multivariate.csv"
)

# ---- 4A. Shared interannual pattern / temporal asynchrony ---------------------

rel_depth <- relative_summary |>
  filter(
    Scope == "by_depth"
  ) |>
  mutate(
    Depth = factor(
      Depth,
      levels = c(
        "0-15",
        "15-30",
        "30-50"
      )
    ),
    Domain = factor(
      Domain,
      levels = c(
        "Taxonomy",
        "N gene potential",
        "N functioning"
      ),
      labels = c(
        "Taxonomy",
        "N functional-gene potential",
        "Potential N activities"
      )
    ),
    Interval = factor(
      Interval,
      levels = INTERVAL_LEVELS,
      labels = c(
        "2022-2023",
        "2023-2024"
      )
    )
  )

timing_subtitle <- if (nrow(timing_pairwise) >= 2L) {
  gene_vs_tax <- timing_pairwise |>
    filter(comparison == "Gene potential - Taxonomy") |>
    slice(1)

  activity_vs_gene <- timing_pairwise |>
    filter(comparison == "N activities - Gene potential") |>
    slice(1)

  paste0(
    "Exact whole-profile timing tests: gene vs taxonomy ",
    format_p(gene_vs_tax$p_exact_two_sided),
    "; N activities vs gene ",
    format_p(activity_vs_gene$p_exact_two_sided)
  )
} else if (nrow(timing_test) == 1L) {
  paste0(
    "Whole-profile timing-offset test: ",
    format_p(timing_test$p_exact_two_sided)
  )
} else {
  NULL
}

p4a <- ggplot(
  rel_depth,
  aes(
    x = Interval,
    y = mean_fraction,
    group = Domain,
    shape = Domain,
    linetype = Domain
  )
) +
  geom_hline(
    yintercept = 0.5,
    linewidth = 0.35,
    linetype = 3,
    colour = "grey55"
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2.8
  ) +
  geom_errorbar(
    aes(
      ymin = pmax(
        0,
        mean_fraction -
          se_fraction
      ),
      ymax = pmin(
        1,
        mean_fraction +
          se_fraction
      )
    ),
    width = 0.08,
    linewidth = 0.4
  ) +
  facet_wrap(
    ~ Depth,
    nrow = 1
  ) +
  scale_shape_manual(
    values = c(
      "Taxonomy" = 16,
      "N functional-gene potential" = 15,
      "Potential N activities" = 17
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Taxonomy" = "solid",
      "N functional-gene potential" = "dotdash",
      "Potential N activities" = "dashed"
    )
  ) +
  scale_y_continuous(
    limits = c(
      0,
      1
    ),
    labels = scales::percent_format(
      accuracy = 1
    )
  ) +
  labs(
    x = NULL,
    y = "Fraction of the 3-year trajectory",
    shape = NULL,
    linetype = NULL,
    title = "A  Temporal partitioning of change across biological domains",
    subtitle = timing_subtitle
  ) +
  theme_soilwarm(
    9
  )

# ---- 4B. Land-use-associated divergence --------------------------------------

landuse_plot <- landuse_multi |>
  filter(
    Interval %in%
      INTERVAL_LEVELS
  ) |>
  mutate(
    Interval = factor(
      Interval,
      levels =
        INTERVAL_LEVELS
    ),
    Domain = recode(
      Domain,
      Taxonomy = "Taxonomy",
      N_gene_potential = "N functional-gene potential",
      N_function = "Potential N activities"
    ),
    Domain = factor(
      Domain,
      levels = c(
        "Taxonomy",
        "N functional-gene potential",
        "Potential N activities"
      )
    ),
    p_label = format_pq_inline(
      p_exact,
      p_FDR_within_domain
    )
  )

p4b <- ggplot(
  landuse_plot,
  aes(
    x = Interval,
    y = vector_norm
  )
) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = p_label
    ),
    vjust = -0.8,
    size = 2.8
  ) +
  facet_wrap(
    ~ Domain,
    scales = "free_y"
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0.06,
        0.22
      )
    )
  ) +
  labs(
    x = NULL,
    y = "Norm of mean PP-referenced change vector",
    title = "B  Land-use-associated divergence relative to Stable PP",
    subtitle = "Stable permanent pasture is the contemporary interannual reference"
  ) +
  theme_soilwarm(
    9
  )

# ---- 4C. Warming-associated divergence ---------------------------------------

warming_plot <- warming_multi |>
  filter(
    Interval %in%
      INTERVAL_LEVELS
  ) |>
  mutate(
    Interval = factor(
      Interval,
      levels =
        INTERVAL_LEVELS
    ),
    Domain = recode(
      Domain,
      Taxonomy = "Taxonomy",
      N_gene_potential = "N functional-gene potential",
      N_activities = "Potential N activities"
    ),
    Domain = factor(
      Domain,
      levels = c(
        "Taxonomy",
        "N functional-gene potential",
        "Potential N activities"
      )
    ),
    p_label = format_pq_inline(
      p_exact,
      p_FDR_within_domain
    )
  )

p4c <- ggplot(
  warming_plot,
  aes(
    x = Interval,
    y = vector_norm
  )
) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = p_label
    ),
    vjust = -0.8,
    size = 2.8
  ) +
  facet_wrap(
    ~ Domain,
    scales = "free_y"
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0.06,
        0.22
      )
    )
  ) +
  labs(
    x = NULL,
    y = "Norm of mean Heated-Control change vector",
    title = "C  Heated-Control divergence in temporal change",
    subtitle = "Temporal change under Heated minus temporal change under Control"
  ) +
  theme_soilwarm(
    9
  )

p_fig4 <- patchwork::wrap_plots(
  p4a,
  p4b,
  p4c,
  ncol = 1,
  heights = c(
    1.2,
    1,
    1
  ),
  guides = "collect"
) &
  theme(
    legend.position = "bottom"
  )

save_plot(
  p_fig4,
  "Figure4_temporal_drivers",
  width = 9.8,
  height = 11.5
)

# ==============================================================================
# FIGURE S4 — WHICH FUNCTIONAL GENES MOVE IN EACH INTERVAL?
# ==============================================================================
# Panel A is the inferential view: interval means are shown globally and the
# asterisk refers ONLY to the exact whole-profile early-vs-late test (FDR < .05).
# Panel B is depth-stratified and deliberately descriptive: no significance
# symbols are repeated across depths.

gene_interval_summary <- read_required_csv(
  "08_gene_variable_interval_summary.csv"
)

gene_interval_changes <- read_required_csv(
  "08_gene_variable_interval_changes.csv"
)

gene_timing_tests <- read_required_csv(
  "08_gene_variable_timing_tests.csv"
)

gene_levels <- c(
  "AOA",
  "AOB",
  "ComA",
  "ComB",
  "nirS",
  "nirK",
  "nosZ1"
)

# Profile-level SE for the global display. Depths are averaged first, so the
# descriptive uncertainty is based on the 12 independent profiles rather than
# on 36 depth observations. The plotted global means themselves are taken from
# the exact-test table to remain identical to the inferential analysis.
gene_profile_se <- gene_interval_changes |>
  group_by(
    profile_id,
    Gene,
    Interval
  ) |>
  summarise(
    profile_mean_abs_change = mean(
      abs_delta,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  group_by(
    Gene,
    Interval
  ) |>
  summarise(
    n_profiles = sum(
      is.finite(profile_mean_abs_change)
    ),
    se_profile = ifelse(
      n_profiles > 1L,
      sd(
        profile_mean_abs_change,
        na.rm = TRUE
      ) / sqrt(n_profiles),
      NA_real_
    ),
    .groups = "drop"
  )

gene_global <- gene_timing_tests |>
  transmute(
    Gene,
    p_exact_two_sided,
    p_FDR,
    `2022-2023` = mean_abs_change_2022_2023,
    `2023-2024` = mean_abs_change_2023_2024
  ) |>
  pivot_longer(
    cols = all_of(INTERVAL_LEVELS),
    names_to = "Interval",
    values_to = "mean_abs_change"
  ) |>
  left_join(
    gene_profile_se,
    by = c(
      "Gene",
      "Interval"
    )
  ) |>
  mutate(
    Gene_label = ifelse(
      !is.na(p_FDR) & p_FDR < 0.05,
      paste0(Gene, "*"),
      Gene
    ),
    Gene_label = factor(
      Gene_label,
      levels = ifelse(
        gene_timing_tests$p_FDR[match(gene_levels, gene_timing_tests$Gene)] < 0.05,
        paste0(gene_levels, "*"),
        gene_levels
      )
    ),
    Interval = factor(
      Interval,
      levels = INTERVAL_LEVELS
    )
  )

p_gene_global <- ggplot(
  gene_global,
  aes(
    x = Gene_label,
    y = mean_abs_change,
    group = Interval,
    shape = Interval,
    linetype = Interval
  )
) +
  geom_line(
    linewidth = 0.55,
    position = position_dodge(width = 0.28)
  ) +
  geom_point(
    size = 2.5,
    position = position_dodge(width = 0.28)
  ) +
  geom_errorbar(
    aes(
      ymin = pmax(
        0,
        mean_abs_change - se_profile
      ),
      ymax = mean_abs_change + se_profile
    ),
    width = 0.12,
    linewidth = 0.35,
    position = position_dodge(width = 0.28)
  ) +
  labs(
    x = NULL,
    y = "Mean absolute annual change\n(standardised log-gene abundance)",
    shape = NULL,
    linetype = NULL,
    title = "A  Whole-profile functional-gene timing",
    subtitle = "Depths averaged for descriptive SE; * exact whole-profile early-vs-late test, FDR < 0.05"
  ) +
  theme_soilwarm(9) +
  theme(
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    ),
    legend.position = "bottom"
  )

gene_depth <- gene_interval_summary |>
  mutate(
    Gene = factor(
      Gene,
      levels = gene_levels
    ),
    Depth = factor(
      Depth,
      levels = c(
        "0-15",
        "15-30",
        "30-50"
      )
    ),
    Interval = factor(
      Interval,
      levels = INTERVAL_LEVELS
    )
  )

p_gene_depth <- ggplot(
  gene_depth,
  aes(
    x = Gene,
    y = mean_abs_change,
    group = Interval,
    shape = Interval,
    linetype = Interval
  )
) +
  geom_line(
    linewidth = 0.5,
    position = position_dodge(width = 0.28)
  ) +
  geom_point(
    size = 2.2,
    position = position_dodge(width = 0.28)
  ) +
  geom_errorbar(
    aes(
      ymin = pmax(
        0,
        mean_abs_change - se_abs_change
      ),
      ymax = mean_abs_change + se_abs_change
    ),
    width = 0.12,
    linewidth = 0.32,
    position = position_dodge(width = 0.28)
  ) +
  facet_wrap(
    ~ Depth,
    nrow = 1
  ) +
  labs(
    x = NULL,
    y = "Mean absolute annual change\n(standardised log-gene abundance)",
    shape = NULL,
    linetype = NULL,
    title = "B  Depth-stratified patterns",
    subtitle = "Descriptive only; no depth-specific significance is implied"
  ) +
  theme_soilwarm(9) +
  theme(
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    ),
    legend.position = "bottom"
  )

p_gene_timing <- patchwork::wrap_plots(
  p_gene_global,
  p_gene_depth,
  ncol = 1,
  heights = c(
    0.9,
    1.1
  ),
  guides = "collect"
) &
  theme(
    legend.position = "bottom"
  )

save_plot(
  p_gene_timing,
  "FigureS4_N_gene_potential_timing",
  width = 10.0,
  height = 7.4
)

# ==============================================================================
# FIGURE 5 — WHICH POTENTIAL N ACTIVITIES CARRY THE RESPONSE?
# ==============================================================================

process_traj <- read_required_csv(
  "08c_process_changes_by_trajectory.csv"
)

process_landuse <- read_required_csv(
  "08c_process_DiD.csv"
)

process_warming <- read_required_csv(
  "08d_warming_processes.csv"
)

analysis_08c_path <- file.path(
  INT_DIR,
  "08c_PP_reference_landuse_change.rds"
)

analysis_08d_path <- file.path(
  INT_DIR,
  "08d_warming_landuse_interaction.rds"
)

if (!file.exists(analysis_08c_path)) {
  stop(
    "Required file not found: ",
    analysis_08c_path,
    call. = FALSE
  )
}

if (!file.exists(analysis_08d_path)) {
  stop(
    "Required file not found: ",
    analysis_08d_path,
    call. = FALSE
  )
}

analysis_08c <- readRDS(
  analysis_08c_path
)

analysis_08d <- readRDS(
  analysis_08d_path
)

# Six profile-pair inference units for the land-use contrast:
# Treatment x Block, with the three depths kept together.
landuse_profile_effects <- analysis_08c$process_DiD |>
  filter(
    Interval %in% INTERVAL_LEVELS
  ) |>
  group_by(
    pair_profile_id,
    Interval,
    Process
  ) |>
  summarise(
    profile_effect = mean(
      DiD,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Six profile-pair inference units for the warming contrast:
# LandUseTrajectory x Block, with the three depths kept together.
warming_profile_effects <- analysis_08d$process_warming |>
  filter(
    Interval %in% INTERVAL_LEVELS
  ) |>
  group_by(
    warming_pair_profile,
    Interval,
    Process
  ) |>
  summarise(
    profile_effect = mean(
      warming_contrast,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ---- 5A. Raw signed changes by land-use trajectory ---------------------------

if (!"n_profiles" %in% names(process_traj)) {
  stop(
    "08c_process_changes_by_trajectory.csv is from the older depth-level-SE workflow. ",
    "Re-run scripts/08c_PP_reference_landuse_change.R before Figure 5.",
    call. = FALSE
  )
}

if (any(process_traj$n_profiles != 6L)) {
  stop(
    "Figure 5A expects six independent profiles per land-use trajectory.",
    call. = FALSE
  )
}

p5a_dat <- process_traj |>
  filter(
    Interval %in%
      INTERVAL_LEVELS
  ) |>
  mutate(
    Process = factor(
      Process,
      levels =
        PROCESS_LEVELS
    ),
    Interval = factor(
      Interval,
      levels =
        INTERVAL_LEVELS
    ),
    LandUseTrajectory = factor(
      LandUseTrajectory,
      levels = c(
        "Stable_PP",
        "TP_to_Crop"
      ),
      labels = c(
        "Stable PP",
        "TP -> Crop"
      )
    )
  )

pd <- position_dodge(
  width = 0.45
)

p5a <- ggplot(
  p5a_dat,
  aes(
    x = Process,
    y = mean_delta,
    shape = LandUseTrajectory
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    linetype = 3,
    colour = "grey55"
  ) +
  geom_point(
    size = 2.8,
    position = pd
  ) +
  geom_errorbar(
    aes(
      ymin =
        mean_delta -
        se_delta,
      ymax =
        mean_delta +
        se_delta
    ),
    width = 0.08,
    linewidth = 0.4,
    position = pd
  ) +
  facet_wrap(
    ~ Interval,
    nrow = 1
  ) +
  scale_x_discrete(
    labels =
      PROCESS_LABELS
  ) +
  labs(
    x = NULL,
    y = expression(
      "Mean signed " * Delta * "z"
    ),
    shape = "Land-use trajectory",
    title = "A  Potential N-activity changes in the two land-use trajectories",
    subtitle = "Means and SE across 6 independent profiles; three depths averaged within profile"
  ) +
  theme_soilwarm(
    9
  ) +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

# ---- 5B. PP-referenced process contrasts -------------------------------------

p5b_dat <- process_landuse |>
  filter(
    Interval %in%
      INTERVAL_LEVELS
  ) |>
  mutate(
    Process = factor(
      Process,
      levels =
        PROCESS_LEVELS
    ),
    Interval = factor(
      Interval,
      levels =
        INTERVAL_LEVELS
    ),
    p_label = format_pq_inline(
      p_exact_two_sided,
      p_FDR_within_interval
    )
  )

landuse_profile_effects <- landuse_profile_effects |>
  mutate(
    Process = factor(
      Process,
      levels = PROCESS_LEVELS
    ),
    Interval = factor(
      Interval,
      levels = INTERVAL_LEVELS
    )
  )

p5b <- ggplot(
  p5b_dat,
  aes(
    x = Process,
    y = mean_DiD
  )
) +
  geom_point(
    data = landuse_profile_effects,
    aes(
      x = Process,
      y = profile_effect
    ),
    inherit.aes = FALSE,
    position = position_jitter(
      width = 0.06,
      height = 0,
      seed = 805
    ),
    shape = 1,
    size = 1.8,
    stroke = 0.45,
    alpha = 0.65
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    linetype = 3,
    colour = "grey55"
  ) +
  geom_point(
    shape = 18,
    size = 3.4
  ) +
  geom_text(
    aes(
      label = p_label,
      y = mean_DiD +
        ifelse(
          mean_DiD >= 0,
          0.32,
          -0.32
        )
    ),
    size = 2.55
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0.14,
        0.18
      )
    )
  ) +
  facet_wrap(
    ~ Interval,
    nrow = 1
  ) +
  scale_x_discrete(
    labels =
      PROCESS_LABELS
  ) +
  labs(
    x = NULL,
    y = expression(
      "TP->Crop minus Stable PP (" *
        Delta * Delta * "z)"
    ),
    title = "B  Land-use-associated process divergence",
    subtitle = "Open circles = 6 profile-pair inference units; black diamond = mean; labels: exact p; within-interval FDR q"
  ) +
  theme_soilwarm(
    9
  ) +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

# ---- 5C. Heated-Control process contrasts ------------------------------------

p5c_dat <- process_warming |>
  filter(
    Interval %in%
      INTERVAL_LEVELS
  ) |>
  mutate(
    Process = factor(
      Process,
      levels =
        PROCESS_LEVELS
    ),
    Interval = factor(
      Interval,
      levels =
        INTERVAL_LEVELS
    ),
    p_label = format_pq_inline(
      p_exact_two_sided,
      p_FDR_within_interval
    )
  )

warming_profile_effects <- warming_profile_effects |>
  mutate(
    Process = factor(
      Process,
      levels = PROCESS_LEVELS
    ),
    Interval = factor(
      Interval,
      levels = INTERVAL_LEVELS
    )
  )

p5c <- ggplot(
  p5c_dat,
  aes(
    x = Process,
    y =
      mean_warming_contrast
  )
) +
  geom_point(
    data = warming_profile_effects,
    aes(
      x = Process,
      y = profile_effect
    ),
    inherit.aes = FALSE,
    position = position_jitter(
      width = 0.06,
      height = 0,
      seed = 806
    ),
    shape = 1,
    size = 1.8,
    stroke = 0.45,
    alpha = 0.65
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    linetype = 3,
    colour = "grey55"
  ) +
  geom_point(
    shape = 18,
    size = 3.4
  ) +
  geom_text(
    aes(
      label = p_label,
      y =
        mean_warming_contrast +
        ifelse(
          mean_warming_contrast >= 0,
          0.30,
          -0.30
        )
    ),
    size = 2.55
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0.14,
        0.18
      )
    )
  ) +
  facet_wrap(
    ~ Interval,
    nrow = 1
  ) +
  scale_x_discrete(
    labels =
      PROCESS_LABELS
  ) +
  labs(
    x = NULL,
    y = expression(
      "Heated-Control (" *
        Delta * "z)"
    ),
    title = "C  Warming-associated process divergence",
    subtitle = "Open circles = 6 profile-pair inference units; black diamond = mean; labels: exact p; within-interval FDR q"
  ) +
  theme_soilwarm(
    9
  ) +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

p_fig5 <- patchwork::wrap_plots(
  p5a,
  p5b,
  p5c,
  ncol = 1,
  heights = c(
    1.05,
    1,
    1
  ),
  guides = "collect"
) &
  theme(
    legend.position = "bottom"
  )

save_plot(
  p_fig5,
  "Figure5_N_activity_drivers",
  width = 9.5,
  height = 10.5
)

# ==============================================================================
# SUPPLEMENTARY — LAND-USE x WARMING INTERACTION
# ==============================================================================

interaction_multi <- read_required_csv(
  "08d_landuse_x_warming_multivariate.csv"
)

interaction_process <- read_required_csv(
  "08d_landuse_x_warming_processes.csv"
)

sA_dat <- interaction_multi |>
  filter(
    Interval %in%
      INTERVAL_LEVELS
  ) |>
  mutate(
    Interval = factor(
      Interval,
      levels =
        INTERVAL_LEVELS
    ),
    Domain = recode(
      Domain,
      Taxonomy = "Taxonomy",
      N_gene_potential = "N functional-gene potential",
      N_activities = "Potential N activities"
    ),
    Domain = factor(
      Domain,
      levels = c(
        "Taxonomy",
        "N functional-gene potential",
        "Potential N activities"
      )
    ),
    p_label = format_p(
      p_exact
    )
  )

sA <- ggplot(
  sA_dat,
  aes(
    x = Interval,
    y = vector_norm
  )
) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = p_label
    ),
    vjust = -0.8,
    size = 3
  ) +
  facet_wrap(
    ~ Domain,
    scales = "free_y"
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0.06,
        0.20
      )
    )
  ) +
  labs(
    x = NULL,
    y = "Norm of land-use x warming interaction",
    title = "A  Multivariate land-use x warming interaction",
    subtitle = "Only 8 exact block-level sign flips: interpret effect sizes cautiously"
  ) +
  theme_soilwarm(
    9
  )

sB_dat <- interaction_process |>
  filter(
    Interval %in%
      INTERVAL_LEVELS
  ) |>
  mutate(
    Process = factor(
      Process,
      levels =
        PROCESS_LEVELS
    ),
    Interval = factor(
      Interval,
      levels =
        INTERVAL_LEVELS
    ),
    p_label = format_p(
      p_exact_two_sided
    )
  )

sB <- ggplot(
  sB_dat,
  aes(
    x = Process,
    y = mean_DDD
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    linetype = 3,
    colour = "grey55"
  ) +
  geom_point(
    size = 2.8
  ) +
  geom_text(
    aes(
      label = p_label,
      y = mean_DDD +
        ifelse(
          mean_DDD >= 0,
          0.15,
          -0.15
        )
    ),
    size = 2.8
  ) +
  facet_wrap(
    ~ Interval,
    nrow = 1
  ) +
  scale_x_discrete(
    labels =
      PROCESS_LABELS
  ) +
  labs(
    x = NULL,
    y = "Land-use x warming interaction",
    title = "B  Process-specific interaction",
    subtitle = "(Heated-Control)_TP->Crop minus (Heated-Control)_Stable_PP"
  ) +
  theme_soilwarm(
    9
  ) +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

p_supp <- patchwork::wrap_plots(
  sA,
  sB,
  ncol = 1,
  heights = c(
    1,
    1
  )
)

save_plot(
  p_supp,
  "FigureS_landuse_x_warming",
  width = 9.2,
  height = 7.5
)

message(
  "09b temporal-driver figures completed."
)
message(
  "Main figures: Figure4_temporal_drivers and Figure5_N_activity_drivers."
)
message(
  "Land-use x warming interaction moved to Supplement because exact inference has only 8 block-level sign flips."
)
# ==============================================================================
