# ==============================================================================
# 09_main_figures.R
# SOILWARM — publication-ready figures from validated 3-year outputs
# ==============================================================================
#
# PRINCIPLE
# ---------
# This script does NOT recompute statistical tests. It reads the validated CSV
# outputs from scripts 05-08 and produces the manuscript figures.
#
# MAIN FIGURES
# ------------
# Figure 2 — Inter-kingdom co-structure
#
# Figure 3 — Taxonomy ↔ N-function coupling
#   A. potential N-activity PCA loadings
#   B. global MCOA ↔ N-block RV values
#   C. kingdom-specific RV with potential N activities
#   D. N functional-gene potential PCA loadings
#
# Supplementary Figure S3 retains the descriptive taxonomic/N-activity
# co-inertia configuration.
#
# Figure 4 — Three-year temporal trajectories
#   A. 2022→2023→2024 trajectories in MCOA1 × MCOA2
#   B. 2022→2023→2024 trajectories in N-PC1 × N-PC2
#   C. total taxonomic trajectory length vs total N trajectory length
#   D. RV coupling for pooled annual transitions, each interval and net 2022→2024
#
# SUPPLEMENTARY
# -------------
# Figure S1 — correlations between trajectory geometry metrics
# Figure S2 — treatment-specific annual-transition RV
# ==============================================================================

source("R/config.R")

required_pkgs <- c(
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "forcats",
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
    "Missing packages required by script 09: ",
    paste(missing_pkgs, collapse = ", "),
    ".",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(forcats)
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

# Remove supplementary figures from the obsolete 2022→2024-only workflow.
legacy_main_figures <- unlist(
  lapply(
    c(
      "FigureS1_temporal_axis_correlations"
    ),
    function(stem) {
      file.path(
        FIG_MAIN_DIR,
        paste0(
          stem,
          c(
            ".pdf",
            ".png",
            ".svg"
          )
        )
      )
    }
  )
)

invisible(
  file.remove(
    legacy_main_figures[
      file.exists(
        legacy_main_figures
      )
    ]
  )
)

# ------------------------------------------------------------------------------
# 1. Graphical conventions
# ------------------------------------------------------------------------------

LANDUSE_COL <- c(
  "Cropland" = "#D55E00",
  "Grassland" = "#009E73"
)

TREATMENT_LTY <- c(
  "Control" = "solid",
  "Heated" = "dashed"
)

TREATMENT_SHAPE <- c(
  "Control" = 21,
  "Heated" = 24
)

YEAR_SHAPE <- c(
  "2022" = 21,
  "2023" = 22,
  "2024" = 24
)

ADJUSTMENT_SHAPE <- c(
  "Raw" = 16,
  "Design-residual sensitivity" = 17
)

theme_soilwarm <- function(base_size = 10) {
  theme_bw(
    base_size = base_size
  ) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        linewidth = 0.25,
        colour = "grey90"
      ),
      strip.background = element_blank(),
      strip.text = element_text(
        face = "bold"
      ),
      axis.title = element_text(
        face = "bold"
      ),
      plot.title = element_text(
        face = "bold",
        size = base_size + 1
      ),
      plot.subtitle = element_text(
        colour = "grey30"
      ),
      legend.title = element_text(
        face = "bold"
      ),
      legend.key.height = grid::unit(
        0.45,
        "cm"
      ),
      plot.margin = margin(
        6,
        8,
        6,
        6
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

save_plot <- function(
  p,
  filename,
  width,
  height
) {
  pdf_file <- file.path(
    FIG_MAIN_DIR,
    paste0(
      filename,
      ".pdf"
    )
  )

  png_file <- file.path(
    FIG_MAIN_DIR,
    paste0(
      filename,
      ".png"
    )
  )

  ggsave(
    pdf_file,
    p,
    width = width,
    height = height,
    units = "in",
    device = cairo_pdf
  )

  ggsave(
    png_file,
    p,
    width = width,
    height = height,
    units = "in",
    dpi = 300
  )

  if (
    requireNamespace(
      "svglite",
      quietly = TRUE
    )
  ) {
    ggsave(
      file.path(
        FIG_MAIN_DIR,
        paste0(
          filename,
          ".svg"
        )
      ),
      p,
      width = width,
      height = height,
      units = "in",
      device = svglite::svglite
    )
  }
}

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

adjustment_label <- function(x) {
  case_when(
    x == "raw" ~ "Raw",
    grepl(
      "^residual_",
      x
    ) ~ "Design-residual sensitivity",
    x == "design_residual_sensitivity" ~
      "Design-residual sensitivity",
    TRUE ~ x
  )
}

# ==============================================================================
# FIGURE 2 — INTER-KINGDOM CO-STRUCTURE
# ==============================================================================

rv05 <- read_required_csv(
  "05_pairwise_RV_permutation_tests.csv"
)

marker_order <- c(
  "Archaea",
  "Bacteria",
  "Fungi",
  "Nematodes",
  "Protists"
)

rv05_plot <- rv05 |>
  mutate(
    adjustment_plot = adjustment_label(
      adjustment
    ),
    adjustment_plot = factor(
      adjustment_plot,
      levels = c(
        "Raw",
        "Design-residual sensitivity"
      )
    ),
    marker1 = factor(
      marker1,
      levels = rev(
        marker_order
      )
    ),
    marker2 = factor(
      marker2,
      levels = marker_order
    ),
    significant = p_FDR < 0.05,
    cell_label = paste0(
      formatC(
        RV,
        digits = 2,
        format = "f"
      ),
      ifelse(
        significant,
        "*",
        ""
      )
    )
  )

p_fig2 <- ggplot(
  rv05_plot,
  aes(
    x = marker2,
    y = marker1
  )
) +
  geom_point(
    aes(
      fill = RV,
      alpha = significant
    ),
    shape = 21,
    size = 10,
    colour = "grey20",
    stroke = 0.45
  ) +
  geom_text(
    aes(
      label = cell_label
    ),
    size = 2.6,
    colour = "black"
  ) +
  facet_wrap(
    ~ adjustment_plot,
    nrow = 1
  ) +
  scale_fill_viridis_c(
    option = "C",
    limits = c(
      0,
      max(
        rv05_plot$RV,
        na.rm = TRUE
      )
    )
  ) +
  scale_alpha_manual(
    values = c(
      "FALSE" = 0.40,
      "TRUE" = 1
    ),
    guide = "none"
  ) +
  coord_equal() +
  labs(
    x = NULL,
    y = NULL,
    fill = "RV",
    title = "Inter-kingdom multivariate co-structure",
    subtitle =
      "Whole-profile permutations preserve all three years and three depths; * FDR < 0.05."
  ) +
  theme_soilwarm(
    base_size = 10
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "right"
  )

save_plot(
  p_fig2,
  "Figure2_interkingdom_RV",
  width = 8.2,
  height = 4.8
)

# ==============================================================================
# FIGURE 3 — TAXONOMY ↔ N-FUNCTION COUPLING
# ==============================================================================

loadings <- read_required_csv(
  "06_N_process_PCA_loadings.csv"
)

process_variance <- read_required_csv(
  "06_N_process_PCA_variance.csv"
)

gene_loadings <- read_required_csv(
  "06_N_gene_PCA_loadings.csv"
)

gene_variance <- read_required_csv(
  "06_N_gene_PCA_variance.csv"
)

block_rv <- read_required_csv(
  "07_block_RV_summary.csv"
)

kingdom_rv <- read_required_csv(
  "07_kingdom_process_RV.csv"
)

coinertia_scores <- read_required_csv(
  "07_coinertia_process_sample_scores.csv"
)

process_order <- c(
  "PGNP",
  "PNNP",
  "N2O_reduction",
  "NEA"
)

pc_order <- intersect(
  c(
    "PC1",
    "PC2",
    "PC3"
  ),
  names(loadings)
)

if (length(pc_order) < 2L) {
  stop(
    "At least PC1 and PC2 must be present in process PCA loadings.",
    call. = FALSE
  )
}

# ---- 3A. Process PCA loadings -------------------------------------------------

loadings_long <- loadings |>
  select(
    process,
    all_of(
      pc_order
    )
  ) |>
  pivot_longer(
    cols = all_of(
      pc_order
    ),
    names_to = "PC",
    values_to = "loading"
  ) |>
  mutate(
    process = factor(
      process,
      levels = rev(
        process_order
      )
    ),
    PC = factor(
      PC,
      levels = pc_order
    )
  )

variance_labels <- process_variance |>
  filter(
    axis %in% pc_order
  ) |>
  mutate(
    PC_label = paste0(
      axis,
      "\n",
      scales::percent(
        variance,
        accuracy = 0.1
      )
    )
  ) |>
  select(
    axis,
    PC_label
  )

pc_label_map <- setNames(
  variance_labels$PC_label,
  variance_labels$axis
)

p3a <- ggplot(
  loadings_long,
  aes(
    x = PC,
    y = process,
    fill = loading
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(
      label = sprintf(
        "%.2f",
        loading
      )
    ),
    size = 2.8
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = max(
      abs(
        loadings_long$loading
      ),
      na.rm = TRUE
    ) * c(
      -1,
      1
    )
  ) +
  scale_x_discrete(
    labels = pc_label_map
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Loading",
    title = "A  Potential N-activity PCA"
  ) +
  theme_soilwarm(
    9
  )

# ---- 3B. Global block RV ------------------------------------------------------

block_plot <- block_rv |>
  mutate(
    adjustment_plot = adjustment_label(
      adjustment
    ),
    block_plot = case_when(
      N_block %in% c(
        "realised_N_processes",
        "potential_N_activities"
      ) ~ "Potential N activities",
      N_block == "N_gene_potential" ~ "Gene potential",
      TRUE ~ N_block
    ),
    sig = p_FDR_all_blocks < 0.05
  )

p3b <- ggplot(
  block_plot,
  aes(
    x = block_plot,
    y = RV,
    shape = adjustment_plot
  )
) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = ifelse(
        sig,
        "*",
        ""
      )
    ),
    nudge_y = 0.025,
    size = 4
  ) +
  scale_shape_manual(
    values = ADJUSTMENT_SHAPE
  ) +
  labs(
    x = NULL,
    y = "RV",
    shape = NULL,
    title = "B  Taxonomy × N-block co-structure",
    subtitle = "* pooled-block FDR < 0.05"
  ) +
  theme_soilwarm(
    9
  ) +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

# ---- 3C. Kingdom-specific process RV -----------------------------------------

kingdom_plot <- kingdom_rv |>
  mutate(
    adjustment_plot = adjustment_label(
      adjustment
    ),
    marker = factor(
      marker,
      levels = marker_order
    ),
    sig = p_FDR < 0.05
  )

p3c <- ggplot(
  kingdom_plot,
  aes(
    x = marker,
    y = RV,
    shape = adjustment_plot
  )
) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = ifelse(
        sig,
        "*",
        ""
      )
    ),
    nudge_y = 0.02,
    size = 4
  ) +
  scale_shape_manual(
    values = ADJUSTMENT_SHAPE
  ) +
  labs(
    x = NULL,
    y = "RV with potential N activities",
    shape = NULL,
    title = "C  Kingdom-specific coupling",
    subtitle = "* within-adjustment FDR < 0.05"
  ) +
  theme_soilwarm(
    9
  ) +
  theme(
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    )
  )

# ---- S3. Descriptive co-inertia configuration --------------------------------------------

coia_needed <- c(
  "Tax_CoIA1",
  "Tax_CoIA2",
  "N_CoIA1",
  "N_CoIA2"
)

if (!all(coia_needed %in% names(coinertia_scores))) {
  stop(
    "Co-inertia sample-score file lacks the first two paired axes.",
    call. = FALSE
  )
}

coinertia_scores <- coinertia_scores |>
  mutate(
    Year = factor(
      Year,
      levels = PRIMARY_YEARS
    ),
    LandUseTrajectory = case_when(
      as.character(LandUse) == "Grassland" ~ "Stable PP",
      as.character(LandUse) == "Cropland" ~ "TP -> Crop",
      TRUE ~ as.character(LandUse)
    )
  )

process_block_names <- c(
  "realised_N_processes",
  "potential_N_activities"
)

process_raw_rv <- block_rv |>
  filter(
    N_block %in% process_block_names,
    adjustment == "raw"
  ) |>
  slice(1)

coia_subtitle <- if (nrow(process_raw_rv) == 1L) {
  paste0(
    "Descriptive co-inertia; global raw RV = ",
    formatC(process_raw_rv$RV, format = "f", digits = 3),
    ", ",
    format_p(process_raw_rv$p_perm)
  )
} else {
  "Descriptive co-inertia; interpret together with the global RV test"
}

TRAJECTORY_COL <- c(
  "Stable PP" = LANDUSE_COL[["Grassland"]],
  "TP -> Crop" = LANDUSE_COL[["Cropland"]]
)

p3s_coia <- ggplot(
  coinertia_scores
) +
  geom_segment(
    aes(
      x = Tax_CoIA1,
      y = Tax_CoIA2,
      xend = N_CoIA1,
      yend = N_CoIA2,
      colour = LandUseTrajectory,
      linetype = Treatment
    ),
    alpha = 0.35,
    linewidth = 0.35
  ) +
  geom_point(
    aes(
      x = Tax_CoIA1,
      y = Tax_CoIA2,
      colour = LandUseTrajectory,
      shape = Year
    ),
    size = 1.7,
    alpha = 0.8
  ) +
  scale_colour_manual(
    values = TRAJECTORY_COL
  ) +
  scale_linetype_manual(
    values = TREATMENT_LTY
  ) +
  scale_shape_manual(
    values = YEAR_SHAPE
  ) +
  labs(
    x = "Co-inertia axis 1",
    y = "Co-inertia axis 2",
    colour = "Land-use trajectory",
    linetype = "Treatment",
    shape = "Year",
    title = "D  Taxonomic–functional co-inertia",
    subtitle = coia_subtitle
  ) +
  theme_soilwarm(
    9
  )


save_plot(
  p3s_coia,
  "FigureS3_taxonomy_N_activity_coinertia",
  width = 7.5,
  height = 5.8
)

# ---- 3D. Functional-gene potential PCA loadings ------------------------------

gene_pc_order <- intersect(
  c("PC1", "PC2", "PC3"),
  names(gene_loadings)
)

if (length(gene_pc_order) < 2L) {
  stop(
    "At least PC1 and PC2 must be present in gene-potential PCA loadings.",
    call. = FALSE
  )
}

gene_loadings_long <- gene_loadings |>
  select(
    gene,
    all_of(gene_pc_order)
  ) |>
  pivot_longer(
    cols = all_of(gene_pc_order),
    names_to = "PC",
    values_to = "loading"
  ) |>
  mutate(
    gene = factor(
      gene,
      levels = rev(c("AOA", "AOB", "ComA", "nirS", "nirK", "nosZ1"))
    ),
    PC = factor(
      PC,
      levels = gene_pc_order
    )
  )

gene_variance_labels <- gene_variance |>
  filter(axis %in% gene_pc_order) |>
  mutate(
    PC_label = paste0(
      axis,
      "\n",
      scales::percent(variance, accuracy = 0.1)
    )
  ) |>
  select(axis, PC_label)

gene_pc_label_map <- setNames(
  gene_variance_labels$PC_label,
  gene_variance_labels$axis
)

p3d <- ggplot(
  gene_loadings_long,
  aes(
    x = PC,
    y = gene,
    fill = loading
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = sprintf("%.2f", loading)),
    size = 2.8
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = max(abs(gene_loadings_long$loading), na.rm = TRUE) * c(-1, 1)
  ) +
  scale_x_discrete(
    labels = gene_pc_label_map
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Loading",
    title = "D  N functional-gene potential PCA",
    subtitle = "AOA, AOB, ComA, nirS, nirK and nosZ1; ComB excluded from PCA because of one missing value"
  ) +
  theme_soilwarm(9)

p_fig3 <- (
  p3a +
    p3b +
    p3c +
    p3d
) +
  patchwork::plot_layout(
    ncol = 2,
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

save_plot(
  p_fig3,
  "Figure3_taxonomy_N_coupling",
  width = 10.5,
  height = 8.0
)

# ==============================================================================
# FIGURE 4 — THREE-YEAR TEMPORAL TRAJECTORIES
# ==============================================================================

trajectory_scores <- read_required_csv(
  "08_trajectory_scores.csv"
)

trajectory_metrics <- read_required_csv(
  "08_trajectory_metrics.csv"
)

trajectory_tests <- read_required_csv(
  "08_trajectory_metric_correlations.csv"
)

temporal_rv <- read_required_csv(
  "08_temporal_RV_summary.csv"
)

relative_movement <- read_required_csv(
  "08_relative_interval_movement.csv"
)

relative_movement_summary <- read_required_csv(
  "08_relative_interval_movement_summary.csv"
)

timing_shift_test <- read_required_csv(
  "08_timing_shift_test.csv"
)

timing_shift_pairwise <- read_required_csv(
  "08_timing_shift_pairwise.csv"
)

# ---- 4A. MCOA trajectories ----------------------------------------------------

trajectory_scores <- trajectory_scores |>
  mutate(
    Year = factor(
      Year,
      levels = PRIMARY_YEARS
    ),
    LandUseTrajectory = case_when(
      as.character(LandUse) == "Grassland" ~ "Stable PP",
      as.character(LandUse) == "Cropland" ~ "TP -> Crop",
      TRUE ~ as.character(LandUse)
    ),
    Depth = factor(
      Depth,
      levels = c(
        "0-15",
        "15-30",
        "30-50"
      )
    )
  )

trajectory_centroids <- trajectory_scores |>
  group_by(
    LandUseTrajectory,
    Treatment,
    Depth,
    Year
  ) |>
  summarise(
    MCOA1 = mean(
      MCOA1,
      na.rm = TRUE
    ),
    MCOA2 = mean(
      MCOA2,
      na.rm = TRUE
    ),
    N_PC1 = mean(
      N_PC1,
      na.rm = TRUE
    ),
    N_PC2 = mean(
      N_PC2,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

p4a <- ggplot() +
  geom_path(
    data = trajectory_scores,
    aes(
      x = MCOA1,
      y = MCOA2,
      group = sample_unit_id,
      colour = LandUseTrajectory,
      linetype = Treatment
    ),
    linewidth = 0.30,
    alpha = 0.16,
    show.legend = FALSE
  ) +
  geom_point(
    data = trajectory_scores,
    aes(
      x = MCOA1,
      y = MCOA2,
      colour = LandUseTrajectory
    ),
    size = 0.8,
    alpha = 0.16,
    show.legend = FALSE
  ) +
  geom_path(
    data = trajectory_centroids,
    aes(
      x = MCOA1,
      y = MCOA2,
      group = interaction(
        LandUseTrajectory,
        Treatment
      ),
      colour = LandUseTrajectory,
      linetype = Treatment
    ),
    linewidth = 1.05,
    alpha = 0.95,
    arrow = grid::arrow(
      length = grid::unit(
        0.09,
        "inches"
      ),
      type = "closed"
    )
  ) +
  geom_point(
    data = trajectory_centroids,
    aes(
      x = MCOA1,
      y = MCOA2,
      colour = LandUseTrajectory,
      shape = Year
    ),
    size = 2.8,
    stroke = 0.65
  ) +
  facet_wrap(
    ~ Depth,
    nrow = 1
  ) +
  scale_colour_manual(
    values = TRAJECTORY_COL
  ) +
  scale_linetype_manual(
    values = TREATMENT_LTY
  ) +
  scale_shape_manual(
    values = YEAR_SHAPE
  ) +
  labs(
    x = "MCOA1",
    y = "MCOA2",
    colour = "Land-use trajectory",
    linetype = "Treatment",
    shape = "Year",
    title = "A  Multi-kingdom community trajectories",
    subtitle = "Pale lines: individual units; thick arrows: land-use trajectory × treatment centroids"
  ) +
  theme_soilwarm(
    9
  )

# ---- 4B. N-process PCA trajectories ------------------------------------------

if (!all(c("N_PC1", "N_PC2") %in% names(trajectory_scores))) {
  stop(
    "08_trajectory_scores.csv must contain N_PC1 and N_PC2.",
    call. = FALSE
  )
}

p4b <- ggplot() +
  geom_path(
    data = trajectory_scores,
    aes(
      x = N_PC1,
      y = N_PC2,
      group = sample_unit_id,
      colour = LandUseTrajectory,
      linetype = Treatment
    ),
    linewidth = 0.30,
    alpha = 0.16,
    show.legend = FALSE
  ) +
  geom_point(
    data = trajectory_scores,
    aes(
      x = N_PC1,
      y = N_PC2,
      colour = LandUseTrajectory
    ),
    size = 0.8,
    alpha = 0.16,
    show.legend = FALSE
  ) +
  geom_path(
    data = trajectory_centroids,
    aes(
      x = N_PC1,
      y = N_PC2,
      group = interaction(
        LandUseTrajectory,
        Treatment
      ),
      colour = LandUseTrajectory,
      linetype = Treatment
    ),
    linewidth = 1.05,
    alpha = 0.95,
    arrow = grid::arrow(
      length = grid::unit(
        0.09,
        "inches"
      ),
      type = "closed"
    )
  ) +
  geom_point(
    data = trajectory_centroids,
    aes(
      x = N_PC1,
      y = N_PC2,
      colour = LandUseTrajectory,
      shape = Year
    ),
    size = 2.8,
    stroke = 0.65
  ) +
  facet_wrap(
    ~ Depth,
    nrow = 1
  ) +
  scale_colour_manual(
    values = TRAJECTORY_COL
  ) +
  scale_linetype_manual(
    values = TREATMENT_LTY
  ) +
  scale_shape_manual(
    values = YEAR_SHAPE
  ) +
  labs(
    x = "N-process PC1",
    y = "N-process PC2",
    colour = "Land-use trajectory",
    linetype = "Treatment",
    shape = "Year",
    title = "B  Potential N-activity trajectories",
    subtitle = "Pale lines: individual units; thick arrows: land-use trajectory × treatment centroids"
  ) +
  theme_soilwarm(
    9
  )

# ---- 4C. Relative timing of trajectory movement -------------------------------

relative_summary_depth <- relative_movement_summary |>
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
      )
    ),
    Interval = factor(
      Interval,
      levels = c(
        "2022-2023",
        "2023-2024"
      ),
      labels = c(
        "2022→2023",
        "2023→2024"
      )
    )
  )

timing_subtitle <- if (nrow(timing_shift_pairwise) >= 2L) {
  gene_vs_tax <- timing_shift_pairwise |>
    filter(comparison == "Gene potential - Taxonomy") |>
    slice(1)

  activity_vs_gene <- timing_shift_pairwise |>
    filter(comparison == "N activities - Gene potential") |>
    slice(1)

  paste0(
    "Exact whole-profile timing tests: gene vs taxonomy ",
    format_p(gene_vs_tax$p_exact_two_sided),
    "; N activities vs gene ",
    format_p(activity_vs_gene$p_exact_two_sided)
  )
} else if (nrow(timing_shift_test) == 1L) {
  paste0(
    "Exact whole-profile label-swap test: ",
    format_p(timing_shift_test$p_exact_two_sided)
  )
} else {
  NULL
}

p4c <- ggplot(
  relative_summary_depth,
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
    linewidth = 0.75
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
      "N gene potential" = 15,
      "N functioning" = 17
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Taxonomy" = "solid",
      "N gene potential" = "dotdash",
      "N functioning" = "dashed"
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
    y = "Fraction of total 3-year trajectory",
    shape = NULL,
    linetype = NULL,
    title = "C  Temporal ordering across community, gene potential and N activity",
    subtitle = timing_subtitle
  ) +
  theme_soilwarm(
    9
  )

# ---- 4D. Temporal RV across time scales --------------------------------------

scope_order <- c(
  "pooled_annual_intervals",
  "2022-2023",
  "2023-2024",
  "net_2022-2024"
)

scope_labels <- c(
  pooled_annual_intervals =
    "Both annual\ntransitions",
  `2022-2023` =
    "2022→2023",
  `2023-2024` =
    "2023→2024",
  `net_2022-2024` =
    "Net\n2022→2024"
)

temporal_rv_plot <- temporal_rv |>
  mutate(
    scope = factor(
      scope,
      levels = scope_order
    ),
    adjustment_plot = adjustment_label(
      adjustment
    ),
    significant = p_perm < 0.05
  )

p4d <- ggplot(
  temporal_rv_plot,
  aes(
    x = scope,
    y = RV,
    shape = adjustment_plot
  )
) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = ifelse(
        significant,
        "*",
        ""
      )
    ),
    nudge_y = 0.02,
    size = 4
  ) +
  scale_x_discrete(
    labels = scope_labels,
    drop = FALSE
  ) +
  scale_shape_manual(
    values = ADJUSTMENT_SHAPE
  ) +
  labs(
    x = NULL,
    y = "RV between taxonomic and N-process change",
    shape = NULL,
    title = "D  Temporal covariance",
    subtitle =
      "Annual differences are permuted as whole-profile two-interval trajectories; * p < 0.05."
  ) +
  theme_soilwarm(
    9
  )

p_fig4 <- (
  p4a /
    p4b /
    (
      p4c +
        p4d
    )
) +
  patchwork::plot_layout(
    heights = c(
      1,
      1,
      0.95
    ),
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

save_plot(
  p_fig4,
  "Figure4_temporal_coupling",
  width = 11.5,
  height = 11.5
)

# ==============================================================================
# FIGURE S1 — TRAJECTORY GEOMETRY CORRELATIONS
# ==============================================================================

metric_labels <- c(
  total_length =
    "Total length",
  net_displacement =
    "Net displacement",
  straightness =
    "Straightness",
  early_fraction =
    "Fraction of movement\nin 2022→2023",
  turning_angle_deg =
    "Turning angle"
)

traj_test_plot <- trajectory_tests |>
  mutate(
    metric = factor(
      metric,
      levels = names(
        metric_labels
      )
    ),
    significant = p_FDR < 0.05
  )

p_s1 <- ggplot(
  traj_test_plot,
  aes(
    x = metric,
    y = rho
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    colour = "grey60"
  ) +
  geom_point(
    size = 3
  ) +
  geom_text(
    aes(
      label = ifelse(
        significant,
        "*",
        ""
      )
    ),
    nudge_y = 0.05,
    size = 4
  ) +
  scale_x_discrete(
    labels = metric_labels
  ) +
  coord_cartesian(
    ylim = c(
      -1,
      1
    )
  ) +
  labs(
    x = NULL,
    y = "Spearman ρ: taxonomy vs N trajectory metric",
    title = "Trajectory-geometry concordance",
    subtitle =
      "Whole-profile exact permutations; * FDR < 0.05."
  ) +
  theme_soilwarm(
    10
  ) +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

save_plot(
  p_s1,
  "FigureS1_trajectory_metric_correlations",
  width = 8.0,
  height = 4.8
)

# ==============================================================================
# FIGURE S2 — TREATMENT-SPECIFIC ANNUAL-TRANSITION RV
# ==============================================================================

treatment_rv <- read_required_csv(
  "08_treatment_specific_RV.csv"
)

treatment_diff <- read_required_csv(
  "08_treatment_difference_test.csv"
)

diff_subtitle <- if (
  nrow(treatment_diff) == 1L
) {
  paste0(
    "Heated − Control RV difference = ",
    sprintf(
      "%.2f",
      treatment_diff$RV_difference_Heated_minus_Control
    ),
    "; exact paired-label ",
    format_p(
      treatment_diff$p_exact_two_sided
    )
  )
} else {
  NULL
}

p_s2 <- ggplot(
  treatment_rv,
  aes(
    x = Treatment,
    y = RV,
    shape = Treatment
  )
) +
  geom_point(
    size = 3.5
  ) +
  scale_shape_manual(
    values = TREATMENT_SHAPE
  ) +
  labs(
    x = NULL,
    y = "RV across both annual transitions",
    shape = NULL,
    title = "Treatment-specific temporal coupling",
    subtitle = diff_subtitle
  ) +
  theme_soilwarm(
    10
  ) +
  theme(
    legend.position = "none"
  )

save_plot(
  p_s2,
  "FigureS2_treatment_specific_RV",
  width = 5.5,
  height = 4.5
)

message("Script 09 completed.")
message("Main figures written to: ", FIG_MAIN_DIR)
# ==============================================================================
