# ==============================================================================
# 09c_Figure2_temporal_decoupling.R
# SOILWARM — manuscript Figure 2 for Soil Biology & Biochemistry
# ==============================================================================
#
# FIGURE 2
# --------
# A. Temporal allocation of multivariate change across the three biological
#    domains. Depths are averaged within each experimental profile before
#    plotting, so the descriptive SE is based on 12 independent profiles.
#
# B. Magnitude of annual change in the four potential N activities. Again,
#    depths are averaged within profile before plotting. Asterisks identify
#    processes for which the early-vs-late difference remains significant after
#    FDR correction (q < 0.05).
#
# This figure deliberately omits depth-specific results, RV coefficients and
# most p-values; these belong in the text, Table 1 and Supplementary Material.
#
# REQUIRED INPUT TABLES
# ---------------------
# results/tables/08_relative_interval_movement.csv
# results/tables/08_timing_shift_pairwise.csv
# results/tables/08b_process_interval_changes.csv
# results/tables/08b_process_timing_tests.csv
#
# OUTPUT
# ------
# results/figures_main/Figure2_temporal_decoupling.pdf/png
# ============================================================================== 

source("R/config.R")

required_pkgs <- c("ggplot2", "dplyr", "tidyr", "readr", "patchwork", "scales")
missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_pkgs) > 0L) {
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(patchwork)
  library(scales)
})

FIG_MAIN_DIR <- file.path(OUT_DIR, "figures_main")
dir.create(FIG_MAIN_DIR, recursive = TRUE, showWarnings = FALSE)

read_required_csv <- function(filename) {
  path <- file.path(TABLE_DIR, filename)
  if (!file.exists(path)) stop("Required file not found: ", path, call. = FALSE)
  readr::read_csv(path, show_col_types = FALSE)
}

save_plot <- function(p, stem, width, height) {
  ggsave(
    file.path(FIG_MAIN_DIR, paste0(stem, ".pdf")),
    p, width = width, height = height, units = "in", device = cairo_pdf
  )
  ggsave(
    file.path(FIG_MAIN_DIR, paste0(stem, ".png")),
    p, width = width, height = height, units = "in", dpi = 400
  )
}

theme_sbb <- function(base_size = 10) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(colour = "grey30"),
      plot.margin = margin(6, 8, 6, 6)
    )
}

DOMAIN_LEVELS <- c(
  "Taxonomy",
  "N functional-gene potential",
  "Potential N activities"
)

DOMAIN_LABELS <- c(
  "Taxonomy" = "Taxonomy",
  "N functional-gene potential" = "N functional-gene\npotential",
  "Potential N activities" = "Potential N\nactivities"
)

PROCESS_LEVELS <- c("PGNP", "PNNP", "N2O_reduction", "NEA")
PROCESS_LABELS <- c(
  "PGNP" = "PGNP",
  "PNNP" = "PNNP",
  "N2O_reduction" = "N₂O reduction",
  "NEA" = "NEA"
)

INTERVAL_LEVELS <- c("2022-2023", "2023-2024")
INTERVAL_LABELS <- c("2022-2023" = "2022–2023", "2023-2024" = "2023–2024")

# ==============================================================================
# A. TEMPORAL ALLOCATION OF MULTIVARIATE CHANGE
# ==============================================================================

relative_individual <- read_required_csv("08_relative_interval_movement.csv")
timing_pairwise <- read_required_csv("08_timing_shift_pairwise.csv")

# The fraction for 2022-2023 is sufficient because both annual fractions sum to 1.
# Average the three depths within each experimental profile BEFORE computing the
# displayed mean and SE. Thus n = 12 independent profiles per domain.
profile_early <- relative_individual |>
  filter(Interval == "2022-2023") |>
  mutate(
    Domain = recode(
      Domain,
      "Taxonomy" = "Taxonomy",
      "N gene potential" = "N functional-gene potential",
      "N functioning" = "Potential N activities"
    )
  ) |>
  group_by(profile_id, Domain) |>
  summarise(
    early_fraction = mean(fraction_total_trajectory, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Domain = factor(Domain, levels = DOMAIN_LEVELS)
  )

profile_early_summary <- profile_early |>
  group_by(Domain) |>
  summarise(
    n_profiles = sum(is.finite(early_fraction)),
    mean_early = mean(early_fraction, na.rm = TRUE),
    se_early = sd(early_fraction, na.rm = TRUE) / sqrt(n_profiles),
    .groups = "drop"
  ) |>
  mutate(
    label = scales::percent(mean_early, accuracy = 1)
  )

# Pairwise tests give: Taxonomy = Gene potential != Potential N activities.
# Use compact letters only if the expected pattern is present; otherwise omit.
letters_df <- tibble(
  Domain = factor(DOMAIN_LEVELS, levels = DOMAIN_LEVELS),
  letter = c("a", "a", "b")
)

if (nrow(timing_pairwise) == 3L) {
  q_gene_tax <- timing_pairwise |>
    filter(comparison == "Gene potential - Taxonomy") |>
    pull(p_FDR_pairwise)
  q_act_gene <- timing_pairwise |>
    filter(comparison == "N activities - Gene potential") |>
    pull(p_FDR_pairwise)
  q_act_tax <- timing_pairwise |>
    filter(comparison == "N activities - Taxonomy") |>
    pull(p_FDR_pairwise)

  if (
    length(q_gene_tax) != 1L || length(q_act_gene) != 1L || length(q_act_tax) != 1L ||
    is.na(q_gene_tax) || is.na(q_act_gene) || is.na(q_act_tax) ||
    !(q_gene_tax >= 0.05 && q_act_gene < 0.05 && q_act_tax < 0.05)
  ) {
    letters_df$letter <- ""
  }
} else {
  letters_df$letter <- ""
}

profile_early_summary <- profile_early_summary |>
  left_join(letters_df, by = "Domain")

p2a <- ggplot(profile_early, aes(x = Domain, y = early_fraction)) +
  geom_hline(yintercept = 0.5, linetype = 3, linewidth = 0.45, colour = "grey50") +
  geom_jitter(width = 0.08, height = 0, alpha = 0.42, size = 1.7) +
  geom_errorbar(
    data = profile_early_summary,
    aes(x = Domain, ymin = mean_early - se_early, ymax = mean_early + se_early),
    width = 0.10, linewidth = 0.55, inherit.aes = FALSE
  ) +
  geom_point(
    data = profile_early_summary,
    aes(x = Domain, y = mean_early),
    size = 3.2, shape = 21, fill = "white", stroke = 0.8,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = profile_early_summary,
    aes(x = Domain, y = pmin(0.95, mean_early + se_early + 0.07), label = label),
    size = 3.2, fontface = "bold", inherit.aes = FALSE
  ) +
  geom_text(
    data = profile_early_summary,
    aes(x = Domain, y = pmin(0.99, mean_early + se_early + 0.14), label = letter),
    size = 4, inherit.aes = FALSE
  ) +
  scale_x_discrete(labels = DOMAIN_LABELS) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Fraction of total 3-year trajectory\noccurring in 2022–2023",
    title = "A  Timing of multivariate change",
    subtitle = "Small points are profile means after averaging the three soil depths; large points are mean ± SE"
  ) +
  theme_sbb(10)

# ==============================================================================
# B. MAGNITUDE OF CHANGE IN INDIVIDUAL N ACTIVITIES
# ==============================================================================

process_changes <- read_required_csv("08b_process_interval_changes.csv")
process_tests <- read_required_csv("08b_process_timing_tests.csv")

# Average depths first: one value per profile × process × interval.
process_profile <- process_changes |>
  group_by(profile_id, Process, Interval) |>
  summarise(
    profile_mean_abs_change = mean(abs_delta, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Process = factor(Process, levels = PROCESS_LEVELS),
    Interval = factor(Interval, levels = INTERVAL_LEVELS)
  )

process_summary <- process_profile |>
  group_by(Process, Interval) |>
  summarise(
    n_profiles = sum(is.finite(profile_mean_abs_change)),
    mean_abs = mean(profile_mean_abs_change, na.rm = TRUE),
    se_abs = sd(profile_mean_abs_change, na.rm = TRUE) / sqrt(n_profiles),
    .groups = "drop"
  )

process_ann <- process_tests |>
  mutate(
    Process = factor(Process, levels = PROCESS_LEVELS),
    sig = ifelse(!is.na(p_FDR) & p_FDR < 0.05, "*", "")
  ) |>
  left_join(
    process_summary |>
      group_by(Process) |>
      summarise(y = max(mean_abs + se_abs, na.rm = TRUE), .groups = "drop"),
    by = "Process"
  ) |>
  mutate(y = y + 0.12)

# Use shape/fill rather than colour so the figure remains clear in greyscale.
p2b <- ggplot(
  process_profile,
  aes(x = Process, y = profile_mean_abs_change, shape = Interval, fill = Interval)
) +
  geom_point(
    position = position_jitterdodge(jitter.width = 0.08, dodge.width = 0.48),
    alpha = 0.35, size = 1.5, stroke = 0.3
  ) +
  geom_errorbar(
    data = process_summary,
    aes(x = Process, ymin = mean_abs - se_abs, ymax = mean_abs + se_abs, group = Interval),
    position = position_dodge(width = 0.48),
    width = 0.10, linewidth = 0.5, inherit.aes = FALSE
  ) +
  geom_point(
    data = process_summary,
    aes(x = Process, y = mean_abs, shape = Interval, fill = Interval, group = Interval),
    position = position_dodge(width = 0.48),
    size = 3.0, stroke = 0.8, inherit.aes = FALSE
  ) +
  geom_text(
    data = process_ann,
    aes(x = Process, y = y, label = sig),
    size = 5, vjust = 0, inherit.aes = FALSE
  ) +
  scale_x_discrete(labels = PROCESS_LABELS) +
  scale_shape_manual(values = c("2022-2023" = 21, "2023-2024" = 24), labels = INTERVAL_LABELS) +
  scale_fill_manual(values = c("2022-2023" = "white", "2023-2024" = "grey45"), labels = INTERVAL_LABELS) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  labs(
    x = NULL,
    y = "Mean absolute annual change\n(standardised activity units)",
    title = "B  Timing of individual potential N activities",
    subtitle = "* FDR-adjusted q < 0.05 for the exact profile-level early-versus-late test"
  ) +
  theme_sbb(10) +
  theme(legend.position = "top")

# ==============================================================================
# COMBINE AND SAVE
# ==============================================================================

figure2 <- p2a / p2b +
  plot_layout(heights = c(1.0, 1.05)) +
  plot_annotation(
    caption = paste0(
      "Profile-level means are based on 12 independent experimental profiles after averaging the three soil depths. ",
      "Exact profile-level sign-flip tests are reported in Table 1 and the Supplementary Material."
    )
  ) &
  theme(plot.caption = element_text(size = 8, hjust = 0, colour = "grey30"))

save_plot(
  figure2,
  "Figure2_temporal_decoupling",
  width = 7.4,
  height = 8.0
)

message("Figure 2 written to: ", FIG_MAIN_DIR)
