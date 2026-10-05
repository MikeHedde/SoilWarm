# ==============================================================================
# 09d_Figure3_global_change_drivers.R
# SOILWARM — manuscript Figure 3 for Soil Biology & Biochemistry
# ==============================================================================
#
# FIGURE 3
# --------
# A. Multivariate divergence associated with land-use conversion
#    (TP -> Crop minus Stable PP), across the three biological domains.
# B. Multivariate divergence associated with experimental warming
#    (Heated minus Control), across the three biological domains.
# C. Signed land-use contrasts for the four potential N activities.
# D. Signed warming contrasts for the four potential N activities.
#
# Only the two annual intervals are shown. Net 2022-2024 contrasts are reported
# in Table 1, not repeated here. Vector norms are domain-specific and MUST NOT be
# compared quantitatively among Taxonomy, N gene potential and N activities.
#
# REQUIRED INPUT TABLES
# ---------------------
# results/tables/08c_multivariate_DiD.csv
# results/tables/08d_warming_multivariate.csv
# results/tables/08c_process_DiD.csv
# results/tables/08d_warming_processes.csv
#
# OUTPUT
# ------
# results/figures_main/Figure3_global_change_drivers.pdf/png
# ============================================================================== 

source("R/config.R")

required_pkgs <- c("ggplot2", "dplyr", "tidyr", "readr", "patchwork")
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

theme_sbb <- function(base_size = 9.5) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text = element_text(colour = "black"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", size = base_size + 1),
      plot.subtitle = element_text(colour = "grey30"),
      panel.spacing.x = grid::unit(0.7, "lines"),
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
  "N functional-gene potential" = "N functional-gene potential",
  "Potential N activities" = "Potential N activities"
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
# 1. MULTIVARIATE CONTRASTS
# ==============================================================================

landuse_multi <- read_required_csv("08c_multivariate_DiD.csv") |>
  filter(Interval %in% INTERVAL_LEVELS) |>
  mutate(
    Domain = recode(
      Domain,
      "Taxonomy" = "Taxonomy",
      "N_gene_potential" = "N functional-gene potential",
      "N_function" = "Potential N activities"
    ),
    Domain = factor(Domain, levels = DOMAIN_LEVELS),
    Interval = factor(Interval, levels = INTERVAL_LEVELS),
    sig = ifelse(!is.na(p_FDR_within_domain) & p_FDR_within_domain < 0.05, "*", "")
  )

warming_multi <- read_required_csv("08d_warming_multivariate.csv") |>
  filter(Interval %in% INTERVAL_LEVELS) |>
  mutate(
    Domain = recode(
      Domain,
      "Taxonomy" = "Taxonomy",
      "N_gene_potential" = "N functional-gene potential",
      "N_activities" = "Potential N activities"
    ),
    Domain = factor(Domain, levels = DOMAIN_LEVELS),
    Interval = factor(Interval, levels = INTERVAL_LEVELS),
    # No warming contrast survives FDR in the current analysis. A dagger marks
    # nominal exact p < 0.05 only, to distinguish this from FDR significance.
    sig = case_when(
      !is.na(p_FDR_within_domain) & p_FDR_within_domain < 0.05 ~ "*",
      !is.na(p_exact) & p_exact < 0.05 ~ "\u2020",
      TRUE ~ ""
    )
  )

add_sig_height <- function(df) {
  df |>
    group_by(Domain) |>
    mutate(y_sig = vector_norm + 0.12 * max(vector_norm, na.rm = TRUE)) |>
    ungroup()
}

landuse_multi <- add_sig_height(landuse_multi)
warming_multi <- add_sig_height(warming_multi)

p3a <- ggplot(
  landuse_multi,
  aes(x = Interval, y = vector_norm, group = 1)
) +
  geom_line(linewidth = 0.55, colour = "grey45") +
  geom_point(size = 2.8, shape = 21, fill = "white", stroke = 0.8) +
  geom_text(aes(y = y_sig, label = sig), size = 4.5, vjust = 0) +
  facet_wrap(~ Domain, scales = "free_y", nrow = 1, labeller = as_labeller(DOMAIN_LABELS)) +
  scale_x_discrete(labels = INTERVAL_LABELS) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  labs(
    x = NULL,
    y = "Norm of mean change contrast",
    title = "A  Land-use conversion",
    subtitle = "TP → Crop minus Stable PP"
  ) +
  theme_sbb(9.5) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

p3b <- ggplot(
  warming_multi,
  aes(x = Interval, y = vector_norm, group = 1)
) +
  geom_line(linewidth = 0.55, colour = "grey45") +
  geom_point(size = 2.8, shape = 21, fill = "white", stroke = 0.8) +
  geom_text(aes(y = y_sig, label = sig), size = 4.5, vjust = 0) +
  facet_wrap(~ Domain, scales = "free_y", nrow = 1, labeller = as_labeller(DOMAIN_LABELS)) +
  scale_x_discrete(labels = INTERVAL_LABELS) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  labs(
    x = NULL,
    y = "Norm of mean change contrast",
    title = "B  Experimental warming",
    subtitle = "Heated minus Control"
  ) +
  theme_sbb(9.5) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# ==============================================================================
# 2. PROCESS-SPECIFIC SIGNED CONTRASTS
# ==============================================================================

landuse_process <- read_required_csv("08c_process_DiD.csv") |>
  filter(Interval %in% INTERVAL_LEVELS) |>
  transmute(
    Interval,
    Process,
    estimate = mean_DiD,
    p_exact = p_exact_two_sided,
    q = p_FDR_within_interval
  ) |>
  mutate(
    Process = factor(Process, levels = PROCESS_LEVELS),
    Interval = factor(Interval, levels = INTERVAL_LEVELS)
  )

warming_process <- read_required_csv("08d_warming_processes.csv") |>
  filter(Interval %in% INTERVAL_LEVELS) |>
  transmute(
    Interval,
    Process,
    estimate = mean_warming_contrast,
    p_exact = p_exact_two_sided,
    q = p_FDR_within_interval
  ) |>
  mutate(
    Process = factor(Process, levels = PROCESS_LEVELS),
    Interval = factor(Interval, levels = INTERVAL_LEVELS)
  )

# No process-specific contrast survives FDR in these panels; the purpose is to
# show the direction and reorganisation of the multivariate signal rather than
# to promote individual nominal tests.
p3c <- ggplot(
  landuse_process,
  aes(x = Interval, y = estimate, group = Process)
) +
  geom_hline(yintercept = 0, linewidth = 0.45, colour = "grey50") +
  geom_line(linewidth = 0.55, colour = "grey45") +
  geom_point(size = 2.8, shape = 21, fill = "white", stroke = 0.8) +
  facet_wrap(~ Process, nrow = 1, labeller = as_labeller(PROCESS_LABELS)) +
  scale_x_discrete(labels = INTERVAL_LABELS) +
  labs(
    x = NULL,
    y = "Signed activity contrast\n(standardised units)",
    title = "C  N activities underlying the land-use response"
  ) +
  theme_sbb(9.5)

p3d <- ggplot(
  warming_process,
  aes(x = Interval, y = estimate, group = Process)
) +
  geom_hline(yintercept = 0, linewidth = 0.45, colour = "grey50") +
  geom_line(linewidth = 0.55, colour = "grey45") +
  geom_point(size = 2.8, shape = 21, fill = "white", stroke = 0.8) +
  facet_wrap(~ Process, nrow = 1, labeller = as_labeller(PROCESS_LABELS)) +
  scale_x_discrete(labels = INTERVAL_LABELS) +
  labs(
    x = NULL,
    y = "Signed activity contrast\n(standardised units)",
    title = "D  N activities underlying the warming response"
  ) +
  theme_sbb(9.5)

# ==============================================================================
# 3. COMBINE AND SAVE
# ==============================================================================

# Top panels need width for three domain facets; bottom panels are aligned below.
figure3 <- ((p3a / p3c) | (p3b / p3d)) +
  plot_layout(widths = c(1, 1))

figure3 <- figure3 +
  plot_annotation(
    caption = paste0(
      "* FDR-adjusted q < 0.05; \u2020 nominal exact p < 0.05 but q >= 0.05. ",
      "Vector norms are expressed in domain-specific ordination spaces and are not directly comparable among domains. ",
      "Net 2022–2024 contrasts are reported in Table 1."
    )
  ) &
  theme(plot.caption = element_text(size = 8, hjust = 0, colour = "grey30"))

save_plot(
  figure3,
  "Figure3_global_change_drivers",
  width = 12.0,
  height = 7.8
)

message("Figure 3 written to: ", FIG_MAIN_DIR)
