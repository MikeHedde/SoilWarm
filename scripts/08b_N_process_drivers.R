# ==============================================================================
# 08b_N_process_drivers.R
# SOILWARM — which realised N processes drive the late functional shift?
# ==============================================================================
#
# PURPOSE
# -------
# Decompose the 2022->2023 and 2023->2024 movement of the realised N-process
# block into its four original variables:
#   PGNP, PNNP, N2O_reduction, NEA
#
# The input variables are the exact processed variables used to build the N PCA
# in script 06:
#   log1p(raw corrected value), then z-standardisation across pooled 2022-2024.
#
# PRIMARY TEST
# ------------
# For each process and each depth-specific sampling unit:
#
#   timing_difference = |Delta_2023-2024| - |Delta_2022-2023|
#
# Positive values mean that the process moved more strongly in the late
# transition. Significance is assessed by exact interval-label swaps at the
# WHOLE-PROFILE level: the same sign is applied to all three depths of a profile.
# With 12 profiles this gives 2^12 = 4096 exact permutations.
#
# SECONDARY OUTPUTS
# -----------------
# - signed changes in each interval, to identify the direction of the shift;
# - contribution of each process to squared Euclidean movement in the complete
#   four-variable standardised process space.
#
# The contribution calculation is descriptive:
#
#   contribution_j = Delta_j^2 / sum_k Delta_k^2
#
# Because PCA is an orthogonal rotation, total squared distance is preserved if
# all four PCA axes are retained. The contribution is therefore interpretable as
# the variable-level share of movement in the complete standardised process
# space, not as a causal contribution.
#
# INPUT
# -----
# intermediate/06_N_temporal_block.rds
#
# OUTPUTS
# -------
# results/tables/08b_process_interval_changes.csv
# results/tables/08b_process_interval_summary.csv
# results/tables/08b_process_interval_summary_by_depth.csv
# results/tables/08b_process_timing_tests.csv
# results/tables/08b_process_timing_tests_by_depth.csv
# results/tables/08b_process_direction_tests.csv
# results/tables/08b_process_contribution_summary.csv
# results/figures/08b_N_process_drivers.pdf
# ==============================================================================

source("R/config.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
})

source("R/functions_soilwarm.R")

require_intermediate(
  file.path(INT_DIR, "06_N_temporal_block.rds"),
  "scripts/06_build_N_function_block.R"
)

n_dat <- readRDS(
  file.path(INT_DIR, "06_N_temporal_block.rds")
)

PROCESS_VARS <- c(
  "PGNP",
  "PNNP",
  "N2O_reduction",
  "NEA"
)

EXPECTED_YEARS <- c(
  2022L,
  2023L,
  2024L
)

# ------------------------------------------------------------------------------
# 1. Structural QC
# ------------------------------------------------------------------------------

required_cols <- c(
  "MET",
  "Year",
  "LandUse",
  "Treatment",
  "Plot",
  "Block",
  "Depth",
  "profile_id",
  "sample_unit_id",
  PROCESS_VARS
)

missing_cols <- setdiff(
  required_cols,
  names(n_dat)
)

if (length(missing_cols) > 0L) {
  stop(
    "Missing columns in 06_N_temporal_block.rds: ",
    paste(
      missing_cols,
      collapse = ", "
    ),
    call. = FALSE
  )
}

n_dat <- n_dat |>
  mutate(
    Year = as.integer(
      as.character(
        Year
      )
    ),
    profile_id = as.character(
      profile_id
    ),
    sample_unit_id = as.character(
      sample_unit_id
    ),
    Depth = as.character(
      Depth
    ),
    LandUse = as.character(
      LandUse
    ),
    Treatment = as.character(
      Treatment
    ),
    Block = as.character(
      Block
    )
  )

if (nrow(n_dat) != 108L) {
  stop(
    "Expected 108 processed N observations; found ",
    nrow(n_dat),
    ".",
    call. = FALSE
  )
}

if (!identical(
  sort(
    unique(
      n_dat$Year
    )
  ),
  EXPECTED_YEARS
)) {
  stop(
    "Expected years 2022, 2023 and 2024.",
    call. = FALSE
  )
}

unit_qc <- n_dat |>
  count(
    sample_unit_id,
    name = "n_rows"
  )

if (
  nrow(
    unit_qc
  ) != 36L ||
    any(
      unit_qc$n_rows != 3L
    )
) {
  stop(
    "Expected 36 sample_unit_id values with exactly 3 annual observations each.",
    call. = FALSE
  )
}

profile_qc <- n_dat |>
  count(
    profile_id,
    name = "n_rows"
  )

if (
  nrow(
    profile_qc
  ) != 12L ||
    any(
      profile_qc$n_rows != 9L
    )
) {
  stop(
    "Expected 12 profiles with exactly 9 observations each.",
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 2. Build signed and absolute annual changes
# ------------------------------------------------------------------------------

build_process_changes <- function(
  df,
  vars
) {
  groups <- split(
    df,
    df$sample_unit_id
  )

  purrr::map_dfr(
    groups,
    function(g) {
      g <- g |>
        arrange(
          Year
        )

      if (!identical(
        g$Year,
        EXPECTED_YEARS
      )) {
        stop(
          "Each sample_unit_id must contain exactly 2022, 2023, 2024.",
          call. = FALSE
        )
      }

      purrr::map_dfr(
        1:2,
        function(i) {
          interval <- paste0(
            g$Year[i],
            "-",
            g$Year[i + 1L]
          )

          purrr::map_dfr(
            vars,
            function(v) {
              delta <-
                g[[v]][i + 1L] -
                g[[v]][i]

              tibble(
                sample_unit_id =
                  g$sample_unit_id[i],
                profile_id =
                  g$profile_id[i],
                LandUse =
                  g$LandUse[i],
                Treatment =
                  g$Treatment[i],
                Plot =
                  g$Plot[i],
                Block =
                  g$Block[i],
                Depth =
                  g$Depth[i],
                Year_from =
                  g$Year[i],
                Year_to =
                  g$Year[i + 1L],
                Interval =
                  interval,
                Process =
                  v,
                delta =
                  delta,
                abs_delta =
                  abs(
                    delta
                  ),
                sq_delta =
                  delta ^ 2
              )
            }
          )
        }
      )
    }
  )
}

changes <- build_process_changes(
  n_dat,
  PROCESS_VARS
)

if (nrow(changes) != 288L) {
  stop(
    "Expected 288 process-change rows (36 units × 2 intervals × 4 processes); found ",
    nrow(changes),
    ".",
    call. = FALSE
  )
}

# Variable-level share of complete 4-D process movement.
changes <- changes |>
  group_by(
    sample_unit_id,
    Interval
  ) |>
  mutate(
    total_sq_distance =
      sum(
        sq_delta
      ),
    contribution =
      if_else(
        total_sq_distance > 0,
        sq_delta /
          total_sq_distance,
        NA_real_
      )
  ) |>
  ungroup()

# ------------------------------------------------------------------------------
# 3. Exact whole-profile sign-flip helper
# ------------------------------------------------------------------------------

exact_profile_signflip <- function(
  values,
  profile_id
) {
  values <- as.numeric(
    values
  )

  profile_id <- as.character(
    profile_id
  )

  profiles <- unique(
    profile_id
  )

  n_profiles <- length(
    profiles
  )

  if (n_profiles > 20L) {
    stop(
      "Exact sign-flip enumeration is limited to <=20 profiles.",
      call. = FALSE
    )
  }

  sign_grid <- expand.grid(
    rep(
      list(
        c(
          -1,
          1
        )
      ),
      n_profiles
    ),
    KEEP.OUT.ATTRS = FALSE
  )

  observed <- mean(
    values,
    na.rm = TRUE
  )

  null <- numeric(
    nrow(
      sign_grid
    )
  )

  for (
    i in seq_len(
      nrow(
        sign_grid
      )
    )
  ) {
    signs <- setNames(
      as.numeric(
        sign_grid[
          i,
          ,
          drop = TRUE
        ]
      ),
      profiles
    )

    null[i] <- mean(
      values *
        unname(
          signs[
            profile_id
          ]
        ),
      na.rm = TRUE
    )
  }

  tibble(
    estimate = observed,
    p_exact_two_sided =
      mean(
        abs(
          null
        ) >=
          abs(
            observed
          ) -
          1e-12
      ),
    n_profile_sign_flips =
      length(
        null
      ),
    null_q025 =
      unname(
        quantile(
          null,
          0.025
        )
      ),
    null_median =
      unname(
        quantile(
          null,
          0.5
        )
      ),
    null_q975 =
      unname(
        quantile(
          null,
          0.975
        )
      )
  )
}

# ------------------------------------------------------------------------------
# 4. Descriptive summaries
# ------------------------------------------------------------------------------

process_interval_summary <- changes |>
  group_by(
    Process,
    Interval
  ) |>
  summarise(
    n = n(),
    mean_delta =
      mean(
        delta
      ),
    median_delta =
      median(
        delta
      ),
    sd_delta =
      sd(
        delta
      ),
    se_delta =
      sd_delta /
      sqrt(
        n
      ),
    mean_abs_delta =
      mean(
        abs_delta
      ),
    sd_abs_delta =
      sd(
        abs_delta
      ),
    se_abs_delta =
      sd_abs_delta /
      sqrt(
        n
      ),
    mean_sq_delta =
      mean(
        sq_delta
      ),
    mean_contribution =
      mean(
        contribution,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

process_interval_summary_by_depth <- changes |>
  group_by(
    Depth,
    Process,
    Interval
  ) |>
  summarise(
    n = n(),
    mean_delta =
      mean(
        delta
      ),
    median_delta =
      median(
        delta
      ),
    sd_delta =
      sd(
        delta
      ),
    se_delta =
      sd_delta /
      sqrt(
        n
      ),
    mean_abs_delta =
      mean(
        abs_delta
      ),
    sd_abs_delta =
      sd(
        abs_delta
      ),
    se_abs_delta =
      sd_abs_delta /
      sqrt(
        n
      ),
    mean_sq_delta =
      mean(
        sq_delta
      ),
    mean_contribution =
      mean(
        contribution,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 5. Primary timing test: is |Delta| larger in 2023->2024?
# ------------------------------------------------------------------------------

timing_wide <- changes |>
  select(
    sample_unit_id,
    profile_id,
    LandUse,
    Treatment,
    Plot,
    Block,
    Depth,
    Process,
    Interval,
    abs_delta
  ) |>
  pivot_wider(
    names_from =
      Interval,
    values_from =
      abs_delta,
    names_prefix =
      "abs_"
  ) |>
  mutate(
    late_minus_early =
      `abs_2023-2024` -
      `abs_2022-2023`
  )

process_timing_tests <- purrr::map_dfr(
  PROCESS_VARS,
  function(v) {
    dat <- timing_wide |>
      filter(
        Process == v
      )

    exact_profile_signflip(
      dat$late_minus_early,
      dat$profile_id
    ) |>
      mutate(
        Process = v,
        mean_abs_2022_2023 =
          mean(
            dat$`abs_2022-2023`
          ),
        mean_abs_2023_2024 =
          mean(
            dat$`abs_2023-2024`
          ),
        late_minus_early =
          mean_abs_2023_2024 -
          mean_abs_2022_2023,
        .before = 1
      )
  }
) |>
  mutate(
    p_FDR =
      p.adjust(
        p_exact_two_sided,
        method = "BH"
      )
  )

process_timing_tests_by_depth <- purrr::map_dfr(
  c(
    "0-15",
    "15-30",
    "30-50"
  ),
  function(one_depth) {
    purrr::map_dfr(
      PROCESS_VARS,
      function(v) {
        dat <- timing_wide |>
          filter(
            Depth == one_depth,
            Process == v
          )

        exact_profile_signflip(
          dat$late_minus_early,
          dat$profile_id
        ) |>
          mutate(
            Depth = one_depth,
            Process = v,
            mean_abs_2022_2023 =
              mean(
                dat$`abs_2022-2023`
              ),
            mean_abs_2023_2024 =
              mean(
                dat$`abs_2023-2024`
              ),
            late_minus_early =
              mean_abs_2023_2024 -
              mean_abs_2022_2023,
            .before = 1
          )
      }
    )
  }
) |>
  group_by(
    Depth
  ) |>
  mutate(
    p_FDR_within_depth =
      p.adjust(
        p_exact_two_sided,
        method = "BH"
      )
  ) |>
  ungroup() |>
  mutate(
    p_FDR_all_12 =
      p.adjust(
        p_exact_two_sided,
        method = "BH"
      )
  )

# ------------------------------------------------------------------------------
# 6. Direction tests within each interval
# ------------------------------------------------------------------------------

process_direction_tests <- purrr::map_dfr(
  c(
    "2022-2023",
    "2023-2024"
  ),
  function(one_interval) {
    purrr::map_dfr(
      PROCESS_VARS,
      function(v) {
        dat <- changes |>
          filter(
            Interval == one_interval,
            Process == v
          )

        exact_profile_signflip(
          dat$delta,
          dat$profile_id
        ) |>
          mutate(
            Interval =
              one_interval,
            Process =
              v,
            mean_delta =
              mean(
                dat$delta
              ),
            median_delta =
              median(
                dat$delta
              ),
            .before = 1
          )
      }
    ) |>
      mutate(
        p_FDR_within_interval =
          p.adjust(
            p_exact_two_sided,
            method = "BH"
          )
      )
  }
) |>
  mutate(
    p_FDR_all_8 =
      p.adjust(
        p_exact_two_sided,
        method = "BH"
      )
  )

# ------------------------------------------------------------------------------
# 7. Contribution summary
# ------------------------------------------------------------------------------

process_contribution_summary <- changes |>
  group_by(
    Depth,
    Interval,
    Process
  ) |>
  summarise(
    n = n(),
    mean_contribution =
      mean(
        contribution,
        na.rm = TRUE
      ),
    sd_contribution =
      sd(
        contribution,
        na.rm = TRUE
      ),
    se_contribution =
      sd_contribution /
      sqrt(
        n
      ),
    .groups = "drop"
  )

process_contribution_overall <- changes |>
  group_by(
    Interval,
    Process
  ) |>
  summarise(
    n = n(),
    mean_contribution =
      mean(
        contribution,
        na.rm = TRUE
      ),
    sd_contribution =
      sd(
        contribution,
        na.rm = TRUE
      ),
    se_contribution =
      sd_contribution /
      sqrt(
        n
      ),
    .groups = "drop"
  ) |>
  mutate(
    Depth = "All",
    .before = 1
  )

process_contribution_summary <- bind_rows(
  process_contribution_overall,
  process_contribution_summary
)

# ------------------------------------------------------------------------------
# 8. Exports
# ------------------------------------------------------------------------------

write.csv(
  changes,
  file.path(
    TABLE_DIR,
    "08b_process_interval_changes.csv"
  ),
  row.names = FALSE
)

write.csv(
  process_interval_summary,
  file.path(
    TABLE_DIR,
    "08b_process_interval_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  process_interval_summary_by_depth,
  file.path(
    TABLE_DIR,
    "08b_process_interval_summary_by_depth.csv"
  ),
  row.names = FALSE
)

write.csv(
  process_timing_tests,
  file.path(
    TABLE_DIR,
    "08b_process_timing_tests.csv"
  ),
  row.names = FALSE
)

write.csv(
  process_timing_tests_by_depth,
  file.path(
    TABLE_DIR,
    "08b_process_timing_tests_by_depth.csv"
  ),
  row.names = FALSE
)

write.csv(
  process_direction_tests,
  file.path(
    TABLE_DIR,
    "08b_process_direction_tests.csv"
  ),
  row.names = FALSE
)

write.csv(
  process_contribution_summary,
  file.path(
    TABLE_DIR,
    "08b_process_contribution_summary.csv"
  ),
  row.names = FALSE
)

analysis_08b <- list(
  process_variables =
    PROCESS_VARS,
  process_changes =
    changes,
  interval_summary =
    process_interval_summary,
  interval_summary_by_depth =
    process_interval_summary_by_depth,
  timing_tests =
    process_timing_tests,
  timing_tests_by_depth =
    process_timing_tests_by_depth,
  direction_tests =
    process_direction_tests,
  contribution_summary =
    process_contribution_summary
)

saveRDS(
  analysis_08b,
  file.path(
    INT_DIR,
    "08b_N_process_drivers.rds"
  )
)

# ------------------------------------------------------------------------------
# 9. Publication-oriented process-driver figure
# ------------------------------------------------------------------------------
#
# Panel A: overall magnitude of change in each interval.
# Panel B: late-minus-early change magnitude by depth (direct effect-size view).
# Panel C: signed direction of change in each interval.
#
# Categorical processes are NOT connected by lines because there is no natural
# continuity from PGNP -> PNNP -> N2O reduction -> NEA.

process_levels <- c(
  "PGNP",
  "PNNP",
  "N2O_reduction",
  "NEA"
)

process_labels <- c(
  "PGNP" = "PGNP",
  "PNNP" = "PNNP",
  "N2O_reduction" = expression(N[2]*O~reduction),
  "NEA" = "NEA"
)

interval_levels <- c(
  "2022-2023",
  "2023-2024"
)

interval_labels <- c(
  "2022-2023" = "2022-2023",
  "2023-2024" = "2023-2024"
)

interval_shapes <- c(
  "2022-2023" = 16,
  "2023-2024" = 17
)

# ---- Panel A: overall magnitude ----------------------------------------------

plot_summary <- process_interval_summary |>
  mutate(
    Process = factor(
      Process,
      levels = process_levels
    ),
    Interval = factor(
      Interval,
      levels = interval_levels
    )
  )

timing_labels <- process_timing_tests |>
  mutate(
    Process = factor(
      Process,
      levels = process_levels
    ),
    label = if_else(
      p_FDR < 0.001,
      "q < 0.001",
      paste0(
        "q = ",
        formatC(
          p_FDR,
          format = "f",
          digits = 3
        )
      )
    ),
    y = purrr::map_dbl(
      as.character(Process),
      function(v) {
        max(
          plot_summary$mean_abs_delta[
            as.character(plot_summary$Process) == v
          ] +
            plot_summary$se_abs_delta[
              as.character(plot_summary$Process) == v
            ],
          na.rm = TRUE
        ) * 1.13
      }
    )
  )

pd <- position_dodge(
  width = 0.42
)

pA <- ggplot(
  plot_summary,
  aes(
    x = Process,
    y = mean_abs_delta,
    shape = Interval
  )
) +
  geom_point(
    size = 3.0,
    position = pd
  ) +
  geom_errorbar(
    aes(
      ymin = pmax(
        0,
        mean_abs_delta - se_abs_delta
      ),
      ymax = mean_abs_delta + se_abs_delta
    ),
    width = 0.10,
    linewidth = 0.45,
    position = pd
  ) +
  geom_text(
    data = timing_labels,
    aes(
      x = Process,
      y = y,
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.2
  ) +
  scale_shape_manual(
    values = interval_shapes,
    labels = interval_labels,
    name = "Interval"
  ) +
  scale_x_discrete(
    labels = process_labels
  ) +
  labs(
    x = NULL,
    y = expression(
      "Mean |" * Delta * "z|"
    ),
    title = "A  Magnitude of process change",
    subtitle = "q-values test whether |change| differs between the two annual transitions"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

# ---- Panel B: timing effect by depth -----------------------------------------
#
# Plot the paired effect directly:
# mean(|Delta_2023-2024| - |Delta_2022-2023|).
# Positive values mean a stronger late change.

timing_effect_by_depth <- timing_wide |>
  group_by(
    Depth,
    Process
  ) |>
  summarise(
    n = n(),
    late_minus_early = mean(
      late_minus_early
    ),
    sd_effect = sd(
      late_minus_early
    ),
    se_effect = sd_effect / sqrt(
      n
    ),
    .groups = "drop"
  ) |>
  left_join(
    process_timing_tests_by_depth |>
      select(
        Depth,
        Process,
        p_FDR_all_12
      ),
    by = c(
      "Depth",
      "Process"
    )
  ) |>
  mutate(
    Process = factor(
      Process,
      levels = process_levels
    ),
    Depth = factor(
      Depth,
      levels = c(
        "0-15",
        "15-30",
        "30-50"
      )
    ),
    sig = case_when(
      p_FDR_all_12 < 0.001 ~ "***",
      p_FDR_all_12 < 0.01 ~ "**",
      p_FDR_all_12 < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

pB <- ggplot(
  timing_effect_by_depth,
  aes(
    x = Process,
    y = late_minus_early
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    linetype = 3,
    colour = "grey50"
  ) +
  geom_point(
    size = 2.7
  ) +
  geom_errorbar(
    aes(
      ymin = late_minus_early - se_effect,
      ymax = late_minus_early + se_effect
    ),
    width = 0.09,
    linewidth = 0.4
  ) +
  geom_text(
    aes(
      label = sig
    ),
    nudge_y = 0.12,
    size = 4
  ) +
  facet_wrap(
    ~ Depth,
    nrow = 1
  ) +
  scale_x_discrete(
    labels = process_labels
  ) +
  labs(
    x = NULL,
    y = expression(
      "Late - early change magnitude  (" *
        "|" * Delta * z * "|" *
        ")"
    ),
    title = "B  Depth dependence of the delayed functional shift",
    subtitle = "Positive values indicate greater movement in 2023-2024; * FDR < 0.05 across 12 depth x process tests"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

# ---- Panel C: signed direction ------------------------------------------------

direction_plot <- process_interval_summary |>
  mutate(
    Process = factor(
      Process,
      levels = process_levels
    ),
    Interval = factor(
      Interval,
      levels = interval_levels
    )
  ) |>
  left_join(
    process_direction_tests |>
      select(
        Interval,
        Process,
        p_FDR_within_interval
      ),
    by = c(
      "Interval",
      "Process"
    )
  ) |>
  mutate(
    sig = case_when(
      p_FDR_within_interval < 0.001 ~ "***",
      p_FDR_within_interval < 0.01 ~ "**",
      p_FDR_within_interval < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

pC <- ggplot(
  direction_plot,
  aes(
    x = Process,
    y = mean_delta,
    shape = Interval
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    linetype = 3,
    colour = "grey50"
  ) +
  geom_point(
    size = 3.0,
    position = pd
  ) +
  geom_errorbar(
    aes(
      ymin = mean_delta - se_delta,
      ymax = mean_delta + se_delta
    ),
    width = 0.10,
    linewidth = 0.45,
    position = pd
  ) +
  geom_text(
    aes(
      label = sig,
      y = mean_delta + ifelse(mean_delta >= 0, 0.13, -0.13)
    ),
    position = position_dodge(
      width = 0.42
    ),
    size = 4
  ) +
  scale_shape_manual(
    values = interval_shapes,
    labels = interval_labels,
    name = "Interval"
  ) +
  scale_x_discrete(
    labels = process_labels
  ) +
  labs(
    x = NULL,
    y = expression(
      "Mean signed " * Delta * "z"
    ),
    title = "C  Direction of process change",
    subtitle = "* FDR < 0.05 within each interval"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

if (
  requireNamespace(
    "patchwork",
    quietly = TRUE
  )
) {
  p_all <- patchwork::wrap_plots(
    pA,
    pB,
    pC,
    ncol = 1,
    heights = c(
      1,
      1.15,
      1
    )
  )

  ggsave(
    file.path(
      FIG_DIR,
      "08b_N_process_drivers.pdf"
    ),
    p_all,
    width = 9.0,
    height = 9.3,
    device = cairo_pdf
  )

  ggsave(
    file.path(
      FIG_DIR,
      "08b_N_process_drivers.png"
    ),
    p_all,
    width = 9.0,
    height = 9.3,
    dpi = 300
  )
}

# ------------------------------------------------------------------------------
# 10. Console summary
# ------------------------------------------------------------------------------

message(
  "Process-specific timing tests:"
)

print(
  process_timing_tests |>
    select(
      Process,
      mean_abs_2022_2023,
      mean_abs_2023_2024,
      late_minus_early,
      p_exact_two_sided,
      p_FDR
    )
)

message(
  "Signed process changes by interval:"
)

print(
  process_direction_tests |>
    select(
      Interval,
      Process,
      mean_delta,
      p_exact_two_sided,
      p_FDR_within_interval
    )
)

message(
  "Script 08b completed."
)
# ==============================================================================
