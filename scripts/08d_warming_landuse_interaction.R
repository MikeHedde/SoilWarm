# ==============================================================================
# 08d_warming_landuse_interaction.R
# SOILWARM — warming effects within the 3-year land-use trajectory design
# ==============================================================================
#
# PURPOSE
# -------
# Reintroduce the Heated-Control contrast explicitly into the longitudinal
# interpretation developed in scripts 08-08c.
#
# DESIGN
# ------
# Land-use trajectory:
#   Stable_PP:
#     2022 = Permanent pasture
#     2023 = Permanent pasture
#     2024 = Permanent pasture
#
#   TP_to_Crop:
#     2022 = Temporary pasture
#     2023 = Crop
#     2024 = Crop
#
# Warming:
#   Control
#   Heated
#
# Years:
#   2022, 2023, 2024
#
# IMPORTANT
# ---------
# This script DOES NOT assume that 2022 is a pre-warming baseline. It estimates
# how Heated and Control differ in their observed temporal changes from the 2022
# state. If independent experimental documentation later confirms that the 2022
# sampling preceded warming, the same contrasts can be interpreted more
# specifically as warming treatment effects relative to baseline.
#
# PRIMARY CONTRASTS
# -----------------
# A. Warming contrast within each land-use trajectory:
#
#      W = Delta(Heated) - Delta(Control)
#
# B. Land-use x warming interaction:
#
#      DDD =
#        [Delta(Heated) - Delta(Control)]_TP_to_Crop
#        -
#        [Delta(Heated) - Delta(Control)]_Stable_PP
#
# The DDD asks whether warming modifies the temporal response to land-use
# conversion relative to the stable permanent-pasture reference.
#
# INFERENCE
# ---------
# Main warming contrasts:
#   exact sign-flips at LandUseTrajectory x Block profile-pair level,
#   preserving all three depths together.
#   6 pairs -> 2^6 = 64 exact flips.
#
# Warming x land-use interaction:
#   exact sign-flips at Block level, preserving all three depths together.
#   3 blocks -> 2^3 = 8 exact flips.
#   Therefore interaction tests have very coarse p-value resolution and should
#   be interpreted primarily through effect sizes and consistency.
#
# OUTPUTS
# -------
# results/tables/08d_warming_multivariate.csv
# results/tables/08d_warming_processes.csv
# results/tables/08d_warming_timing_shift.csv
# results/tables/08d_landuse_x_warming_multivariate.csv
# results/tables/08d_landuse_x_warming_processes.csv
# results/tables/08d_landuse_x_warming_timing_shift.csv
# results/figures/08d_warming_landuse_interaction.pdf
# intermediate/08d_warming_landuse_interaction.rds
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

required_files <- c(
  file.path(INT_DIR, "04_primary_scores_with_metadata.rds"),
  file.path(INT_DIR, "06_N_PCA_processes.rds"),
  file.path(INT_DIR, "06_N_PCA_genes.rds"),
  file.path(INT_DIR, "06_N_temporal_block.rds")
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "Missing files required by script 08d:\n - ",
    paste(missing_files, collapse = "\n - "),
    call. = FALSE
  )
}

mcoa <- readRDS(
  file.path(INT_DIR, "04_primary_scores_with_metadata.rds")
)

N_process_pca <- readRDS(
  file.path(INT_DIR, "06_N_PCA_processes.rds")
)

N_gene_pca <- readRDS(
  file.path(INT_DIR, "06_N_PCA_genes.rds")
)

N_processed <- readRDS(
  file.path(INT_DIR, "06_N_temporal_block.rds")
)

# ------------------------------------------------------------------------------
# 1. Harmonise design variables
# ------------------------------------------------------------------------------

add_design <- function(df) {

  df <- df |>
    mutate(
      Year = as.integer(
        as.character(Year)
      ),
      MET = as.character(MET),
      Treatment = as.character(Treatment),
      Block = as.character(Block),
      Depth = as.character(Depth),
      profile_id = as.character(profile_id),
      sample_unit_id = as.character(sample_unit_id)
    )

  if (!"LandUseTrajectory" %in% names(df)) {
    df <- df |>
      mutate(
        LandUseTrajectory = case_when(
          as.character(LandUse) == "Grassland" ~ "Stable_PP",
          as.character(LandUse) == "Cropland" ~ "TP_to_Crop",
          TRUE ~ NA_character_
        )
      )
  }

  if (!"LandUseState" %in% names(df)) {
    df <- df |>
      mutate(
        LandUseState = case_when(
          LandUseTrajectory == "Stable_PP" ~ "PermanentPasture",
          LandUseTrajectory == "TP_to_Crop" & Year == 2022L ~ "TemporaryPasture",
          LandUseTrajectory == "TP_to_Crop" & Year %in% c(2023L, 2024L) ~ "Crop",
          TRUE ~ NA_character_
        )
      )
  }

  df |>
    mutate(
      warming_pair_profile = paste(
        LandUseTrajectory,
        Block,
        sep = "__"
      ),
      warming_pair_unit = paste(
        LandUseTrajectory,
        Block,
        Depth,
        sep = "__"
      ),
      block_depth = paste(
        Block,
        Depth,
        sep = "__"
      )
    )
}

mcoa <- add_design(mcoa)
N_processed <- add_design(N_processed)

if (
  nrow(mcoa) != 108L ||
    nrow(N_processed) != 108L
) {
  stop(
    "Expected 108 rows in both MCOA and processed N tables.",
    call. = FALSE
  )
}

if (!setequal(mcoa$MET, N_processed$MET)) {
  stop(
    "MCOA and N tables do not contain the same MET identifiers.",
    call. = FALSE
  )
}

if (!setequal(
  unique(mcoa$Treatment),
  c("Control", "Heated")
)) {
  stop(
    "Treatment must contain exactly Control and Heated.",
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 2. Attach N-process and N functional-gene PCA scores
# ------------------------------------------------------------------------------

select_pcs_by_variance <- function(pca_object, threshold) {
  if (is.null(pca_object$variance)) {
    stop("PCA object has no $variance element.", call. = FALSE)
  }

  n_keep <- which(
    cumsum(pca_object$variance) >= threshold - 1e-12
  )[1]

  if (is.na(n_keep)) {
    n_keep <- length(pca_object$variance)
  }

  paste0("PC", seq_len(n_keep))
}

PROCESS_PCS <- select_pcs_by_variance(
  N_process_pca,
  PROCESS_PCA_CUMVAR
)

GENE_PCS <- select_pcs_by_variance(
  N_gene_pca,
  GENE_PCA_CUMVAR
)

join_pca_scores <- function(dat, pca_scores, pcs, prefix) {
  x <- pca_scores |>
    mutate(MET = as.character(MET)) |>
    select(MET, all_of(pcs))

  names(x)[match(pcs, names(x))] <- paste0(prefix, "_", pcs)

  dat |>
    left_join(x, by = "MET")
}

dat <- mcoa |>
  join_pca_scores(
    N_process_pca$scores,
    PROCESS_PCS,
    "N"
  ) |>
  join_pca_scores(
    N_gene_pca$scores,
    GENE_PCS,
    "G"
  )

TAX_AXES <- intersect(
  TAX_AXES_PRIMARY,
  names(dat)
)

N_AXES <- paste0("N_", PROCESS_PCS)
G_AXES <- paste0("G_", GENE_PCS)

PROCESS_VARS <- N_FUNCTION_VARS

if (
  length(TAX_AXES) < 3L ||
    anyNA(
      dat[, c(TAX_AXES, G_AXES, N_AXES), drop = FALSE]
    )
) {
  stop(
    "Incomplete taxonomic, gene-potential or N-activity score table.",
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 3. Exact sign-flip helpers
# ------------------------------------------------------------------------------

exact_signflip_scalar <- function(
  values,
  group_id
) {

  values <- as.numeric(values)
  group_id <- as.character(group_id)

  groups <- unique(group_id)
  n_groups <- length(groups)

  if (n_groups > 20L) {
    stop(
      "Exact sign-flip enumeration limited to <=20 groups.",
      call. = FALSE
    )
  }

  sign_grid <- expand.grid(
    rep(
      list(c(-1, 1)),
      n_groups
    ),
    KEEP.OUT.ATTRS = FALSE
  )

  observed <- mean(
    values,
    na.rm = TRUE
  )

  null <- numeric(
    nrow(sign_grid)
  )

  for (i in seq_len(nrow(sign_grid))) {

    signs <- setNames(
      as.numeric(
        sign_grid[
          i,
          ,
          drop = TRUE
        ]
      ),
      groups
    )

    null[i] <- mean(
      values *
        unname(
          signs[
            group_id
          ]
        ),
      na.rm = TRUE
    )
  }

  tibble(
    estimate = observed,
    p_exact_two_sided = mean(
      abs(null) >=
        abs(observed) -
        1e-12
    ),
    n_sign_flips = length(null),
    null_q025 = unname(
      quantile(
        null,
        0.025
      )
    ),
    null_median = unname(
      quantile(
        null,
        0.5
      )
    ),
    null_q975 = unname(
      quantile(
        null,
        0.975
      )
    )
  )
}

exact_signflip_multivariate <- function(
  X,
  group_id
) {

  X <- as.matrix(X)
  storage.mode(X) <- "double"

  group_id <- as.character(group_id)
  groups <- unique(group_id)
  n_groups <- length(groups)

  if (n_groups > 20L) {
    stop(
      "Exact sign-flip enumeration limited to <=20 groups.",
      call. = FALSE
    )
  }

  observed_vector <- colMeans(X)
  observed_stat <- sum(
    observed_vector ^ 2
  )

  sign_grid <- expand.grid(
    rep(
      list(c(-1, 1)),
      n_groups
    ),
    KEEP.OUT.ATTRS = FALSE
  )

  null <- numeric(
    nrow(sign_grid)
  )

  for (i in seq_len(nrow(sign_grid))) {

    signs <- setNames(
      as.numeric(
        sign_grid[
          i,
          ,
          drop = TRUE
        ]
      ),
      groups
    )

    signed_X <- X *
      unname(
        signs[
          group_id
        ]
      )

    null[i] <- sum(
      colMeans(
        signed_X
      ) ^ 2
    )
  }

  list(
    summary = tibble(
      vector_norm = sqrt(
        observed_stat
      ),
      statistic = observed_stat,
      p_exact = mean(
        null >=
          observed_stat -
          1e-12
      ),
      n_sign_flips = length(null),
      null_q025 = unname(
        quantile(
          null,
          0.025
        )
      ),
      null_median = unname(
        quantile(
          null,
          0.5
        )
      ),
      null_q975 = unname(
        quantile(
          null,
          0.975
        )
      )
    ),
    mean_vector = observed_vector
  )
}

# ------------------------------------------------------------------------------
# 4. Build interval changes
# ------------------------------------------------------------------------------

build_changes <- function(
  df,
  variables
) {

  groups <- split(
    df,
    interaction(
      df$LandUseTrajectory,
      df$Treatment,
      df$Block,
      df$Depth,
      drop = TRUE
    )
  )

  interval_defs <- list(
    c(2022L, 2023L),
    c(2023L, 2024L),
    c(2022L, 2024L)
  )

  purrr::map_dfr(
    groups,
    function(g) {

      g <- g |>
        arrange(Year)

      purrr::map_dfr(
        interval_defs,
        function(int) {

          y0 <- int[1]
          y1 <- int[2]

          r0 <- g |>
            filter(
              Year == y0
            )

          r1 <- g |>
            filter(
              Year == y1
            )

          if (
            nrow(r0) != 1L ||
              nrow(r1) != 1L
          ) {
            stop(
              "Incomplete yearly series while constructing changes.",
              call. = FALSE
            )
          }

          out <- tibble(
            LandUseTrajectory =
              r0$LandUseTrajectory,
            Treatment =
              r0$Treatment,
            Block =
              r0$Block,
            Depth =
              r0$Depth,
            warming_pair_profile =
              r0$warming_pair_profile,
            warming_pair_unit =
              r0$warming_pair_unit,
            block_depth =
              r0$block_depth,
            Interval =
              paste0(
                y0,
                "-",
                y1
              )
          )

          for (v in variables) {
            out[[paste0("d_", v)]] <-
              r1[[v]] -
              r0[[v]]
          }

          out
        }
      )
    }
  )
}

score_changes <- build_changes(
  dat,
  c(
    TAX_AXES,
    G_AXES,
    N_AXES
  )
)

process_changes <- build_changes(
  N_processed,
  PROCESS_VARS
)

# ------------------------------------------------------------------------------
# 5. Heated - Control contrasts
# ------------------------------------------------------------------------------

make_warming_contrasts <- function(
  change_df,
  delta_cols
) {

  control <- change_df |>
    filter(
      Treatment == "Control"
    ) |>
    select(
      LandUseTrajectory,
      Block,
      Depth,
      warming_pair_profile,
      warming_pair_unit,
      block_depth,
      Interval,
      all_of(delta_cols)
    )

  heated <- change_df |>
    filter(
      Treatment == "Heated"
    ) |>
    select(
      LandUseTrajectory,
      Block,
      Depth,
      warming_pair_profile,
      warming_pair_unit,
      block_depth,
      Interval,
      all_of(delta_cols)
    )

  names(heated)[
    match(
      delta_cols,
      names(heated)
    )
  ] <- paste0(
    delta_cols,
    "__Heated"
  )

  out <- control |>
    inner_join(
      heated,
      by = c(
        "LandUseTrajectory",
        "Block",
        "Depth",
        "warming_pair_profile",
        "warming_pair_unit",
        "block_depth",
        "Interval"
      )
    )

  for (v in delta_cols) {
    out[[paste0("W_", sub("^d_", "", v))]] <-
      out[[paste0(v, "__Heated")]] -
      out[[v]]
  }

  out
}

tax_delta_cols <- paste0(
  "d_",
  TAX_AXES
)

G_delta_cols <- paste0(
  "d_",
  G_AXES
)

N_delta_cols <- paste0(
  "d_",
  N_AXES
)

score_warming <- make_warming_contrasts(
  score_changes,
  c(
    tax_delta_cols,
    G_delta_cols,
    N_delta_cols
  )
)

process_delta_cols <- paste0(
  "d_",
  PROCESS_VARS
)

process_warming <- make_warming_contrasts(
  process_changes,
  process_delta_cols
)

# ------------------------------------------------------------------------------
# 6. Main warming effect: pooled across land-use trajectories
# ------------------------------------------------------------------------------

warming_multivariate <- purrr::map_dfr(
  c(
    "2022-2023",
    "2023-2024",
    "2022-2024"
  ),
  function(int) {

    d <- score_warming |>
      filter(Interval == int)

    domain_specs <- list(
      Taxonomy = TAX_AXES,
      N_gene_potential = G_AXES,
      N_activities = N_AXES
    )

    purrr::imap_dfr(
      domain_specs,
      function(axes, domain_name) {
        cols <- paste0("W_", axes)

        test <- exact_signflip_multivariate(
          d[, cols, drop = FALSE],
          d$warming_pair_profile
        )

        test$summary |>
          mutate(
            Interval = int,
            Domain = domain_name,
            n_axes = length(axes),
            mean_vector = paste(
              round(test$mean_vector, 4),
              collapse = ";"
            ),
            .before = 1
          )
      }
    )
  }
) |>
  group_by(Domain) |>
  mutate(
    p_FDR_within_domain = p.adjust(
      p_exact,
      method = "BH"
    )
  ) |>
  ungroup()

write.csv(
  warming_multivariate,
  file.path(
    TABLE_DIR,
    "08d_warming_multivariate.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 7. Main warming effect on individual potential N activities
# ------------------------------------------------------------------------------

process_warming_long <- process_warming |>
  select(
    LandUseTrajectory,
    Block,
    Depth,
    warming_pair_profile,
    warming_pair_unit,
    Interval,
    starts_with("W_")
  ) |>
  pivot_longer(
    cols = starts_with("W_"),
    names_to = "Process",
    values_to = "warming_contrast"
  ) |>
  mutate(
    Process = sub(
      "^W_",
      "",
      Process
    )
  )

warming_processes <- purrr::map_dfr(
  c(
    "2022-2023",
    "2023-2024",
    "2022-2024"
  ),
  function(int) {

    purrr::map_dfr(
      PROCESS_VARS,
      function(proc) {

        d <- process_warming_long |>
          filter(
            Interval == int,
            Process == proc
          )

        exact_signflip_scalar(
          d$warming_contrast,
          d$warming_pair_profile
        ) |>
          mutate(
            Interval = int,
            Process = proc,
            mean_warming_contrast =
              mean(
                d$warming_contrast
              ),
            .before = 1
          )
      }
    )
  }
) |>
  group_by(
    Interval
  ) |>
  mutate(
    p_FDR_within_interval =
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

write.csv(
  warming_processes,
  file.path(
    TABLE_DIR,
    "08d_warming_processes.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 8. Does warming alter taxonomy-function timing asynchrony?
# ------------------------------------------------------------------------------

trajectory_timing <- purrr::map_dfr(
  split(
    dat,
    interaction(
      dat$LandUseTrajectory,
      dat$Treatment,
      dat$Block,
      dat$Depth,
      drop = TRUE
    )
  ),
  function(g) {

    g <- g |>
      arrange(
        Year
      )

    if (!identical(
      g$Year,
      c(
        2022L,
        2023L,
        2024L
      )
    )) {
      stop(
        "Trajectory timing requires complete 2022-2024 series.",
        call. = FALSE
      )
    }

    Xt <- as.matrix(
      g[
        ,
        TAX_AXES,
        drop = FALSE
      ]
    )

    Xn <- as.matrix(
      g[
        ,
        N_AXES,
        drop = FALSE
      ]
    )

    tax_d1 <- sqrt(
      sum(
        (
          Xt[2, ] -
            Xt[1, ]
        ) ^ 2
      )
    )

    tax_d2 <- sqrt(
      sum(
        (
          Xt[3, ] -
            Xt[2, ]
        ) ^ 2
      )
    )

    n_d1 <- sqrt(
      sum(
        (
          Xn[2, ] -
            Xn[1, ]
        ) ^ 2
      )
    )

    n_d2 <- sqrt(
      sum(
        (
          Xn[3, ] -
            Xn[2, ]
        ) ^ 2
      )
    )

    tax_balance <- (
      tax_d2 -
        tax_d1
    ) /
      (
        tax_d1 +
          tax_d2
      )

    n_balance <- (
      n_d2 -
        n_d1
    ) /
      (
        n_d1 +
          n_d2
      )

    tibble(
      LandUseTrajectory =
        g$LandUseTrajectory[1],
      Treatment =
        g$Treatment[1],
      Block =
        g$Block[1],
      Depth =
        g$Depth[1],
      warming_pair_profile =
        g$warming_pair_profile[1],
      warming_pair_unit =
        g$warming_pair_unit[1],
      block_depth =
        g$block_depth[1],
      timing_shift =
        n_balance -
        tax_balance
    )
  }
)

timing_control <- trajectory_timing |>
  filter(
    Treatment == "Control"
  ) |>
  select(
    LandUseTrajectory,
    Block,
    Depth,
    warming_pair_profile,
    warming_pair_unit,
    block_depth,
    timing_Control =
      timing_shift
  )

timing_heated <- trajectory_timing |>
  filter(
    Treatment == "Heated"
  ) |>
  select(
    LandUseTrajectory,
    Block,
    Depth,
    warming_pair_profile,
    warming_pair_unit,
    block_depth,
    timing_Heated =
      timing_shift
  )

timing_warming <- timing_control |>
  inner_join(
    timing_heated,
    by = c(
      "LandUseTrajectory",
      "Block",
      "Depth",
      "warming_pair_profile",
      "warming_pair_unit",
      "block_depth"
    )
  ) |>
  mutate(
    warming_effect_timing =
      timing_Heated -
      timing_Control
  )

warming_timing_shift <- exact_signflip_scalar(
  timing_warming$warming_effect_timing,
  timing_warming$warming_pair_profile
) |>
  mutate(
    scope = "pooled_landuse_trajectories",
    mean_Heated_minus_Control =
      mean(
        timing_warming$warming_effect_timing
      ),
    .before = 1
  )

write.csv(
  warming_timing_shift,
  file.path(
    TABLE_DIR,
    "08d_warming_timing_shift.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 9. Land-use x warming interaction (DDD)
# ------------------------------------------------------------------------------

make_DDD <- function(
  warming_df,
  warming_cols
) {

  stable <- warming_df |>
    filter(
      LandUseTrajectory ==
        "Stable_PP"
    ) |>
    select(
      Block,
      Depth,
      block_depth,
      Interval,
      all_of(
        warming_cols
      )
    )

  converted <- warming_df |>
    filter(
      LandUseTrajectory ==
        "TP_to_Crop"
    ) |>
    select(
      Block,
      Depth,
      block_depth,
      Interval,
      all_of(
        warming_cols
      )
    )

  names(converted)[
    match(
      warming_cols,
      names(converted)
    )
  ] <- paste0(
    warming_cols,
    "__TP_to_Crop"
  )

  out <- stable |>
    inner_join(
      converted,
      by = c(
        "Block",
        "Depth",
        "block_depth",
        "Interval"
      )
    )

  for (v in warming_cols) {
    out[[paste0("DDD_", sub("^W_", "", v))]] <-
      out[[paste0(v, "__TP_to_Crop")]] -
      out[[v]]
  }

  out
}

score_warming_cols <- c(
  paste0("W_", TAX_AXES),
  paste0("W_", G_AXES),
  paste0("W_", N_AXES)
)

score_DDD <- make_DDD(
  score_warming,
  score_warming_cols
)

interaction_multivariate <- purrr::map_dfr(
  c(
    "2022-2023",
    "2023-2024",
    "2022-2024"
  ),
  function(int) {

    d <- score_DDD |>
      filter(Interval == int)

    domain_specs <- list(
      Taxonomy = TAX_AXES,
      N_gene_potential = G_AXES,
      N_activities = N_AXES
    )

    purrr::imap_dfr(
      domain_specs,
      function(axes, domain_name) {
        cols <- paste0("DDD_", axes)

        test <- exact_signflip_multivariate(
          d[, cols, drop = FALSE],
          d$Block
        )

        test$summary |>
          mutate(
            Interval = int,
            Domain = domain_name,
            n_axes = length(axes),
            mean_vector = paste(
              round(test$mean_vector, 4),
              collapse = ";"
            ),
            .before = 1
          )
      }
    )
  }
)

write.csv(
  interaction_multivariate,
  file.path(
    TABLE_DIR,
    "08d_landuse_x_warming_multivariate.csv"
  ),
  row.names = FALSE
)

process_warming_cols <- paste0(
  "W_",
  PROCESS_VARS
)

process_DDD <- make_DDD(
  process_warming,
  process_warming_cols
)

process_DDD_long <- process_DDD |>
  select(
    Block,
    Depth,
    block_depth,
    Interval,
    starts_with("DDD_")
  ) |>
  pivot_longer(
    cols = starts_with("DDD_"),
    names_to = "Process",
    values_to = "DDD"
  ) |>
  mutate(
    Process = sub(
      "^DDD_",
      "",
      Process
    )
  )

interaction_processes <- purrr::map_dfr(
  c(
    "2022-2023",
    "2023-2024",
    "2022-2024"
  ),
  function(int) {

    purrr::map_dfr(
      PROCESS_VARS,
      function(proc) {

        d <- process_DDD_long |>
          filter(
            Interval == int,
            Process == proc
          )

        exact_signflip_scalar(
          d$DDD,
          d$Block
        ) |>
          mutate(
            Interval = int,
            Process = proc,
            mean_DDD =
              mean(
                d$DDD
              ),
            .before = 1
          )
      }
    )
  }
) |>
  group_by(
    Interval
  ) |>
  mutate(
    p_FDR_within_interval =
      p.adjust(
        p_exact_two_sided,
        method = "BH"
      )
  ) |>
  ungroup()

write.csv(
  interaction_processes,
  file.path(
    TABLE_DIR,
    "08d_landuse_x_warming_processes.csv"
  ),
  row.names = FALSE
)

# Timing-shift interaction.
timing_stable <- timing_warming |>
  filter(
    LandUseTrajectory ==
      "Stable_PP"
  ) |>
  select(
    Block,
    Depth,
    block_depth,
    warming_effect_PP =
      warming_effect_timing
  )

timing_converted <- timing_warming |>
  filter(
    LandUseTrajectory ==
      "TP_to_Crop"
  ) |>
  select(
    Block,
    Depth,
    block_depth,
    warming_effect_TP =
      warming_effect_timing
  )

timing_DDD <- timing_stable |>
  inner_join(
    timing_converted,
    by = c(
      "Block",
      "Depth",
      "block_depth"
    )
  ) |>
  mutate(
    DDD_timing =
      warming_effect_TP -
      warming_effect_PP
  )

interaction_timing_shift <- exact_signflip_scalar(
  timing_DDD$DDD_timing,
  timing_DDD$Block
) |>
  mutate(
    contrast =
      "(Heated-Control)_TP_to_Crop - (Heated-Control)_Stable_PP",
    mean_DDD =
      mean(
        timing_DDD$DDD_timing
      ),
    .before = 1
  )

write.csv(
  interaction_timing_shift,
  file.path(
    TABLE_DIR,
    "08d_landuse_x_warming_timing_shift.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 10. Candidate figure
# ------------------------------------------------------------------------------

# Panel A: timing asynchrony under Control vs Heated.
timing_plot <- trajectory_timing |>
  group_by(
    Treatment,
    LandUseTrajectory
  ) |>
  summarise(
    n = n(),
    mean_timing =
      mean(
        timing_shift
      ),
    se_timing =
      sd(
        timing_shift
      ) /
      sqrt(
        n
      ),
    .groups = "drop"
  ) |>
  mutate(
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
    ),
    Treatment = factor(
      Treatment,
      levels = c(
        "Control",
        "Heated"
      )
    )
  )

pd <- position_dodge(
  width = 0.35
)

pA <- ggplot(
  timing_plot,
  aes(
    x = LandUseTrajectory,
    y = mean_timing,
    shape = Treatment
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    linetype = 3,
    colour = "grey55"
  ) +
  geom_point(
    size = 3,
    position = pd
  ) +
  geom_errorbar(
    aes(
      ymin =
        mean_timing -
        se_timing,
      ymax =
        mean_timing +
        se_timing
    ),
    width = 0.08,
    linewidth = 0.4,
    position = pd
  ) +
  labs(
    x = NULL,
    y = "Taxonomy-function timing shift",
    shape = "Warming",
    title = "A  Warming does not strongly alter temporal asynchrony",
    subtitle = paste0(
      "Pooled Heated-Control timing contrast: p = ",
      formatC(
        warming_timing_shift$p_exact_two_sided,
        format = "f",
        digits = 3
      ),
      "; land-use x warming interaction: p = ",
      formatC(
        interaction_timing_shift$p_exact_two_sided,
        format = "f",
        digits = 3
      )
    )
  ) +
  theme_bw() +
  theme(
    panel.grid.minor =
      element_blank(),
    panel.grid.major.x =
      element_blank()
  )

# Panel B: multivariate warming effect by interval.
warm_multi_plot <- warming_multivariate |>
  filter(
    Interval %in% c(
      "2022-2023",
      "2023-2024"
    )
  ) |>
  mutate(
    Interval = factor(
      Interval,
      levels = c(
        "2022-2023",
        "2023-2024"
      )
    ),
    Domain = factor(
      Domain,
      levels = c(
        "Taxonomy",
        "N_gene_potential",
        "N_activities"
      ),
      labels = c(
        "Taxonomy",
        "N functional-gene potential",
        "Potential N activities"
      )
    ),
    label = paste0(
      "p=",
      formatC(
        p_exact,
        format = "f",
        digits = 3
      )
    )
  )

pB <- ggplot(
  warm_multi_plot,
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
      label = label
    ),
    vjust = -0.8,
    size = 3
  ) +
  facet_wrap(
    ~ Domain,
    scales = "free_y"
  ) +
  labs(
    x = NULL,
    y = "Norm of mean Heated-Control change vector",
    title = "B  Multivariate warming effect on temporal change",
    subtitle = "Exact sign-flips preserve all three depths within each LandUseTrajectory x Block pair"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor =
      element_blank()
  )

# Panel C: individual process warming contrasts.
warm_proc_plot <- process_warming_long |>
  filter(
    Interval %in% c(
      "2022-2023",
      "2023-2024"
    )
  ) |>
  group_by(
    Interval,
    Process
  ) |>
  summarise(
    n = n(),
    mean_warming =
      mean(
        warming_contrast
      ),
    se_warming =
      sd(
        warming_contrast
      ) /
      sqrt(
        n
      ),
    .groups = "drop"
  ) |>
  left_join(
    warming_processes |>
      filter(
        Interval %in% c(
          "2022-2023",
          "2023-2024"
        )
      ) |>
      select(
        Interval,
        Process,
        p_exact_two_sided
      ),
    by = c(
      "Interval",
      "Process"
    )
  ) |>
  mutate(
    Process = factor(
      Process,
      levels = PROCESS_VARS
    ),
    Interval = factor(
      Interval,
      levels = c(
        "2022-2023",
        "2023-2024"
      )
    ),
    p_label = paste0(
      "p=",
      formatC(
        p_exact_two_sided,
        format = "f",
        digits = 3
      )
    )
  )

pC <- ggplot(
  warm_proc_plot,
  aes(
    x = Process,
    y = mean_warming
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
  geom_errorbar(
    aes(
      ymin =
        mean_warming -
        se_warming,
      ymax =
        mean_warming +
        se_warming
    ),
    width = 0.08,
    linewidth = 0.4
  ) +
  geom_text(
    aes(
      label = p_label,
      y = mean_warming +
        ifelse(
          mean_warming >= 0,
          0.12,
          -0.12
        )
    ),
    size = 2.8
  ) +
  facet_wrap(
    ~ Interval,
    nrow = 1
  ) +
  labs(
    x = NULL,
    y = expression(
      "Heated - Control change  (" *
        Delta * "z)"
    ),
    title = "C  Process-specific warming contrasts",
    subtitle = "Positive = more positive temporal change under warming"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor =
      element_blank(),
    panel.grid.major.x =
      element_blank(),
    axis.text.x =
      element_text(
        angle = 25,
        hjust = 1
      )
  )

# Panel D: land-use x warming interaction.
ddd_plot <- process_DDD_long |>
  filter(
    Interval %in% c(
      "2022-2023",
      "2023-2024"
    )
  ) |>
  group_by(
    Interval,
    Process
  ) |>
  summarise(
    n = n(),
    mean_DDD =
      mean(
        DDD
      ),
    se_DDD =
      sd(
        DDD
      ) /
      sqrt(
        n
      ),
    .groups = "drop"
  ) |>
  left_join(
    interaction_processes |>
      filter(
        Interval %in% c(
          "2022-2023",
          "2023-2024"
        )
      ) |>
      select(
        Interval,
        Process,
        p_exact_two_sided
      ),
    by = c(
      "Interval",
      "Process"
    )
  ) |>
  mutate(
    Process = factor(
      Process,
      levels = PROCESS_VARS
    ),
    Interval = factor(
      Interval,
      levels = c(
        "2022-2023",
        "2023-2024"
      )
    ),
    p_label = paste0(
      "p=",
      formatC(
        p_exact_two_sided,
        format = "f",
        digits = 3
      )
    )
  )

pD <- ggplot(
  ddd_plot,
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
  geom_errorbar(
    aes(
      ymin =
        mean_DDD -
        se_DDD,
      ymax =
        mean_DDD +
        se_DDD
    ),
    width = 0.08,
    linewidth = 0.4
  ) +
  geom_text(
    aes(
      label = p_label,
      y = mean_DDD +
        ifelse(
          mean_DDD >= 0,
          0.14,
          -0.14
        )
    ),
    size = 2.8
  ) +
  facet_wrap(
    ~ Interval,
    nrow = 1
  ) +
  labs(
    x = NULL,
    y = "Land-use x warming interaction",
    title = "D  Does warming modify the TP -> Crop response?",
    subtitle = "DDD = warming contrast in TP -> Crop minus warming contrast in Stable PP; only 8 exact block sign-flips"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor =
      element_blank(),
    panel.grid.major.x =
      element_blank(),
    axis.text.x =
      element_text(
        angle = 25,
        hjust = 1
      )
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
    pD,
    ncol = 1,
    heights = c(
      0.9,
      1,
      1,
      1
    ),
    guides = "collect"
  ) &
    theme(
      legend.position = "bottom"
    )

  ggsave(
    file.path(
      FIG_DIR,
      "08d_warming_landuse_interaction.pdf"
    ),
    p_all,
    width = 9.5,
    height = 12,
    device = cairo_pdf
  )

  ggsave(
    file.path(
      FIG_DIR,
      "08d_warming_landuse_interaction.png"
    ),
    p_all,
    width = 9.5,
    height = 12,
    dpi = 300
  )
}

# ------------------------------------------------------------------------------
# 11. Save analysis object
# ------------------------------------------------------------------------------

analysis_08d <- list(
  note =
    "2022 is treated as the first observed state, not assumed to be pre-warming.",

  score_changes =
    score_changes,

  score_warming =
    score_warming,

  warming_multivariate =
    warming_multivariate,

  process_warming =
    process_warming_long,

  warming_processes =
    warming_processes,

  trajectory_timing =
    trajectory_timing,

  warming_timing_shift =
    warming_timing_shift,

  score_DDD =
    score_DDD,

  interaction_multivariate =
    interaction_multivariate,

  process_DDD =
    process_DDD_long,

  interaction_processes =
    interaction_processes,

  interaction_timing_shift =
    interaction_timing_shift
)

saveRDS(
  analysis_08d,
  file.path(
    INT_DIR,
    "08d_warming_landuse_interaction.rds"
  )
)

# ------------------------------------------------------------------------------
# 12. Console summary
# ------------------------------------------------------------------------------

message(
  "Main warming effect on multivariate temporal change:"
)

print(
  warming_multivariate |>
    select(
      Interval,
      Domain,
      vector_norm,
      p_exact,
      p_FDR_within_domain
    )
)

message(
  "Process-specific Heated-Control temporal contrasts:"
)

print(
  warming_processes |>
    select(
      Interval,
      Process,
      mean_warming_contrast,
      p_exact_two_sided,
      p_FDR_within_interval
    )
)

message(
  "Warming effect on taxonomy-function timing asynchrony:"
)

print(
  warming_timing_shift |>
    select(
      scope,
      mean_Heated_minus_Control,
      p_exact_two_sided
    )
)

message(
  "Land-use x warming multivariate interaction:"
)

print(
  interaction_multivariate |>
    select(
      Interval,
      Domain,
      vector_norm,
      p_exact
    )
)

message(
  "Land-use x warming process interactions:"
)

print(
  interaction_processes |>
    select(
      Interval,
      Process,
      mean_DDD,
      p_exact_two_sided,
      p_FDR_within_interval
    )
)

message(
  "Land-use x warming interaction on timing asynchrony:"
)

print(
  interaction_timing_shift |>
    select(
      contrast,
      mean_DDD,
      p_exact_two_sided
    )
)

message(
  "Script 08d completed."
)
# ==============================================================================
