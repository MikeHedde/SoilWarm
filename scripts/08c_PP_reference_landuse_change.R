# ==============================================================================
# 08c_PP_reference_landuse_change.R
# SOILWARM — permanent pasture as contemporary reference for interannual change
# ==============================================================================
#
# BIOLOGICAL DESIGN
# -----------------
# Two land-use trajectories are followed over the same three years:
#
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
# IMPORTANT INTERPRETATION
# ------------------------
# Stable_PP is used as a CONTEMPORARY REFERENCE for shared interannual
# variation (including weather and other year-specific forcing). It is NOT a
# pure "weather control": PP and temporary pasture can differ in baseline state
# and in their sensitivity to weather.
#
# The land-use-change contrast is therefore a paired difference-in-differences:
#
#   DiD = Delta(TP_to_Crop) - Delta(Stable_PP)
#
# Pairing is by Treatment x Block x Depth. Exact sign-flip tests are performed
# at the matched PROFILE-PAIR level (Treatment x Block), preserving the three
# depths together. There are 6 matched profile pairs -> 2^6 = 64 exact flips.
#
# PRIMARY QUESTIONS
# -----------------
# 1. Does the previously observed taxonomy-function timing offset occur in BOTH
#    trajectories, or is it enhanced by TP -> Crop conversion?
# 2. After subtracting the Stable_PP interannual response, is there additional
#    multivariate taxonomic or N-functional divergence attributable to the
#    TP -> Crop trajectory?
# 3. Which individual potential N-cycle activities carry that PP-referenced
#    divergence?
#
# INPUTS
# ------
# intermediate/04_primary_scores_with_metadata.rds
# intermediate/06_N_PCA_processes.rds
# intermediate/06_N_PCA_genes.rds
# intermediate/06_N_temporal_block.rds
#
# OUTPUTS
# -------
# results/tables/08c_landuse_state_map.csv
# results/tables/08c_timing_shift_by_trajectory.csv
# results/tables/08c_timing_shift_trajectory_difference.csv
# results/tables/08c_multivariate_DiD.csv
# results/tables/08c_process_changes_by_trajectory.csv
# results/tables/08c_process_DiD.csv
# results/tables/08c_process_DiD_by_depth.csv
# results/figures/08c_PP_reference_landuse_change.pdf
# intermediate/08c_PP_reference_landuse_change.rds
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
    "Missing files required by script 08c:\n - ",
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
# 1. Explicit land-use trajectory coding
# ------------------------------------------------------------------------------
#
# Current legacy metadata labels:
#   Grassland = permanent-pasture trajectory
#   Cropland  = trajectory that was temporary pasture in 2022 and crop in
#               2023/2024.
#
# "Cropland" is therefore NOT a valid state label for 2022. We retain the
# legacy column only for backward compatibility and create explicit trajectory
# and annual state variables here.

add_landuse_design <- function(df) {

  legacy_levels <- sort(
    unique(
      as.character(df$LandUse)
    )
  )

  if (!setequal(
    legacy_levels,
    c(
      "Cropland",
      "Grassland"
    )
  )) {
    stop(
      "08c expects the current legacy LandUse labels 'Grassland' and 'Cropland'. ",
      "Found: ",
      paste(
        legacy_levels,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  df |>
    mutate(
      Year = as.integer(
        as.character(
          Year
        )
      ),

      LandUseTrajectory = case_when(
        as.character(LandUse) == "Grassland" ~
          "Stable_PP",

        as.character(LandUse) == "Cropland" ~
          "TP_to_Crop",

        TRUE ~
          NA_character_
      ),

      LandUseState = case_when(
        LandUseTrajectory == "Stable_PP" ~
          "PermanentPasture",

        LandUseTrajectory == "TP_to_Crop" &
          Year == 2022L ~
          "TemporaryPasture",

        LandUseTrajectory == "TP_to_Crop" &
          Year %in% c(
            2023L,
            2024L
          ) ~
          "Crop",

        TRUE ~
          NA_character_
      ),

      MET = as.character(
        MET
      ),

      Treatment = as.character(
        Treatment
      ),

      Block = as.character(
        Block
      ),

      Depth = as.character(
        Depth
      ),

      profile_id = as.character(
        profile_id
      ),

      sample_unit_id = as.character(
        sample_unit_id
      ),

      pair_profile_id = paste(
        Treatment,
        Block,
        sep = "__"
      ),

      pair_unit_id = paste(
        Treatment,
        Block,
        Depth,
        sep = "__"
      )
    )
}

mcoa <- add_landuse_design(
  mcoa
)

N_processed <- add_landuse_design(
  N_processed
)

if (
  nrow(mcoa) != 108L ||
    nrow(N_processed) != 108L
) {
  stop(
    "Expected 108 observations in both MCOA and N tables.",
    call. = FALSE
  )
}

if (!setequal(
  mcoa$MET,
  N_processed$MET
)) {
  stop(
    "MCOA and processed N tables do not contain the same MET IDs.",
    call. = FALSE
  )
}

expected_state_map <- tibble(
  LandUseTrajectory = c(
    "Stable_PP",
    "Stable_PP",
    "Stable_PP",
    "TP_to_Crop",
    "TP_to_Crop",
    "TP_to_Crop"
  ),

  Year = c(
    2022L,
    2023L,
    2024L,
    2022L,
    2023L,
    2024L
  ),

  LandUseState = c(
    "PermanentPasture",
    "PermanentPasture",
    "PermanentPasture",
    "TemporaryPasture",
    "Crop",
    "Crop"
  )
)

observed_state_map <- mcoa |>
  distinct(
    LandUseTrajectory,
    Year,
    LandUseState
  ) |>
  arrange(
    LandUseTrajectory,
    Year
  )

if (
  nrow(
    anti_join(
      expected_state_map,
      observed_state_map,
      by = c(
        "LandUseTrajectory",
        "Year",
        "LandUseState"
      )
    )
  ) > 0L
) {
  stop(
    "Land-use trajectory/state reconstruction failed.",
    call. = FALSE
  )
}

write.csv(
  observed_state_map,
  file.path(
    TABLE_DIR,
    "08c_landuse_state_map.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 2. Join N-process and N functional-gene PCA scores
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

if (
  length(TAX_AXES) < 3L ||
    anyNA(
      dat[
        ,
        c(TAX_AXES, G_AXES, N_AXES),
        drop = FALSE
      ]
    )
) {
  stop(
    "Failed to construct complete taxonomic, gene-potential and N-activity score tables.",
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 3. Exact sign-flip helpers
# ------------------------------------------------------------------------------

exact_profile_signflip_univariate <- function(
  values,
  profile_pair
) {

  values <- as.numeric(
    values
  )

  profile_pair <- as.character(
    profile_pair
  )

  pairs <- unique(
    profile_pair
  )

  n_pairs <- length(
    pairs
  )

  if (n_pairs > 20L) {
    stop(
      "Exact sign-flip enumeration intentionally limited to <=20 profile pairs.",
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
      n_pairs
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
      pairs
    )

    null[
      i
    ] <- mean(
      values *
        unname(
          signs[
            profile_pair
          ]
        ),
      na.rm = TRUE
    )
  }

  tibble(
    estimate = observed,

    p_exact_two_sided = mean(
      abs(
        null
      ) >=
        abs(
          observed
        ) -
        1e-12
    ),

    n_profile_pair_flips = length(
      null
    ),

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

exact_profile_signflip_multivariate <- function(
  X,
  profile_pair
) {

  X <- as.matrix(
    X
  )

  storage.mode(
    X
  ) <- "double"

  profile_pair <- as.character(
    profile_pair
  )

  pairs <- unique(
    profile_pair
  )

  n_pairs <- length(
    pairs
  )

  if (n_pairs > 20L) {
    stop(
      "Exact sign-flip enumeration intentionally limited to <=20 profile pairs.",
      call. = FALSE
    )
  }

  mean_vector <- colMeans(
    X
  )

  observed_stat <- sum(
    mean_vector ^ 2
  )

  sign_grid <- expand.grid(
    rep(
      list(
        c(
          -1,
          1
        )
      ),
      n_pairs
    ),
    KEEP.OUT.ATTRS = FALSE
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
      pairs
    )

    signed_X <- X *
      unname(
        signs[
          profile_pair
        ]
      )

    null[
      i
    ] <- sum(
      colMeans(
        signed_X
      ) ^ 2
    )
  }

  list(
    summary = tibble(
      statistic =
        observed_stat,

      vector_norm =
        sqrt(
          observed_stat
        ),

      p_exact =
        mean(
          null >=
            observed_stat -
            1e-12
        ),

      n_profile_pair_flips =
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
    ),

    mean_vector =
      mean_vector
  )
}

# ------------------------------------------------------------------------------
# 4. Three-year trajectory timing shift within each land-use trajectory
# ------------------------------------------------------------------------------

trajectory_metrics_one <- function(
  df
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
        c(
          2022L,
          2023L,
          2024L
        )
      )) {
        stop(
          "Each sample_unit_id must contain exactly 2022, 2023 and 2024.",
          call. = FALSE
        )
      }

      X_tax <- as.matrix(
        g[
          ,
          TAX_AXES,
          drop = FALSE
        ]
      )

      X_N <- as.matrix(
        g[
          ,
          N_AXES,
          drop = FALSE
        ]
      )

      tax_d1 <- sqrt(
        sum(
          (
            X_tax[
              2,
            ] -
              X_tax[
                1,
              ]
          ) ^ 2
        )
      )

      tax_d2 <- sqrt(
        sum(
          (
            X_tax[
              3,
            ] -
              X_tax[
                2,
              ]
          ) ^ 2
        )
      )

      N_d1 <- sqrt(
        sum(
          (
            X_N[
              2,
            ] -
              X_N[
                1,
              ]
          ) ^ 2
        )
      )

      N_d2 <- sqrt(
        sum(
          (
            X_N[
              3,
            ] -
              X_N[
                2,
              ]
          ) ^ 2
        )
      )

      tax_total <- tax_d1 +
        tax_d2

      N_total <- N_d1 +
        N_d2

      tax_balance <- (
        tax_d2 -
          tax_d1
      ) /
        tax_total

      N_balance <- (
        N_d2 -
          N_d1
      ) /
        N_total

      tibble(
        LandUseTrajectory =
          g$LandUseTrajectory[
            1
          ],

        Treatment =
          g$Treatment[
            1
          ],

        Block =
          g$Block[
            1
          ],

        Depth =
          g$Depth[
            1
          ],

        profile_id =
          g$profile_id[
            1
          ],

        sample_unit_id =
          g$sample_unit_id[
            1
          ],

        pair_profile_id =
          g$pair_profile_id[
            1
          ],

        pair_unit_id =
          g$pair_unit_id[
            1
          ],

        tax_early_fraction =
          tax_d1 /
          tax_total,

        N_early_fraction =
          N_d1 /
          N_total,

        tax_interval_balance =
          tax_balance,

        N_interval_balance =
          N_balance,

        timing_shift =
          N_balance -
          tax_balance
      )
    }
  )
}

trajectory_metrics <- trajectory_metrics_one(
  dat
)

if (
  nrow(
    trajectory_metrics
  ) != 36L
) {
  stop(
    "Expected 36 depth-specific trajectories.",
    call. = FALSE
  )
}

timing_by_trajectory <- purrr::map_dfr(
  c(
    "Stable_PP",
    "TP_to_Crop"
  ),
  function(
    tr
  ) {

    d <- trajectory_metrics |>
      filter(
        LandUseTrajectory == tr
      )

    exact_profile_signflip_univariate(
      d$timing_shift,
      d$profile_id
    ) |>
      mutate(
        LandUseTrajectory =
          tr,

        mean_timing_shift =
          mean(
            d$timing_shift
          ),

        mean_tax_early_fraction =
          mean(
            d$tax_early_fraction
          ),

        mean_N_early_fraction =
          mean(
            d$N_early_fraction
          ),

        .before = 1
      )
  }
)

# Compare the timing shift itself between trajectories:
# TP_to_Crop - Stable_PP, paired by Treatment x Block x Depth.

stable_timing <- trajectory_metrics |>
  filter(
    LandUseTrajectory ==
      "Stable_PP"
  ) |>
  select(
    pair_unit_id,
    pair_profile_id,
    Depth,
    timing_shift_PP =
      timing_shift
  )

converted_timing <- trajectory_metrics |>
  filter(
    LandUseTrajectory ==
      "TP_to_Crop"
  ) |>
  select(
    pair_unit_id,
    timing_shift_TP_to_Crop =
      timing_shift
  )

timing_diff_units <- stable_timing |>
  inner_join(
    converted_timing,
    by = "pair_unit_id"
  ) |>
  mutate(
    timing_shift_difference =
      timing_shift_TP_to_Crop -
      timing_shift_PP
  )

if (
  nrow(
    timing_diff_units
  ) != 18L
) {
  stop(
    "Expected 18 matched depth-specific timing contrasts.",
    call. = FALSE
  )
}

timing_trajectory_difference <-
  exact_profile_signflip_univariate(
    timing_diff_units$timing_shift_difference,
    timing_diff_units$pair_profile_id
  ) |>
  mutate(
    contrast =
      "TP_to_Crop_minus_Stable_PP",

    mean_difference =
      mean(
        timing_diff_units$timing_shift_difference
      ),

    interpretation =
      "Positive = stronger taxonomy-function temporal offset under TP_to_Crop; negative = stronger offset under Stable_PP.",

    .before = 1
  )

write.csv(
  timing_by_trajectory,
  file.path(
    TABLE_DIR,
    "08c_timing_shift_by_trajectory.csv"
  ),
  row.names = FALSE
)

write.csv(
  timing_trajectory_difference,
  file.path(
    TABLE_DIR,
    "08c_timing_shift_trajectory_difference.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 5. Build annual and net changes within each trajectory
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
    c(
      2022L,
      2023L
    ),

    c(
      2023L,
      2024L
    ),

    c(
      2022L,
      2024L
    )
  )

  purrr::map_dfr(
    groups,
    function(g) {

      g <- g |>
        arrange(
          Year
        )

      purrr::map_dfr(
        interval_defs,
        function(int) {

          y0 <- int[
            1
          ]

          y1 <- int[
            2
          ]

          r0 <- g |>
            filter(
              Year == y0
            )

          r1 <- g |>
            filter(
              Year == y1
            )

          if (
            nrow(
              r0
            ) != 1L ||
              nrow(
                r1
              ) != 1L
          ) {
            stop(
              "Missing annual observation while constructing changes.",
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

            pair_profile_id =
              r0$pair_profile_id,

            pair_unit_id =
              r0$pair_unit_id,

            Interval =
              paste0(
                y0,
                "-",
                y1
              )
          )

          for (
            v in variables
          ) {
            out[[paste0("d_",v)]] <- r1[[v]] - r0[[v]]}
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

PROCESS_VARS <- c(
  "PGNP",
  "PNNP",
  "N2O_reduction",
  "NEA"
)

process_changes <- build_changes(
  N_processed,
  PROCESS_VARS
)

# ------------------------------------------------------------------------------
# 6. Paired PP-referenced difference-in-differences
# ------------------------------------------------------------------------------

make_DiD <- function(
  change_df,
  delta_cols
) {

  stable <- change_df |>
    filter(
      LandUseTrajectory ==
        "Stable_PP"
    ) |>
    select(
      pair_unit_id,
      pair_profile_id,
      Treatment,
      Block,
      Depth,
      Interval,
      all_of(
        delta_cols
      )
    )

  converted <- change_df |>
    filter(
      LandUseTrajectory ==
        "TP_to_Crop"
    ) |>
    select(
      pair_unit_id,
      Interval,
      all_of(
        delta_cols
      )
    )

  names(
    converted
  )[
    match(
      delta_cols,
      names(
        converted
      )
    )
  ] <- paste0(
    delta_cols,
    "__TP_to_Crop"
  )

  out <- stable |>
    inner_join(
      converted,
      by = c(
        "pair_unit_id",
        "Interval"
      )
    )

  for (
    v in delta_cols
  ) {

    out[[paste0("DiD_",sub("^d_","",v))]] <-out[[paste0(v,"__TP_to_Crop")]] - out[[v]]}
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

score_DiD <- make_DiD(
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

process_DiD <- make_DiD(
  process_changes,
  process_delta_cols
)

# ------------------------------------------------------------------------------
# 7. Multivariate PP-referenced divergence
# ------------------------------------------------------------------------------

multivariate_DiD <- purrr::map_dfr(
  c(
    "2022-2023",
    "2023-2024",
    "2022-2024"
  ),
  function(int) {

    d <- score_DiD |>
      filter(Interval == int)

    domain_specs <- list(
      Taxonomy = TAX_AXES,
      N_gene_potential = G_AXES,
      N_function = N_AXES
    )

    purrr::imap_dfr(
      domain_specs,
      function(axes, domain_name) {
        cols <- paste0("DiD_", axes)

        test <- exact_profile_signflip_multivariate(
          d[, cols, drop = FALSE],
          d$pair_profile_id
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
  multivariate_DiD,
  file.path(
    TABLE_DIR,
    "08c_multivariate_DiD.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 8. Individual potential N-process PP-referenced contrasts
# ------------------------------------------------------------------------------

process_changes_long <- process_changes |>
  pivot_longer(
    cols = all_of(
      process_delta_cols
    ),
    names_to = "Process",
    values_to = "delta"
  ) |>
  mutate(
    Process = sub(
      "^d_",
      "",
      Process
    )
  )

# Descriptive uncertainty must respect the experimental unit. The three depths
# within a profile are therefore averaged first, and the SE is calculated across
# the six independent profiles in each land-use trajectory (2 treatments ×
# 3 blocks). The grand mean is unchanged because the design is balanced.
process_changes_profile <- process_changes_long |>
  group_by(
    LandUseTrajectory,
    Treatment,
    Block,
    Interval,
    Process
  ) |>
  summarise(
    profile_delta = mean(
      delta
    ),
    profile_abs_delta = mean(
      abs(
        delta
      )
    ),
    n_depths = n(),
    .groups = "drop"
  )

if (any(process_changes_profile$n_depths != 3L)) {
  stop(
    "Each trajectory profile must contribute exactly three depths to the descriptive process summary.",
    call. = FALSE
  )
}

process_changes_by_trajectory <- process_changes_profile |>
  group_by(
    LandUseTrajectory,
    Interval,
    Process
  ) |>
  summarise(
    n_profiles = n(),

    mean_delta =
      mean(
        profile_delta
      ),

    sd_delta =
      sd(
        profile_delta
      ),

    se_delta =
      sd_delta /
      sqrt(
        n_profiles
      ),

    mean_abs_delta =
      mean(
        profile_abs_delta
      ),

    .groups =
      "drop"
  )

if (any(process_changes_by_trajectory$n_profiles != 6L)) {
  stop(
    "Expected six independent profiles per land-use trajectory.",
    call. = FALSE
  )
}

write.csv(
  process_changes_by_trajectory,
  file.path(
    TABLE_DIR,
    "08c_process_changes_by_trajectory.csv"
  ),
  row.names = FALSE
)

process_DiD_long <- process_DiD |>
  select(
    pair_unit_id,
    pair_profile_id,
    Treatment,
    Block,
    Depth,
    Interval,
    starts_with(
      "DiD_"
    )
  ) |>
  pivot_longer(
    cols = starts_with(
      "DiD_"
    ),
    names_to = "Process",
    values_to = "DiD"
  ) |>
  mutate(
    Process = sub(
      "^DiD_",
      "",
      Process
    )
  )

process_DiD_tests <- purrr::map_dfr(
  c(
    "2022-2023",
    "2023-2024",
    "2022-2024"
  ),
  function(
    int
  ) {

    purrr::map_dfr(
      PROCESS_VARS,
      function(
        proc
      ) {

        d <- process_DiD_long |>
          filter(
            Interval == int,
            Process == proc
          )

        exact_profile_signflip_univariate(
          d$DiD,
          d$pair_profile_id
        ) |>
          mutate(
            Interval =
              int,

            Process =
              proc,

            mean_DiD =
              mean(
                d$DiD
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
  process_DiD_tests,
  file.path(
    TABLE_DIR,
    "08c_process_DiD.csv"
  ),
  row.names = FALSE
)

process_DiD_by_depth <- process_DiD_long |>
  group_by(
    Depth,
    Interval,
    Process
  ) |>
  summarise(
    n = n(),

    mean_DiD =
      mean(
        DiD
      ),

    sd_DiD =
      sd(
        DiD
      ),

    se_DiD =
      sd_DiD /
      sqrt(
        n
      ),

    .groups =
      "drop"
  )

write.csv(
  process_DiD_by_depth,
  file.path(
    TABLE_DIR,
    "08c_process_DiD_by_depth.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 9. Publication-oriented candidate figure
# ------------------------------------------------------------------------------

# Panel A — profile-level timing shift, paired Stable_PP vs TP_to_Crop.
timing_profile_means <- trajectory_metrics |>
  group_by(
    LandUseTrajectory,
    pair_profile_id,
    Treatment,
    Block
  ) |>
  summarise(
    timing_shift =
      mean(
        timing_shift
      ),

    .groups =
      "drop"
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
    )
  )

pA <- ggplot(
  timing_profile_means,
  aes(
    x = LandUseTrajectory,
    y = timing_shift,
    group = pair_profile_id
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    linetype = 3,
    colour = "grey55"
  ) +
  geom_line(
    alpha = 0.45,
    linewidth = 0.45
  ) +
  geom_point(
    aes(
      shape = Treatment
    ),
    size = 2.8
  ) +
  labs(
    x = NULL,
    y = "Taxonomy-function timing shift",
    shape = "Warming",
    title = "A  Temporal asynchrony occurs in both land-use trajectories",
    subtitle = paste0(
      "Trajectory difference: exact paired-profile p = ",
      formatC(
        timing_trajectory_difference$p_exact_two_sided,
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

# Panel B — PP-referenced multivariate divergence.
multi_plot <- multivariate_DiD |>
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
        "N_function"
      ),
      labels = c(
        "Taxonomy",
        "N functional-gene potential",
        "Potential N activities"
      )
    ),
    label = paste0(
      "p = ",
      formatC(
        p_exact,
        format = "f",
        digits = 3
      )
    )
  )

pB <- ggplot(
  multi_plot,
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
    vjust = -0.7,
    size = 3
  ) +
  facet_wrap(
    ~ Domain,
    scales = "free_y"
  ) +
  labs(
    x = NULL,
    y = "Norm of mean PP-referenced change vector",
    title = "B  Divergence from the stable-PP interannual reference",
    subtitle = "Scales differ among domains; compare intervals within panels"
  ) +
  theme_bw() +
  theme(
    panel.grid.minor =
      element_blank()
  )

# Panel C — signed process changes in each trajectory.
process_plot <- process_changes_by_trajectory |>
  filter(
    Interval %in% c(
      "2022-2023",
      "2023-2024"
    )
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
    Interval = factor(
      Interval,
      levels = c(
        "2022-2023",
        "2023-2024"
      )
    ),
    Process = factor(
      Process,
      levels = PROCESS_VARS
    )
  )

pd <- position_dodge(
  width = 0.45
)

pC <- ggplot(
  process_plot,
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
    size = 2.6,
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
  labs(
    x = NULL,
    y = expression(
      "Mean signed " * Delta * "z"
    ),
    shape = "Trajectory",
    title = "C  Potential N-activity changes: shared year response vs land-use divergence"
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

# Panel D — direct PP-referenced process DiD.
did_plot <- process_DiD_long |>
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

    mean_DiD =
      mean(
        DiD
      ),

    se_DiD =
      sd(
        DiD
      ) /
      sqrt(
        n
      ),

    .groups =
      "drop"
  ) |>
  left_join(
    process_DiD_tests |>
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
  did_plot,
  aes(
    x = Process,
    y = mean_DiD
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    linetype = 3,
    colour = "grey55"
  ) +
  geom_point(
    size = 2.7
  ) +
  geom_errorbar(
    aes(
      ymin =
        mean_DiD -
        se_DiD,

      ymax =
        mean_DiD +
        se_DiD
    ),
    width = 0.08,
    linewidth = 0.4
  ) +
  geom_text(
    aes(
      label = p_label
    ),
    vjust = -0.8,
    size = 2.8
  ) +
  facet_wrap(
    ~ Interval,
    nrow = 1
  ) +
  labs(
    x = NULL,
    y = expression(
      "PP-referenced " * Delta * Delta * "z"
    ),
    title = "D  Process-specific TP -> Crop divergence relative to Stable PP",
    subtitle = "Positive = more positive change under TP -> Crop; exact p-values have 1/64 resolution"
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
      "08c_PP_reference_landuse_change.pdf"
    ),
    p_all,
    width = 9.5,
    height = 12,
    device = cairo_pdf
  )

  ggsave(
    file.path(
      FIG_DIR,
      "08c_PP_reference_landuse_change.png"
    ),
    p_all,
    width = 9.5,
    height = 12,
    dpi = 300
  )
}

# ------------------------------------------------------------------------------
# 10. Save analysis object
# ------------------------------------------------------------------------------

analysis_08c <- list(
  design =
    observed_state_map,

  interpretation =
    "Stable_PP is a contemporary reference for shared interannual variation, not a pure weather control.",

  trajectory_metrics =
    trajectory_metrics,

  timing_by_trajectory =
    timing_by_trajectory,

  timing_trajectory_difference =
    timing_trajectory_difference,

  score_changes =
    score_changes,

  score_DiD =
    score_DiD,

  multivariate_DiD =
    multivariate_DiD,

  process_changes =
    process_changes,

  process_changes_by_trajectory =
    process_changes_by_trajectory,

  process_DiD =
    process_DiD_long,

  process_DiD_tests =
    process_DiD_tests
)

saveRDS(
  analysis_08c,
  file.path(
    INT_DIR,
    "08c_PP_reference_landuse_change.rds"
  )
)

# ------------------------------------------------------------------------------
# 11. Console summary
# ------------------------------------------------------------------------------

message(
  "Timing shift within each trajectory:"
)

print(
  timing_by_trajectory |>
    select(
      LandUseTrajectory,
      mean_timing_shift,
      mean_tax_early_fraction,
      mean_N_early_fraction,
      p_exact_two_sided
    )
)

message(
  "Difference in timing shift (TP_to_Crop - Stable_PP):"
)

print(
  timing_trajectory_difference |>
    select(
      contrast,
      mean_difference,
      p_exact_two_sided
    )
)

message(
  "PP-referenced multivariate divergence:"
)

print(
  multivariate_DiD |>
    select(
      Interval,
      Domain,
      vector_norm,
      p_exact
    )
)

message(
  "PP-referenced process contrasts:"
)

print(
  process_DiD_tests |>
    select(
      Interval,
      Process,
      mean_DiD,
      p_exact_two_sided,
      p_FDR_within_interval
    )
)

message(
  "Script 08c completed."
)
# ==============================================================================
