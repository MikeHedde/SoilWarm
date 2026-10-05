# ==============================================================================
# 08_temporal_coupling_decoupling.R
# SOILWARM — 3-year temporal trajectories and interval covariance
# ==============================================================================
#
# PRIMARY TEMPORAL QUESTIONS
# --------------------------
# 1. Do taxonomy and realised N functioning follow comparable 2022→2023→2024
#    trajectories in their respective common factor spaces?
# 2. Across the two annual transitions (2022→2023 and 2023→2024), do
#    within-unit taxonomic changes covary with functional changes?
# 3. Is coupling similar in the two intervals and between Control/Heated plots?
#
# IMPORTANT
# ---------
# MCOA and N PCA spaces were fitted ONCE on pooled 2022-2024 observations in
# scripts 04 and 06. No year-specific ordination is fitted here.
#
# DEPENDENCE STRUCTURE
# --------------------
# Adjacent differences share the 2023 observation. We therefore NEVER permute
# individual interval rows independently. Whole profiles are permuted, preserving
# all three depths and both intervals together.
#
# PRIMARY OUTPUTS
# ---------------
# results/tables/08_trajectory_scores.csv
# results/tables/08_trajectory_metrics.csv
# results/tables/08_trajectory_metric_correlations.csv
# results/tables/08_relative_interval_movement.csv
# results/tables/08_relative_interval_movement_summary.csv
# results/tables/08_timing_shift_test.csv
# results/tables/08_timing_shift_pairwise.csv
# results/tables/08_timing_shift_by_depth.csv
# results/tables/08_interval_changes.csv
# results/tables/08_temporal_RV_summary.csv
# results/tables/08_treatment_specific_RV.csv
# results/tables/08_treatment_difference_test.csv
# results/tables/08_procrustes_summary.csv
# results/tables/08_individual_process_delta_correlations.csv
# results/tables/08_gene_temporal_RV.csv
# results/tables/08_gene_variable_interval_changes.csv
# results/tables/08_gene_variable_interval_summary.csv
# results/tables/08_gene_variable_timing_tests.csv
# intermediate/08_temporal_coupling.rds
# ==============================================================================

source("R/config.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(vegan)
})

source("R/functions_soilwarm.R")

# ------------------------------------------------------------------------------
# 0b. Remove obsolete outputs from the former 2022→2024-only script 08
# ------------------------------------------------------------------------------

legacy_08_tables <- c(
  "08_configuration_mismatch.csv",
  "08_configuration_mismatch_model.csv",
  "08_delta_RV_primary.csv",
  "08_delta_axis_correlations.csv",
  "08_delta_nested_model_coefficients.csv",
  "08_delta_nested_model_tests.csv",
  "08_delta_primary.csv",
  "08_pairing_QC.csv",
  "08_secondary_temporal_RV.csv",
  "08_individual_process_model_coefficients.csv",
  "08_individual_process_nested_tests.csv"
)

legacy_08_figures <- c(
  "08_configuration_mismatch.pdf",
  "08_delta_correlation_heatmap.pdf",
  "08_strongest_delta_relationship.pdf"
)

invisible(
  file.remove(
    file.path(
      TABLE_DIR,
      legacy_08_tables[
        file.exists(
          file.path(
            TABLE_DIR,
            legacy_08_tables
          )
        )
      ]
    )
  )
)

invisible(
  file.remove(
    file.path(
      FIG_DIR,
      legacy_08_figures[
        file.exists(
          file.path(
            FIG_DIR,
            legacy_08_figures
          )
        )
      ]
    )
  )
)

required_files <- c(
  file.path(INT_DIR, "04_primary_scores_with_metadata.rds"),
  file.path(INT_DIR, "06_N_temporal_block.rds"),
  file.path(INT_DIR, "06_N_PCA_processes.rds"),
  file.path(INT_DIR, "06_N_PCA_genes.rds")
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop(
    "Missing files required by script 08:\n - ",
    paste(missing_files, collapse = "\n - "),
    call. = FALSE
  )
}

mcoa_scores <- readRDS(
  file.path(INT_DIR, "04_primary_scores_with_metadata.rds")
)

N_temporal <- readRDS(
  file.path(INT_DIR, "06_N_temporal_block.rds")
)

N_process_pca <- readRDS(
  file.path(INT_DIR, "06_N_PCA_processes.rds")
)

N_gene_pca <- readRDS(
  file.path(INT_DIR, "06_N_PCA_genes.rds")
)

TAX_AXES <- c("MCOA1", "MCOA2", "MCOA3")
TAX_AXES <- TAX_AXES[TAX_AXES %in% names(mcoa_scores)]

PROCESS_PCA_CUMVAR <- 0.95
GENE_PCA_CUMVAR <- 0.80

N_FUNCTION_VARS_3Y <- c(
  "PGNP",
  "PNNP",
  "N2O_reduction",
  "NEA"
)

if (length(TAX_AXES) < 2L) {
  stop("At least two MCOA axes are required.", call. = FALSE)
}

# ------------------------------------------------------------------------------
# 1. General helpers
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

normalise_meta <- function(df) {
  df |>
    mutate(
      MET = as.character(MET),
      Year = as.integer(as.character(Year)),
      LandUse = as.character(LandUse),
      Treatment = as.character(Treatment),
      Plot = as.character(Plot),
      Block = as.character(Block),
      Depth = as.character(Depth),
      profile_id = as.character(profile_id),
      sample_unit_id = as.character(sample_unit_id)
    )
}

mcoa_scores <- normalise_meta(mcoa_scores)
N_temporal <- normalise_meta(N_temporal)

if (nrow(mcoa_scores) != 108L) {
  stop("Expected 108 MCOA score rows.", call. = FALSE)
}
if (nrow(N_temporal) != 108L) {
  stop("Expected 108 processed N rows.", call. = FALSE)
}

if (!setequal(mcoa_scores$MET, N_temporal$MET)) {
  stop("MCOA and N temporal tables do not contain the same MET IDs.", call. = FALSE)
}

if (!identical(sort(unique(mcoa_scores$Year)), c(2022L, 2023L, 2024L))) {
  stop("Script 08 requires 2022, 2023 and 2024.", call. = FALSE)
}

align_pca_scores <- function(mcoa_df, pca_scores, pcs, prefix) {
  x <- pca_scores |>
    mutate(MET = as.character(MET)) |>
    select(MET, all_of(pcs))

  names(x)[match(pcs, names(x))] <- paste0(prefix, "_", pcs)

  out <- mcoa_df |>
    left_join(
      x,
      by = "MET"
    )

  new_cols <- paste0(prefix, "_", pcs)

  if (anyNA(out[, new_cols, drop = FALSE])) {
    stop(
      "PCA score alignment failed for prefix ",
      prefix,
      ".",
      call. = FALSE
    )
  }

  out
}

process_joined <- align_pca_scores(
  mcoa_scores,
  N_process_pca$scores,
  PROCESS_PCS,
  "N"
)

gene_joined <- align_pca_scores(
  mcoa_scores,
  N_gene_pca$scores,
  GENE_PCS,
  "G"
)

N_AXES <- paste0("N_", PROCESS_PCS)
G_AXES <- paste0("G_", GENE_PCS)

# ------------------------------------------------------------------------------
# 2. Exact whole-profile permutation helper
# ------------------------------------------------------------------------------

rv_coefficient <- function(X, Y) {
  X <- scale(as.matrix(X), center = TRUE, scale = FALSE)
  Y <- scale(as.matrix(Y), center = TRUE, scale = FALSE)

  XX <- X %*% t(X)
  YY <- Y %*% t(Y)

  den <- sqrt(sum(XX * XX) * sum(YY * YY))
  if (!is.finite(den) || den == 0) return(NA_real_)

  sum(XX * YY) / den
}

all_permutations <- function(x) {
  x <- as.character(x)

  if (length(x) <= 1L) {
    return(matrix(x, nrow = 1L))
  }

  do.call(
    rbind,
    lapply(
      seq_along(x),
      function(i) {
        cbind(
          x[i],
          all_permutations(x[-i])
        )
      }
    )
  )
}

build_profile_permutations <- function(
  metadata,
  strata_vars,
  match_vars,
  max_exact = 20000L,
  n_monte_carlo = 9999L,
  seed = 808L
) {
  required <- c(
    "profile_id",
    strata_vars,
    match_vars
  )

  missing <- setdiff(
    required,
    names(metadata)
  )

  if (length(missing) > 0L) {
    stop(
      "Missing permutation metadata: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  meta <- metadata |>
    mutate(
      profile_id = as.character(profile_id),
      across(
        all_of(c(strata_vars, match_vars)),
        as.character
      )
    )

  source_key <- do.call(
    paste,
    c(
      list(meta$profile_id),
      meta[match_vars],
      sep = "|||"
    )
  )

  if (anyDuplicated(source_key)) {
    stop(
      "profile_id × matched variables do not uniquely identify rows.",
      call. = FALSE
    )
  }

  profile_info <- meta |>
    distinct(
      profile_id,
      across(all_of(strata_vars))
    )

  if (anyDuplicated(profile_info$profile_id)) {
    stop(
      "A profile belongs to more than one permutation stratum.",
      call. = FALSE
    )
  }

  profile_info$.stratum <- do.call(
    interaction,
    c(
      profile_info[strata_vars],
      list(
        drop = TRUE,
        lex.order = TRUE
      )
    )
  )

  profiles_by_stratum <- split(
    profile_info$profile_id,
    profile_info$.stratum
  )
  profiles_by_stratum <- lapply(
    profiles_by_stratum,
    as.character
  )

  n_per_stratum <- vapply(
    profiles_by_stratum,
    length,
    integer(1)
  )

  n_possible <- prod(
    factorial(n_per_stratum)
  )

  make_key <- function(profile_vec, dat) {
    do.call(
      paste,
      c(
        list(profile_vec),
        dat[match_vars],
        sep = "|||"
      )
    )
  }

  build_index <- function(maps) {
    target_profile <- meta$profile_id

    for (s in names(maps)) {
      mp <- maps[[s]]

      for (p in names(mp)) {
        target_profile[
          meta$profile_id == p
        ] <- unname(mp[p])
      }
    }

    idx <- match(
      make_key(target_profile, meta),
      source_key
    )

    if (anyNA(idx)) {
      stop(
        "Could not map profile permutation back to rows.",
        call. = FALSE
      )
    }

    if (length(unique(idx)) != nrow(meta)) {
      stop(
        "Permutation mapping is not one-to-one.",
        call. = FALSE
      )
    }

    idx
  }

  if (n_possible <= max_exact) {
    perm_mats <- lapply(
      profiles_by_stratum,
      all_permutations
    )

    combo_grid <- expand.grid(
      lapply(
        perm_mats,
        function(x) seq_len(nrow(x))
      ),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )

    perm_idx <- matrix(
      NA_integer_,
      nrow = nrow(combo_grid),
      ncol = nrow(meta)
    )

    for (i in seq_len(nrow(combo_grid))) {
      maps <- vector(
        "list",
        length(profiles_by_stratum)
      )
      names(maps) <- names(profiles_by_stratum)

      for (j in seq_along(profiles_by_stratum)) {
        src <- profiles_by_stratum[[j]]
        tgt <- perm_mats[[j]][
          combo_grid[i, j],
          ,
          drop = TRUE
        ]

        maps[[j]] <- setNames(
          as.character(tgt),
          src
        )
      }

      perm_idx[i, ] <- build_index(maps)
    }

    method <- "exact"
  } else {
    set.seed(seed)

    perm_idx <- matrix(
      NA_integer_,
      nrow = n_monte_carlo,
      ncol = nrow(meta)
    )

    for (i in seq_len(n_monte_carlo)) {
      maps <- lapply(
        profiles_by_stratum,
        function(p) {
          setNames(
            sample(
              p,
              length(p),
              replace = FALSE
            ),
            p
          )
        }
      )

      perm_idx[i, ] <- build_index(maps)
    }

    method <- "monte_carlo"
  }

  list(
    indices = perm_idx,
    qc = tibble(
      method = method,
      n_rows = nrow(meta),
      n_profiles = n_distinct(meta$profile_id),
      n_strata = length(profiles_by_stratum),
      profiles_per_stratum = paste(
        n_per_stratum,
        collapse = ";"
      ),
      theoretical_permutations = n_possible,
      generated_permutations = nrow(perm_idx),
      strata = paste(
        strata_vars,
        collapse = " × "
      ),
      matched_within_profile = paste(
        match_vars,
        collapse = " × "
      )
    )
  )
}

rv_test_restricted <- function(
  X,
  Y,
  perm_idx
) {
  obs <- rv_coefficient(
    X,
    Y
  )

  null <- apply(
    perm_idx,
    1,
    function(idx) {
      rv_coefficient(
        X,
        Y[idx, , drop = FALSE]
      )
    }
  )

  tibble(
    RV = obs,
    p_perm = mean(
      null >= obs - 1e-12,
      na.rm = TRUE
    ),
    n_perm = length(null)
  )
}

cor_test_restricted <- function(
  x,
  y,
  perm_idx
) {
  obs <- suppressWarnings(
    cor(
      x,
      y,
      method = "spearman",
      use = "complete.obs"
    )
  )

  null <- apply(
    perm_idx,
    1,
    function(idx) {
      suppressWarnings(
        cor(
          x,
          y[idx],
          method = "spearman",
          use = "complete.obs"
        )
      )
    }
  )

  tibble(
    rho = obs,
    p_perm = mean(
      abs(null) >= abs(obs) - 1e-12,
      na.rm = TRUE
    ),
    n_perm = length(null)
  )
}

# ------------------------------------------------------------------------------
# 3. Three-point trajectory metrics
# ------------------------------------------------------------------------------

trajectory_metrics_one_space <- function(
  df,
  axes,
  prefix
) {
  groups <- split(
    df,
    df$sample_unit_id
  )

  purrr::map_dfr(
    groups,
    function(g) {
      g <- g |>
        arrange(Year)

      if (!identical(g$Year, c(2022L, 2023L, 2024L))) {
        stop(
          "Each sample_unit_id must contain exactly 2022, 2023, 2024.",
          call. = FALSE
        )
      }

      X <- as.matrix(
        g[, axes, drop = FALSE]
      )
      storage.mode(X) <- "double"

      d1 <- X[2, ] - X[1, ]
      d2 <- X[3, ] - X[2, ]

      l1 <- sqrt(sum(d1 ^ 2))
      l2 <- sqrt(sum(d2 ^ 2))
      total <- l1 + l2
      net <- sqrt(
        sum(
          (X[3, ] - X[1, ]) ^ 2
        )
      )

      straightness <- if (
        is.finite(total) &&
          total > 0
      ) {
        net / total
      } else {
        NA_real_
      }

      early_fraction <- if (
        is.finite(total) &&
          total > 0
      ) {
        l1 / total
      } else {
        NA_real_
      }

      denom <- l1 * l2

      turning_angle <- if (
        is.finite(denom) &&
          denom > 0
      ) {
        z <- sum(d1 * d2) / denom
        z <- max(-1, min(1, z))
        acos(z) * 180 / pi
      } else {
        NA_real_
      }

      tibble(
        sample_unit_id = g$sample_unit_id[1],
        profile_id = g$profile_id[1],
        LandUse = g$LandUse[1],
        Treatment = g$Treatment[1],
        Plot = g$Plot[1],
        Block = g$Block[1],
        Depth = g$Depth[1],
        !!paste0(prefix, "_length_2022_2023") := l1,
        !!paste0(prefix, "_length_2023_2024") := l2,
        !!paste0(prefix, "_total_length") := total,
        !!paste0(prefix, "_net_displacement") := net,
        !!paste0(prefix, "_straightness") := straightness,
        !!paste0(prefix, "_early_fraction") := early_fraction,
        !!paste0(prefix, "_turning_angle_deg") := turning_angle
      )
    }
  )
}

tax_metrics <- trajectory_metrics_one_space(
  process_joined,
  TAX_AXES,
  "tax"
)

N_metrics <- trajectory_metrics_one_space(
  process_joined,
  N_AXES,
  "N"
) |>
  select(
    sample_unit_id,
    starts_with("N_")
  )

G_metrics <- trajectory_metrics_one_space(
  gene_joined,
  G_AXES,
  "G"
) |>
  select(
    sample_unit_id,
    starts_with("G_")
  )

trajectory_metrics <- tax_metrics |>
  left_join(
    N_metrics,
    by = "sample_unit_id"
  ) |>
  left_join(
    G_metrics,
    by = "sample_unit_id"
  )

if (nrow(trajectory_metrics) != 36L) {
  stop(
    "Expected 36 depth-specific 3-year trajectories; found ",
    nrow(trajectory_metrics),
    ".",
    call. = FALSE
  )
}

trajectory_perm <- build_profile_permutations(
  trajectory_metrics,
  strata_vars = c(
    "LandUse",
    "Treatment"
  ),
  match_vars = "Depth",
  max_exact = 20000L,
  seed = 811L
)

metric_pairs <- tibble(
  metric = c(
    "total_length",
    "net_displacement",
    "straightness",
    "early_fraction",
    "turning_angle_deg"
  ),
  tax_col = paste0(
    "tax_",
    metric
  ),
  N_col = paste0(
    "N_",
    metric
  )
)

trajectory_metric_correlations <- purrr::pmap_dfr(
  metric_pairs,
  function(metric, tax_col, N_col) {
    cor_test_restricted(
      trajectory_metrics[[tax_col]],
      trajectory_metrics[[N_col]],
      trajectory_perm$indices
    ) |>
      mutate(
        metric = metric,
        tax_column = tax_col,
        N_column = N_col,
        .before = 1
      )
  }
) |>
  mutate(
    p_FDR = p.adjust(
      p_perm,
      method = "BH"
    )
  )

# ------------------------------------------------------------------------------
# 3b. Formal test of the temporal offset in movement magnitude
# ------------------------------------------------------------------------------
#
# Raw distances from MCOA and N-PCA live in different factor spaces and should
# not be subtracted directly. We therefore compare a dimensionless within-space
# interval balance:
#
#   balance = (distance_2023-2024 - distance_2022-2023) / total_length
#
# balance < 0 : movement concentrated in 2022→2023
# balance > 0 : movement concentrated in 2023→2024
#
# The primary statistic is:
#   timing_shift = N_balance - tax_balance
#
# Under the null that taxonomy and N functioning have the same temporal
# allocation of movement, the labels "taxonomy" and "N" are exchangeable within
# a profile. Swapping them changes the sign of timing_shift. We therefore use all
# 2^12 exact whole-profile sign flips, preserving the three depths together.

trajectory_metrics <- trajectory_metrics |>
  mutate(
    tax_interval_balance =
      (
        tax_length_2023_2024 -
          tax_length_2022_2023
      ) /
      tax_total_length,

    G_interval_balance =
      (
        G_length_2023_2024 -
          G_length_2022_2023
      ) /
      G_total_length,

    N_interval_balance =
      (
        N_length_2023_2024 -
          N_length_2022_2023
      ) /
      N_total_length,

    timing_shift =
      N_interval_balance -
      tax_interval_balance
  )

exact_profile_signflip_test <- function(
  values,
  profile_id
) {
  profile_id <- as.character(profile_id)

  profiles <- unique(
    profile_id
  )

  n_profiles <- length(
    profiles
  )

  if (n_profiles > 20L) {
    stop(
      "Exact profile sign-flip enumeration is intentionally limited to <=20 profiles.",
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

    null[
      i
    ] <- mean(
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
    observed_mean_timing_shift = observed,
    p_exact_two_sided = mean(
      abs(
        null
      ) >=
        abs(
          observed
        ) -
        1e-12
    ),
    n_profile_sign_flips = length(
      null
    ),
    null_q025 = unname(
      stats::quantile(
        null,
        0.025
      )
    ),
    null_median = unname(
      stats::quantile(
        null,
        0.5
      )
    ),
    null_q975 = unname(
      stats::quantile(
        null,
        0.975
      )
    )
  )
}

timing_shift_test <- exact_profile_signflip_test(
  trajectory_metrics$timing_shift,
  trajectory_metrics$profile_id
) |>
  mutate(
    tax_mean_early_fraction =
      mean(
        trajectory_metrics$tax_early_fraction,
        na.rm = TRUE
      ),
    N_mean_early_fraction =
      mean(
        trajectory_metrics$N_early_fraction,
        na.rm = TRUE
      ),
    tax_mean_interval_balance =
      mean(
        trajectory_metrics$tax_interval_balance,
        na.rm = TRUE
      ),
    N_mean_interval_balance =
      mean(
        trajectory_metrics$N_interval_balance,
        na.rm = TRUE
      ),
    interpretation =
      "Positive timing_shift means N movement is relatively later than taxonomic movement."
  )

# Pairwise temporal-offset tests among the three biological levels.
# Positive observed_mean_timing_shift means the second domain is later than the
# first domain in its allocation of 3-year movement.
timing_shift_pairwise <- bind_rows(
  exact_profile_signflip_test(
    trajectory_metrics$G_interval_balance -
      trajectory_metrics$tax_interval_balance,
    trajectory_metrics$profile_id
  ) |>
    mutate(
      comparison = "Gene potential - Taxonomy",
      domain_1 = "Taxonomy",
      domain_2 = "N gene potential",
      domain_1_mean_early_fraction =
        mean(trajectory_metrics$tax_early_fraction, na.rm = TRUE),
      domain_2_mean_early_fraction =
        mean(trajectory_metrics$G_early_fraction, na.rm = TRUE),
      .before = 1
    ),

  exact_profile_signflip_test(
    trajectory_metrics$N_interval_balance -
      trajectory_metrics$G_interval_balance,
    trajectory_metrics$profile_id
  ) |>
    mutate(
      comparison = "N activities - Gene potential",
      domain_1 = "N gene potential",
      domain_2 = "Potential N activities",
      domain_1_mean_early_fraction =
        mean(trajectory_metrics$G_early_fraction, na.rm = TRUE),
      domain_2_mean_early_fraction =
        mean(trajectory_metrics$N_early_fraction, na.rm = TRUE),
      .before = 1
    ),

  exact_profile_signflip_test(
    trajectory_metrics$N_interval_balance -
      trajectory_metrics$tax_interval_balance,
    trajectory_metrics$profile_id
  ) |>
    mutate(
      comparison = "N activities - Taxonomy",
      domain_1 = "Taxonomy",
      domain_2 = "Potential N activities",
      domain_1_mean_early_fraction =
        mean(trajectory_metrics$tax_early_fraction, na.rm = TRUE),
      domain_2_mean_early_fraction =
        mean(trajectory_metrics$N_early_fraction, na.rm = TRUE),
      .before = 1
    )
) |>
  mutate(
    p_FDR_pairwise = p.adjust(
      p_exact_two_sided,
      method = "BH"
    )
  )

timing_shift_by_depth <- purrr::map_dfr(
  unique(
    trajectory_metrics$Depth
  ),
  function(
    one_depth
  ) {
    dat <- trajectory_metrics |>
      filter(
        Depth == one_depth
      )

    exact_profile_signflip_test(
      dat$timing_shift,
      dat$profile_id
    ) |>
      mutate(
        Depth = one_depth,
        tax_mean_early_fraction =
          mean(
            dat$tax_early_fraction,
            na.rm = TRUE
          ),
        N_mean_early_fraction =
          mean(
            dat$N_early_fraction,
            na.rm = TRUE
          ),
        tax_mean_interval_balance =
          mean(
            dat$tax_interval_balance,
            na.rm = TRUE
          ),
        N_mean_interval_balance =
          mean(
            dat$N_interval_balance,
            na.rm = TRUE
          ),
        .before = 1
      )
  }
) |>
  mutate(
    p_FDR_depth =
      p.adjust(
        p_exact_two_sided,
        method = "BH"
      )
  )

relative_interval_movement <- bind_rows(
  trajectory_metrics |>
    transmute(
      sample_unit_id,
      profile_id,
      LandUse,
      Treatment,
      Plot,
      Block,
      Depth,
      Domain = "Taxonomy",
      `2022-2023` =
        tax_early_fraction,
      `2023-2024` =
        1 -
        tax_early_fraction
    ),

  trajectory_metrics |>
    transmute(
      sample_unit_id,
      profile_id,
      LandUse,
      Treatment,
      Plot,
      Block,
      Depth,
      Domain = "N gene potential",
      `2022-2023` =
        G_early_fraction,
      `2023-2024` =
        1 -
        G_early_fraction
    ),

  trajectory_metrics |>
    transmute(
      sample_unit_id,
      profile_id,
      LandUse,
      Treatment,
      Plot,
      Block,
      Depth,
      Domain = "N functioning",
      `2022-2023` =
        N_early_fraction,
      `2023-2024` =
        1 -
        N_early_fraction
    )
) |>
  pivot_longer(
    cols = c(
      `2022-2023`,
      `2023-2024`
    ),
    names_to = "Interval",
    values_to = "fraction_total_trajectory"
  ) |>
  mutate(
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
      )
    )
  )

relative_interval_movement_summary <- relative_interval_movement |>
  group_by(
    Depth,
    Domain,
    Interval
  ) |>
  summarise(
    n = n(),
    mean_fraction =
      mean(
        fraction_total_trajectory,
        na.rm = TRUE
      ),
    sd_fraction =
      sd(
        fraction_total_trajectory,
        na.rm = TRUE
      ),
    se_fraction =
      sd_fraction /
      sqrt(
        n
      ),
    .groups = "drop"
  )

# Overall descriptive summary, useful for Results text.
relative_interval_movement_overall <- relative_interval_movement |>
  group_by(
    Domain,
    Interval
  ) |>
  summarise(
    n = n(),
    mean_fraction =
      mean(
        fraction_total_trajectory,
        na.rm = TRUE
      ),
    sd_fraction =
      sd(
        fraction_total_trajectory,
        na.rm = TRUE
      ),
    se_fraction =
      sd_fraction /
      sqrt(
        n
      ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 4. Interval changes: 2022→2023 and 2023→2024
# ------------------------------------------------------------------------------

build_interval_changes <- function(
  df,
  tax_axes,
  n_axes
) {
  groups <- split(
    df,
    df$sample_unit_id
  )

  purrr::map_dfr(
    groups,
    function(g) {
      g <- g |>
        arrange(Year)

      if (!identical(g$Year, c(2022L, 2023L, 2024L))) {
        stop(
          "Interval change construction requires exactly 2022, 2023, 2024.",
          call. = FALSE
        )
      }

      purrr::map_dfr(
        1:2,
        function(i) {
          out <- tibble(
            sample_unit_id = g$sample_unit_id[i],
            profile_id = g$profile_id[i],
            LandUse = g$LandUse[i],
            Treatment = g$Treatment[i],
            Plot = g$Plot[i],
            Block = g$Block[i],
            Depth = g$Depth[i],
            Year_from = g$Year[i],
            Year_to = g$Year[i + 1L],
            Interval = paste0(
              g$Year[i],
              "-",
              g$Year[i + 1L]
            )
          )

          for (v in c(tax_axes, n_axes)) {
            out[[paste0("d_", v)]] <-
              g[[v]][i + 1L] -
              g[[v]][i]
          }

          out
        }
      )
    }
  )
}

interval_changes <- build_interval_changes(
  process_joined,
  TAX_AXES,
  N_AXES
)

if (nrow(interval_changes) != 72L) {
  stop(
    "Expected 72 annual transitions (36 units × 2 intervals); found ",
    nrow(interval_changes),
    ".",
    call. = FALSE
  )
}

TAX_DELTA <- paste0(
  "d_",
  TAX_AXES
)
N_DELTA <- paste0(
  "d_",
  N_AXES
)

interval_perm <- build_profile_permutations(
  interval_changes,
  strata_vars = c(
    "LandUse",
    "Treatment"
  ),
  match_vars = c(
    "Interval",
    "Depth"
  ),
  max_exact = 20000L,
  seed = 812L
)

pooled_raw <- rv_test_restricted(
  interval_changes |>
    select(all_of(TAX_DELTA)),
  interval_changes |>
    select(all_of(N_DELTA)),
  interval_perm$indices
) |>
  mutate(
    scope = "pooled_annual_intervals",
    adjustment = "raw",
    .before = 1
  )

# Design-residual sensitivity on stacked annual transitions.
residualize_interval_design <- function(
  Y,
  metadata
) {
  design <- metadata |>
    transmute(
      Interval = factor(Interval),
      Treatment = factor(Treatment),
      LandUse = factor(LandUse),
      Depth = factor(Depth),
      Block = factor(Block)
    )

  Xd <- model.matrix(
    ~ Interval * Treatment * LandUse * Depth + Block,
    data = design
  )

  Y <- as.matrix(Y)

  out <- apply(
    Y,
    2,
    function(y) {
      lm.fit(
        x = Xd,
        y = y
      )$residuals
    }
  )

  if (is.vector(out)) {
    out <- matrix(
      out,
      ncol = 1L
    )
  }

  colnames(out) <- colnames(Y)
  out
}

X_interval <- as.matrix(
  interval_changes[, TAX_DELTA, drop = FALSE]
)
Y_interval <- as.matrix(
  interval_changes[, N_DELTA, drop = FALSE]
)

X_interval_resid <- residualize_interval_design(
  X_interval,
  interval_changes
)

Y_interval_resid <- residualize_interval_design(
  Y_interval,
  interval_changes
)

pooled_adjusted <- rv_test_restricted(
  X_interval_resid,
  Y_interval_resid,
  interval_perm$indices
) |>
  mutate(
    scope = "pooled_annual_intervals",
    adjustment = "design_residual_sensitivity",
    .before = 1
  )

# Interval-specific raw RV.
interval_specific_RV <- purrr::map_dfr(
  c(
    "2022-2023",
    "2023-2024"
  ),
  function(one_interval) {
    dat <- interval_changes |>
      filter(Interval == one_interval)

    po <- build_profile_permutations(
      dat,
      strata_vars = c(
        "LandUse",
        "Treatment"
      ),
      match_vars = "Depth",
      max_exact = 20000L,
      seed = ifelse(
        one_interval == "2022-2023",
        813L,
        814L
      )
    )

    rv_test_restricted(
      dat |>
        select(all_of(TAX_DELTA)),
      dat |>
        select(all_of(N_DELTA)),
      po$indices
    ) |>
      mutate(
        scope = one_interval,
        adjustment = "raw",
        .before = 1
      )
  }
)

# ------------------------------------------------------------------------------
# 5. Net 2022→2024 change
# ------------------------------------------------------------------------------

build_net_change <- function(
  df,
  tax_axes,
  n_axes
) {
  groups <- split(
    df,
    df$sample_unit_id
  )

  purrr::map_dfr(
    groups,
    function(g) {
      g <- g |>
        arrange(Year)

      if (!identical(g$Year, c(2022L, 2023L, 2024L))) {
        stop(
          "Net-change construction requires exactly 2022, 2023, 2024.",
          call. = FALSE
        )
      }

      out <- tibble(
        sample_unit_id = g$sample_unit_id[1],
        profile_id = g$profile_id[1],
        LandUse = g$LandUse[1],
        Treatment = g$Treatment[1],
        Plot = g$Plot[1],
        Block = g$Block[1],
        Depth = g$Depth[1]
      )

      for (v in c(tax_axes, n_axes)) {
        out[[paste0("d_", v)]] <-
          g[[v]][3] -
          g[[v]][1]
      }

      out
    }
  )
}

net_changes <- build_net_change(
  process_joined,
  TAX_AXES,
  N_AXES
)

net_perm <- build_profile_permutations(
  net_changes,
  strata_vars = c(
    "LandUse",
    "Treatment"
  ),
  match_vars = "Depth",
  max_exact = 20000L,
  seed = 815L
)

net_RV <- rv_test_restricted(
  net_changes |>
    select(all_of(TAX_DELTA)),
  net_changes |>
    select(all_of(N_DELTA)),
  net_perm$indices
) |>
  mutate(
    scope = "net_2022-2024",
    adjustment = "raw",
    .before = 1
  )

temporal_RV_summary <- bind_rows(
  pooled_raw,
  pooled_adjusted,
  interval_specific_RV,
  net_RV
) |>
  group_by(adjustment) |>
  mutate(
    p_FDR_within_adjustment = p.adjust(
      p_perm,
      method = "BH"
    )
  ) |>
  ungroup()

# ------------------------------------------------------------------------------
# 6. Procrustes of stacked annual-change configurations
# ------------------------------------------------------------------------------

procrustes_correlation <- function(
  X,
  Y
) {
  k <- min(
    ncol(X),
    ncol(Y)
  )

  fit <- vegan::procrustes(
    as.matrix(X)[, seq_len(k), drop = FALSE],
    as.matrix(Y)[, seq_len(k), drop = FALSE],
    symmetric = TRUE
  )

  sqrt(
    max(
      0,
      1 - fit$ss
    )
  )
}

procrustes_obs <- procrustes_correlation(
  X_interval,
  Y_interval
)

procrustes_null <- apply(
  interval_perm$indices,
  1,
  function(idx) {
    procrustes_correlation(
      X_interval,
      Y_interval[idx, , drop = FALSE]
    )
  }
)

procrustes_summary <- tibble(
  scope = "pooled_annual_intervals",
  procrustes_correlation = procrustes_obs,
  p_perm = mean(
    procrustes_null >= procrustes_obs - 1e-12,
    na.rm = TRUE
  ),
  n_perm = length(procrustes_null)
)

# ------------------------------------------------------------------------------
# 7. Treatment-specific coupling
# ------------------------------------------------------------------------------

treatment_specific_RV <- purrr::map_dfr(
  c(
    "Control",
    "Heated"
  ),
  function(trt) {
    dat <- interval_changes |>
      filter(Treatment == trt)

    po <- build_profile_permutations(
      dat,
      strata_vars = "LandUse",
      match_vars = c(
        "Interval",
        "Depth"
      ),
      max_exact = 20000L,
      seed = ifelse(
        trt == "Control",
        816L,
        817L
      )
    )

    rv_test_restricted(
      dat |>
        select(all_of(TAX_DELTA)),
      dat |>
        select(all_of(N_DELTA)),
      po$indices
    ) |>
      mutate(
        Treatment = trt,
        scope = "pooled_annual_intervals",
        permutation =
          "whole_profile_within_LandUse_matched_Interval_x_Depth",
        .before = 1
      )
  }
)

# Exact Control/Heated label swaps within LandUse × Block profile pairs.
profile_info <- interval_changes |>
  distinct(
    profile_id,
    LandUse,
    Treatment,
    Block
  )

pair_check <- profile_info |>
  count(
    LandUse,
    Block,
    name = "n"
  )

if (any(pair_check$n != 2L)) {
  stop(
    "Treatment label-swap test requires exactly two profiles per LandUse × Block pair.",
    call. = FALSE
  )
}

pair_treatment_check <- profile_info |>
  group_by(
    LandUse,
    Block
  ) |>
  summarise(
    treatments = paste(
      sort(unique(Treatment)),
      collapse = ";"
    ),
    .groups = "drop"
  )

if (any(pair_treatment_check$treatments != "Control;Heated")) {
  stop(
    "Each LandUse × Block pair must contain one Control and one Heated profile.",
    call. = FALSE
  )
}

rv_by_label <- function(labels) {
  heated_idx <- which(labels == "Heated")
  control_idx <- which(labels == "Control")

  rv_heated <- rv_coefficient(
    X_interval[heated_idx, , drop = FALSE],
    Y_interval[heated_idx, , drop = FALSE]
  )

  rv_control <- rv_coefficient(
    X_interval[control_idx, , drop = FALSE],
    Y_interval[control_idx, , drop = FALSE]
  )

  c(
    RV_Heated = rv_heated,
    RV_Control = rv_control,
    difference = rv_heated - rv_control
  )
}

observed_labels <- interval_changes$Treatment
obs_treatment_diff <- rv_by_label(
  observed_labels
)

pair_ids <- interaction(
  profile_info$LandUse,
  profile_info$Block,
  drop = TRUE,
  lex.order = TRUE
)

pairs <- split(
  profile_info,
  pair_ids
)

swap_grid <- expand.grid(
  rep(
    list(0:1),
    length(pairs)
  ),
  KEEP.OUT.ATTRS = FALSE
)

swap_null <- numeric(
  nrow(swap_grid)
)

for (i in seq_len(nrow(swap_grid))) {
  label_map <- setNames(
    profile_info$Treatment,
    profile_info$profile_id
  )

  for (j in seq_along(pairs)) {
    if (swap_grid[i, j] == 1L) {
      pp <- pairs[[j]]$profile_id
      old <- label_map[pp]

      label_map[pp] <- ifelse(
        old == "Control",
        "Heated",
        "Control"
      )
    }
  }

  permuted_labels <- unname(
    label_map[
      interval_changes$profile_id
    ]
  )

  swap_null[i] <- rv_by_label(
    permuted_labels
  )[["difference"]]
}

treatment_difference_test <- tibble(
  RV_Heated = obs_treatment_diff[["RV_Heated"]],
  RV_Control = obs_treatment_diff[["RV_Control"]],
  RV_difference_Heated_minus_Control =
    obs_treatment_diff[["difference"]],
  p_exact_two_sided = mean(
    abs(swap_null) >=
      abs(obs_treatment_diff[["difference"]]) - 1e-12
  ),
  n_label_swaps = length(swap_null),
  scheme =
    "exact profile treatment-label swaps within LandUse x Block"
)

# ------------------------------------------------------------------------------
# 7b. Individual functional-gene temporal changes
# ------------------------------------------------------------------------------
#
# These are not folded into the measured N-activity PCA. They are retained as a
# separate biological layer: abundance of functional genes (qPCR potential),
# transformed in script 06 as log10(x+1) and z-standardised across 2022-2024.
# Absolute annual change is used here to ask WHEN each marker moves, regardless
# of the sign of that change. Exact sign flips preserve all depths within a
# profile.

GENE_VARS_ALL <- intersect(
  c(
    "AOA",
    "AOB",
    "ComA",
    "ComB",
    "nirS",
    "nirK",
    "nosZ1"
  ),
  names(N_temporal)
)

build_gene_variable_interval_changes <- function(df, vars) {
  groups <- split(df, df$sample_unit_id)

  purrr::map_dfr(
    groups,
    function(g) {
      g <- g |> arrange(Year)

      if (!identical(g$Year, c(2022L, 2023L, 2024L))) {
        stop(
          "Gene-variable interval changes require exactly 2022, 2023 and 2024.",
          call. = FALSE
        )
      }

      purrr::map_dfr(
        1:2,
        function(i) {
          purrr::map_dfr(
            vars,
            function(v) {
              delta <- g[[v]][i + 1L] - g[[v]][i]

              tibble(
                sample_unit_id = g$sample_unit_id[i],
                profile_id = g$profile_id[i],
                LandUse = g$LandUse[i],
                Treatment = g$Treatment[i],
                Plot = g$Plot[i],
                Block = g$Block[i],
                Depth = g$Depth[i],
                Year_from = g$Year[i],
                Year_to = g$Year[i + 1L],
                Interval = paste0(g$Year[i], "-", g$Year[i + 1L]),
                Gene = v,
                delta = delta,
                abs_delta = abs(delta)
              )
            }
          )
        }
      )
    }
  )
}

gene_variable_interval_changes <- build_gene_variable_interval_changes(
  N_temporal,
  GENE_VARS_ALL
)

gene_variable_interval_summary <- gene_variable_interval_changes |>
  group_by(
    Depth,
    Interval,
    Gene
  ) |>
  summarise(
    n = sum(is.finite(abs_delta)),
    mean_abs_change = mean(abs_delta, na.rm = TRUE),
    sd_abs_change = sd(abs_delta, na.rm = TRUE),
    se_abs_change = ifelse(
      n > 1L,
      sd_abs_change / sqrt(n),
      NA_real_
    ),
    .groups = "drop"
  )

gene_timing_wide <- gene_variable_interval_changes |>
  select(
    sample_unit_id,
    profile_id,
    Depth,
    Gene,
    Interval,
    abs_delta
  ) |>
  pivot_wider(
    names_from = Interval,
    values_from = abs_delta
  )

gene_variable_timing_tests <- purrr::map_dfr(
  GENE_VARS_ALL,
  function(v) {
    d <- gene_timing_wide |>
      filter(Gene == v)

    diff_early_minus_late <-
      d[["2022-2023"]] -
      d[["2023-2024"]]

    exact_profile_signflip_test(
      diff_early_minus_late,
      d$profile_id
    ) |>
      mutate(
        Gene = v,
        mean_abs_change_2022_2023 =
          mean(d[["2022-2023"]], na.rm = TRUE),
        mean_abs_change_2023_2024 =
          mean(d[["2023-2024"]], na.rm = TRUE),
        mean_early_minus_late =
          mean(diff_early_minus_late, na.rm = TRUE),
        n_complete_units =
          sum(is.finite(diff_early_minus_late)),
        .before = 1
      )
  }
) |>
  mutate(
    p_FDR = p.adjust(
      p_exact_two_sided,
      method = "BH"
    )
  )

# ------------------------------------------------------------------------------
# 8. Secondary gene-potential annual-change RV
# ------------------------------------------------------------------------------

gene_interval_changes <- build_interval_changes(
  gene_joined,
  TAX_AXES,
  G_AXES
)

G_DELTA <- paste0(
  "d_",
  G_AXES
)

gene_perm <- build_profile_permutations(
  gene_interval_changes,
  strata_vars = c(
    "LandUse",
    "Treatment"
  ),
  match_vars = c(
    "Interval",
    "Depth"
  ),
  max_exact = 20000L,
  seed = 818L
)

gene_temporal_RV <- rv_test_restricted(
  gene_interval_changes |>
    select(all_of(TAX_DELTA)),
  gene_interval_changes |>
    select(all_of(G_DELTA)),
  gene_perm$indices
) |>
  mutate(
    block = "gene_potential",
    scope = "pooled_annual_intervals",
    adjustment = "raw",
    .before = 1
  )

# ------------------------------------------------------------------------------
# 9. Individual realised-process annual changes
# ------------------------------------------------------------------------------

missing_process_vars <- setdiff(
  N_FUNCTION_VARS_3Y,
  names(N_temporal)
)

if (length(missing_process_vars) > 0L) {
  stop(
    "Processed N temporal block lacks: ",
    paste(missing_process_vars, collapse = ", "),
    call. = FALSE
  )
}

process_variable_joined <- mcoa_scores |>
  left_join(
    N_temporal |>
      select(
        MET,
        all_of(N_FUNCTION_VARS_3Y)
      ) |>
      rename_with(
        ~ paste0("PROC_", .x),
        all_of(N_FUNCTION_VARS_3Y)
      ),
    by = "MET"
  )

PROC_AXES <- paste0(
  "PROC_",
  N_FUNCTION_VARS_3Y
)

if (anyNA(process_variable_joined[, PROC_AXES, drop = FALSE])) {
  stop(
    "Processed individual N variables failed to align by MET.",
    call. = FALSE
  )
}

process_variable_intervals <- build_interval_changes(
  process_variable_joined,
  TAX_AXES,
  PROC_AXES
)

PROC_DELTA <- paste0(
  "d_",
  PROC_AXES
)

individual_process_delta_correlations <- purrr::map_dfr(
  c(
    "pooled_annual_intervals",
    "2022-2023",
    "2023-2024"
  ),
  function(scope_name) {
    dat <- if (
      scope_name == "pooled_annual_intervals"
    ) {
      process_variable_intervals
    } else {
      process_variable_intervals |>
        filter(
          Interval == scope_name
        )
    }

    match_vars <- if (
      scope_name == "pooled_annual_intervals"
    ) {
      c(
        "Interval",
        "Depth"
      )
    } else {
      "Depth"
    }

    po <- build_profile_permutations(
      dat,
      strata_vars = c(
        "LandUse",
        "Treatment"
      ),
      match_vars = match_vars,
      max_exact = 20000L,
      seed = ifelse(
        scope_name == "2022-2023",
        819L,
        ifelse(
          scope_name == "2023-2024",
          820L,
          821L
        )
      )
    )

    tidyr::expand_grid(
      taxonomy_axis = TAX_DELTA,
      process = PROC_DELTA
    ) |>
      purrr::pmap_dfr(
        function(
          taxonomy_axis,
          process
        ) {
          cor_test_restricted(
            dat[[taxonomy_axis]],
            dat[[process]],
            po$indices
          ) |>
            mutate(
              scope = scope_name,
              taxonomy_axis = sub(
                "^d_",
                "",
                taxonomy_axis
              ),
              process = sub(
                "^d_PROC_",
                "",
                process
              ),
              .before = 1
            )
        }
      ) |>
      mutate(
        p_FDR = p.adjust(
          p_perm,
          method = "BH"
        )
      )
  }
)

# ------------------------------------------------------------------------------
# 10. Export tables
# ------------------------------------------------------------------------------

dir.create(
  TABLE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

trajectory_scores_all <- process_joined |>
  left_join(
    gene_joined |>
      select(
        MET,
        all_of(G_AXES)
      ),
    by = "MET"
  )

write.csv(
  trajectory_scores_all,
  file.path(
    TABLE_DIR,
    "08_trajectory_scores.csv"
  ),
  row.names = FALSE
)

write.csv(
  trajectory_metrics,
  file.path(
    TABLE_DIR,
    "08_trajectory_metrics.csv"
  ),
  row.names = FALSE
)

write.csv(
  trajectory_metric_correlations,
  file.path(
    TABLE_DIR,
    "08_trajectory_metric_correlations.csv"
  ),
  row.names = FALSE
)

write.csv(
  relative_interval_movement,
  file.path(
    TABLE_DIR,
    "08_relative_interval_movement.csv"
  ),
  row.names = FALSE
)

write.csv(
  bind_rows(
    relative_interval_movement_summary |>
      mutate(
        Scope = "by_depth",
        .before = 1
      ),
    relative_interval_movement_overall |>
      mutate(
        Depth = "All",
        Scope = "overall",
        .before = 1
      )
  ),
  file.path(
    TABLE_DIR,
    "08_relative_interval_movement_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  timing_shift_test,
  file.path(
    TABLE_DIR,
    "08_timing_shift_test.csv"
  ),
  row.names = FALSE
)

write.csv(
  timing_shift_pairwise,
  file.path(
    TABLE_DIR,
    "08_timing_shift_pairwise.csv"
  ),
  row.names = FALSE
)

write.csv(
  timing_shift_by_depth,
  file.path(
    TABLE_DIR,
    "08_timing_shift_by_depth.csv"
  ),
  row.names = FALSE
)

write.csv(
  interval_changes,
  file.path(
    TABLE_DIR,
    "08_interval_changes.csv"
  ),
  row.names = FALSE
)

write.csv(
  net_changes,
  file.path(
    TABLE_DIR,
    "08_net_changes_2022_2024.csv"
  ),
  row.names = FALSE
)

write.csv(
  temporal_RV_summary,
  file.path(
    TABLE_DIR,
    "08_temporal_RV_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  treatment_specific_RV,
  file.path(
    TABLE_DIR,
    "08_treatment_specific_RV.csv"
  ),
  row.names = FALSE
)

write.csv(
  treatment_difference_test,
  file.path(
    TABLE_DIR,
    "08_treatment_difference_test.csv"
  ),
  row.names = FALSE
)

write.csv(
  procrustes_summary,
  file.path(
    TABLE_DIR,
    "08_procrustes_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  individual_process_delta_correlations,
  file.path(
    TABLE_DIR,
    "08_individual_process_delta_correlations.csv"
  ),
  row.names = FALSE
)

write.csv(
  gene_variable_interval_changes,
  file.path(
    TABLE_DIR,
    "08_gene_variable_interval_changes.csv"
  ),
  row.names = FALSE
)

write.csv(
  gene_variable_interval_summary,
  file.path(
    TABLE_DIR,
    "08_gene_variable_interval_summary.csv"
  ),
  row.names = FALSE
)

write.csv(
  gene_variable_timing_tests,
  file.path(
    TABLE_DIR,
    "08_gene_variable_timing_tests.csv"
  ),
  row.names = FALSE
)

write.csv(
  gene_temporal_RV,
  file.path(
    TABLE_DIR,
    "08_gene_temporal_RV.csv"
  ),
  row.names = FALSE
)

permutation_QC <- bind_rows(
  trajectory_perm$qc |>
    mutate(analysis = "trajectory_metrics"),
  interval_perm$qc |>
    mutate(analysis = "pooled_annual_intervals"),
  net_perm$qc |>
    mutate(analysis = "net_2022_2024"),
  gene_perm$qc |>
    mutate(analysis = "gene_pooled_annual_intervals")
)

write.csv(
  permutation_QC,
  file.path(
    TABLE_DIR,
    "08_permutation_scheme_QC.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 11. Save analysis object
# ------------------------------------------------------------------------------

analysis_08 <- list(
  settings = list(
    years = c(
      2022L,
      2023L,
      2024L
    ),
    tax_axes = TAX_AXES,
    process_pcs = PROCESS_PCS,
    gene_pcs = GENE_PCS,
    process_pca_cumulative_threshold =
      PROCESS_PCA_CUMVAR,
    gene_pca_cumulative_threshold =
      GENE_PCA_CUMVAR,
    note =
      "Adjacent interval differences share 2023; all permutations preserve both intervals within whole profiles."
  ),
  trajectory_scores = trajectory_scores_all,
  trajectory_metrics = trajectory_metrics,
  trajectory_metric_correlations =
    trajectory_metric_correlations,
  timing_shift_test = timing_shift_test,
  timing_shift_pairwise = timing_shift_pairwise,
  timing_shift_by_depth = timing_shift_by_depth,
  relative_interval_movement = relative_interval_movement,
  relative_interval_movement_summary =
    relative_interval_movement_summary,
  interval_changes = interval_changes,
  net_changes = net_changes,
  temporal_RV_summary = temporal_RV_summary,
  treatment_specific_RV =
    treatment_specific_RV,
  treatment_difference_test =
    treatment_difference_test,
  procrustes_summary = procrustes_summary,
  individual_process_delta_correlations =
    individual_process_delta_correlations,
  gene_variable_interval_changes = gene_variable_interval_changes,
  gene_variable_interval_summary = gene_variable_interval_summary,
  gene_variable_timing_tests = gene_variable_timing_tests,
  gene_temporal_RV = gene_temporal_RV,
  permutation_QC = permutation_QC
)

saveRDS(
  analysis_08,
  file.path(
    INT_DIR,
    "08_temporal_coupling.rds"
  )
)

# ------------------------------------------------------------------------------
# 12. Console summary
# ------------------------------------------------------------------------------

message(
  "Script 08 completed: 36 three-point trajectories and 72 annual transitions."
)

message(
  "Pooled annual-transition RV = ",
  round(
    pooled_raw$RV,
    3
  ),
  "; exact profile-permutation p = ",
  signif(
    pooled_raw$p_perm,
    4
  )
)

message(
  "Net 2022-2024 RV = ",
  round(
    net_RV$RV,
    3
  ),
  "; exact profile-permutation p = ",
  signif(
    net_RV$p_perm,
    4
  )
)

message(
  "Trajectory metric tests written to 08_trajectory_metric_correlations.csv."
)

message(
  "Relative timing shift (N later than taxonomy when positive): ",
  round(
    timing_shift_test$observed_mean_timing_shift,
    3
  ),
  "; exact profile label-swap p = ",
  signif(
    timing_shift_test$p_exact_two_sided,
    4
  ),
  "."
)

message(
  "Next script: scripts/09_main_figures.R"
)
# ==============================================================================
