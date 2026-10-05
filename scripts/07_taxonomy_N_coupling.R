# ==============================================================================
# 07_taxonomy_N_coupling.R
# SOILWARM — cross-sectional taxonomy × N coupling across 2022-2024
# ==============================================================================
#
# PRIMARY QUESTION
# ----------------
# Across the pooled 108 observations, how strongly does multi-kingdom community
# structure covary with realised N-process structure?
#
# INFERENCE
# ---------
# Whole profiles are permuted within LandUse × Treatment strata while preserving
# the complete 3-year × 3-depth structure of each profile.
#
# A design-residual analysis is retained as a sensitivity analysis. It removes
# fixed design structure from both blocks before the same restricted permutation
# test. This is not presented as causal inference.
#
# PRIMARY BLOCKS
# --------------
# taxonomy: MCOA1-MCOA3
# realised N processes: PCA fitted in script 06
# gene potential: secondary PCA fitted in script 06
#
# Nitrate is not analysed here because no complete 3-year source is available.
# ==============================================================================

source("R/config.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ade4)
  library(stringr)
})

source("R/functions_soilwarm.R")

required_files <- c(
  file.path(INT_DIR, "04_primary_MCOA.rds"),
  file.path(INT_DIR, "04_primary_scores_with_metadata.rds"),
  file.path(INT_DIR, "06_N_PCA_processes.rds"),
  file.path(INT_DIR, "06_N_PCA_genes.rds")
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop(
    "Missing files:\n - ",
    paste(missing_files, collapse = "\n - "),
    call. = FALSE
  )
}

primary <- readRDS(file.path(INT_DIR, "04_primary_MCOA.rds"))
mcoa_scores_meta <- readRDS(
  file.path(INT_DIR, "04_primary_scores_with_metadata.rds")
)
N_process_pca <- readRDS(
  file.path(INT_DIR, "06_N_PCA_processes.rds")
)
N_gene_pca <- readRDS(
  file.path(INT_DIR, "06_N_PCA_genes.rds")
)

MCOA_AXES <- c("MCOA1", "MCOA2", "MCOA3")
MCOA_AXES <- MCOA_AXES[MCOA_AXES %in% names(mcoa_scores_meta)]

if (length(MCOA_AXES) < 2L) {
  stop("At least two MCOA axes are required.", call. = FALSE)
}

# ------------------------------------------------------------------------------
# 1. Alignment helpers
# ------------------------------------------------------------------------------

get_pc_columns <- function(df) {
  x <- grep("^PC[0-9]+$", names(df), value = TRUE)
  if (length(x) == 0L) stop("No PC columns found.", call. = FALSE)
  x
}

align_score_tables <- function(mcoa_df, n_score_df) {
  if (!"MET" %in% names(mcoa_df) || !"MET" %in% names(n_score_df)) {
    stop("MET must be present in both score tables.", call. = FALSE)
  }

  mcoa_df <- mcoa_df |>
    mutate(MET = as.character(MET))

  n_score_df <- n_score_df |>
    mutate(MET = as.character(MET))

  if (anyDuplicated(mcoa_df$MET)) {
    stop("Duplicated MET values in MCOA metadata.", call. = FALSE)
  }
  if (anyDuplicated(n_score_df$MET)) {
    stop("Duplicated MET values in N PCA scores.", call. = FALSE)
  }

  pc_cols <- get_pc_columns(n_score_df)

  out <- mcoa_df |>
    left_join(
      n_score_df |>
        select(MET, all_of(pc_cols)),
      by = "MET"
    )

  if (nrow(out) != nrow(mcoa_df)) {
    stop("MCOA/N alignment changed the number of rows.", call. = FALSE)
  }

  if (anyNA(out[, pc_cols, drop = FALSE])) {
    stop("At least one N PCA score failed to match by MET.", call. = FALSE)
  }

  out
}

process_joined <- align_score_tables(
  mcoa_scores_meta,
  N_process_pca$scores
)

gene_joined <- align_score_tables(
  mcoa_scores_meta,
  N_gene_pca$scores
)

# Normalise metadata types.
process_joined <- process_joined |>
  mutate(
    Year = as.integer(as.character(Year)),
    profile_id = as.character(profile_id),
    sample_unit_id = as.character(sample_unit_id),
    LandUse = as.character(LandUse),
    Treatment = as.character(Treatment),
    Depth = as.character(Depth),
    Block = as.character(Block)
  )

gene_joined <- gene_joined |>
  mutate(
    Year = as.integer(as.character(Year)),
    profile_id = as.character(profile_id),
    sample_unit_id = as.character(sample_unit_id),
    LandUse = as.character(LandUse),
    Treatment = as.character(Treatment),
    Depth = as.character(Depth),
    Block = as.character(Block)
  )

if (nrow(process_joined) != 108L) {
  stop("Script 07 expects 108 aligned observations.", call. = FALSE)
}

if (!identical(sort(unique(process_joined$Year)), c(2022L, 2023L, 2024L))) {
  stop("Script 07 requires 2022, 2023 and 2024.", call. = FALSE)
}

layout_qc <- process_joined |>
  count(profile_id, name = "n_rows")

if (any(layout_qc$n_rows != 9L)) {
  stop(
    "Each profile must contain 9 observations (3 years × 3 depths).",
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 2. RV + exact restricted permutation helpers
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
  if (length(x) <= 1L) return(matrix(x, nrow = 1L))
  do.call(
    rbind,
    lapply(
      seq_along(x),
      function(i) cbind(x[i], all_permutations(x[-i]))
    )
  )
}

build_profile_permutations <- function(
  metadata,
  strata_vars = c("LandUse", "Treatment"),
  match_vars = c("Year", "Depth"),
  max_exact = 20000L,
  n_monte_carlo = 9999L,
  seed = 707L
) {
  required <- c("profile_id", strata_vars, match_vars)
  missing <- setdiff(required, names(metadata))

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
      across(all_of(c(strata_vars, match_vars)), as.character)
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
    distinct(profile_id, across(all_of(strata_vars)))

  if (anyDuplicated(profile_info$profile_id)) {
    stop("A profile belongs to more than one permutation stratum.", call. = FALSE)
  }

  profile_info$.stratum <- do.call(
    interaction,
    c(
      profile_info[strata_vars],
      list(drop = TRUE, lex.order = TRUE)
    )
  )

  profiles_by_stratum <- split(
    profile_info$profile_id,
    profile_info$.stratum
  )
  profiles_by_stratum <- lapply(profiles_by_stratum, as.character)

  n_per_stratum <- vapply(
    profiles_by_stratum,
    length,
    integer(1)
  )
  n_possible <- prod(factorial(n_per_stratum))

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
        target_profile[meta$profile_id == p] <- unname(mp[p])
      }
    }

    idx <- match(
      make_key(target_profile, meta),
      source_key
    )

    if (anyNA(idx)) {
      stop("Could not map a profile permutation back to rows.", call. = FALSE)
    }
    if (length(unique(idx)) != nrow(meta)) {
      stop("Permutation mapping is not one-to-one.", call. = FALSE)
    }

    idx
  }

  if (n_possible <= max_exact) {
    perm_mats <- lapply(
      profiles_by_stratum,
      all_permutations
    )

    combo_grid <- expand.grid(
      lapply(perm_mats, function(x) seq_len(nrow(x))),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )

    perm_idx <- matrix(
      NA_integer_,
      nrow = nrow(combo_grid),
      ncol = nrow(meta)
    )

    for (i in seq_len(nrow(combo_grid))) {
      maps <- vector("list", length(profiles_by_stratum))
      names(maps) <- names(profiles_by_stratum)

      for (j in seq_along(profiles_by_stratum)) {
        src <- profiles_by_stratum[[j]]
        tgt <- perm_mats[[j]][combo_grid[i, j], , drop = TRUE]
        maps[[j]] <- setNames(as.character(tgt), src)
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
        function(p) setNames(
          sample(p, length(p), replace = FALSE),
          p
        )
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
      profiles_per_stratum = paste(n_per_stratum, collapse = ";"),
      theoretical_permutations = n_possible,
      generated_permutations = nrow(perm_idx),
      strata = paste(strata_vars, collapse = " × "),
      matched_within_profile = paste(match_vars, collapse = " × ")
    )
  )
}

rv_test_restricted <- function(X, Y, perm_idx) {
  obs <- rv_coefficient(X, Y)

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
    p_perm = mean(null >= obs - 1e-12, na.rm = TRUE),
    n_perm = length(null)
  )
}

cor_test_restricted <- function(x, y, perm_idx) {
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
    p_perm = mean(abs(null) >= abs(obs) - 1e-12, na.rm = TRUE),
    n_perm = length(null)
  )
}

axis_tests_restricted <- function(
  X,
  Y,
  perm_idx,
  block,
  adjustment
) {
  X <- as.matrix(X)
  Y <- as.matrix(Y)

  tidyr::expand_grid(
    taxonomy_axis = colnames(X),
    N_axis = colnames(Y)
  ) |>
    purrr::pmap_dfr(
      function(taxonomy_axis, N_axis) {
        cor_test_restricted(
          X[, taxonomy_axis],
          Y[, N_axis],
          perm_idx
        ) |>
          mutate(
            block = block,
            adjustment = adjustment,
            taxonomy_axis = taxonomy_axis,
            N_axis = N_axis,
            .before = 1
          )
      }
    ) |>
    mutate(
      p_FDR = p.adjust(p_perm, method = "BH")
    )
}

# ------------------------------------------------------------------------------
# 3. Design-residual sensitivity
# ------------------------------------------------------------------------------

residualize_design <- function(Y, metadata) {
  needed <- c(
    "Year",
    "Treatment",
    "LandUse",
    "Depth",
    "Block"
  )

  missing <- setdiff(needed, names(metadata))
  if (length(missing) > 0L) {
    stop(
      "Missing design variables: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  design <- metadata |>
    transmute(
      Year = factor(Year),
      Treatment = factor(Treatment),
      LandUse = factor(LandUse),
      Depth = factor(Depth),
      Block = factor(Block)
    )

  Xd <- model.matrix(
    ~ Year * Treatment * LandUse * Depth + Block,
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

  if (is.vector(out)) out <- matrix(out, ncol = 1L)
  colnames(out) <- colnames(Y)

  out
}

# ------------------------------------------------------------------------------
# 4. Primary realised-process block
# ------------------------------------------------------------------------------

process_pc_cols <- get_pc_columns(N_process_pca$scores)

X_process <- process_joined |>
  select(all_of(MCOA_AXES)) |>
  as.matrix()

Y_process <- process_joined |>
  select(all_of(process_pc_cols)) |>
  as.matrix()

perm_obj <- build_profile_permutations(
  process_joined,
  strata_vars = c("LandUse", "Treatment"),
  match_vars = c("Year", "Depth"),
  max_exact = 20000L,
  n_monte_carlo = if (exists("N_RV_PERM")) N_RV_PERM else 9999L,
  seed = 707L
)

perm_idx <- perm_obj$indices

message(
  "Script 07 permutation scheme: ",
  perm_obj$qc$method,
  "; ",
  perm_obj$qc$generated_permutations,
  " whole-profile permutations."
)

raw_process_RV <- rv_test_restricted(
  X_process,
  Y_process,
  perm_idx
) |>
  mutate(
    N_block = "realised_N_processes",
    adjustment = "raw",
    permutation =
      "whole_profile_within_LandUse_x_Treatment_matched_Year_x_Depth",
    .before = 1
  )

X_process_resid <- residualize_design(
  X_process,
  process_joined
)

Y_process_resid <- residualize_design(
  Y_process,
  process_joined
)

adjusted_process_RV <- rv_test_restricted(
  X_process_resid,
  Y_process_resid,
  perm_idx
) |>
  mutate(
    N_block = "realised_N_processes",
    adjustment =
      "residual_Year_x_Treatment_x_LandUse_x_Depth_plus_Block",
    permutation =
      "whole_profile_within_LandUse_x_Treatment_matched_Year_x_Depth",
    .before = 1
  )

axis_cor_process <- bind_rows(
  axis_tests_restricted(
    X_process,
    Y_process,
    perm_idx,
    "realised_N_processes",
    "raw"
  ),
  axis_tests_restricted(
    X_process_resid,
    Y_process_resid,
    perm_idx,
    "realised_N_processes",
    "design_residual_sensitivity"
  )
)

# ------------------------------------------------------------------------------
# 5. Secondary gene-potential block
# ------------------------------------------------------------------------------

gene_pc_cols <- get_pc_columns(N_gene_pca$scores)

X_gene <- gene_joined |>
  select(all_of(MCOA_AXES)) |>
  as.matrix()

Y_gene <- gene_joined |>
  select(all_of(gene_pc_cols)) |>
  as.matrix()

raw_gene_RV <- rv_test_restricted(
  X_gene,
  Y_gene,
  perm_idx
) |>
  mutate(
    N_block = "N_gene_potential",
    adjustment = "raw",
    permutation =
      "whole_profile_within_LandUse_x_Treatment_matched_Year_x_Depth",
    .before = 1
  )

X_gene_resid <- residualize_design(
  X_gene,
  gene_joined
)

Y_gene_resid <- residualize_design(
  Y_gene,
  gene_joined
)

adjusted_gene_RV <- rv_test_restricted(
  X_gene_resid,
  Y_gene_resid,
  perm_idx
) |>
  mutate(
    N_block = "N_gene_potential",
    adjustment =
      "residual_Year_x_Treatment_x_LandUse_x_Depth_plus_Block",
    permutation =
      "whole_profile_within_LandUse_x_Treatment_matched_Year_x_Depth",
    .before = 1
  )

axis_cor_genes <- bind_rows(
  axis_tests_restricted(
    X_gene,
    Y_gene,
    perm_idx,
    "N_gene_potential",
    "raw"
  ),
  axis_tests_restricted(
    X_gene_resid,
    Y_gene_resid,
    perm_idx,
    "N_gene_potential",
    "design_residual_sensitivity"
  )
)

block_RV_summary <- bind_rows(
  raw_process_RV,
  adjusted_process_RV,
  raw_gene_RV,
  adjusted_gene_RV
) |>
  mutate(
    p_FDR_all_blocks = p.adjust(
      p_perm,
      method = "BH"
    )
  )

# ------------------------------------------------------------------------------
# 6. Descriptive co-inertia: taxonomy × realised N processes
# ------------------------------------------------------------------------------

tax_dudi <- ade4::dudi.pca(
  as.data.frame(X_process),
  center = TRUE,
  scale = FALSE,
  scannf = FALSE,
  nf = min(3L, ncol(X_process))
)

process_dudi <- ade4::dudi.pca(
  as.data.frame(Y_process),
  center = TRUE,
  scale = FALSE,
  scannf = FALSE,
  nf = min(3L, ncol(Y_process))
)

coinertia_process <- ade4::coinertia(
  tax_dudi,
  process_dudi,
  scannf = FALSE,
  nf = min(3L, ncol(X_process), ncol(Y_process))
)

coinertia_variance <- coinertia_process$eig / sum(coinertia_process$eig)

coinertia_summary <- tibble(
  axis = paste0("CoIA", seq_along(coinertia_process$eig)),
  eigenvalue = coinertia_process$eig,
  variance = coinertia_variance,
  cumulative_variance = cumsum(coinertia_variance)
)

tax_sample_scores <- as.data.frame(coinertia_process$lX)
N_sample_scores <- as.data.frame(coinertia_process$lY)

colnames(tax_sample_scores) <- paste0(
  "Tax_CoIA",
  seq_len(ncol(tax_sample_scores))
)
colnames(N_sample_scores) <- paste0(
  "N_CoIA",
  seq_len(ncol(N_sample_scores))
)

sample_id_cols <- intersect(
  c(
    "MET", "Number", "Year", "LandUse", "Treatment",
    "Plot", "Depth", "Block", "profile_id", "sample_unit_id"
  ),
  names(process_joined)
)

coinertia_sample_scores <- bind_cols(
  process_joined |>
    select(all_of(sample_id_cols)),
  tax_sample_scores,
  N_sample_scores
)

coinertia_N_variable_scores <- as.data.frame(
  coinertia_process$li
) |>
  rownames_to_column("variable")

coinertia_tax_variable_scores <- as.data.frame(
  coinertia_process$co
) |>
  rownames_to_column("variable")

# ------------------------------------------------------------------------------
# 7. Kingdom-specific RV with realised N processes
# ------------------------------------------------------------------------------

kingdom_process_RV <- NULL

if (!is.null(primary$transformed)) {
  target_MET <- as.character(process_joined$MET)

  kingdom_process_RV <- purrr::imap_dfr(
    primary$transformed,
    function(Xk, marker) {
      Xk <- as.matrix(Xk)

      idx <- match(
        target_MET,
        rownames(Xk)
      )

      if (anyNA(idx)) {
        stop(
          "Could not align all MET IDs for kingdom ",
          marker,
          ".",
          call. = FALSE
        )
      }

      Xk <- Xk[
        idx,
        ,
        drop = FALSE
      ]

      raw_test <- rv_test_restricted(
        Xk,
        Y_process,
        perm_idx
      ) |>
        mutate(
          marker = marker,
          adjustment = "raw",
          permutation =
            "whole_profile_within_LandUse_x_Treatment_matched_Year_x_Depth",
          .before = 1
        )

      Xk_resid <- residualize_design(
        Xk,
        process_joined
      )

      adjusted_test <- rv_test_restricted(
        Xk_resid,
        Y_process_resid,
        perm_idx
      ) |>
        mutate(
          marker = marker,
          adjustment =
            "residual_Year_x_Treatment_x_LandUse_x_Depth_plus_Block",
          permutation =
            "whole_profile_within_LandUse_x_Treatment_matched_Year_x_Depth",
          .before = 1
        )

      bind_rows(
        raw_test,
        adjusted_test
      )
    }
  ) |>
    group_by(adjustment) |>
    mutate(
      p_FDR = p.adjust(
        p_perm,
        method = "BH"
      )
    ) |>
    ungroup()
}

# ------------------------------------------------------------------------------
# 8. Exports
# ------------------------------------------------------------------------------

dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(INT_DIR, recursive = TRUE, showWarnings = FALSE)

write.csv(
  perm_obj$qc,
  file.path(TABLE_DIR, "07_permutation_scheme_QC.csv"),
  row.names = FALSE
)

write.csv(
  block_RV_summary,
  file.path(TABLE_DIR, "07_block_RV_summary.csv"),
  row.names = FALSE
)

write.csv(
  axis_cor_process,
  file.path(TABLE_DIR, "07_axis_correlations_processes.csv"),
  row.names = FALSE
)

write.csv(
  axis_cor_genes,
  file.path(TABLE_DIR, "07_axis_correlations_genes.csv"),
  row.names = FALSE
)

write.csv(
  coinertia_summary,
  file.path(TABLE_DIR, "07_coinertia_process_summary.csv"),
  row.names = FALSE
)

write.csv(
  coinertia_sample_scores,
  file.path(TABLE_DIR, "07_coinertia_process_sample_scores.csv"),
  row.names = FALSE
)

write.csv(
  coinertia_N_variable_scores,
  file.path(TABLE_DIR, "07_coinertia_N_process_variable_scores.csv"),
  row.names = FALSE
)

write.csv(
  coinertia_tax_variable_scores,
  file.path(TABLE_DIR, "07_coinertia_taxonomy_axis_scores.csv"),
  row.names = FALSE
)

if (!is.null(kingdom_process_RV)) {
  write.csv(
    kingdom_process_RV,
    file.path(TABLE_DIR, "07_kingdom_process_RV.csv"),
    row.names = FALSE
  )
}

# Explicitly remove obsolete state-output table if left by an older run.
obsolete_state_csv <- file.path(
  TABLE_DIR,
  "07_axis_correlations_state.csv"
)
if (file.exists(obsolete_state_csv)) {
  file.remove(obsolete_state_csv)
}

analysis_07 <- list(
  settings = list(
    MCOA_axes = MCOA_AXES,
    permutation_QC = perm_obj$qc,
    residual_design =
      "~ Year * Treatment * LandUse * Depth + Block",
    note =
      "Design-residual results are sensitivity analyses, not causal estimates."
  ),
  permutation_indices = perm_idx,
  realised_process = list(
    joined = process_joined,
    X = X_process,
    Y = Y_process,
    X_residual = X_process_resid,
    Y_residual = Y_process_resid,
    raw_RV = raw_process_RV,
    adjusted_RV = adjusted_process_RV,
    axis_correlations = axis_cor_process,
    coinertia = coinertia_process,
    coinertia_summary = coinertia_summary
  ),
  gene_potential = list(
    raw_RV = raw_gene_RV,
    adjusted_RV = adjusted_gene_RV,
    axis_correlations = axis_cor_genes
  ),
  kingdom_process_RV = kingdom_process_RV
)

saveRDS(
  analysis_07,
  file.path(INT_DIR, "07_taxonomy_N_coupling.rds")
)

message("Script 07 completed for the 108 pooled 2022-2024 observations.")
message(
  "Raw taxonomy-process RV = ",
  round(raw_process_RV$RV, 3),
  "; restricted permutation p = ",
  signif(raw_process_RV$p_perm, 4)
)
message(
  "Design-residual sensitivity RV = ",
  round(adjusted_process_RV$RV, 3),
  "; restricted permutation p = ",
  signif(adjusted_process_RV$p_perm, 4)
)
message("Next script: scripts/08_temporal_coupling_decoupling.R")
# ==============================================================================
