# ==============================================================================
# 06_build_N_function_block.R
# SOILWARM — 3-year nitrogen functional blocks
# ==============================================================================
#
# PRIMARY TEMPORAL SCOPE
# ----------------------
# 2022 + 2023 + 2024, 36 observations per year.
#
# PRIMARY potential-N-activity block:
#   PGNP, PNNP, N2O_reduction, NEA
#
# N2O_reduction is intentionally retained in the primary PCA together with PGNP
# and PNNP. This is a deliberate biological/analytical choice, not an oversight.
# Sensitivity to that inclusion can be assessed later without changing the
# primary specification here.
#
# SECONDARY gene-potential block:
#   AOA, AOB, ComA, ComB, nirS, nirK, nosZ1
#
# ComB has one missing observation. It is retained in the processed table but
# excluded from the primary gene PCA so that all 108 samples remain available.
#
# PNNP_PGNP is supplementary only.
#
# Nitrate and nosZ2 are NOT included because no complete 3-year source has yet
# been supplied. Any stale 2-year N-state outputs are deleted here to prevent
# accidental reuse.
#
# TRANSFORMATIONS
# ---------------
# potential N activities: log1p, then z-standardisation across all 108 observations
# qPCR genes: log10(x + 1), then z-standardisation across all 108 observations
#
# PCA spaces are fitted ONCE on pooled 2022-2024 data. This is required before
# temporal trajectories or interval changes are calculated.
# ==============================================================================

source("R/config.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)
})

source("R/functions_soilwarm.R")

require_intermediate(
  file.path(INT_DIR, "01b_metadata_final.rds"),
  "scripts/01b_build_metadata_final.R"
)

metadata <- readRDS(file.path(INT_DIR, "01b_metadata_final.rds"))

# ------------------------------------------------------------------------------
# 1. Variables and structural checks
# ------------------------------------------------------------------------------

N_FUNCTION_VARS_3Y <- N_FUNCTION_VARS

# nosZ2 is configured globally but is not available as a complete 3-year source.
N_GENE_VARS_3Y <- setdiff(
  N_GENE_VARS,
  "nosZ2"
)

N_GENE_VARS_PCA <- setdiff(
  N_GENE_VARS_3Y,
  "ComB"
)

N_DERIVED_SUPPLEMENTARY_VARS_3Y <- "PNNP_PGNP"

requested <- unique(c(
  N_FUNCTION_VARS_3Y,
  N_GENE_VARS_3Y,
  N_DERIVED_SUPPLEMENTARY_VARS_3Y
))

missing_vars <- setdiff(requested, names(metadata))
if (length(missing_vars) > 0L) {
  stop(
    "Missing 3-year N variables in metadata_final: ",
    paste(missing_vars, collapse = ", "),
    call. = FALSE
  )
}

if (nrow(metadata) != EXPECTED_PRIMARY_N) {
  stop(
    "Script 06 expects ", EXPECTED_PRIMARY_N, " observations; found ",
    nrow(metadata),
    ".",
    call. = FALSE
  )
}

years <- sort(unique(as.integer(metadata$Year)))
if (!identical(years, sort(PRIMARY_YEARS))) {
  stop(
    "Script 06 years disagree with PRIMARY_YEARS; found: ",
    paste(years, collapse = ", "),
    call. = FALSE
  )
}

if (!all(table(metadata$Year) == EXPECTED_SAMPLES_PER_YEAR)) {
  stop("Expected exactly ", EXPECTED_SAMPLES_PER_YEAR, " observations in each year.", call. = FALSE)
}

# Ensure numeric type without silently replacing non-numeric content.
n_raw <- metadata |>
  mutate(
    across(
      all_of(requested),
      ~ readr::parse_number(
        dplyr::na_if(as.character(.x), "NA")
      )
    )
  )

# ------------------------------------------------------------------------------
# 2. Raw-data QC
# ------------------------------------------------------------------------------

safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
safe_median <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
safe_sd <- function(x) if (sum(!is.na(x)) < 2L) NA_real_ else sd(x, na.rm = TRUE)

variable_qc <- purrr::map_dfr(
  requested,
  function(v) {
    x <- n_raw[[v]]
    tibble(
      variable = v,
      block = case_when(
        v %in% N_FUNCTION_VARS_3Y ~ "potential_N_activity",
        v %in% N_GENE_VARS_3Y ~ "gene_potential",
        v %in% N_DERIVED_SUPPLEMENTARY_VARS_3Y ~ "derived_supplementary",
        TRUE ~ "other"
      ),
      n_total = length(x),
      n_nonmissing = sum(!is.na(x)),
      n_missing = sum(is.na(x)),
      n_infinite = sum(is.infinite(x)),
      n_unique_nonmissing = n_distinct(x[!is.na(x)]),
      min = safe_min(x),
      median = safe_median(x),
      max = safe_max(x),
      sd = safe_sd(x)
    )
  }
)

temporal_availability <- n_raw |>
  group_by(Year) |>
  summarise(
    across(
      all_of(c(N_FUNCTION_VARS_3Y, N_GENE_VARS_3Y)),
      ~ sum(!is.na(.x))
    ),
    .groups = "drop"
  )

# Explicit QC for samples with simultaneous zero qPCR values. A zero may be a
# genuine non-detection, but a sample that is zero for many/all functional genes
# can dominate the log10(x+1) + z-standardised PCA and must therefore remain
# visible in the audit trail.
gene_zero_pattern_qc <- n_raw |>
  mutate(
    n_gene_zero = rowSums(
      across(
        all_of(N_GENE_VARS_3Y),
        ~ !is.na(.x) & .x == 0
      )
    )
  ) |>
  filter(n_gene_zero >= 2L) |>
  select(
    any_of(c(
      "Number", "MET", "Year", "LandUse", "LandUseTrajectory", "LandUseState",
      "Treatment", "Plot", "Block", "Depth", "profile_id", "sample_unit_id",
      "DNA_concentration"
    )),
    all_of(N_GENE_VARS_3Y),
    n_gene_zero
  )

if (nrow(gene_zero_pattern_qc) > 0L) {
  all_zero <- gene_zero_pattern_qc |>
    filter(n_gene_zero == length(N_GENE_VARS_3Y))

  if (nrow(all_zero) > 0L) {
    warning(
      "At least one sample is zero for every available qPCR functional-gene marker: ",
      paste(all_zero$MET, collapse = ", "),
      ". See 06_N_gene_zero_pattern_QC.csv. Values are retained pending laboratory/QC review.",
      call. = FALSE
    )
  }
}

# Processes must be non-negative before log1p.
negative_process <- purrr::map_dfr(
  N_FUNCTION_VARS_3Y,
  function(v) {
    tibble(
      variable = v,
      n_negative = sum(n_raw[[v]] < 0, na.rm = TRUE),
      min = min(n_raw[[v]], na.rm = TRUE)
    )
  }
)

if (any(negative_process$n_negative > 0L)) {
  stop(
    "Negative corrected process values prevent log1p transformation: ",
    paste(
      negative_process$variable[negative_process$n_negative > 0L],
      collapse = ", "
    ),
    call. = FALSE
  )
}

negative_genes <- purrr::map_dfr(
  N_GENE_VARS_3Y,
  function(v) {
    tibble(
      variable = v,
      n_negative = sum(n_raw[[v]] < 0, na.rm = TRUE),
      min = if (all(is.na(n_raw[[v]]))) NA_real_ else min(n_raw[[v]], na.rm = TRUE)
    )
  }
)

if (any(negative_genes$n_negative > 0L)) {
  stop(
    "Negative qPCR values prevent log10(x+1) transformation: ",
    paste(
      negative_genes$variable[negative_genes$n_negative > 0L],
      collapse = ", "
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 3. Explicit missingness policy
# ------------------------------------------------------------------------------

gene_missingness <- tibble(
  variable = N_GENE_VARS_3Y,
  n_missing = vapply(
    n_raw[N_GENE_VARS_3Y],
    function(x) sum(is.na(x)),
    integer(1)
  )
)

unexpected_gene_missing <- gene_missingness |>
  filter(
    n_missing > 0L,
    variable != "ComB"
  )

if (nrow(unexpected_gene_missing) > 0L) {
  stop(
    "Unexpected missing values in the 3-year gene block: ",
    paste(unexpected_gene_missing$variable, collapse = ", "),
    call. = FALSE
  )
}

comb_missing_qc <- n_raw |>
  filter(is.na(ComB)) |>
  select(
    any_of(c(
      "Number", "MET", "Year", "LandUse", "Treatment",
      "Plot", "Block", "Depth", "profile_id", "sample_unit_id", "ComB"
    ))
  )

if (nrow(comb_missing_qc) != 1L) {
  stop(
    "Expected exactly one missing ComB observation; found ",
    nrow(comb_missing_qc),
    ".",
    call. = FALSE
  )
}

gene_inclusion <- gene_missingness |>
  mutate(
    included_primary_PCA = variable %in% N_GENE_VARS_PCA,
    reason_excluded = case_when(
      variable == "ComB" ~
        "One missing observation; retained in data but excluded from primary gene PCA.",
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------------------------
# 4. Transform + standardise
# ------------------------------------------------------------------------------

z_scale_vars <- function(df, vars) {
  for (v in vars) {
    x <- df[[v]]
    sx <- sd(x, na.rm = TRUE)
    mu <- mean(x, na.rm = TRUE)

    if (!is.finite(sx) || sx == 0) {
      stop(
        "Variable '", v,
        "' has zero or undefined variance after transformation.",
        call. = FALSE
      )
    }

    df[[v]] <- (x - mu) / sx
  }
  df
}

n_processed <- n_raw

# Potential N activities: log1p + z.
n_processed <- n_processed |>
  mutate(
    across(
      all_of(N_FUNCTION_VARS_3Y),
      log1p
    )
  )

n_processed <- z_scale_vars(
  n_processed,
  N_FUNCTION_VARS_3Y
)

# Gene abundances: log10(x + 1) + z.
n_processed <- n_processed |>
  mutate(
    across(
      all_of(N_GENE_VARS_3Y),
      ~ log10(.x + 1)
    )
  )

n_processed <- z_scale_vars(
  n_processed,
  N_GENE_VARS_3Y
)

check_complete <- function(df, vars, block_name) {
  bad <- vapply(
    df[vars],
    function(x) sum(is.na(x) | is.infinite(x)),
    integer(1)
  )

  if (any(bad > 0L)) {
    stop(
      block_name,
      " contains missing/infinite values in: ",
      paste(names(bad)[bad > 0L], collapse = ", "),
      call. = FALSE
    )
  }
}

check_complete(
  n_processed,
  N_FUNCTION_VARS_3Y,
  "Primary potential-N-activity block"
)
check_complete(
  n_processed,
  N_GENE_VARS_PCA,
  "Primary gene-potential block"
)

# ------------------------------------------------------------------------------
# 5. PCA helper and common 3-year factor spaces
# ------------------------------------------------------------------------------

run_pca_block_06 <- function(df, vars) {
  X <- as.matrix(df[, vars, drop = FALSE])
  storage.mode(X) <- "double"

  pca <- stats::prcomp(
    X,
    center = FALSE,
    scale. = FALSE
  )

  scores <- as.data.frame(pca$x)
  names(scores) <- paste0("PC", seq_len(ncol(scores)))

  loadings <- as.data.frame(pca$rotation)
  names(loadings) <- paste0("PC", seq_len(ncol(loadings)))

  variance <- (pca$sdev ^ 2) / sum(pca$sdev ^ 2)

  list(
    variables = vars,
    model = pca,
    scores = scores,
    loadings = loadings,
    variance = variance
  )
}

N_process_pca <- run_pca_block_06(
  n_processed,
  N_FUNCTION_VARS_3Y
)

N_gene_pca <- run_pca_block_06(
  n_processed,
  N_GENE_VARS_PCA
)

N_global_vars_complete <- c(
  N_FUNCTION_VARS_3Y,
  N_GENE_VARS_PCA
)

N_global_pca <- run_pca_block_06(
  n_processed,
  N_global_vars_complete
)

id_candidates <- c(
  "Number", "MET", "Year", "LandUse", "Treatment",
  "Plot", "Block", "Depth", "profile_id", "sample_unit_id"
)

sample_ids <- n_processed |>
  select(all_of(intersect(id_candidates, names(n_processed))))

N_process_pca$scores <- bind_cols(
  sample_ids,
  N_process_pca$scores
)

N_gene_pca$scores <- bind_cols(
  sample_ids,
  N_gene_pca$scores
)

N_global_pca$scores <- bind_cols(
  sample_ids,
  N_global_pca$scores
)

# ------------------------------------------------------------------------------
# 6. Save canonical intermediate objects
# ------------------------------------------------------------------------------

dir.create(INT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

# Processed temporal table: individual-process deltas in script 08 are calculated
# from these exact transformed/scaled variables.
saveRDS(
  n_processed,
  file.path(INT_DIR, "06_N_temporal_block.rds")
)

saveRDS(
  n_raw,
  file.path(INT_DIR, "06_N_clean_raw.rds")
)

saveRDS(
  N_GENE_VARS_PCA,
  file.path(INT_DIR, "06_N_gene_variables_primary.rds")
)

saveRDS(
  N_process_pca,
  file.path(INT_DIR, "06_N_PCA_processes.rds")
)

saveRDS(
  N_gene_pca,
  file.path(INT_DIR, "06_N_PCA_genes.rds")
)

saveRDS(
  N_global_pca,
  file.path(INT_DIR, "06_N_PCA_all_complete.rds")
)

# Prevent accidental reuse of obsolete 2-year nitrate/state objects.
stale_state_files <- c(
  file.path(INT_DIR, "06_N_PCA_state.rds"),
  file.path(TABLE_DIR, "06_N_state_PCA_variance.csv"),
  file.path(TABLE_DIR, "06_N_state_PCA_loadings.csv"),
  file.path(TABLE_DIR, "06_N_state_PCA_scores.csv")
)

invisible(
  file.remove(
    stale_state_files[file.exists(stale_state_files)]
  )
)

# ------------------------------------------------------------------------------
# 7. Exports
# ------------------------------------------------------------------------------

write.csv(
  variable_qc,
  file.path(TABLE_DIR, "06_N_variable_QC.csv"),
  row.names = FALSE
)

write.csv(
  temporal_availability,
  file.path(TABLE_DIR, "06_N_temporal_availability.csv"),
  row.names = FALSE
)

write.csv(
  gene_zero_pattern_qc,
  file.path(TABLE_DIR, "06_N_gene_zero_pattern_QC.csv"),
  row.names = FALSE
)

write.csv(
  gene_inclusion,
  file.path(TABLE_DIR, "06_N_gene_PCA_inclusion.csv"),
  row.names = FALSE
)

write.csv(
  comb_missing_qc,
  file.path(TABLE_DIR, "06_ComB_missing_observation.csv"),
  row.names = FALSE
)

write.csv(
  negative_process,
  file.path(TABLE_DIR, "06_N_process_negative_value_QC.csv"),
  row.names = FALSE
)

transformation_table <- tibble(
  block = c("potential_N_activities", "gene_potential"),
  variables = c(
    paste(N_FUNCTION_VARS_3Y, collapse = ";"),
    paste(N_GENE_VARS_3Y, collapse = ";")
  ),
  transformation = c(
    "log1p then z-standardisation across pooled 2022-2024",
    "log10(x+1) then z-standardisation across pooled 2022-2024"
  ),
  primary_PCA_variables = c(
    paste(N_FUNCTION_VARS_3Y, collapse = ";"),
    paste(N_GENE_VARS_PCA, collapse = ";")
  )
)

write.csv(
  transformation_table,
  file.path(TABLE_DIR, "06_N_transformations.csv"),
  row.names = FALSE
)

make_variance_table <- function(pca_object) {
  tibble(
    axis = paste0("PC", seq_along(pca_object$variance)),
    variance = pca_object$variance,
    cumulative_variance = cumsum(pca_object$variance)
  )
}

write.csv(
  make_variance_table(N_process_pca),
  file.path(TABLE_DIR, "06_N_process_PCA_variance.csv"),
  row.names = FALSE
)

write.csv(
  tibble::rownames_to_column(
    N_process_pca$loadings,
    "process"
  ),
  file.path(TABLE_DIR, "06_N_process_PCA_loadings.csv"),
  row.names = FALSE
)

write.csv(
  make_variance_table(N_gene_pca),
  file.path(TABLE_DIR, "06_N_gene_PCA_variance.csv"),
  row.names = FALSE
)

write.csv(
  tibble::rownames_to_column(
    N_gene_pca$loadings,
    "gene"
  ),
  file.path(TABLE_DIR, "06_N_gene_PCA_loadings.csv"),
  row.names = FALSE
)

write.csv(
  N_process_pca$scores,
  file.path(TABLE_DIR, "06_N_process_PCA_scores.csv"),
  row.names = FALSE
)

write.csv(
  N_gene_pca$scores,
  file.path(TABLE_DIR, "06_N_gene_PCA_scores.csv"),
  row.names = FALSE
)

write.csv(
  n_processed,
  file.path(TABLE_DIR, "06_N_processed_table.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# 8. Final sanity checks
# ------------------------------------------------------------------------------

stopifnot(
  nrow(N_process_pca$scores) == EXPECTED_PRIMARY_N,
  nrow(N_gene_pca$scores) == EXPECTED_PRIMARY_N,
  nrow(n_processed) == EXPECTED_PRIMARY_N
)

message("Script 06 completed for pooled 2022-2024 data.")
message(
  "Primary process PCA: ",
  length(N_FUNCTION_VARS_3Y),
  " variables / ", EXPECTED_PRIMARY_N, " observations; log1p + z."
)
message(
  "Primary gene PCA: ",
  length(N_GENE_VARS_PCA),
  " variables / ", EXPECTED_PRIMARY_N, " observations; ComB retained in data but excluded from PCA."
)
if (nrow(gene_zero_pattern_qc) > 0L) {
  message(
    "Gene-potential QC: ", nrow(gene_zero_pattern_qc),
    " sample(s) have >=2 simultaneous zero qPCR markers; see 06_N_gene_zero_pattern_QC.csv."
  )
}
message("No Nitrate/nosZ2 block is created in this 3-year workflow.")
message("Next script: scripts/07_taxonomy_N_coupling.R")
# ==============================================================================
