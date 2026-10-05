# ==============================================================================
# 10_structural_robustness.R
# ==============================================================================
# AIM
# ---
# Ask whether the compromise is dominated by one kingdom or by a small subset of
# taxa. These analyses are particularly useful as supplementary reviewer checks.
#
# PART A: leave-one-kingdom-out MCOA.
# PART B: taxon bootstrap within each kingdom (optional; controlled in config.R).
# ============================================================================== 

source("R/config.R")
source("R/functions_soilwarm.R")
require_intermediate(file.path(INT_DIR, "04_primary_MCOA.rds"), "scripts/04_primary_MCOA.R")
primary <- readRDS(file.path(INT_DIR, "04_primary_MCOA.rds"))

loo_results <- purrr::map_dfr(names(primary$transformed), function(drop_marker) {
  keep_tables <- primary$transformed[names(primary$transformed) != drop_marker]
  shared <- rownames(keep_tables[[1]])
  kta <- ade4::ktab.list.df(keep_tables, rownames = shared, tabnames = names(keep_tables),
                            w.row = rep(1 / length(shared), length(shared)))
  fit <- ade4::mcoa(kta, option = PRIMARY_MCOA_WEIGHT, scannf = FALSE, nf = PRIMARY_NF)
  sc <- as.data.frame(fit$SynVar)
  rownames(sc) <- shared
  names(sc) <- paste0("MCOA", seq_len(ncol(sc)))
  align_axes(primary$scores, sc, k = 3) %>% mutate(removed_kingdom = drop_marker, .before = 1)
})
write.csv(loo_results, file.path(TABLE_DIR, "10_leave_one_kingdom_out_stability.csv"), row.names = FALSE)

bootstrap_taxa_stability <- function(primary_obj, B = 200L, seed = 3001L) {
  set.seed(seed)
  markers <- names(primary_obj$transformed)
  ref <- primary_obj$scores
  out <- vector("list", B)
  for (b in seq_len(B)) {
    boot_tables <- lapply(primary_obj$transformed, function(X) {
      X <- as.data.frame(X)
      idx <- sample(seq_len(ncol(X)), ncol(X), replace = TRUE)
      Y <- X[, idx, drop = FALSE]
      names(Y) <- make.unique(names(Y))
      Y
    })
    kta <- ade4::ktab.list.df(boot_tables, rownames = rownames(boot_tables[[1]]),
                              tabnames = markers,
                              w.row = rep(1 / nrow(boot_tables[[1]]), nrow(boot_tables[[1]])))
    fit <- ade4::mcoa(kta, option = PRIMARY_MCOA_WEIGHT, scannf = FALSE, nf = 3)
    sc <- as.data.frame(fit$SynVar)
    rownames(sc) <- rownames(ref)
    out[[b]] <- align_axes(ref, sc, k = 3) %>% mutate(bootstrap = b, .before = 1)
  }
  bind_rows(out)
}

if (RUN_TAXON_BOOTSTRAP) {
  boot <- bootstrap_taxa_stability(primary, B = N_TAXON_BOOT)
  saveRDS(boot, file.path(INT_DIR, "10_taxon_bootstrap.rds"))
  write.csv(boot, file.path(TABLE_DIR, "10_taxon_bootstrap_axis_stability.csv"), row.names = FALSE)
  boot_summary <- boot %>% group_by(primary_axis) %>% summarise(
    median_abs_r = median(abs_correlation),
    q025_abs_r = quantile(abs_correlation, 0.025),
    q975_abs_r = quantile(abs_correlation, 0.975), .groups = "drop")
  write.csv(boot_summary, file.path(TABLE_DIR, "10_taxon_bootstrap_summary.csv"), row.names = FALSE)
}

message("Structural robustness analyses complete.")
