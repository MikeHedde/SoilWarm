# ==============================================================================
# 11_mixed_models_MCOA_axes.R
# ==============================================================================
# AIM
# ---
# Test experimental effects on the first three synthetic multi-kingdom axes.
# This stage uses only saved MCOA scores; changing the model never triggers any
# sequence preprocessing.
#
# REPEATED-MEASURES STRUCTURE
# ---------------------------
# profile_id     = LandUse x Treatment x Block (soil profile / experimental unit)
# sample_unit_id = profile_id x Depth (same depth resampled across years)
# We include random intercepts for BOTH levels:
#   (1 | profile_id) + (1 | sample_unit_id)
# This explicitly accounts for correlation among depths within a profile and
# repeated years within each depth-specific sampling unit.
#
# PRIMARY FIXED EFFECTS
# ---------------------
# Year * Treatment * LandUse * Depth
# The Year:Treatment interaction is the warming-response / DiD-type component;
# higher interactions ask whether this response differs by land use or depth.
# ============================================================================== 

source("R/config.R")
source("R/functions_soilwarm.R")
require_intermediate(file.path(INT_DIR, "04_primary_scores_with_metadata.rds"), "scripts/04_primary_MCOA.R")
primary_scores <- readRDS(file.path(INT_DIR, "04_primary_scores_with_metadata.rds"))

axis_names <- intersect(c("MCOA1", "MCOA2", "MCOA3"), names(primary_scores))

fit_axis_model <- function(axis) {
  f <- as.formula(paste0(
    axis,
    " ~ Year * Treatment * LandUse * Depth + (1 | profile_id) + (1 | sample_unit_id)"
  ))
  lmerTest::lmer(f, data = primary_scores, REML = FALSE)
}

axis_models <- setNames(lapply(axis_names, fit_axis_model), axis_names)
saveRDS(axis_models, file.path(INT_DIR, "11_MCOA_axis_mixed_models.rds"))

# Report singular fits explicitly rather than silently simplifying the model.
singularity <- tibble(
  axis = names(axis_models),
  singular = vapply(axis_models, lme4::isSingular, logical(1), tol = 1e-4)
)
write.csv(singularity, file.path(TABLE_DIR, "11_model_singularity_check.csv"), row.names = FALSE)

anova_axes <- purrr::imap_dfr(axis_models, function(mod, axis) {
  an <- as.data.frame(anova(mod, type = 3)) %>% rownames_to_column("term")
  names(an) <- make.names(names(an))
  an %>% mutate(axis = axis, .before = 1)
})
p_col <- grep("Pr.*F", names(anova_axes), value = TRUE)[1]
if (length(p_col) == 1L) anova_axes$p_FDR_global <- p.adjust(anova_axes[[p_col]], method = "BH")
write.csv(anova_axes, file.path(TABLE_DIR, "11_MAIN_MCOA_axis_mixed_model_ANOVA.csv"), row.names = FALSE)

# EMMs and difference-in-differences-style Year x Treatment contrasts.
emm_axes <- purrr::imap_dfr(axis_models, function(mod, axis) {
  emm <- emmeans::emmeans(mod, ~ Year * Treatment | LandUse * Depth)
  as.data.frame(emm) %>% mutate(axis = axis, .before = 1)
})
write.csv(emm_axes, file.path(TABLE_DIR, "11_MCOA_axis_EMMs.csv"), row.names = FALSE)

did_axes <- purrr::imap_dfr(axis_models, function(mod, axis) {
  emm <- emmeans::emmeans(mod, ~ Year * Treatment | LandUse * Depth)
  cc <- emmeans::contrast(emm, interaction = c("revpairwise", "revpairwise"))
  as.data.frame(cc) %>% mutate(axis = axis, .before = 1)
})
write.csv(did_axes, file.path(TABLE_DIR, "11_MAIN_MCOA_axis_DiD_contrasts.csv"), row.names = FALSE)

# Reviewer-facing diagnostics.
for (axis in names(axis_models)) {
  mod <- axis_models[[axis]]
  dd <- tibble(fitted = fitted(mod), residual = resid(mod))
  p1 <- ggplot(dd, aes(fitted, residual)) +
    geom_hline(yintercept = 0, linetype = 2) + geom_point() +
    labs(title = paste(axis, "residuals vs fitted")) + theme_bw()
  p2 <- ggplot(tibble(residual = resid(mod)), aes(sample = residual)) +
    stat_qq() + stat_qq_line() + labs(title = paste(axis, "normal Q-Q")) + theme_bw()
  ggsave(file.path(FIG_DIR, paste0("11_model_diagnostic_", axis, "_resid.pdf")), p1, width = 5, height = 4)
  ggsave(file.path(FIG_DIR, paste0("11_model_diagnostic_", axis, "_qq.pdf")), p2, width = 5, height = 4)
}

message("Mixed models complete. Check 11_model_singularity_check.csv before interpretation.")
