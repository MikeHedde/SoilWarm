# ==============================================================================
# 12_kingdom_specific_checks.R
# ==============================================================================
# AIM
# ---
# Provide kingdom-specific multivariate checks so conclusions from synthetic
# MCOA axes are not accepted without checking the underlying communities.
#
# THREE COMPLEMENTARY OUTPUTS
# ---------------------------
# 1. PERMDISP: tests heterogeneity of multivariate dispersion.
# 2. Repeated-measures PERMANOVA: tests temporal terms only, with permutations
#    restricted within sample_unit_id (same depth-specific unit in 2022/2024).
# 3. Bray-Curtis temporal turnover: magnitude of 2022 -> 2024 change per unit.
#
# NOTE
# ----
# The restricted PERMANOVA is supplementary to the mixed models on MCOA axes;
# it is not used to test purely between-unit main effects.
# ============================================================================== 

source("R/config.R")
source("R/functions_soilwarm.R")
require_intermediate(file.path(INT_DIR, "01_sample_map.rds"), "scripts/01_build_sample_map.R")
require_intermediate(file.path(INT_DIR, "04_primary_MCOA.rds"), "scripts/04_primary_MCOA.R")
sample_map <- readRDS(file.path(INT_DIR, "01_sample_map.rds"))
primary <- readRDS(file.path(INT_DIR, "04_primary_MCOA.rds"))

community_checks <- purrr::imap_dfr(primary$count_tables, function(X, marker) {
  md <- sample_map$biological %>% filter(Year %in% PRIMARY_YEARS) %>% slice(match(rownames(X), MET))
  d <- vegan::vegdist(X, method = "bray")
  group <- interaction(md$Year, md$Treatment, md$LandUse, md$Depth, drop = TRUE)
  bd <- vegan::betadisper(d, group)
  bp <- vegan::permutest(bd, permutations = 9999)
  tibble(marker = marker,
         betadisper_F = unname(bp$tab[1, "F"]),
         betadisper_p = unname(bp$tab[1, "Pr(>F)"]))
}) %>% mutate(p_FDR = p.adjust(betadisper_p, method = "BH"))
write.csv(community_checks, file.path(TABLE_DIR, "12_kingdom_PERMDISP_checks.csv"), row.names = FALSE)

repeated_permanova <- purrr::imap_dfr(primary$count_tables, function(X, marker) {
  md <- sample_map$biological %>%
    filter(Year %in% PRIMARY_YEARS) %>%
    slice(match(rownames(X), MET)) %>%
    mutate(Year=factor(Year), Treatment=factor(Treatment), LandUse=factor(LandUse),
           Depth=factor(Depth), sample_unit_id=factor(sample_unit_id))
  d <- vegan::vegdist(X, method = "bray")
  ctrl <- permute::how(nperm = 9999, blocks = md$sample_unit_id)
  fit <- vegan::adonis2(
    d ~ Year + Year:Treatment + Year:LandUse + Year:Depth +
      Year:Treatment:LandUse + Year:Treatment:Depth + Year:LandUse:Depth +
      Year:Treatment:LandUse:Depth,
    data = md, permutations = ctrl, by = "terms"
  )
  as.data.frame(fit) %>% rownames_to_column("term") %>%
    filter(str_detect(term, "Year")) %>% mutate(marker = marker, .before = 1)
})
if ("Pr(>F)" %in% names(repeated_permanova)) {
  repeated_permanova$p_FDR <- p.adjust(repeated_permanova$`Pr(>F)`, method = "BH")
}
write.csv(repeated_permanova, file.path(TABLE_DIR, "12_repeated_measures_PERMANOVA_temporal_terms.csv"), row.names = FALSE)

turnover_results <- purrr::imap_dfr(primary$count_tables, function(X, marker) {
  md <- sample_map$biological %>% filter(Year %in% PRIMARY_YEARS) %>% slice(match(rownames(X), MET))
  dat <- md %>% select(MET, Year, Treatment, LandUse, Depth, Block, sample_unit_id)
  purrr::map_dfr(unique(dat$sample_unit_id), function(u) {
    ii <- dat$sample_unit_id == u
    ids <- dat$MET[ii]; yrs <- dat$Year[ii]
    if (!setequal(yrs, PRIMARY_YEARS) || length(ids) != 2L) return(NULL)
    xx <- X[ids, , drop = FALSE]
    tibble(marker=marker, sample_unit_id=u, Treatment=dat$Treatment[ii][1],
           LandUse=dat$LandUse[ii][1], Depth=dat$Depth[ii][1], Block=dat$Block[ii][1],
           bray_turnover_2022_2024=as.numeric(vegan::vegdist(xx, method="bray")))
  })
})
write.csv(turnover_results, file.path(TABLE_DIR, "12_Bray_temporal_turnover.csv"), row.names = FALSE)

turnover_models <- split(turnover_results, turnover_results$marker) |>
  lapply(function(dd) lm(bray_turnover_2022_2024 ~ Treatment * LandUse * Depth + factor(Block), data = dd))
turnover_anova <- purrr::imap_dfr(turnover_models, function(mod, marker) {
  as.data.frame(anova(mod)) %>% rownames_to_column("term") %>% mutate(marker = marker, .before = 1)
})
write.csv(turnover_anova, file.path(TABLE_DIR, "12_Bray_temporal_turnover_ANOVA.csv"), row.names = FALSE)

message("Kingdom-specific checks complete.")
