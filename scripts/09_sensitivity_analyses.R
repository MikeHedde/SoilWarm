# ==============================================================================
# 09_sensitivity_analyses.R
# ==============================================================================
# AIM
# ---
# Test whether the primary MCOA axes depend on analytical choices likely to be
# challenged by reviewers. Each scenario changes one main decision where possible.
#
# PRIMARY TAXONOMIC RESOLUTION IN v8
# ----------------------------------
# Archaea = Family; Bacteria = Family; Fungi/Nematodes/Protists = Genus.
# Therefore `bacteria_genus` is now an explicit sensitivity scenario.
# `family_all` asks whether using one common Family rank for all five groups
# changes the compromise.
#
# BUG FIX: legacy protist definition
# ----------------------------------
# The legacy filter is now applied to the already aligned broad Protists object,
# subsetting taxonomy AND count columns by feature ID in one operation. This
# prevents the previous 4782-vs-4846 replacement-length mismatch.
#
# ROBUST EXECUTION
# ----------------
# A failed sensitivity scenario no longer aborts all subsequent scenarios. Each
# scenario writes its own RDS; warnings/errors are saved in a status table.
# ============================================================================== 

source("R/config.R")
source("R/functions_soilwarm.R")
require_intermediate(file.path(INT_DIR, "02_raw_markers_broad.rds"), "scripts/02_import_raw_markers.R")
require_intermediate(file.path(INT_DIR, "04_primary_MCOA.rds"), "scripts/04_primary_MCOA.R")

raw_markers_broad <- readRDS(file.path(INT_DIR, "02_raw_markers_broad.rds"))
primary <- readRDS(file.path(INT_DIR, "04_primary_MCOA.rds"))
dir.create(file.path(INT_DIR, "sensitivity_v8"), recursive = TRUE, showWarnings = FALSE)

family_ranks <- c(Archaea="Family", Bacteria="Family", Fungi="Family",
                  Nematodes="Family", Protists="Family")
bacteria_genus_ranks <- PRIMARY_RANKS
bacteria_genus_ranks[["Bacteria"]] <- "Genus"

legacy_markers <- raw_markers_broad
legacy_markers[["Protists"]] <- apply_protist_definition(
  raw_markers_broad[["Protists"]], "legacy_narrow"
)

scenario_specs <- list(
  prevalence_05 = list(prevalence = 0.05),
  prevalence_20 = list(prevalence = 0.20),
  bacteria_genus = list(ranks = bacteria_genus_ranks),
  family_all = list(ranks = family_ranks),
  CLR = list(transform = "clr"),
  lambda1_weight = list(mcoa_weight = "lambda1"),
  uniform_weight = list(mcoa_weight = "uniform"),
  no_decontam = list(contaminant_method = "none"),
  decontam_05 = list(decontam_threshold = 0.50),
  mean_negative_5x = list(contaminant_method = "mean_neg_5x"),
  protists_legacy_narrow = list(raw_markers = legacy_markers)
)

run_scenario <- function(spec) {
  pars <- modifyList(list(
    raw_markers = raw_markers_broad,
    ranks = PRIMARY_RANKS,
    prevalence = PRIMARY_PREVALENCE,
    contaminant_method = "decontam",
    decontam_threshold = PRIMARY_DECONTAM_THRESHOLD,
    transform = PRIMARY_TRANSFORM,
    mcoa_weight = PRIMARY_MCOA_WEIGHT
  ), spec)

  prep <- purrr::imap(pars$raw_markers, function(obj, marker) {
    prepare_marker_table(
      obj = obj,
      rank = pars$ranks[[marker]],
      analysis_years = PRIMARY_YEARS,
      prevalence = pars$prevalence,
      contaminant_method = pars$contaminant_method,
      decontam_threshold = pars$decontam_threshold
    )
  })

  fit_mcoa_from_prepared(
    prep,
    transform = pars$transform,
    mcoa_weight = pars$mcoa_weight,
    nf = PRIMARY_NF
  )
}

alignment <- list()
summary_list <- list()
status_list <- list()

for (nm in names(scenario_specs)) {
  message("Running sensitivity scenario: ", nm)
  scenario_warnings <- character(0)

  obj <- tryCatch(
    withCallingHandlers(
      run_scenario(scenario_specs[[nm]]),
      warning = function(w) {
        scenario_warnings <<- c(scenario_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(obj, "error")) {
    status_list[[nm]] <- tibble(
      scenario = nm,
      status = "ERROR",
      message = conditionMessage(obj),
      warnings = paste(unique(scenario_warnings), collapse = " | ")
    )
    message("  FAILED: ", conditionMessage(obj))
    next
  }

  saveRDS(obj, file.path(INT_DIR, "sensitivity_v8", paste0(nm, ".rds")))
  alignment[[nm]] <- align_axes(primary$scores, obj$scores, k = 3) %>%
    mutate(scenario = nm, .before = 1)
  summary_list[[nm]] <- bind_rows(purrr::map(obj$prep, "summary")) %>%
    mutate(scenario = nm, .before = 1)
  status_list[[nm]] <- tibble(
    scenario = nm,
    status = "OK",
    message = NA_character_,
    warnings = paste(unique(scenario_warnings), collapse = " | ")
  )
}

sensitivity_status <- bind_rows(status_list)
write.csv(sensitivity_status, file.path(TABLE_DIR, "09_sensitivity_status_and_warnings.csv"), row.names = FALSE)

if (length(alignment) > 0L) {
  sensitivity_alignment <- bind_rows(alignment)
  sensitivity_summary <- bind_rows(summary_list)
  write.csv(sensitivity_alignment, file.path(TABLE_DIR, "09_sensitivity_axis_stability.csv"), row.names = FALSE)
  write.csv(sensitivity_summary, file.path(TABLE_DIR, "09_sensitivity_preprocessing_summary.csv"), row.names = FALSE)

  p <- sensitivity_alignment %>%
    ggplot(aes(x = scenario, y = abs_correlation, shape = primary_axis)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.35)) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = NULL, y = "|Correlation| with primary MCOA axis") +
    theme_bw() + theme(axis.text.x = element_text(angle = 50, hjust = 1))
  ggsave(file.path(FIG_DIR, "09_sensitivity_axis_stability.pdf"), p, width = 10, height = 5.5)
}

message("Sensitivity scenarios complete. Check 09_sensitivity_status_and_warnings.csv.")
