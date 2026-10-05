
# ==============================================================================
# R/config.R
# SOILWARM — multi-kingdom taxonomy × nitrogen functioning
# Consolidated configuration
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Reproducibility
# ------------------------------------------------------------------------------

SOILWARM_SEED <- 260930L
set.seed(SOILWARM_SEED)

# ------------------------------------------------------------------------------
# 1. Project directories
# ------------------------------------------------------------------------------

RAW_DIR   <- file.path("data", "raw")
INT_DIR   <- "intermediate"

OUT_DIR   <- "results"
TABLE_DIR <- file.path(OUT_DIR, "tables")
FIG_DIR   <- file.path(OUT_DIR, "figures")
RDS_DIR   <- file.path(OUT_DIR, "rds")

SENS_DIR <- file.path(INT_DIR, "sensitivity_v8")

dir.create(INT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RDS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SENS_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 2. Primary temporal scope
# ------------------------------------------------------------------------------

# Primary analysis uses one common 3-year sample set throughout.
PRIMARY_YEARS <- c(2022L, 2023L, 2024L)

# Kept as an explicit alias because some preprocessing code distinguishes
# community availability from other data blocks.
COMMUNITY_ONLY_YEARS <- PRIMARY_YEARS

# ------------------------------------------------------------------------------
# 3. Primary metabarcoding preprocessing choices
# ------------------------------------------------------------------------------

PRIMARY_PREVALENCE <- 0.10
PRIMARY_DECONTAM_THRESHOLD <- 0.10
SENS_DECONTAM_THRESHOLD <- 0.50

PRIMARY_TRANSFORM <- "hellinger"
PRIMARY_MCOA_WEIGHT <- "inertia"
PRIMARY_NF <- 5L

# ------------------------------------------------------------------------------
# 4. Permutation / bootstrap settings
# ------------------------------------------------------------------------------

N_RV_PERM <- 9999L
RUN_TAXON_BOOTSTRAP <- TRUE
N_TAXON_BOOT <- 200L
N_PERM_TEMPORAL <- N_RV_PERM

# ------------------------------------------------------------------------------
# 5. Raw input files
# ------------------------------------------------------------------------------

FILES <- list(
  
  metadata = file.path(
    RAW_DIR,
    "final_metadata.xlsx"
  ),
  
  plate_map = file.path(
    RAW_DIR,
    "plan de plaque ADN envoyé_correspondance code ADNid.xlsx"
  ),
  
  archaea = list(
    file = file.path(
      RAW_DIR,
      "Archae analyse 2_Silva 16S_AR.xlsx"
    ),
    sheet = "Archaea"
  ),
  
  bacteria = list(
    file = file.path(
      RAW_DIR,
      "Bacteries analyse 2_Silva 16S_AR.xlsx"
    ),
    sheet = "bact"
  ),
  
  fungi = list(
    file = file.path(
      RAW_DIR,
      "Champignons_UNITE_AR.xlsx"
    ),
    sheet = "ITS86"
  ),
  
  nematodes = list(
    file = file.path(
      RAW_DIR,
      "Nematodes_Nemabase_AR.xlsx"
    ),
    sheet = "Nametode_Nemabase"
  ),
  
  protists = list(
    file = file.path(
      RAW_DIR,
      "MicroEuk_PR2_AR.xlsx"
    ),
    sheet = "08-biom2tsv_TAREUK_PR2"
  )
)

# ------------------------------------------------------------------------------
# 6. Marker-specific taxonomy configuration
# ------------------------------------------------------------------------------

MARKER_CONFIG <- list(
  
  Archaea = list(
    key = "archaea",
    tax_names = c(
      "Domain", "Phylum", "Class",
      "Order", "Family", "Genus", "Species"
    ),
    primary_rank = "Family"
  ),
  
  Bacteria = list(
    key = "bacteria",
    tax_names = c(
      "Domain", "Phylum", "Class",
      "Order", "Family", "Genus", "Species"
    ),
    primary_rank = "Family"
  ),
  
  Fungi = list(
    key = "fungi",
    tax_names = c(
      "Kingdom", "Phylum", "Class",
      "Order", "Family", "Genus", "Species"
    ),
    primary_rank = "Genus"
  ),
  
  Nematodes = list(
    key = "nematodes",
    tax_names = c(
      "Domain", "Phylum", "Class",
      "Order", "Family", "Genus", "Species"
    ),
    primary_rank = "Genus"
  ),
  
  Protists = list(
    key = "protists",
    tax_names = c(
      "Domain", "Supergroup", "Major_group",
      "Kingdom", "Phylum", "Class",
      "Order", "Family", "Genus"
    ),
    primary_rank = "Genus"
  )
)

PRIMARY_RANKS <- vapply(
  MARKER_CONFIG,
  function(x) x$primary_rank,
  character(1)
)

# ------------------------------------------------------------------------------
# 7. Taxonomic-rank sensitivity settings
# ------------------------------------------------------------------------------

FAMILY_ALL_RANKS <- c(
  Archaea   = "Family",
  Bacteria  = "Family",
  Fungi     = "Family",
  Nematodes = "Family",
  Protists  = "Family"
)

BACTERIA_GENUS_RANKS <- PRIMARY_RANKS
BACTERIA_GENUS_RANKS["Bacteria"] <- "Genus"

# ------------------------------------------------------------------------------
# 8. Nitrogen-cycle variables available across the primary 3-year analysis
# ------------------------------------------------------------------------------

# PRIMARY functional block: potential N-cycle activities.
# N2O_reduction is intentionally retained together with PGNP and PNNP as a
# biologically interpretable activity metric. This is a deliberate modelling
# choice; any sensitivity to this inclusion will be assessed separately.
N_FUNCTION_VARS <- c(
  "PGNP",
  "PNNP",
  "N2O_reduction",
  "NEA"
)

# SECONDARY mechanistic block: molecular functional potential
N_GENE_VARS <- c(
  "AOA",
  "AOB",
  "ComA",
  "ComB",
  "nirS",
  "nirK",
  "nosZ1",
  "nosZ2"
)

# State variable measured in both years
N_STATE_VARS <- c(
  "Nitrate"
)

# Derived metric retained only as supplementary information
N_DERIVED_SUPPLEMENTARY_VARS <- c(
  "PNNP_PGNP"
)

# ------------------------------------------------------------------------------
# 9. Variables excluded from paired temporal analysis
# ------------------------------------------------------------------------------

N_EXCLUDED_TEMPORAL_VARS <- c(
  "NH4",
  "NO2",
  "NO3"
)

# Kept only for compatibility with older development scripts
N_2024_ONLY_VARS <- character(0)

# ------------------------------------------------------------------------------
# 10. N-block PCA / temporal-coupling settings
# ------------------------------------------------------------------------------

PROCESS_PCA_CUMVAR <- 0.95
GENE_PCA_CUMVAR <- 0.80

TAX_AXES_PRIMARY <- c(
  "MCOA1",
  "MCOA2",
  "MCOA3"
)

# ------------------------------------------------------------------------------
# 11. Expected experimental structure
# ------------------------------------------------------------------------------

EXPECTED_PRIMARY_N <- 108L
EXPECTED_SAMPLES_PER_YEAR <- 36L
EXPECTED_PROFILES <- 12L
EXPECTED_TEMPORAL_PAIRS <- 36L
EXPECTED_BIOLOGICAL_LIBRARIES <- 108L
EXPECTED_EXTRACTION_CONTROLS <- 3L

EXPECTED_MET_IDS <- paste0(
  "MET",
  250125:250235
)

# ------------------------------------------------------------------------------
# 12. Known plate-map source-data correction
# ------------------------------------------------------------------------------

KNOWN_PLATE_MAP_REPAIR <- c(
  "PL2_B9_10" = "MET250225"
)

# ==============================================================================
# END CONFIGURATION
# ==============================================================================
