# SoilWarm

Analysis workflow for the **SoilWarm / ORE ACBB experiment**, investigating how soil biodiversity, nitrogen-cycle functional potential, and potential nitrogen activities respond through time to **land-use change and experimental warming**.

## Scientific objectives

The project examines whether changes in different levels of soil biological organization occur synchronously under global change.

The analyses distinguish three complementary domains:

1. **Multi-kingdom taxonomic structure**  
   Archaea, Bacteria, Fungi, Nematodes and Protists.

2. **Nitrogen-cycle functional-gene potential**  
   AOA, AOB, ComA, ComB, `nirS`, `nirK` and `nosZ1`.

3. **Potential nitrogen activities**  
   PGNP, PNNP, N2O reduction and NEA.

The main questions are:

- Do taxonomic structure, N-cycle functional potential and potential N activities change at the same time?
- Are temporal changes in these domains multivariately coupled?
- Does conversion from temporary pasture to cropland modify these trajectories relative to stable permanent pasture?
- Does experimental warming modify these responses, either independently or through an interaction with land-use change?

## Experimental design

The primary analysis covers **2022–2024** and includes:

- 2 land-use trajectories:
  - `Stable PP`: permanent pasture maintained throughout the study
  - `TP_to_Crop`: temporary pasture converted to cropland
- 2 temperature treatments:
  - Control
  - Heated
- 3 experimental blocks
- 3 soil depths:
  - 0–15 cm
  - 15–30 cm
  - 30–50 cm
- 3 sampling years:
  - 2022
  - 2023
  - 2024

This results in **108 observations** in the primary dataset.

The experimental profile defined by land use × warming × block is treated as the independent unit for permutation-based inference, with the three depths kept together whenever appropriate.

## Analytical workflow

### Community composition

Sequence features are screened for contamination before taxonomic aggregation.

Taxonomic resolution used in the primary analysis:

- Archaea: family
- Bacteria: family
- Fungi: genus
- Nematodes: genus
- Protists: genus

Taxa are filtered by prevalence and community tables are Hellinger-transformed.

Multi-kingdom community structure is summarized using **Multiple Co-Inertia Analysis (MCOA)**.

### N-cycle functional-gene potential

Gene abundances are log-transformed and standardized before PCA.

The multivariate gene-potential block currently includes:

- AOA
- AOB
- ComA
- `nirS`
- `nirK`
- `nosZ1`

`ComB` is retained in gene-specific analyses but excluded from the primary PCA because of one missing observation.

### Potential N activities

The activity block includes:

- PGNP
- PNNP
- N2O reduction
- NEA

Variables are transformed and standardized before PCA.

`N2O_reduction` is intentionally retained in the primary activity block.

## Main statistical analyses

The workflow includes:

- pooled three-year MCOA and PCA ordinations;
- RV coefficients for cross-domain covariation;
- temporal change vectors for 2022–2023 and 2023–2024;
- comparison of the temporal allocation of change among biological domains;
- exact profile-level sign-flip tests;
- land-use difference-in-change contrasts:
  
  `TP_to_Crop − Stable PP`

- warming contrasts:
  
  `Heated − Control`

- land-use × warming interaction contrasts;
- process- and gene-specific temporal analyses;
- FDR correction for families of related tests.

The principal temporal result is evaluated independently from multivariate coupling: two domains may change predominantly during the same period without necessarily changing in the same multivariate direction.

## Repository structure

```text
SoilWarm/
│
├── README.md
├── SoilWarm.Rproj
│
├── R/
│   ├── config.R
│   ├── functions.R
│   └── RUN.R
│
├── scripts/
│   ├── 01_...
│   ├── 02_...
│   └── ...
│
├── data/
│   ├── raw/
│   └── processed/
│
├── results/
│   ├── figures_main/
│   ├── tables/
│   └── supplementary/
│
└── docs/
```

File names and exact subdirectories may evolve while the analysis is under development.

## Running the analysis

Open `SoilWarm.Rproj` in RStudio and run:

```r
source("R/RUN.R")
```

Individual scripts can also be executed separately when developing or checking specific parts of the workflow.

For example:

```r
source("scripts/09c_Figure2_temporal_decoupling.R")
source("scripts/09d_Figure3_global_change_drivers.R")
```

## Main outputs

The current manuscript-oriented outputs include:

- temporal partitioning of change across taxonomic, functional-gene and N-activity domains;
- multivariate effects of land-use conversion and experimental warming;
- process-specific N-activity responses;
- functional-gene timing analyses;
- supplementary cross-domain coupling and interaction analyses.

Main figures are written to:

```text
results/figures_main/
```

and statistical summaries to:

```text
results/tables/
```

## Reproducibility

All analyses are performed in R.

Package versions should be recorded using `renv`:

```r
install.packages("renv")
renv::init()
renv::snapshot()
```

A future clone of the repository can then restore the package environment with:

```r
renv::restore()
```

## Data

Raw experimental and molecular data are not intended to be modified directly by analysis scripts.

Large, sensitive, or non-public raw datasets should remain outside version control and be listed in `.gitignore`.

Processed datasets required for full reproducibility may be archived separately when the associated manuscript is published.

## Status

**Work in progress.**

The primary analytical workflow is currently stabilized for the 2022–2024 dataset. Sensitivity analyses and manuscript preparation are ongoing.

## Citation

A manuscript based on this analysis is currently in preparation.

Citation information and links to archived data and code will be added upon publication.

## Contact

**Mickael Hedde**  
INRAE, France
