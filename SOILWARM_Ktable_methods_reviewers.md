# SOILWARM multi-kingdom K-table / MCOA — methodological note

## Primary objective

The analysis is designed to derive **sample-level synthetic multi-kingdom community scores** from five metabarcoding tables (Archaea, Bacteria, Fungi, Nematodes, Protists). The synthetic scores are intended for integration into the SOILWARM experimental models, not as replacements for kingdom-specific community analyses.

The primary multiblock method is **Multiple Co-Inertia Analysis (MCOA)** because the five biological tables share the same samples (rows) but have different taxonomic variables and dimensions (columns). MCOA explicitly searches for sample configurations that are common across the K tables and returns synthetic sample scores (`SynVar`).

## Data audit completed before writing the R workflow

The supplied sequencing matrices contain 111 `MET` libraries (`MET250125`–`MET250235`). The plate workbook resolves them into 108 biological samples (36 each in 2022, 2023 and 2024) plus three extraction-negative controls in 2024. The extraction controls are `MET250200`, `MET250204`, and `MET250208`. In addition, each marker table contains four technical blank columns.

The supplied `final_metadata.xlsx` contains 72 observations, corresponding to the 36 biological samples from 2022 and 36 from 2024. These 72 rows can be matched one-to-one to the sequencing libraries using land use, treatment, depth and replicate.

A critical coding point is that the plate workbook uses **C = Chauffage** and **T = Témoin**, whereas `final_metadata.xlsx` uses `Heated` and `Control`. The script handles this translation explicitly and stops if the 72 expected one-to-one matches are not obtained.

A preliminary audit of the raw tables also showed that taxonomic completeness differs strongly among markers. This is why the script reports the fraction of post-decontamination reads retained at the focal taxonomic rank for every sample rather than silently dropping unassigned reads.

## Primary preprocessing choices

### 1. Contamination

Contaminants are classified at the ASV/sequence-feature level using `decontam::isContaminant(..., method = "prevalence")` with the seven available negative controls (four technical blanks plus three extraction blanks). The primary threshold is the package default-style value `0.10`. An aggressive threshold (`0.50`), no decontamination, and a transparent mean-negative/mean-positive heuristic are run as sensitivity analyses.

This is preferable to the initial workflow's comparison of summed reads from four blanks against summed reads from 111 libraries, because the latter depends strongly on the number of positive libraries.

### 2. Taxonomic aggregation

Decontamination is performed **before** aggregation. ASVs are then aggregated by summing reads belonging to the same focal taxon. The primary ranks are:

- Archaea: Family
- Bacteria: Genus
- Fungi: Genus
- Nematodes: Genus
- Protists: Genus

This corrects the initial script, which selected ASVs that had a genus/family label but did not actually sum ASVs within genera/families.

A reviewer-oriented sensitivity analysis repeats the complete workflow at a **common Family rank for all five kingdoms**. This directly tests whether the mixed taxonomic resolution of the primary analysis materially affects the synthetic gradients.

### 3. Prevalence

Taxon prevalence is calculated **after taxonomic aggregation**, not at ASV level. The primary threshold is 10% of biological samples. Sensitivity analyses use 5% and 20%.

This order avoids eliminating rare ASVs that collectively belong to a common genus/family.

### 4. Transformation

The primary community transformation is relative abundance followed by a **Hellinger transformation**. Hellinger-transformed variables are column-centred but not standardized to unit variance. This retains abundance structure while making Euclidean geometry appropriate for ecological community data and avoids giving rare taxa disproportionate leverage.

A centered-log-ratio (CLR) analysis with a small pseudocount is included as a sensitivity analysis because amplicon data are compositional. The CLR solution is not treated as intrinsically superior; its purpose here is to demonstrate whether the multi-kingdom synthetic structure is robust to a compositionally motivated alternative transformation.

### 5. MCOA table weighting

The primary MCOA uses `option = "inertia"`, which weights each table by the inverse of its total inertia. This is appropriate here because the kingdoms have very different numbers of retained taxa, and the objective is not to let the largest table dominate simply because it has more columns.

`lambda1` and `uniform` weighting are included as sensitivity analyses.

## Protist definition

The original script excluded several oomycete/stramenopile orders under a comment indicating fungal affiliation. Oomycetes are not fungi. Therefore, the primary workflow uses a broad protist definition based on the retained PR2 eukaryotic supergroups while excluding Opisthokonta/Fungi/Metazoa and ambiguous high-level assignments. The original narrower exclusion is reproduced as a sensitivity scenario so that its effect can be quantified rather than silently discarded.

## Reviewer-oriented robustness analyses generated by the script

1. **Pairwise RV coefficients** among the five transformed kingdom tables, each with a permutation test and BH correction. This asks whether there is demonstrable common structure before interpreting an MCOA compromise.
2. **Prevalence sensitivity**: 5%, 10%, 20%.
3. **Taxonomic-rank sensitivity**: primary mixed ranks versus Family for every kingdom.
4. **Transformation sensitivity**: Hellinger versus CLR.
5. **MCOA weighting sensitivity**: inertia, lambda1, uniform.
6. **Contamination sensitivity**: primary decontam, aggressive decontam, no decontamination, alternative negative-control heuristic.
7. **Protist-definition sensitivity**: broad primary versus original narrow filter.
8. **Leave-one-kingdom-out analysis**: rerun MCOA after removing each kingdom in turn; align axes and quantify correlations with the primary synthetic axes.
9. **Taxon bootstrap**: resample taxa within each kingdom and re-estimate the MCOA; report the distribution of correlations of the first three synthetic axes with the primary solution.
10. **PERMDISP** by kingdom to detect heterogeneous multivariate dispersion.
11. **Repeated-measures PERMANOVA focused on temporal terms**, with permutations restricted within the same sample unit (LandUse × Treatment × Block × Depth). This avoids free permutation of repeated observations.
12. **Bray–Curtis temporal turnover (2022→2024)** for each repeated sample unit, analysed as a complementary magnitude-of-change response.
13. **Sequencing depth and taxonomic assignment diagnostics**, exported at sample level.
14. **All-sequenced-years sensitivity (2022–2024)**: the community MCOA is rerun including 2023 and then aligned to the primary 2022/2024 solution. This tests whether excluding 2023 because of missing LEM metadata materially changes the synthetic gradients.

## Inferential use of the synthetic axes

The first three MCOA synthetic scores are modelled with repeated-measures mixed models:

`MCOA axis ~ Year * Treatment * LandUse * Depth + (1 | profile_id)`

where `profile_id = LandUse × Treatment × Block`. The script exports Type-III tests, estimated marginal means and Year × Treatment interaction contrasts (difference-in-differences interpretation) within LandUse × Depth.

The MCOA axes should be interpreted as **derived multivariate responses**. They should not be presented as universal biodiversity indices unless their taxonomic loadings, kingdom contributions, stability analyses and biological interpretation support such wording.

## What should appear in the manuscript versus supplements

### Main text / main figures

- concise description of decontamination, aggregation, Hellinger transformation and MCOA;
- number of taxa retained per kingdom after the primary workflow;
- pairwise/common-structure evidence (RV summary);
- MCOA scree plot and sample score figure;
- interpretation of the first biologically interpretable synthetic axes;
- experimental mixed-model results on those axes.

### Supplementary material

- complete library-size QC;
- contaminant diagnostics;
- taxonomic assignment fraction by sample and kingdom;
- retained-taxon prevalence tables;
- all sensitivity-analysis axis correlations;
- leave-one-kingdom-out results;
- taxon-bootstrap confidence distributions;
- pairwise RV permutation table;
- PERMDISP and repeated-measures PERMANOVA;
- 2022→2024 Bray turnover analysis;
- all-years (2022–2024) sensitivity;
- complete taxon coordinates/loadings;
- `sessionInfo()` and the full R script.

## Key methodological references

- Davis NM, Proctor DM, Holmes SP, Relman DA, Callahan BJ. 2018. Simple statistical identification and removal of contaminant sequences in marker-gene and metagenomics data. *Microbiome* 6:226. https://doi.org/10.1186/s40168-018-0605-2
- Legendre P, Gallagher ED. 2001. Ecologically meaningful transformations for ordination of species data. *Oecologia* 129:271–280.
- Chessel D, Hanafi M. 1996. Analyses de la co-inertie de K nuages de points. *Revue de Statistique Appliquée* 44:35–60.
- `ade4::mcoa` current documentation: https://adeverse.github.io/ade4/reference/mcoa.html

