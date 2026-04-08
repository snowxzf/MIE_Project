# MIE 286 Line Tracer — analysis code

R workflow for a **paired** design: each participant has **numerical** vs **spatial-colour** feedback. Outcomes are **completion time** (seconds) and **accuracy** (area off target in px²; lower is better).

Run scripts from this directory (`code/`) so paths to `data_mie286.R` and helpers resolve correctly. You can open **`MIE_Project.Rproj`** in RStudio to set the working directory automatically.

## Requirements

```r
install.packages(c("ggplot2", "tidyr", "dplyr", "patchwork", "nortest"))
```

**ggplot2**, **tidyr**, **dplyr**, **patchwork**, and **nortest** (Lilliefors / K–S-type normality checks) are required for the main pipeline.

## Quick start

1. Ensure **`data_mie286.R`** exists (see **Regenerating the data file** below if not).
2. **Full sample** (figures in `graphs/`, includes full vs outlier-restricted sensitivity when enabled):

   ```bash
   Rscript analysis_mie286.R
   ```

3. **Outlier-excluded pipeline** — same statistics and figures on the restricted **N** after IQR / \|*z*\|\>3 screening (figures in `graphs_no_outliers/`):

   ```bash
   Rscript analysis_mie286_no_outliers.R
   ```

4. **Q–Q plots only (outlier sample)** — fast rerun of normality figures for the filtered dataset; writes clearly named PNGs under `graphs_no_outliers/` (`qq_outliers_main_2x2.png`, `qq_outliers_by_gender.png`, `qq_outliers_by_gaming.png`):

   ```bash
   Rscript mie286_qq_outliers.R
   ```

Console output includes descriptives, **Shapiro–Wilk** (and Lilliefors where computed), paired *t*-tests, Pearson/Spearman correlations, and gender / gaming stratification when demographics are present.

## Regenerating the data file

Trial summaries live in **`export/trial_metrics_summary.csv`**. **`build_mie286_vectors.R`** pairs numerical vs spatial-colour by participant and writes **`data_mie286.R`** (named vectors for the analysis).

```bash
Rscript build_mie286_vectors.R
```

Optional fields (e.g. gender, gaming hours) can be merged via your process; missing columns are handled as `NA`.

## How the analysis is wired

| File | Role |
|------|------|
| `analysis_mie286.R` | Entry point: full sample; runs pipeline with sensitivity compare on by default. |
| `analysis_mie286_no_outliers.R` | Drops outlier-flagged rows, sets `graphs_no_outliers/`, runs the same pipeline. |
| `mie286_qq_outliers.R` | Optional: load → screen outliers → **only** Q–Q figures (outlier sample). |
| `mie286_load_data_and_active.R` | Packages, `data_mie286.R`, `paired_complete`, `active`, default `graphs/`; sources `mie286_outlier_rules.R`. |
| `mie286_outlier_rules.R` | `mie286_outlier_screen()`: Tukey IQR + \|*z*\|\>3 on levels and paired diffs; **union** flags participants. |
| `mie286_analysis_pipeline.R` | Plots, tests, exports (e.g. `shapiro_wilk_all_strata.csv`). Sensitivity block respects `RUN_SENSITIVITY_COMPARE`. |
| `post_survey_analysis.R` | Standalone ggplot figures from **aggregated** post-survey counts (preference, learning, sentiment). |
| `visualize_data.py` | Optional Python tooling for trial / curve visualizations (separate from the main R pipeline). |

## Outlier rules (short)

A participant is excluded if **either** rule fires on the six screened quantities (four outcome columns and paired `diff_time` / `diff_area`): **IQR** (1.5×IQR tails) or **Z** (\|(*x* − mean)/*sd*\| \> 3), with guards for degenerate *sd*. The whole paired row is removed.

The main script keeps the **full** sample for primary stratified plots; **`analysis_mie286_no_outliers.R`** uses only the **restricted** sample throughout.

## Outputs

| Location | Contents |
|----------|-----------|
| `graphs/` | Figures from **`analysis_mie286.R`** (boxplots, scatters, Q–Q, etc.). May include sensitivity comparison figures when enabled. |
| `graphs_no_outliers/` | Figures from **`analysis_mie286_no_outliers.R`** and from **`mie286_qq_outliers.R`** (including `qq_outliers_*.png`). |
| `graphs/shapiro_wilk_all_strata.csv` | Shapiro–Wilk *W* and *p* (and related fields) for main, gender, gaming, and paired-difference strata (**full** sample). |
| `graphs_no_outliers/shapiro_wilk_all_strata.csv` | Same structure after outlier exclusion (regenerate with the no-outliers script). |

## Paired *t*-tests and *df*

Paired tests use **within-participant** differences. Effective *df* is **n_pairs − 1** for the sample you analyze (31 with **N = 32**, 27 with **N = 28**). Any illustrative *t*-density plot with a fixed *df* in the repo is for teaching / visualization only unless it matches your current **n**.

---

*If you see **could not find `mie286_outlier_screen`**, set the working directory to this **`code/`** folder (or use the `.Rproj`) and run the entry-point scripts from there.*
