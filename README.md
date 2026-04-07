# MIE 286 Line Tracer — R analysis code

R workflow for a **paired** design: each participant has **numerical** vs **spatial-color** feedback. Outcomes are **completion time** (seconds) and **accuracy** (area off target in px²; lower is better).

Run everything from this directory (`code/`) so paths to `data_mie286.R` and the helper scripts resolve correctly.

## Requirements

Install once in R:

```r
install.packages(c("ggplot2", "tidyr", "dplyr", "patchwork", "nortest"))
```

## Quick start

1. Ensure `data_mie286.R` exists (see below if you need to rebuild it).
2. Full sample + sensitivity (full vs outlier-restricted comparison):

   ```bash
   Rscript analysis_mie286.R
   ```

3. Optional: repeat the **entire** analysis after dropping outlier-flagged participants (same rules as the sensitivity block; figures go to a separate folder):

   ```bash
   Rscript analysis_mie286_no_outliers.R
   ```

Console output includes descriptive stats, Shapiro–Wilk and Lilliefors normality checks, paired *t*-tests, Pearson/Spearman correlations, plain-language summaries, and gender/gaming stratified plots when demographics are present.

## Regenerating `data_mie286.R`

Trial summaries live in `export/trial_metrics_summary.csv`. The build script parses messy duration strings from filenames, pairs numerical vs spatial-color by participant, and writes **`data_mie286.R`** (named vectors used by the analysis).

```bash
Rscript build_mie286_vectors.R
```

Optional demographics can be filled via your pipeline (e.g. `participant_demographics.csv`); missing columns are handled with `NA`s.

## How the analysis is wired

| File | Role |
|------|------|
| `analysis_mie286.R` | Entry point: full sample, runs sensitivity comparison. |
| `analysis_mie286_no_outliers.R` | Entry point: drop outliers first, then run the same pipeline. |
| `mie286_load_data_and_active.R` | Loads packages, sources `data_mie286.R`, builds `paired_complete` and `active`, sets default `out_dir`, sources **`mie286_outlier_rules.R`** so outlier helpers always exist after load. |
| `mie286_outlier_rules.R` | Tukey IQR (1.5×IQR) and \|*z*\|\>3 on four level variables plus paired differences; **union** of rule hits → `mie286_outlier_screen()`. |
| `mie286_analysis_pipeline.R` | All plots and statistics after data load. Sensitivity block runs only when `RUN_SENSITIVITY_COMPARE` is `TRUE` (default). If `mie286_outlier_screen` is missing, the pipeline tries to source `mie286_outlier_rules.R` from `proj_root` or `getwd()`. |
| `order_analysis.R` | Separate helper: block order vs paired contrasts (optional). |

## Outlier rules (short version)

A participant row is flagged if **either**:

- **IQR:** outside 1.5×IQR on any of the four outcome columns *or* on `diff_time` / `diff_area`, or  
- **Z-score:** \|(*x* − mean)/*sd*\| \> 3 on those same six quantities,

with **sd** guards so flat data does not explode. Flagged participants are removed **whole row** (paired structure preserved).

The main script keeps stratified gender/gaming plots on the **full** `paired_complete`; the dedicated no-outliers script uses the **restricted** sample for everything.

## Output folders

| Run | Figures |
|-----|--------|
| `analysis_mie286.R` | `graphs/` |
| `analysis_mie286_no_outliers.R` | `graphs_no_outliers/` |

The main run can also write `r_feedback_*_no_outliers.png` into `graphs/` when sensitivity figures are enabled.

## Paired *t*-tests and the *t*-distribution figure

Paired tests use differences within participant; the script at the bottom that plots a *t* curve with **df = 31** is illustrative for a fixed *n* — your actual degrees of freedom for a paired test are `n_pairs - 1` for the sample you analyzed.

---

*If something says “could not find `mie286_outlier_screen`”, set your working directory to this `code/` folder (or open the project there) and run `analysis_mie286.R` / `analysis_mie286_no_outliers.R` from the top.*
