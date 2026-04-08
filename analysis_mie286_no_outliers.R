# =============================================================================
# MIE 286 — full pipeline on outlier-excluded sample only
# -----------------------------------------------------------------------------
# Applies mie286_outlier_screen() (Tukey IQR ∪ |z|>3 on 4 levels + paired diffs),
#   drops flagged rows from paired_complete, rebuilds active, redirects out_dir.
# Figures and shapiro_wilk_all_strata.csv go to graphs_no_outliers/ (not graphs/).
# RUN_SENSITIVITY_COMPARE is FALSE (no second sensitivity pass on an already-filtered set).
# Run: Rscript analysis_mie286_no_outliers.R  (wd = code/)
# =============================================================================

proj_root <- getwd()
source(file.path(proj_root, "mie286_outlier_rules.R"))
source(file.path(proj_root, "mie286_load_data_and_active.R"))

n_full <- nrow(paired_complete)
scr <- mie286_outlier_screen(paired_complete)

cat("\n========== OUTLIER-EXCLUDED PIPELINE (full analysis rerun) ==========\n")
cat("Rule A (Tukey IQR, 4 levels + diff_time + diff_area): n flagged = ", sum(scr$iqr_hit), "\n")
cat("Rule B (|z| > 3, same): n flagged = ", sum(scr$z_hit), "\n")
cat("Union removed: n = ", sum(scr$outlier_flag), " (of ", n_full, " paired)\n", sep = "")
if (any(scr$outlier_flag)) {
  cat(
    "Excluded participants: ",
    paste(paired_complete$participant[scr$outlier_flag], collapse = ", "),
    "\n",
    sep = ""
  )
} else {
  cat("Excluded participants: (none)\n")
}

paired_complete <- dplyr::filter(paired_complete, !scr$outlier_flag)
if (nrow(paired_complete) < 3L) {
  stop(
    "Fewer than 3 paired participants after outlier exclusion; cannot run analysis.",
    call. = FALSE
  )
}

active <- mie286_rebuild_active(paired_complete)
out_dir <- file.path(proj_root, "graphs_no_outliers")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

RUN_SENSITIVITY_COMPARE <- FALSE
source(file.path(proj_root, "mie286_analysis_pipeline.R"))
