# MIE 286 — repeat full analysis after excluding IQR / |z|>3 outliers (same rules as sensitivity).
# Run from project code/ with data_mie286.R present. Figures: graphs_no_outliers/

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
