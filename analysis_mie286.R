# =============================================================================
# MIE 286 — main analysis entry point (full paired sample)
# -----------------------------------------------------------------------------
# Prerequisites: data_mie286.R in this folder (run build_mie286_vectors.R if needed).
# Loads mie286_load_data_and_active.R → paired_complete, active, graphs/
# Sets RUN_SENSITIVITY_COMPARE = TRUE so the pipeline also compares full vs
#   outlier-screened paired t statistics (gender/gaming still use full N).
# Outputs: figures under graphs/ plus shapiro_wilk_all_strata.csv there.
# Run: Rscript analysis_mie286.R  (wd = code/)
# DV: duration_sec (s), area_off_px2 (lower = better trace).
# =============================================================================

proj_root <- getwd()
source(file.path(proj_root, "mie286_load_data_and_active.R"))
source(file.path(proj_root, "mie286_outlier_rules.R"))
RUN_SENSITIVITY_COMPARE <- TRUE
source(file.path(proj_root, "mie286_analysis_pipeline.R"))
