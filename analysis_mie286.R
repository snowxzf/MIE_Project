# MIE 286 (numerical vs spatial-color)
# Data: vectors in data_mie286.R (same participant order in each vector).
# Refresh vectors from CSV
# DV1 = duration_sec (s), DV2 = area_off_px2 (lower = better trace).
#
# Full sample + sensitivity (full vs restricted). Figures: graphs/

proj_root <- getwd()
source(file.path(proj_root, "mie286_load_data_and_active.R"))
source(file.path(proj_root, "mie286_outlier_rules.R"))
RUN_SENSITIVITY_COMPARE <- TRUE
source(file.path(proj_root, "mie286_analysis_pipeline.R"))
