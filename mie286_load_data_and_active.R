# =============================================================================
# Shared data load for MIE 286 analysis (sourced by analysis_mie286.R, etc.)
# -----------------------------------------------------------------------------
# Working directory must be project code/. This file:
#   - Loads ggplot2/tidyr/dplyr/patchwork/nortest
#   - Sources data_mie286.R (vectors: one row per participant, same order)
#   - Builds paired_complete: wide columns use "___" (e.g. duration_sec___numerical)
#   - Builds active: long format (2 rows per participant: numerical + spatial-color)
#   - Sets out_dir = graphs/ and sources mie286_outlier_rules.R
# Run: never alone; use Rscript analysis_mie286.R or analysis_mie286_no_outliers.R
# =============================================================================

need <- c("ggplot2", "tidyr", "dplyr", "patchwork", "nortest")
miss <- need[!vapply(need, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(miss)) {
  stop("Install: install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(patchwork)
  library(nortest)
})

# Print ggplot objects when sourcing the pipeline (side effect for interactive use).
rp <- function(p) {
  print(p)
  invisible(p)
}

# Drop conf.int before print so console matches write-up (no 95% CI lines for htest).
# Use for t.test and cor.test only; keep print() for shapiro.test.
mie286_print_htest_no_ci <- function(x) {
  y <- x
  if (inherits(y, "htest") && !is.null(y[["conf.int"]])) {
    y[["conf.int"]] <- NULL
  }
  print(y)
}

proj_root <- getwd()
data_r <- file.path(proj_root, "data_mie286.R")
if (!file.exists(data_r)) {
  stop("Missing data_mie286.R (run Rscript build_mie286_vectors.R). cwd: ", proj_root)
}
source(data_r, local = FALSE)

n_obs <- length(participant)
stopifnot(
  length(duration_sec_numerical) == n_obs,
  length(duration_sec_spatial_color) == n_obs,
  length(area_off_px2_numerical) == n_obs,
  length(area_off_px2_spatial_color) == n_obs
)

if (!exists("gender", inherits = FALSE)) gender <- rep(NA_character_, n_obs)
if (!exists("avg_gaming_hours_per_day", inherits = FALSE)) {
  avg_gaming_hours_per_day <- if (exists("avg_gaming_times_per_week", inherits = FALSE)) {
    avg_gaming_times_per_week
  } else {
    rep(NA_real_, n_obs)
  }
}
stopifnot(length(gender) == n_obs, length(avg_gaming_hours_per_day) == n_obs)

paired_complete <- tibble::tibble(
  participant = participant,
  gender = gender,
  avg_gaming_hours_per_day = avg_gaming_hours_per_day,
  `duration_sec___numerical` = duration_sec_numerical,
  `duration_sec___spatial-color` = duration_sec_spatial_color,
  `area_off_px2___numerical` = area_off_px2_numerical,
  `area_off_px2___spatial-color` = area_off_px2_spatial_color
)

# Stack each participant's two modes into long format (duration + area per row).
mie286_rebuild_active <- function(paired_complete) {
  bind_rows(
    transmute(
      paired_complete,
      participant,
      mode = "numerical",
      duration_sec = `duration_sec___numerical`,
      area_off_px2 = `area_off_px2___numerical`
    ),
    transmute(
      paired_complete,
      participant,
      mode = "spatial-color",
      duration_sec = `duration_sec___spatial-color`,
      area_off_px2 = `area_off_px2___spatial-color`
    )
  )
}

active <- mie286_rebuild_active(paired_complete)

out_dir <- file.path(proj_root, "graphs")
dir.create(out_dir, showWarnings = FALSE)

rules_r <- file.path(proj_root, "mie286_outlier_rules.R")
if (!file.exists(rules_r)) {
  stop("Missing mie286_outlier_rules.R in cwd: ", proj_root, call. = FALSE)
}
source(rules_r, local = FALSE)
