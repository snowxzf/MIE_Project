# First-block order vs paired contrasts (numerical − spatial). Run from MIE_Project: Rscript order_analysis.R

suppressPackageStartupMessages(library(dplyr))

proj_root <- getwd()
data_r <- file.path(proj_root, "data_mie286.R")
if (!file.exists(data_r)) {
  stop("Missing data_mie286.R (run build_mie286_vectors.R). cwd: ", proj_root)
}
source(data_r, local = FALSE)

n_obs <- length(participant)
stopifnot(
  length(duration_sec_numerical) == n_obs,
  length(duration_sec_spatial_color) == n_obs,
  length(area_off_px2_numerical) == n_obs,
  length(area_off_px2_spatial_color) == n_obs
)

paired_complete <- tibble::tibble(
  participant = participant,
  `duration_sec___numerical` = duration_sec_numerical,
  `duration_sec___spatial-color` = duration_sec_spatial_color,
  `area_off_px2___numerical` = area_off_px2_numerical,
  `area_off_px2___spatial-color` = area_off_px2_spatial_color
)

ord_path_end <- file.path(proj_root, "participant_order.csv")
cat("\n=== order_analysis.R ===\n\n")

if (!file.exists(ord_path_end)) {
  cat("SKIPPED: file not found:\n  ", normalizePath(ord_path_end, winslash = "/", mustWork = FALSE), "\n", sep = "")
} else {
  ord_end <- read.csv(ord_path_end, stringsAsFactors = FALSE, check.names = FALSE)
  names(ord_end) <- trimws(names(ord_end))
  ord_end$participant <- trimws(as.character(ord_end$participant))
  paired_complete <- paired_complete %>% dplyr::left_join(ord_end, by = "participant")

  if (!"block1_mode" %in% names(paired_complete)) {
    cat("SKIPPED: participant_order.csv has no column block1_mode.\n")
  } else {
    pc_o <- paired_complete %>%
      dplyr::filter(!is.na(.data$block1_mode), nzchar(as.character(.data$block1_mode)))
    n_ord <- nrow(pc_o)
    n_levels <- length(unique(pc_o$block1_mode))
    cat("Rows with non-missing block1_mode: n = ", n_ord, "; distinct block1_mode values: ", n_levels, "\n\n", sep = "")

    if (n_ord < 4L || n_levels < 2L) {
      cat("SKIPPED: need at least 4 rows with order and 2 levels of block1_mode.\n")
    } else {
      pc_o <- pc_o %>%
        mutate(
          diff_time_order = .data$`duration_sec___numerical` - .data$`duration_sec___spatial-color`,
          diff_area_order = .data$`area_off_px2___numerical` - .data$`area_off_px2___spatial-color`
        )

      cat("(1) Descriptives — mean(SD) of numerical−spatial differences by block1_mode\n\n")
      desc_order <- pc_o %>%
        dplyr::group_by(block1_mode) %>%
        dplyr::summarise(
          n = dplyr::n(),
          mean_diff_time_s = mean(.data$diff_time_order),
          sd_diff_time = stats::sd(.data$diff_time_order),
          mean_diff_area_px2 = mean(.data$diff_area_order),
          sd_diff_area = stats::sd(.data$diff_area_order),
          .groups = "drop"
        )
      print(desc_order, width = 120)
      cat("\n")

      dt_n <- pc_o$diff_time_order[pc_o$block1_mode == "numerical"]
      dt_s <- pc_o$diff_time_order[pc_o$block1_mode == "spatial-color"]
      da_n <- pc_o$diff_area_order[pc_o$block1_mode == "numerical"]
      da_s <- pc_o$diff_area_order[pc_o$block1_mode == "spatial-color"]

      cat("(2) Welch two-sample t.test: diff_time (numerical-first vs spatial-first)\n\n")
      print(stats::t.test(dt_n, dt_s))
      cat("\n(3) Welch two-sample t.test: diff_area (same groups)\n\n")
      print(stats::t.test(da_n, da_s))

      ord01 <- as.integer(pc_o$block1_mode == "numerical")
      if (stats::sd(ord01) > 0 && stats::sd(pc_o$diff_time_order) > 0) {
        cat("\n(4a) Pearson correlation: order (1 = numerical first) vs diff_time (point-biserial)\n\n")
        print(stats::cor.test(ord01, pc_o$diff_time_order, method = "pearson"))
      }
      if (stats::sd(ord01) > 0 && stats::sd(pc_o$diff_area_order) > 0) {
        cat("\n(4b) Pearson correlation: order vs diff_area (point-biserial)\n\n")
        print(stats::cor.test(ord01, pc_o$diff_area_order, method = "pearson"))
      }
      cat("\n=== end order_analysis.R ===\n\n")
    }
  }
}
