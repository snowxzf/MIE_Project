# Used by analysis_mie286.R sensitivity block and analysis_mie286_no_outliers.R.

col_iqr_extreme <- function(v) {
  q <- stats::quantile(v, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  iqr <- q[2] - q[1]
  if (!is.finite(iqr) || iqr <= 0) {
    return(rep(FALSE, length(v)))
  }
  lo <- q[1] - 1.5 * iqr
  hi <- q[2] + 1.5 * iqr
  v < lo | v > hi
}

col_z_extreme <- function(v, zmax = 3) {
  m <- mean(v, na.rm = TRUE)
  s <- stats::sd(v, na.rm = TRUE)
  if (!is.finite(s) || s < 1e-12) {
    return(rep(FALSE, length(v)))
  }
  abs((v - m) / s) > zmax
}

## @return list(outlier_flag, iqr_hit, z_hit) — logical vectors, length nrow(paired_complete)
mie286_outlier_screen <- function(paired_complete) {
  diff_time <- paired_complete$`duration_sec___numerical` -
    paired_complete$`duration_sec___spatial-color`
  diff_area <- paired_complete$`area_off_px2___numerical` -
    paired_complete$`area_off_px2___spatial-color`

  dn <- paired_complete$`duration_sec___numerical`
  dsc <- paired_complete$`duration_sec___spatial-color`
  an <- paired_complete$`area_off_px2___numerical`
  asc <- paired_complete$`area_off_px2___spatial-color`

  iqr_raw <- col_iqr_extreme(dn) | col_iqr_extreme(dsc) | col_iqr_extreme(an) | col_iqr_extreme(asc)
  iqr_diff <- col_iqr_extreme(diff_time) | col_iqr_extreme(diff_area)
  iqr_hit <- iqr_raw | iqr_diff

  z_raw <- col_z_extreme(dn) | col_z_extreme(dsc) | col_z_extreme(an) | col_z_extreme(asc)
  z_diff <- col_z_extreme(diff_time) | col_z_extreme(diff_area)
  z_hit <- z_raw | z_diff

  list(
    outlier_flag = iqr_hit | z_hit,
    iqr_hit = iqr_hit,
    z_hit = z_hit
  )
}
