# =============================================================================
# MIE 286 — Q–Q plots for outlier-excluded sample only (no full pipeline)
# -----------------------------------------------------------------------------
# Same screening as analysis_mie286_no_outliers.R; QQ point/line logic matches
# mie286_analysis_pipeline.R (theoretical normal quantiles vs sorted samples).
# Writes graphs_no_outliers/qq_outliers_main_2x2.png, qq_outliers_by_gender.png,
#   qq_outliers_by_gaming.png with titles stating outlier exclusion.
# Run: Rscript mie286_qq_outliers.R  (wd = code/)
# =============================================================================

proj_root <- getwd()
source(file.path(proj_root, "mie286_load_data_and_active.R"))
source(file.path(proj_root, "mie286_outlier_rules.R"))

scr <- mie286_outlier_screen(paired_complete)
n_excl <- sum(scr$outlier_flag)
paired_complete <- dplyr::filter(paired_complete, !scr$outlier_flag)
if (nrow(paired_complete) < 3L) {
  stop("Need ≥3 paired participants after outlier exclusion.", call. = FALSE)
}

active <- mie286_rebuild_active(paired_complete)
out_dir <- file.path(proj_root, "graphs_no_outliers")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
g_path <- NULL
gm_path <- NULL

n_paired <- nrow(paired_complete)
sub_txt <- sprintf(
  "Outlier-excluded sample (n = %d paired); union of Tukey IQR flags and |z|>3 (same rules as sensitivity). Excluded: %d.",
  n_paired,
  n_excl
)

mie286_qq_pts_line <- function(data_long, group_cols) {
  stopifnot("value" %in% names(data_long), all(group_cols %in% names(data_long)))
  qq_pts <- data_long %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::group_modify(~ {
      v <- sort(as.numeric(stats::na.omit(.x$value)))
      n <- length(v)
      if (n < 2L) {
        return(tibble::tibble(theoretical = NA_real_, sample = NA_real_))
      }
      tibble::tibble(theoretical = stats::qnorm(stats::ppoints(n)), sample = v)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(theoretical))
  qq_line <- data_long %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::group_modify(~ {
      v <- sort(as.numeric(stats::na.omit(.x$value)))
      if (length(v) < 2L) {
        return(tibble::tibble(intercept = NA_real_, slope = NA_real_))
      }
      yq <- stats::quantile(v, c(0.25, 0.75), names = FALSE, type = 7)
      xq <- stats::qnorm(c(0.25, 0.75))
      sl <- diff(yq) / diff(xq)
      tibble::tibble(intercept = yq[1] - sl * xq[1], slope = sl)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(slope))
  list(pts = qq_pts, line = qq_line)
}

# --- Main 2×2: condition × outcome ---
assump <- active %>%
  dplyr::mutate(
    condition = factor(
      mode,
      levels = c("numerical", "spatial-color"),
      labels = c("Condition 1: Numerical", "Condition 2: Spatial-color")
    )
  ) %>%
  dplyr::transmute(
    condition,
    `Time (s)` = duration_sec,
    `Area off target (px^2)` = area_off_px2
  ) %>%
  tidyr::pivot_longer(-condition, names_to = "outcome", values_to = "value")

qq_pts <- assump %>%
  dplyr::group_by(condition, outcome) %>%
  dplyr::group_modify(~ {
    v <- sort(as.numeric(stats::na.omit(.x$value)))
    n <- length(v)
    if (n < 2L) {
      return(tibble::tibble(theoretical = NA_real_, sample = NA_real_))
    }
    tibble::tibble(theoretical = stats::qnorm(stats::ppoints(n)), sample = v)
  }) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(theoretical))

qq_line <- assump %>%
  dplyr::group_by(condition, outcome) %>%
  dplyr::group_modify(~ {
    v <- sort(as.numeric(stats::na.omit(.x$value)))
    if (length(v) < 2L) {
      return(tibble::tibble(intercept = NA_real_, slope = NA_real_))
    }
    yq <- stats::quantile(v, c(0.25, 0.75), names = FALSE, type = 7)
    xq <- stats::qnorm(c(0.25, 0.75))
    sl <- diff(yq) / diff(xq)
    tibble::tibble(intercept = yq[1] - sl * xq[1], slope = sl)
  }) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(slope))

p_main <- ggplot2::ggplot(qq_pts, ggplot2::aes(theoretical, sample)) +
  ggplot2::geom_point(alpha = 0.65, na.rm = TRUE) +
  ggplot2::geom_abline(
    ggplot2::aes(intercept = intercept, slope = slope),
    data = qq_line,
    color = "#b22222",
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  ggplot2::facet_wrap(ggplot2::vars(condition, outcome), ncol = 2, scales = "free") +
  ggplot2::labs(
    title = "Q-Q plots by condition and outcome (outlier-excluded)",
    subtitle = sub_txt,
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 8))

rp(p_main)
main_path <- file.path(out_dir, "qq_outliers_main_2x2.png")
ggplot2::ggsave(main_path, p_main, width = 8.5, height = 6, dpi = 150)

# --- Gender-stratified (same mapping as mie286_analysis_pipeline.R) ---
norm_gender_label <- function(g) {
  g <- tolower(trimws(as.character(g)))
  dplyr::case_when(
    g %in% c("f", "female", "woman", "w") ~ "Women",
    g %in% c("m", "male", "man") ~ "Men",
    TRUE ~ NA_character_
  )
}

paired_g <- paired_complete %>%
  dplyr::mutate(
    gender_f = factor(norm_gender_label(gender), levels = c("Women", "Men"))
  )

active_g <- dplyr::bind_rows(
  dplyr::transmute(
    paired_g,
    participant,
    gender_f,
    mode = "numerical",
    duration_sec = `duration_sec___numerical`,
    area_off_px2 = `area_off_px2___numerical`
  ),
  dplyr::transmute(
    paired_g,
    participant,
    gender_f,
    mode = "spatial-color",
    duration_sec = `duration_sec___spatial-color`,
    area_off_px2 = `area_off_px2___spatial-color`
  )
) %>%
  dplyr::filter(!is.na(gender_f))

if (nrow(active_g) >= 4L) {
  assump_g <- active_g %>%
    dplyr::mutate(
      condition = factor(
        mode,
        levels = c("numerical", "spatial-color"),
        labels = c("Numerical", "Spatial-color")
      )
    ) %>%
    dplyr::transmute(
      gender_f,
      condition,
      `Time (s)` = duration_sec,
      `Area off target (px^2)` = area_off_px2
    ) %>%
    tidyr::pivot_longer(
      c(`Time (s)`, `Area off target (px^2)`),
      names_to = "outcome",
      values_to = "value"
    )
  lay_g <- mie286_qq_pts_line(assump_g, c("gender_f", "condition", "outcome"))
  p_g <- ggplot2::ggplot(lay_g$pts, ggplot2::aes(theoretical, sample)) +
    ggplot2::geom_point(alpha = 0.6, na.rm = TRUE) +
    ggplot2::geom_abline(
      ggplot2::aes(intercept = intercept, slope = slope),
      data = lay_g$line,
      color = "#b22222",
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(condition, outcome),
      cols = ggplot2::vars(gender_f),
      scales = "free"
    ) +
    ggplot2::labs(
      title = "Q-Q plots by gender (outlier-excluded)",
      subtitle = sub_txt,
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 7.5))
  rp(p_g)
  g_path <- file.path(out_dir, "qq_outliers_by_gender.png")
  ggplot2::ggsave(g_path, p_g, width = 9, height = 9, dpi = 150)
} else {
  message("Skipping gender Q–Q (need ≥4 long rows with known woman/man gender).")
  g_path <- NULL
}

# --- Gaming-stratified (median split on h/day) ---
hours_vec <- paired_complete$avg_gaming_hours_per_day
med_game <- stats::median(hours_vec, na.rm = TRUE)
n_ok_game <- sum(!is.na(hours_vec))
n_distinct_game <- length(unique(stats::na.omit(hours_vec)))

if (n_ok_game >= 4L && n_distinct_game >= 2L && is.finite(med_game)) {
  lg <- "Time spent gaming (~0 hours/day)"
  hg <- "Time spent gaming (>0 hours/day)"
  paired_game <- paired_complete %>%
    dplyr::mutate(
      gaming_f = factor(
        dplyr::case_when(
          is.na(avg_gaming_hours_per_day) ~ NA_character_,
          avg_gaming_hours_per_day <= med_game ~ lg,
          TRUE ~ hg
        ),
        levels = c(lg, hg)
      )
    )
  active_game <- dplyr::bind_rows(
    dplyr::transmute(
      paired_game,
      participant,
      gaming_f,
      mode = "numerical",
      duration_sec = `duration_sec___numerical`,
      area_off_px2 = `area_off_px2___numerical`
    ),
    dplyr::transmute(
      paired_game,
      participant,
      gaming_f,
      mode = "spatial-color",
      duration_sec = `duration_sec___spatial-color`,
      area_off_px2 = `area_off_px2___spatial-color`
    )
  ) %>%
    dplyr::filter(!is.na(gaming_f))

  if (nrow(active_game) >= 4L) {
    assump_game <- active_game %>%
      dplyr::mutate(
        condition = factor(
          mode,
          levels = c("numerical", "spatial-color"),
          labels = c("Numerical", "Spatial-color")
        )
      ) %>%
      dplyr::transmute(
        gaming_f,
        condition,
        `Time (s)` = duration_sec,
        `Area off target (px^2)` = area_off_px2
      ) %>%
      tidyr::pivot_longer(
        c(`Time (s)`, `Area off target (px^2)`),
        names_to = "outcome",
        values_to = "value"
      )
    lay_gm <- mie286_qq_pts_line(assump_game, c("gaming_f", "condition", "outcome"))
    p_gm <- ggplot2::ggplot(lay_gm$pts, ggplot2::aes(theoretical, sample)) +
      ggplot2::geom_point(alpha = 0.6, na.rm = TRUE) +
      ggplot2::geom_abline(
        ggplot2::aes(intercept = intercept, slope = slope),
        data = lay_gm$line,
        color = "#b22222",
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(condition, outcome),
        cols = ggplot2::vars(gaming_f),
        scales = "free"
      ) +
      ggplot2::labs(
        title = "Q-Q plots by gaming group (outlier-excluded)",
        subtitle = sub_txt,
        x = "Theoretical quantiles",
        y = "Sample quantiles"
      ) +
      ggplot2::theme_bw(base_size = 9) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 6.5))
    rp(p_gm)
    gm_path <- file.path(out_dir, "qq_outliers_by_gaming.png")
    ggplot2::ggsave(gm_path, p_gm, width = 10, height = 9, dpi = 150)
  } else {
    message("Skipping gaming Q–Q (insufficient rows after split).")
    gm_path <- NULL
  }
} else {
  message("Skipping gaming Q-Q (need >=4 non-missing gaming hours and >=2 distinct values).")
  gm_path <- NULL
}

cat("\n--- Outlier Q-Q plot maker: done ---\n")
cat("Saved:\n ", main_path, "\n", sep = "")
if (!is.null(g_path)) cat(" ", g_path, "\n", sep = "")
if (!is.null(gm_path)) cat(" ", gm_path, "\n", sep = "")
