# =============================================================================
# MIE 286 analysis pipeline (source only after mie286_load_data_and_active.R)
# -----------------------------------------------------------------------------
# Expects: paired_complete, active, out_dir, rp, mie286_print_htest_no_ci
# Flow: descriptives -> normality (Shapiro–Wilk, QQ) -> paired t & r -> optional
# sensitivity on screened outliers -> box/scatter plots -> gender & gaming strata
# -> export shapirowilkallstrata.csv -> summary & optional t-density figure.
# =============================================================================
if (!exists("RUN_SENSITIVITY_COMPARE", inherits = FALSE)) {
  RUN_SENSITIVITY_COMPARE <- TRUE
}

if (!exists("mie286_print_htest_no_ci", mode = "function", inherits = TRUE)) {
  mie286_print_htest_no_ci <- function(x) {
    y <- x
    if (inherits(y, "htest") && !is.null(y[["conf.int"]])) {
      y[["conf.int"]] <- NULL
    }
    print(y)
  }
}

if (!exists("mie286_outlier_screen", mode = "function", inherits = TRUE)) {
  base <- if (exists("proj_root", inherits = TRUE)) proj_root else getwd()
  rules_r <- file.path(base, "mie286_outlier_rules.R")
  if (!file.exists(rules_r)) {
    stop(
      "Could not find mie286_outlier_screen(); expected ",
      rules_r,
      ". Set working directory to the project code/ folder, or run source(\"mie286_outlier_rules.R\") first.",
      call. = FALSE
    )
  }
  source(rules_r, local = FALSE)
}

# Design / hypotheses
cat("\nHypothesis:\n")
cat("H1 (speed):  Mean completion time differs between numerical and spatial-color feedback.\n")
cat("H2 (accuracy): Mean area off target (px^2) differs (lower = better).\n")
cat("H3 (speed-accuracy): Within each feedback type, time and area correlate; Delta time vs Delta area across conditions.\n")

cat("Sample (paired): n = ", nrow(paired_complete), " participants.\n")
cat("Inferential tests are paired / dependent (NOT independent-samples t-tests).\n\n")

# Descriptives
desc_active <- active %>%
  group_by(mode) %>%
  summarise(
    n = n(),
    duration_mean = mean(duration_sec),
    duration_sd = sd(duration_sec),
    duration_var = var(duration_sec),
    area_off_mean = mean(area_off_px2),
    area_off_sd = sd(area_off_px2),
    area_off_var = var(area_off_px2),
    .groups = "drop"
  )

cat("Descriptive statistics by feedback type\n")
print(desc_active, width = 120)
cat("\n")

# --- Normality: long data for histograms / Q-Q; Shapiro–Wilk by condition x outcome ---
# Assumption figures (long format)
assump <- active %>%
  mutate(
    condition = factor(
      mode,
      levels = c("numerical", "spatial-color"),
      labels = c(
        "Condition 1: Numerical",
        "Condition 2: Spatial-color"
      )
    )
  ) %>%
  transmute(
    condition,
    `Time (s)` = duration_sec,
    `Area off target (px^2)` = area_off_px2
  ) %>%
  pivot_longer(-condition, names_to = "outcome", values_to = "value")

cat(
  "Normality by condition x outcome: Shapiro–Wilk; ",
  "paired t-tests use Shapiro–Wilk on difference scores below\n",
  sep = ""
)
shapiro_by_group <- assump %>%
  group_by(condition, outcome) %>%
  group_modify(~ {
    v <- .x$value
    n <- length(v)
    statistic_W <- NA_real_
    p_value <- NA_real_
    if (n >= 3L && n <= 5000L) {
      sw <- stats::shapiro.test(v)
      statistic_W <- unname(sw$statistic)
      p_value <- sw$p.value
    }
    tibble::tibble(
      n = n,
      statistic_W = statistic_W,
      p_value = p_value
    )
  }) %>%
  ungroup()
print(shapiro_by_group, n = Inf)
cat("\n")

curve_df <- assump %>%
  group_by(condition, outcome) %>%
  group_modify(~ {
    v <- .x$value
    m <- mean(v, na.rm = TRUE)
    s <- stats::sd(v, na.rm = TRUE)
    if (!is.finite(s) || s < 1e-9) s <- 1e-6
    rg <- range(v, na.rm = TRUE)
    pad <- max(diff(rg) * 0.25, 1, na.rm = TRUE)
    xs <- seq(rg[1] - pad, rg[2] + pad, length.out = 200)
    tibble::tibble(x = xs, y = stats::dnorm(xs, m, s))
  }) %>%
  ungroup()

#Plotting the histogram, has colours 
p_hist_norm <- ggplot(assump, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = "#3d4f73", alpha = 0.78, color = "white", linewidth = 0.3) +
  geom_line(
    data = curve_df,
    aes(x = x, y = y),
    color = "#b22222",
    linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  facet_wrap(vars(condition, outcome), ncol = 2, scales = "free") +
  labs(
    title = "Normality check: histogram vs normal curve",
    subtitle = "seconds vs px^2.",
    x = NULL,
    y = "Density"
  ) +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(size = 8))

rp(p_hist_norm)
ggsave(file.path(out_dir, "r_check_normality_histograms_2x2.png"), p_hist_norm, width = 8.5, height = 6, dpi = 150)

qq_pts <- assump %>%
  group_by(condition, outcome) %>%
  group_modify(~ {
    v <- sort(as.numeric(stats::na.omit(.x$value)))
    n <- length(v)
    if (n < 2L) return(tibble::tibble(theoretical = NA_real_, sample = NA_real_))
    tibble::tibble(theoretical = stats::qnorm(stats::ppoints(n)), sample = v)
  }) %>%
  ungroup() %>%
  filter(!is.na(theoretical))

qq_line <- assump %>%
  group_by(condition, outcome) %>%
  group_modify(~ {
    v <- sort(as.numeric(stats::na.omit(.x$value)))
    if (length(v) < 2L) return(tibble::tibble(intercept = NA_real_, slope = NA_real_))
    yq <- stats::quantile(v, c(0.25, 0.75), names = FALSE, type = 7)
    xq <- stats::qnorm(c(0.25, 0.75))
    sl <- diff(yq) / diff(xq)
    tibble::tibble(intercept = yq[1] - sl * xq[1], slope = sl)
  }) %>%
  ungroup() %>%
  filter(!is.na(slope))

p_qq <- ggplot(qq_pts, aes(theoretical, sample)) +
  geom_point(alpha = 0.65, na.rm = TRUE) +
  geom_abline(aes(intercept = intercept, slope = slope), data = qq_line, color = "#b22222", linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(vars(condition, outcome), ncol = 2, scales = "free") +
  labs(
    title = "Q-Q plots by condition and outcome",
    subtitle = "Linear relation means normality",
    x = "Theoretical quantiles",
    y = "Sample quantiles"
  ) +
  theme_bw(base_size = 11) +
  theme(strip.text = element_text(size = 8))

rp(p_qq)
ggsave(file.path(out_dir, "r_check_qq_plots_2x2.png"), p_qq, width = 8.5, height = 6, dpi = 150)

cat("Saved: r_check_normality_histograms_2x2.png, r_check_qq_plots_2x2.png\n\n")

# --- Paired inference (same participants): numerical minus spatial-colour ---
diff_time <- paired_complete$`duration_sec___numerical` - paired_complete$`duration_sec___spatial-color`
diff_area <- paired_complete$`area_off_px2___numerical` - paired_complete$`area_off_px2___spatial-color`

cat("PRIMARY assumption for paired t-tests: Shapiro-Wilk on paired differences\n")
if (length(diff_time) >= 3 && length(diff_time) <= 5000) print(shapiro.test(diff_time))
if (length(diff_area) >= 3 && length(diff_area) <= 5000) print(shapiro.test(diff_area))
cat("\n")

cat("Paired t-tests (one-tailed; directional hypotheses)\n")
tt_time <- t.test(
  paired_complete$`duration_sec___numerical`,
  paired_complete$`duration_sec___spatial-color`,
  paired = TRUE,
  alternative = "greater" # H2.1: numerical time > spatial-color time
)
mie286_print_htest_no_ci(tt_time)
tt_area <- t.test(
  paired_complete$`area_off_px2___numerical`,
  paired_complete$`area_off_px2___spatial-color`,
  paired = TRUE,
  alternative = "less" # H2.2: numerical area < spatial-color area
)
mie286_print_htest_no_ci(tt_area)
cat("\n")

act_num <- active %>% filter(mode == "numerical")
act_spa <- active %>% filter(mode == "spatial-color")

cat("Pearson: duration vs area (Numerical)\n")
cor_num <- cor.test(act_num$duration_sec, act_num$area_off_px2, method = "pearson")
mie286_print_htest_no_ci(cor_num)
cat("\nPearson: duration vs area (Spatial-color)\n")
cor_spa <- cor.test(act_spa$duration_sec, act_spa$area_off_px2, method = "pearson")
mie286_print_htest_no_ci(cor_spa)
cat("\nPearson: Delta time vs Delta area\n")
cor_delta <- cor.test(diff_time, diff_area, method = "pearson")
mie286_print_htest_no_ci(cor_delta)
cat("\nSpearman (each mode)\n")
mie286_print_htest_no_ci(cor.test(act_num$duration_sec, act_num$area_off_px2, method = "spearman"))
mie286_print_htest_no_ci(cor.test(act_spa$duration_sec, act_spa$area_off_px2, method = "spearman"))
cat("\n")

if (isTRUE(RUN_SENSITIVITY_COMPARE)) {
# --- Sensitivity: recompute key stats after removing IQR/|z|>3-flagged participants ---
# Optional boxplots written when save_sensitivity_figures is TRUE.
# Stratified gender/gaming sections below always use the FULL paired_complete.
save_sensitivity_figures <- TRUE

scr <- mie286_outlier_screen(paired_complete)
outlier_flag <- scr$outlier_flag
iqr_hit <- scr$iqr_hit
z_hit <- scr$z_hit

cat("\nOUTLIER SCREENING (sensitivity only)\n")
cat("Rule A (Tukey IQR, any of 4 levels + diff_time + diff_area): n flagged = ", sum(iqr_hit), "\n")
cat("Rule B (|z| > 3, same): n flagged = ", sum(z_hit), "\n")
cat("Union (removed from sensitivity set): n = ", sum(outlier_flag), "\n")
if (any(outlier_flag)) {
  cat("Excluded participants: ", paste(paired_complete$participant[outlier_flag], collapse = ", "), "\n")
} else {
  cat("Excluded participants: (none)\n")
}

paired_complete_sens <- dplyr::filter(paired_complete, !outlier_flag)
n_full <- nrow(paired_complete)
n_sens <- nrow(paired_complete_sens)

if (n_sens < 3L) {
  cat("WARNING: n < 3 after outlier removal; skipping sensitivity re-tests.\n")
  cat("---\n\n")
} else {
  diff_time_sens <- paired_complete_sens$`duration_sec___numerical` -
    paired_complete_sens$`duration_sec___spatial-color`
  diff_area_sens <- paired_complete_sens$`area_off_px2___numerical` -
    paired_complete_sens$`area_off_px2___spatial-color`

  tt_time_sens <- t.test(
    paired_complete_sens$`duration_sec___numerical`,
    paired_complete_sens$`duration_sec___spatial-color`,
    paired = TRUE,
    alternative = "greater" # H2.1
  )
  tt_area_sens <- t.test(
    paired_complete_sens$`area_off_px2___numerical`,
    paired_complete_sens$`area_off_px2___spatial-color`,
    paired = TRUE,
    alternative = "less" # H2.2
  )

  active_sens <- bind_rows(
    transmute(
      paired_complete_sens,
      participant,
      gender = .data$gender,
      avg_gaming_hours_per_day = .data$avg_gaming_hours_per_day,
      mode = "numerical",
      duration_sec = `duration_sec___numerical`,
      area_off_px2 = `area_off_px2___numerical`
    ),
    transmute(
      paired_complete_sens,
      participant,
      gender = .data$gender,
      avg_gaming_hours_per_day = .data$avg_gaming_hours_per_day,
      mode = "spatial-color",
      duration_sec = `duration_sec___spatial-color`,
      area_off_px2 = `area_off_px2___spatial-color`
    )
  )
  act_num_sens <- active_sens %>% filter(mode == "numerical")
  act_spa_sens <- active_sens %>% filter(mode == "spatial-color")

  cor_num_sens <- cor.test(act_num_sens$duration_sec, act_num_sens$area_off_px2, method = "pearson")
  cor_spa_sens <- cor.test(act_spa_sens$duration_sec, act_spa_sens$area_off_px2, method = "pearson")
  cor_delta_sens <- cor.test(diff_time_sens, diff_area_sens, method = "pearson")

  cat("\nBEFORE vs AFTER (paired tests & Pearson r)\n")
  cat(sprintf("N (paired): full = %d, without outliers = %d\n", n_full, n_sens))
  cat(sprintf(
    "TIME  — mean diff (num−spatial) s: full = %.4f, p = %.4f | sens = %.4f, p = %.4f\n",
    unname(tt_time$estimate), tt_time$p.value,
    unname(tt_time_sens$estimate), tt_time_sens$p.value
  ))
  cat(sprintf(
    "AREA  — mean diff (num−spatial) px^2: full = %.4f, p = %.4f | sens = %.4f, p = %.4f\n",
    unname(tt_area$estimate), tt_area$p.value,
    unname(tt_area_sens$estimate), tt_area_sens$p.value
  ))
  cat(sprintf(
    "r (num, time vs area): full = %.4f, p = %.4f | sens = %.4f, p = %.4f\n",
    unname(cor_num$estimate), cor_num$p.value,
    unname(cor_num_sens$estimate), cor_num_sens$p.value
  ))
  cat(sprintf(
    "r (spatial, time vs area): full = %.4f, p = %.4f | sens = %.4f, p = %.4f\n",
    unname(cor_spa$estimate), cor_spa$p.value,
    unname(cor_spa_sens$estimate), cor_spa_sens$p.value
  ))
  cat(sprintf(
    "r (delta time vs delta area): full = %.4f, p = %.4f | sens = %.4f, p = %.4f\n",
    unname(cor_delta$estimate), cor_delta$p.value,
    unname(cor_delta_sens$estimate), cor_delta_sens$p.value
  ))

  removed <- n_full - n_sens
  cat("\nDraft for report: After removing ", removed, " participant(s) flagged by IQR or |z|>3 on ",
    "minutes/area levels and paired differences, ", sep = "")
  cat("the paired t-test p-value for time went from ",
    sprintf("%.4f to %.4f", tt_time$p.value, tt_time_sens$p.value),
    " and for area from ",
    sprintf("%.4f to %.4f", tt_area$p.value, tt_area_sens$p.value), ".\n", sep = "")

  if (isTRUE(save_sensitivity_figures)) {
    lab_x <- c("numerical" = "Numerical", "spatial-color" = "Spatial-color")
    fill_v <- c("numerical" = "#4a36c0", "spatial-color" = "#18a050")
    p_box_s <- ggplot(active_sens, aes(mode, duration_sec, fill = mode)) +
      geom_boxplot(alpha = 0.85, outlier.shape = NA) +
      geom_jitter(width = 0.12, alpha = 0.7, size = 2) +
      scale_x_discrete(labels = lab_x) +
      scale_fill_manual(values = fill_v) +
      labs(
        title = "Completion time (outliers removed for sensitivity)",
        x = "Feedback type",
        y = "Time (s)"
      ) +
      theme_bw(base_size = 12) +
      theme(legend.position = "none")
    rp(p_box_s)
    ggsave(
      file.path(out_dir, "r_feedback_duration_boxplot_no_outliers.png"),
      p_box_s,
      width = 6,
      height = 4,
      dpi = 150
    )
    p_acc_s <- ggplot(active_sens, aes(mode, area_off_px2, fill = mode)) +
      geom_boxplot(alpha = 0.85, outlier.shape = NA) +
      geom_jitter(width = 0.12, alpha = 0.7, size = 2) +
      scale_x_discrete(labels = lab_x) +
      scale_fill_manual(values = fill_v) +
      labs(
        title = "Area off target (outliers removed for sensitivity)",
        x = "Feedback type",
        y = expression("Area (" * px^2 * ")")
      ) +
      theme_bw(base_size = 12) +
      theme(legend.position = "none")
    rp(p_acc_s)
    ggsave(
      file.path(out_dir, "r_feedback_area_boxplot_no_outliers.png"),
      p_acc_s,
      width = 6,
      height = 4,
      dpi = 150
    )
    cat("Saved r_feedback_duration_boxplot_no_outliers.png, ",
      "r_feedback_area_boxplot_no_outliers.png\n",
      sep = ""
    )
  }

  cat("---\n\n")
}
}

# Summary
md_t <- unname(tt_time$estimate) # mean(numerical - spatial) for time (s)
p_t <- tt_time$p.value
if (abs(md_t) < 1e-9) {
  cat("On average, completion time was about the same in numerical and spatial-color.\n")
} else if (md_t > 0) {
  cat(sprintf(
    "On average, people were slower with numerical feedback than spatial-color by about %.2f s (paired difference: numerical minus spatial). ",
    md_t
  ))
} else {
  cat(sprintf(
    "On average, people were faster with numerical feedback than spatial-color by about %.2f s (paired difference: numerical minus spatial). ",
    abs(md_t)
  ))
}
cat(sprintf(
  "Paired t-test one-tailed p = %.4f%s\n",
  p_t,
  if (p_t < 0.05) " (reject equal means at alpha = .05)." else " (not significant at alpha = .05)."
))

md_a <- unname(tt_area$estimate) # mean(numerical - spatial) for area (px^2); lower area = better trace
p_a <- tt_area$p.value
if (abs(md_a) < 1e-6) {
  cat("On average, trace error was about the same in both feedback types.\n")
} else if (md_a > 0) {
  cat(sprintf(
    "On average, area off target was larger (worse) with numerical than spatial-color by about %.1f px^2. ",
    md_a
  ))
} else {
  cat(sprintf(
    "On average, area off target was smaller (better) with numerical than spatial-color by about %.1f px^2. ",
    abs(md_a)
  ))
}
cat(sprintf(
  "Paired t-test one-tailed p = %.4f%s\n",
  p_a,
  if (p_a < 0.05) " (reject equal means at alpha = .05)." else " (not significant at alpha = .05)."
))

rn <- unname(cor_num$estimate)
cat(sprintf(
  "Across people, time and area correlated r = %.3f (p = %.4f). %s\n",
  rn,
  cor_num$p.value,
  if (rn > 0) "Longer runs tended to align with larger (worse) area." else if (rn < 0) "Longer runs tended to align with smaller (better) area." else "No clear linear trend."
))
rs <- unname(cor_spa$estimate)
cat(sprintf(
  "SPEED–ACCURACY (Spatial-color): r = %.3f (p = %.4f). %s\n",
  rs,
  cor_spa$p.value,
  if (rs > 0) "Longer runs tended to align with larger (worse) area." else if (rs < 0) "Longer runs tended to align with smaller (better) area." else "No clear linear trend."
))
rd <- unname(cor_delta$estimate)
cat(sprintf(
  "Correlation of (numerical−spatial) time vs (numerical−spatial) area: r = %.3f (p = %.4f).\n",
  rd,
  cor_delta$p.value
))

# --- Primary figures (full sample): time and area boxplots, speed vs accuracy scatter ---
p_box <- ggplot(active, aes(mode, duration_sec, fill = mode)) +
  geom_boxplot(alpha = 0.85, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.7, size = 2) +
  scale_x_discrete(labels = c("numerical" = "Numerical", "spatial-color" = "Spatial-color")) +
  scale_fill_manual(values = c("numerical" = "#4a36c0", "spatial-color" = "#18a050")) +
  labs(title = "Completion time by feedback type", x = "Feedback type", y = "Time (s)") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

rp(p_box)
ggsave(file.path(out_dir, "r_feedback_duration_boxplot.png"), p_box, width = 6, height = 4, dpi = 150)

p_acc <- ggplot(active, aes(mode, area_off_px2, fill = mode)) +
  geom_boxplot(alpha = 0.85, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.7, size = 2) +
  scale_x_discrete(labels = c("numerical" = "Numerical", "spatial-color" = "Spatial-color")) +
  scale_fill_manual(values = c("numerical" = "#4a36c0", "spatial-color" = "#18a050")) +
  labs(
    title = "Accuracy: area off target curve (lower = better)",
    x = "Feedback type",
    y = expression("Area off target (" * px^2 * ")")
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")

rp(p_acc)
ggsave(file.path(out_dir, "r_feedback_area_boxplot.png"), p_acc, width = 6, height = 4, dpi = 150)

p_sa <- ggplot(active, aes(duration_sec, area_off_px2, color = mode)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.6) +
  scale_color_manual(
    values = c("numerical" = "#4a36c0", "spatial-color" = "#18a050"),
    labels = c("numerical" = "Numerical", "spatial-color" = "Spatial-color")
  ) +
  labs(
    title = "Speed vs trace error (area off target)",
    x = "Completion time (s)",
    y = expression("Area off target (" * px^2 * ")"),
    color = "Feedback type"
  ) +
  theme_bw(base_size = 12)

rp(p_sa)
ggsave(file.path(out_dir, "r_speed_area_scatter.png"), p_sa, width = 7, height = 4.5, dpi = 150)

# --- Exploratory gender stratification: Welch t-tests, Q-Q, Pearson within cells ---
# Map free-text gender to Women/Men; drop unknown for subgroup plots.
norm_gender_label <- function(g) {
  g <- tolower(trimws(as.character(g)))
  dplyr::case_when(
    g %in% c("f", "female", "woman", "w") ~ "Women",
    g %in% c("m", "male", "man") ~ "Men",
    TRUE ~ NA_character_
  )
}

paired_g <- paired_complete %>%
  mutate(
    gender_f = factor(norm_gender_label(.data$gender), levels = c("Women", "Men"))
  )

active_g <- bind_rows(
  transmute(
    paired_g,
    participant,
    gender_f,
    mode = "numerical",
    duration_sec = `duration_sec___numerical`,
    area_off_px2 = `area_off_px2___numerical`
  ),
  transmute(
    paired_g,
    participant,
    gender_f,
    mode = "spatial-color",
    duration_sec = `duration_sec___spatial-color`,
    area_off_px2 = `area_off_px2___spatial-color`
  )
) %>%
  filter(!is.na(.data$gender_f))

# Shapiro–Wilk for subgroup vectors (gender/gaming t-tests and correlation inputs)
shapiro_or_skip_subgroup <- function(x, label) {
  x <- as.numeric(stats::na.omit(x))
  n <- length(x)
  cat(label, " (n = ", n, ")\n", sep = "")
  if (n >= 3L && n <= 5000L) {
    print(stats::shapiro.test(x))
  } else {
    cat("  Shapiro–Wilk skipped (need 3 ≤ n ≤ 5000)\n")
  }
}

## Q-Q points + reference line for faceted normality plots (same logic as main assump QQ)
mie286_qq_pts_line <- function(data_long, group_cols) {
  stopifnot("value" %in% names(data_long), all(group_cols %in% names(data_long)))
  qq_pts <- data_long %>%
    group_by(across(all_of(group_cols))) %>%
    group_modify(~ {
      v <- sort(as.numeric(stats::na.omit(.x$value)))
      n <- length(v)
      if (n < 2L) {
        return(tibble::tibble(theoretical = NA_real_, sample = NA_real_))
      }
      tibble::tibble(theoretical = stats::qnorm(stats::ppoints(n)), sample = v)
    }) %>%
    ungroup() %>%
    filter(!is.na(theoretical))
  qq_line <- data_long %>%
    group_by(across(all_of(group_cols))) %>%
    group_modify(~ {
      v <- sort(as.numeric(stats::na.omit(.x$value)))
      if (length(v) < 2L) {
        return(tibble::tibble(intercept = NA_real_, slope = NA_real_))
      }
      yq <- stats::quantile(v, c(0.25, 0.75), names = FALSE, type = 7)
      xq <- stats::qnorm(c(0.25, 0.75))
      sl <- diff(yq) / diff(xq)
      tibble::tibble(intercept = yq[1] - sl * xq[1], slope = sl)
    }) %>%
    ungroup() %>%
    filter(!is.na(slope))
  list(pts = qq_pts, line = qq_line)
}

if (nrow(active_g) >= 4L) {
  # Separate geoms + patchwork stack: each row gets a proper y-scale (s vs px²).
  lab_x <- c("numerical" = "Numerical", "spatial-color" = "Spatial-color")
  fill_v <- c("numerical" = "#4a36c0", "spatial-color" = "#18a050")

  p_g_time <- ggplot(active_g, aes(mode, duration_sec, fill = mode)) +
    geom_boxplot(alpha = 0.85, outlier.shape = NA) +
    geom_jitter(width = 0.12, alpha = 0.7, size = 2) +
    facet_wrap(vars(gender_f), nrow = 1) +
    scale_x_discrete(labels = lab_x) +
    scale_fill_manual(values = fill_v) +
    labs(title = "Time (s)", x = NULL, y = "Completion time (s)") +
    theme_bw(base_size = 12) +
    theme(legend.position = "none", strip.text = element_text(size = 10))

  p_g_area <- ggplot(active_g, aes(mode, area_off_px2, fill = mode)) +
    geom_boxplot(alpha = 0.85, outlier.shape = NA) +
    geom_jitter(width = 0.12, alpha = 0.7, size = 2) +
    facet_wrap(vars(gender_f), nrow = 1) +
    scale_x_discrete(labels = lab_x) +
    scale_fill_manual(values = fill_v) +
    labs(
      title = "Area off target",
      x = "Feedback type",
      y = expression("Area (" * px^2 * ")")
    ) +
    theme_bw(base_size = 12) +
    theme(legend.position = "none", strip.text = element_text(size = 10))

  p_boxes_gender <- (p_g_time / p_g_area) +
    plot_annotation(
      title = "Feedback type: time and accuracy (Women vs Men)",
      theme = theme(plot.title = element_text(face = "bold", size = 13))
    )

  rp(p_boxes_gender)
  ggsave(file.path(out_dir, "r_feedback_boxplots_by_gender.png"), p_boxes_gender, width = 8.5, height = 7, dpi = 150)

  p_sa_g <- ggplot(active_g, aes(duration_sec, area_off_px2, color = mode)) +
    geom_point(size = 2.8, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.6) +
    facet_wrap(vars(gender_f), ncol = 2) +
    scale_color_manual(
      values = c("numerical" = "#4a36c0", "spatial-color" = "#18a050"),
      labels = c("numerical" = "Numerical", "spatial-color" = "Spatial-color")
    ) +
    labs(
      title = "Speed vs trace error (split by gender)",
      x = "Completion time (s)",
      y = expression("Area off target (" * px^2 * ")"),
      color = "Feedback type"
    ) +
    theme_bw(base_size = 12)

  rp(p_sa_g)
  ggsave(file.path(out_dir, "r_speed_area_scatter_by_gender.png"), p_sa_g, width = 8, height = 4.5, dpi = 150)

  assump_g <- active_g %>%
    mutate(
      condition = factor(
        mode,
        levels = c("numerical", "spatial-color"),
        labels = c("Numerical", "Spatial-color")
      )
    ) %>%
    transmute(
      gender_f,
      condition,
      `Time (s)` = duration_sec,
      `Area off target (px^2)` = area_off_px2
    ) %>%
    pivot_longer(
      c(`Time (s)`, `Area off target (px^2)`),
      names_to = "outcome",
      values_to = "value"
    )

  lay_qq_g <- mie286_qq_pts_line(assump_g, c("gender_f", "condition", "outcome"))
  p_qq_gender <- ggplot(lay_qq_g$pts, aes(theoretical, sample)) +
    geom_point(alpha = 0.6, na.rm = TRUE) +
    geom_abline(
      aes(intercept = intercept, slope = slope),
      data = lay_qq_g$line,
      color = "#b22222",
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    facet_grid(rows = vars(condition, outcome), cols = vars(gender_f), scales = "free") +
    labs(
      title = "Q-Q plots by gender (normality within subgroup)",
      subtitle = "Rows: feedback type and outcome; columns: Women vs Men",
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    theme_bw(base_size = 10) +
    theme(strip.text = element_text(size = 7.5))

  rp(p_qq_gender)
  ggsave(file.path(out_dir, "r_check_qq_by_gender.png"), p_qq_gender, width = 9, height = 9, dpi = 150)

  cat(
    "Saved gender-stratified figures: r_feedback_boxplots_by_gender.png, ",
    "r_speed_area_scatter_by_gender.png, r_check_qq_by_gender.png\n",
    sep = ""
  )

  cat("\n--- Hypothesis tests: gender (Women vs Men, exploratory) ---\n")
  cat("Welch two-sample t: independent participants, H0: equal means.\n")
  cat("Assumption: outcome roughly normal within each group (Shapiro–Wilk below); Welch is somewhat robust.\n")
  cat("See group means in output; factor levels are Women, then Men.\n\n")
  for (md in c("numerical", "spatial-color")) {
    dg <- active_g %>% dplyr::filter(.data$mode == md)
    tab_g <- table(dg$gender_f)
    if (length(tab_g) >= 2L && min(tab_g) >= 2L) {
      cat("Mode: ", md, " — Shapiro–Wilk normality within gender (duration, s)\n", sep = "")
      shapiro_or_skip_subgroup(dg$duration_sec[dg$gender_f == "Women"], "  Women")
      shapiro_or_skip_subgroup(dg$duration_sec[dg$gender_f == "Men"], "  Men")
      cat("Mode: ", md, " — Welch t duration (s)\n", sep = "")
      mie286_print_htest_no_ci(stats::t.test(duration_sec ~ gender_f, data = dg, var.equal = FALSE))
      cat("Mode: ", md, " — Shapiro–Wilk normality within gender (area off target)\n", sep = "")
      shapiro_or_skip_subgroup(dg$area_off_px2[dg$gender_f == "Women"], "  Women")
      shapiro_or_skip_subgroup(dg$area_off_px2[dg$gender_f == "Men"], "  Men")
      cat("Mode: ", md, " — Welch t area (px^2)\n", sep = "")
      mie286_print_htest_no_ci(stats::t.test(area_off_px2 ~ gender_f, data = dg, var.equal = FALSE))
      cat("\n")
    } else {
      cat("Mode: ", md, " — skipped t-tests (need ≥2 per gender; counts: ", paste(names(tab_g), tab_g, collapse = ", "), ")\n\n", sep = "")
    }
  }

  pg <- paired_g %>%
    dplyr::filter(!is.na(.data$gender_f)) %>%
    dplyr::mutate(
      diff_time = `duration_sec___numerical` - `duration_sec___spatial-color`,
      diff_area = `area_off_px2___numerical` - `area_off_px2___spatial-color`
    )
  tab_pg <- table(pg$gender_f)
  if (length(tab_pg) >= 2L && min(tab_pg) >= 2L) {
    cat("Participant-level paired difference (numerical − spatial), by gender:\n")
    cat("Shapiro–Wilk on diff_time within each gender\n")
    shapiro_or_skip_subgroup(pg$diff_time[pg$gender_f == "Women"], "  Women")
    shapiro_or_skip_subgroup(pg$diff_time[pg$gender_f == "Men"], "  Men")
    cat("diff_time (s) — Welch t\n")
    mie286_print_htest_no_ci(stats::t.test(diff_time ~ gender_f, data = pg, var.equal = FALSE))
    cat("Shapiro–Wilk on diff_area within each gender\n")
    shapiro_or_skip_subgroup(pg$diff_area[pg$gender_f == "Women"], "  Women")
    shapiro_or_skip_subgroup(pg$diff_area[pg$gender_f == "Men"], "  Men")
    cat("diff_area (px^2) — Welch t\n")
    mie286_print_htest_no_ci(stats::t.test(diff_area ~ gender_f, data = pg, var.equal = FALSE))
  } else {
    cat("Paired difference by gender: skipped (need ≥2 per gender).\n")
  }

  cat("\nPearson r (time vs area), by gender and feedback mode:\n")
  cat("(Univariate Shapiro–Wilk on duration and area; Pearson assumes linearity / approximate bivariate normality.)\n")
  for (gf in c("Women", "Men")) {
    for (md in c("numerical", "spatial-color")) {
      dg2 <- active_g %>% dplyr::filter(.data$gender_f == gf, .data$mode == md)
      if (nrow(dg2) >= 3L) {
        cat(gf, ", ", md, " — Shapiro–Wilk\n", sep = "")
        shapiro_or_skip_subgroup(dg2$duration_sec, "  duration (s)")
        shapiro_or_skip_subgroup(dg2$area_off_px2, "  area (px^2)")
        cat(gf, ", ", md, " — Pearson correlation\n", sep = "")
        mie286_print_htest_no_ci(stats::cor.test(dg2$duration_sec, dg2$area_off_px2, method = "pearson"))
      } else {
        cat(gf, ", ", md, ": skipped (n < 3)\n", sep = "")
      }
    }
  }
  cat("\n")
} else {
  message(
    "Skipping gender-stratified plots: fill participant_demographics.csv with woman/man ",
    "(or f/m) and rerun build_mie286_vectors.R, or add gender lines to trial CSV exports."
  )
}

# --- Exploratory gaming split: median h/day into low vs high; Welch t, Q-Q, r ---
hours_vec <- paired_complete$avg_gaming_hours_per_day
med_game <- stats::median(hours_vec, na.rm = TRUE)
n_ok_game <- sum(!is.na(hours_vec))
n_distinct_game <- length(unique(stats::na.omit(hours_vec)))

if (n_ok_game >= 4L && n_distinct_game >= 2L && is.finite(med_game)) {
  lg <- "Time spent gaming (~0 hours/day)"
  hg <- "Time spent gaming (>0 hours/day)"

  paired_game <- paired_complete %>%
    mutate(
      gaming_f = factor(
        dplyr::case_when(
          is.na(.data$avg_gaming_hours_per_day) ~ NA_character_,
          .data$avg_gaming_hours_per_day <= med_game ~ lg,
          TRUE ~ hg
        ),
        levels = c(lg, hg)
      )
    )

  active_game <- bind_rows(
    transmute(
      paired_game,
      participant,
      gaming_f,
      mode = "numerical",
      duration_sec = `duration_sec___numerical`,
      area_off_px2 = `area_off_px2___numerical`
    ),
    transmute(
      paired_game,
      participant,
      gaming_f,
      mode = "spatial-color",
      duration_sec = `duration_sec___spatial-color`,
      area_off_px2 = `area_off_px2___spatial-color`
    )
  ) %>%
    filter(!is.na(.data$gaming_f))

  if (nrow(active_game) >= 4L) {
    lab_x <- c("numerical" = "Numerical", "spatial-color" = "Spatial-color")
    fill_v <- c("numerical" = "#4a36c0", "spatial-color" = "#18a050")

    p_game_time <- ggplot(active_game, aes(mode, duration_sec, fill = mode)) +
      geom_boxplot(alpha = 0.85, outlier.shape = NA) +
      geom_jitter(width = 0.12, alpha = 0.7, size = 2) +
      facet_wrap(vars(gaming_f), nrow = 1) +
      scale_x_discrete(labels = lab_x) +
      scale_fill_manual(values = fill_v) +
      labs(title = "Time (s)", x = NULL, y = "Completion time (s)") +
      theme_bw(base_size = 12) +
      theme(legend.position = "none", strip.text = element_text(size = 10))

    p_game_area <- ggplot(active_game, aes(mode, area_off_px2, fill = mode)) +
      geom_boxplot(alpha = 0.85, outlier.shape = NA) +
      geom_jitter(width = 0.12, alpha = 0.7, size = 2) +
      facet_wrap(vars(gaming_f), nrow = 1) +
      scale_x_discrete(labels = lab_x) +
      scale_fill_manual(values = fill_v) +
      labs(
        title = "Area off target",
        x = "Feedback type",
        y = expression("Area (" * px^2 * ")")
      ) +
      theme_bw(base_size = 12) +
      theme(legend.position = "none", strip.text = element_text(size = 10))

    p_boxes_game <- (p_game_time / p_game_area) +
      plot_annotation(
        title = "Feedback type: time and accuracy (gaming vs non-gaming groups)",
        subtitle = "Self-reported typical hours playing games per day",
        theme = theme(
          plot.title = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(size = 10)
        )
      )

    rp(p_boxes_game)
    ggsave(file.path(out_dir, "r_feedback_boxplots_by_gaming.png"), p_boxes_game, width = 8.5, height = 7, dpi = 150)

    p_sa_game <- ggplot(active_game, aes(duration_sec, area_off_px2, color = mode)) +
      geom_point(size = 2.8, alpha = 0.8) +
      geom_smooth(method = "lm", se = TRUE, linewidth = 0.6) +
      facet_wrap(vars(gaming_f), ncol = 2) +
      scale_color_manual(
        values = c("numerical" = "#4a36c0", "spatial-color" = "#18a050"),
        labels = c("numerical" = "Numerical", "spatial-color" = "Spatial-color")
      ) +
      labs(
        title = "Speed vs trace error (split by daily gaming hours)",
        subtitle = "Self-reported typical hours playing games per day",
        x = "Completion time (s)",
        y = expression("Area off target (" * px^2 * ")"),
        color = "Feedback type"
      ) +
      theme_bw(base_size = 12)

    rp(p_sa_game)
    ggsave(file.path(out_dir, "r_speed_area_scatter_by_gaming.png"), p_sa_game, width = 8, height = 4.5, dpi = 150)

    assump_game <- active_game %>%
      mutate(
        condition = factor(
          mode,
          levels = c("numerical", "spatial-color"),
          labels = c("Numerical", "Spatial-color")
        )
      ) %>%
      transmute(
        gaming_f,
        condition,
        `Time (s)` = duration_sec,
        `Area off target (px^2)` = area_off_px2
      ) %>%
      pivot_longer(
        c(`Time (s)`, `Area off target (px^2)`),
        names_to = "outcome",
        values_to = "value"
      )

    lay_qq_game <- mie286_qq_pts_line(assump_game, c("gaming_f", "condition", "outcome"))
    p_qq_gaming <- ggplot(lay_qq_game$pts, aes(theoretical, sample)) +
      geom_point(alpha = 0.6, na.rm = TRUE) +
      geom_abline(
        aes(intercept = intercept, slope = slope),
        data = lay_qq_game$line,
        color = "#b22222",
        linewidth = 0.8,
        na.rm = TRUE
      ) +
      facet_grid(rows = vars(condition, outcome), cols = vars(gaming_f), scales = "free") +
      labs(
        title = "Q-Q plots by gaming group (normality within subgroup)",
        subtitle = "Median split on self-reported h/day; rows: feedback type and outcome",
        x = "Theoretical quantiles",
        y = "Sample quantiles"
      ) +
      theme_bw(base_size = 9) +
      theme(strip.text = element_text(size = 6.5))

    rp(p_qq_gaming)
    ggsave(file.path(out_dir, "r_check_qq_by_gaming.png"), p_qq_gaming, width = 10, height = 9, dpi = 150)

    cat(
      "Saved gaming-stratified figures: r_feedback_boxplots_by_gaming.png, ",
      "r_speed_area_scatter_by_gaming.png, r_check_qq_by_gaming.png\n",
      sep = ""
    )

    cat("\n--- Hypothesis tests: gaming (low vs high hours / day, exploratory) ---\n")
    cat("Groups split at sample median gaming hours.\n")
    cat("Welch two-sample t, H0: equal means; check Shapiro–Wilk within each gaming group first.\n\n")
    for (md in c("numerical", "spatial-color")) {
      dgm <- active_game %>% dplyr::filter(.data$mode == md)
      tab_m <- table(dgm$gaming_f)
      if (length(tab_m) >= 2L && min(tab_m) >= 2L) {
        glev <- levels(dgm$gaming_f)
        cat("Mode: ", md, " — Shapiro–Wilk normality within gaming group (duration, s)\n", sep = "")
        for (gl in glev) {
          shapiro_or_skip_subgroup(dgm$duration_sec[dgm$gaming_f == gl], paste0("  ", gl))
        }
        cat("Mode: ", md, " — Welch t duration (s)\n", sep = "")
        mie286_print_htest_no_ci(stats::t.test(duration_sec ~ gaming_f, data = dgm, var.equal = FALSE))
        cat("Mode: ", md, " — Shapiro–Wilk normality within gaming group (area)\n", sep = "")
        for (gl in glev) {
          shapiro_or_skip_subgroup(dgm$area_off_px2[dgm$gaming_f == gl], paste0("  ", gl))
        }
        cat("Mode: ", md, " — Welch t area (px^2)\n", sep = "")
        mie286_print_htest_no_ci(stats::t.test(area_off_px2 ~ gaming_f, data = dgm, var.equal = FALSE))
        cat("\n")
      } else {
        cat(
          "Mode: ", md, " — skipped t-tests (need ≥2 per gaming group; counts: ",
          paste(names(tab_m), tab_m, collapse = ", "), ")\n\n",
          sep = ""
        )
      }
    }

    pgame <- paired_game %>%
      dplyr::filter(!is.na(.data$gaming_f)) %>%
      dplyr::mutate(
        diff_time = `duration_sec___numerical` - `duration_sec___spatial-color`,
        diff_area = `area_off_px2___numerical` - `area_off_px2___spatial-color`
      )
    tab_pg2 <- table(pgame$gaming_f)
    if (length(tab_pg2) >= 2L && min(tab_pg2) >= 2L) {
      cat("Participant-level paired difference (numerical − spatial), by gaming group:\n")
      glev2 <- levels(pgame$gaming_f)
      cat("Shapiro–Wilk on diff_time within each gaming group\n")
      for (gl in glev2) {
        shapiro_or_skip_subgroup(pgame$diff_time[pgame$gaming_f == gl], paste0("  ", gl))
      }
      cat("diff_time (s) — Welch t\n")
      mie286_print_htest_no_ci(stats::t.test(diff_time ~ gaming_f, data = pgame, var.equal = FALSE))
      cat("Shapiro–Wilk on diff_area within each gaming group\n")
      for (gl in glev2) {
        shapiro_or_skip_subgroup(pgame$diff_area[pgame$gaming_f == gl], paste0("  ", gl))
      }
      cat("diff_area (px^2) — Welch t\n")
      mie286_print_htest_no_ci(stats::t.test(diff_area ~ gaming_f, data = pgame, var.equal = FALSE))
    } else {
      cat("Paired difference by gaming group: skipped (need ≥2 per group).\n")
    }

    cat("\nPearson r (time vs area), by gaming group and feedback mode:\n")
    cat("(Univariate Shapiro–Wilk on duration and area per subgroup.)\n")
    lg_lab <- levels(pgame$gaming_f)[1]
    hg_lab <- levels(pgame$gaming_f)[2]
    for (gf in c(lg_lab, hg_lab)) {
      for (md in c("numerical", "spatial-color")) {
        dgm2 <- active_game %>% dplyr::filter(.data$gaming_f == gf, .data$mode == md)
        if (nrow(dgm2) >= 3L) {
          cat(gf, ", ", md, " — Shapiro–Wilk\n", sep = "")
          shapiro_or_skip_subgroup(dgm2$duration_sec, "  duration (s)")
          shapiro_or_skip_subgroup(dgm2$area_off_px2, "  area (px^2)")
          cat(gf, ", ", md, " — Pearson correlation\n", sep = "")
          mie286_print_htest_no_ci(stats::cor.test(dgm2$duration_sec, dgm2$area_off_px2, method = "pearson"))
        } else {
          cat(gf, ", ", md, ": skipped (n < 3)\n", sep = "")
        }
      }
    }
    cat("\n")
  }
} else {
  message(
    "Skipping gaming-stratified plots: need at least 4 non-NA avg_gaming_hours_per_day ",
    "and 2 distinct values (check participant_demographics / data_mie286.R)."
  )
}

# --- Shapiro–Wilk: all strata (console + CSV) ---
mie286_sw_tbl_row <- function(x) {
  x <- as.numeric(stats::na.omit(x))
  nn <- length(x)
  if (nn < 3L || nn > 5000L) {
    return(tibble::tibble(
      n = nn,
      statistic_W = NA_real_,
      p_value_SW = NA_real_,
      note = if (nn < 3L) "n<3" else "n>5000"
    ))
  }
  sw <- stats::shapiro.test(x)
  tibble::tibble(
    n = nn,
    statistic_W = unname(sw$statistic),
    p_value_SW = sw$p.value,
    note = NA_character_
  )
}

sw_export_parts <- list()
sw_export_parts[[1]] <- shapiro_by_group %>%
  dplyr::mutate(
    layer = "main_condition_outcome",
    p_value_SW = .data$p_value
  ) %>%
  dplyr::select(
    layer,
    condition,
    outcome,
    n,
    statistic_W,
    p_value_SW
  )

sw_export_parts[[2]] <- dplyr::bind_rows(
  dplyr::bind_cols(
    tibble::tibble(
      layer = "paired_difference",
      subgroup = "whole_sample",
      variable = "diff_time_num_minus_spa"
    ),
    mie286_sw_tbl_row(diff_time)
  ),
  dplyr::bind_cols(
    tibble::tibble(
      layer = "paired_difference",
      subgroup = "whole_sample",
      variable = "diff_area_num_minus_spa"
    ),
    mie286_sw_tbl_row(diff_area)
  )
)

if (exists("active_g", inherits = TRUE) && nrow(active_g) >= 4L) {
  gr <- list()
  idx <- 0L
  for (gf in c("Women", "Men")) {
    for (md in c("numerical", "spatial-color")) {
      dg <- active_g %>% dplyr::filter(.data$gender_f == gf, .data$mode == md)
      idx <- idx + 1L
      gr[[idx]] <- dplyr::bind_cols(
        tibble::tibble(
          layer = "gender_mode",
          gender = gf,
          mode = md,
          variable = "duration_sec"
        ),
        mie286_sw_tbl_row(dg$duration_sec)
      )
      idx <- idx + 1L
      gr[[idx]] <- dplyr::bind_cols(
        tibble::tibble(
          layer = "gender_mode",
          gender = gf,
          mode = md,
          variable = "area_off_px2"
        ),
        mie286_sw_tbl_row(dg$area_off_px2)
      )
    }
  }
  sw_export_parts[[length(sw_export_parts) + 1L]] <- dplyr::bind_rows(gr)

  pg_sw <- paired_g %>%
    dplyr::filter(!is.na(.data$gender_f)) %>%
    dplyr::mutate(
      diff_time = `duration_sec___numerical` - `duration_sec___spatial-color`,
      diff_area = `area_off_px2___numerical` - `area_off_px2___spatial-color`
    )
  if (nrow(pg_sw) >= 4L && min(table(pg_sw$gender_f)) >= 2L) {
    gr2 <- list()
    j <- 0L
    for (gf in c("Women", "Men")) {
      j <- j + 1L
      gr2[[j]] <- dplyr::bind_cols(
        tibble::tibble(layer = "gender_paired_diff", gender = gf, variable = "diff_time"),
        mie286_sw_tbl_row(pg_sw$diff_time[pg_sw$gender_f == gf])
      )
      j <- j + 1L
      gr2[[j]] <- dplyr::bind_cols(
        tibble::tibble(layer = "gender_paired_diff", gender = gf, variable = "diff_area"),
        mie286_sw_tbl_row(pg_sw$diff_area[pg_sw$gender_f == gf])
      )
    }
    sw_export_parts[[length(sw_export_parts) + 1L]] <- dplyr::bind_rows(gr2)
  }
}

if (exists("active_game", inherits = TRUE) && nrow(active_game) >= 4L) {
  glev_sw <- levels(droplevels(active_game$gaming_f))
  grg <- list()
  ig <- 0L
  for (gl in glev_sw) {
    for (md in c("numerical", "spatial-color")) {
      dgm <- active_game %>% dplyr::filter(.data$gaming_f == gl, .data$mode == md)
      ig <- ig + 1L
      grg[[ig]] <- dplyr::bind_cols(
        tibble::tibble(
          layer = "gaming_mode",
          gaming_group = as.character(gl),
          mode = md,
          variable = "duration_sec"
        ),
        mie286_sw_tbl_row(dgm$duration_sec)
      )
      ig <- ig + 1L
      grg[[ig]] <- dplyr::bind_cols(
        tibble::tibble(
          layer = "gaming_mode",
          gaming_group = as.character(gl),
          mode = md,
          variable = "area_off_px2"
        ),
        mie286_sw_tbl_row(dgm$area_off_px2)
      )
    }
  }
  sw_export_parts[[length(sw_export_parts) + 1L]] <- dplyr::bind_rows(grg)

  if (exists("pgame", inherits = TRUE) && nrow(pgame) >= 4L && length(table(pgame$gaming_f)) >= 2L &&
        min(table(pgame$gaming_f)) >= 2L) {
    glev_p <- levels(pgame$gaming_f)
    gr3 <- list()
    k <- 0L
    for (gl in glev_p) {
      k <- k + 1L
      gr3[[k]] <- dplyr::bind_cols(
        tibble::tibble(layer = "gaming_paired_diff", gaming_group = as.character(gl), variable = "diff_time"),
        mie286_sw_tbl_row(pgame$diff_time[pgame$gaming_f == gl])
      )
      k <- k + 1L
      gr3[[k]] <- dplyr::bind_cols(
        tibble::tibble(layer = "gaming_paired_diff", gaming_group = as.character(gl), variable = "diff_area"),
        mie286_sw_tbl_row(pgame$diff_area[pgame$gaming_f == gl])
      )
    }
    sw_export_parts[[length(sw_export_parts) + 1L]] <- dplyr::bind_rows(gr3)
  }
}

shapiro_wilk_all_strata <- dplyr::bind_rows(sw_export_parts)
shapiro_csv <- file.path(out_dir, "shapiro_wilk_all_strata.csv")
utils::write.csv(shapiro_wilk_all_strata, shapiro_csv, row.names = FALSE)

cat("\n--- Shapiro–Wilk (all strata) — CSV:\n  ", normalizePath(shapiro_csv, winslash = "/"), "\n", sep = "")
print(as.data.frame(shapiro_wilk_all_strata), row.names = FALSE, right = FALSE)
cat("\n")

cat("Saved figures to ", normalizePath(out_dir, winslash = "/"), "\n", sep = "")
cat("\nDone!!\n")

# --- Console summary table + illustrative t-density (df fixed at 31; use n_pairs-1 in report) ---
cat("\n===============================================\n")
cat("FINAL PERFORMANCE SUMMARY (n =", nrow(paired_complete), ")\n")
cat("===============================================\n")

summary_stats <- active %>%
  group_by(mode) %>%
  summarise(
    avg_time = mean(duration_sec),
    avg_area = mean(area_off_px2),
    .groups = "drop"
  )

# Extract values for easy printing
num_time <- summary_stats$avg_time[summary_stats$mode == "numerical"]
spa_time <- summary_stats$avg_time[summary_stats$mode == "spatial-color"]
num_area <- summary_stats$avg_area[summary_stats$mode == "numerical"]
spa_area <- summary_stats$avg_area[summary_stats$mode == "spatial-color"]

cat(sprintf("Average Time (Numerical):      %.2f seconds\n", num_time))
cat(sprintf("Average Time (Spatial):        %.2f seconds\n", spa_time))
cat("-----------------------------------------------\n")
cat(sprintf("Average Accuracy (Numerical):  %.2f px^2\n", num_area))
cat(sprintf("Average Accuracy (Spatial):    %.2f px^2\n", spa_area))
cat("===============================================\n")

# Finds the T-score that cuts off the top 2.5% and bottom 2.5%
qt(p = 0.025, df = 31, lower.tail = FALSE) 
# Result: ~2.04. If your t-score > 2.04, your p-value will be < 0.05!

# Define the file path in your graphs directory
t_dist_path <- file.path(out_dir, "r_t_distribution_df31.png")

# 1. Open the PNG device
# We set the width, height, and res to match your other plots
png(t_dist_path, width = 7, height = 4.5, units = "in", res = 150)

# 2. Adjust margins to avoid the "margins too large" error
par(mar = c(4.5, 4.5, 3, 1))

# 3. Create the T-distribution data (df = 31 for your 32 participants)
x_vals <- seq(-4, 4, length = 200)
y_vals <- dt(x_vals, df = 31)

# 4. Draw the plot
plot(x_vals, y_vals, type = "l", lwd = 2.5, col = "#3d4f73",
     main = "T-Distribution for Paired Comparison (df = 31)",
     xlab = "t-statistic", 
     ylab = "Density",
     frame.plot = FALSE)

# 5. Add Critical Value lines (alpha = 0.05, two-tailed)
crit_t <- qt(0.025, df = 31, lower.tail = FALSE)
abline(v = c(-crit_t, crit_t), col = "#b22222", lty = 2, linewidth = 1.2)

# Optional: Add a text label for the critical value
text(x = crit_t + 0.5, y = 0.35, labels = paste0("Crit t = ", round(crit_t, 2)), col = "#b22222", cex = 0.8)

# 6. Close the device to save the file
dev.off()

cat("Saved T-distribution plot to: ", normalizePath(t_dist_path, winslash = "/"), "\n")
