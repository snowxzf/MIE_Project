# Sourced after mie286_load_data_and_active.R defines paired_complete, active, out_dir, rp.
if (!exists("RUN_SENSITIVITY_COMPARE", inherits = FALSE)) {
  RUN_SENSITIVITY_COMPARE <- TRUE
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

# Lilliefors = K-S-type normality test with mean & SD estimated from the sample;
# avoids invalid p-values from stats::ks.test(..., mean = mean(x), sd = sd(x)).
cat(
  "Normality by condition x outcome: Shapiro–Wilk + Lilliefors (K–S type); ",
  "paired t-tests use difference normality below\n",
  sep = ""
)
shapiro_by_group <- assump %>%
  group_by(condition, outcome) %>%
  group_modify(~ {
    v <- .x$value
    n <- length(v)
    statistic_W <- NA_real_
    p_value <- NA_real_
    statistic_D <- NA_real_
    p_value_ks <- NA_real_
    if (n >= 3L && n <= 5000L) {
      sw <- stats::shapiro.test(v)
      statistic_W <- unname(sw$statistic)
      p_value <- sw$p.value
    }
    if (n >= 4L && n <= 5000L) {
      lil <- nortest::lillie.test(v)
      statistic_D <- unname(lil$statistic)
      p_value_ks <- lil$p.value
    }
    tibble::tibble(
      n = n,
      statistic_W = statistic_W,
      p_value = p_value,
      statistic_D = statistic_D,
      p_value_ks = p_value_ks
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

# Paired differences
diff_time <- paired_complete$`duration_sec___numerical` - paired_complete$`duration_sec___spatial-color`
diff_area <- paired_complete$`area_off_px2___numerical` - paired_complete$`area_off_px2___spatial-color`

cat("PRIMARY assumption for paired t-tests: Shapiro-Wilk on paired differences\n")
if (length(diff_time) >= 3 && length(diff_time) <= 5000) print(shapiro.test(diff_time))
if (length(diff_area) >= 3 && length(diff_area) <= 5000) print(shapiro.test(diff_area))
cat("\n")

cat("Paired t-tests (two-tailed)\n")
tt_time <- t.test(
  paired_complete$`duration_sec___numerical`,
  paired_complete$`duration_sec___spatial-color`,
  paired = TRUE
)
print(tt_time)
tt_area <- t.test(
  paired_complete$`area_off_px2___numerical`,
  paired_complete$`area_off_px2___spatial-color`,
  paired = TRUE
)
print(tt_area)
cat("\n")

act_num <- active %>% filter(mode == "numerical")
act_spa <- active %>% filter(mode == "spatial-color")

cat("Pearson: duration vs area (Numerical)\n")
cor_num <- cor.test(act_num$duration_sec, act_num$area_off_px2, method = "pearson")
print(cor_num)
cat("\nPearson: duration vs area (Spatial-color)\n")
cor_spa <- cor.test(act_spa$duration_sec, act_spa$area_off_px2, method = "pearson")
print(cor_spa)
cat("\nPearson: Delta time vs Delta area\n")
cor_delta <- cor.test(diff_time, diff_area, method = "pearson")
print(cor_delta)
cat("\nSpearman (each mode)\n")
print(cor.test(act_num$duration_sec, act_num$area_off_px2, method = "spearman"))
print(cor.test(act_spa$duration_sec, act_spa$area_off_px2, method = "spearman"))
cat("\n")

if (isTRUE(RUN_SENSITIVITY_COMPARE)) {
# SENSITIVITY ANALYSIS: OUTLIERS 
# Purpose: robustness of paired t-tests and correlations after removing
#   participants flagged. Entire participant rows are dropped so paired structure is preserved.
# Note: Gender/gaming stratified sections later still use the FULL paired_complete.
# Set to TRUE to write r_feedback_*_no_outliers.png in graphs/
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
    paired = TRUE
  )
  tt_area_sens <- t.test(
    paired_complete_sens$`area_off_px2___numerical`,
    paired_complete_sens$`area_off_px2___spatial-color`,
    paired = TRUE
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
  "Paired t-test two-tailed p = %.4f%s\n",
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
  "Paired t-test two-tailed p = %.4f%s\n",
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

#gender stuff 
#maps the gender names to an acc gender (like puts f, female, woman, whatnot) to Women 
norm_gender_label <- function(g) {
  g <- tolower(trimws(as.character(g)))
  dplyr::case_when(
    g %in% c("f", "female", "woman", "w") ~ "Women",
    g %in% c("m", "male", "man") ~ "Men",
    TRUE ~ NA_character_
  )
}

#adds factor of gender to the person 
paired_g <- paired_complete %>%
  mutate(
    gender_f = factor(norm_gender_label(.data$gender), levels = c("Women", "Men"))
  )

#removes people without gender, binds together 
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

  cat(
    "Saved gender-stratified figures: r_feedback_boxplots_by_gender.png, ",
    "r_speed_area_scatter_by_gender.png\n",
    sep = ""
  )
} else {
  message(
    "Skipping gender-stratified plots: fill participant_demographics.csv with woman/man ",
    "(or f/m) and rerun build_mie286_vectors.R, or add gender lines to trial CSV exports."
  )
}

# Gaming hours: two groups from typical h/day
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

    cat(
      "Saved gaming-stratified figures: r_feedback_boxplots_by_gaming.png, ",
      "r_speed_area_scatter_by_gaming.png\n",
      sep = ""
    )
  }
} else {
  message(
    "Skipping gaming-stratified plots: need at least 4 non-NA avg_gaming_hours_per_day ",
    "and 2 distinct values (check participant_demographics / data_mie286.R)."
  )
}

cat("Saved figures to ", normalizePath(out_dir, winslash = "/"), "\n", sep = "")
cat("\nDone!!\n")

# --- Summary Statistics Report ---
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
