# MIE 286 (numerical vs spatial-color)
# Data: vectors in data_mie286.R (same participant order in each vector).
# Refresh vectors from CSV
# DV1 = duration_sec (s), DV2 = area_off_px2 (lower = better trace).

need <- c("ggplot2", "tidyr", "dplyr")
miss <- need[!vapply(need, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(miss)) {
  stop("Install: install.packages(c(", paste0('"', miss, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
  library(dplyr)
})

rp <- function(p) {
  print(p)
  invisible(p)
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

paired_complete <- tibble::tibble(
  participant = participant,
  `duration_sec___numerical` = duration_sec_numerical,
  `duration_sec___spatial-color` = duration_sec_spatial_color,
  `area_off_px2___numerical` = area_off_px2_numerical,
  `area_off_px2___spatial-color` = area_off_px2_spatial_color
)

# Only participants with complete pairs
active <- bind_rows(
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

out_dir <- file.path(proj_root, "graphs")
dir.create(out_dir, showWarnings = FALSE)

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

cat("Shapiro-Wilk by condition x outcome (paired t-tests use difference normality below)\n")
shapiro_by_group <- assump %>%
  group_by(condition, outcome) %>%
  group_modify(~ {
    v <- .x$value
    n <- length(v)
    if (n >= 3L && n <= 5000L) {
      sw <- stats::shapiro.test(v)
      tibble::tibble(n = n, statistic_W = unname(sw$statistic), p_value = sw$p.value)
    } else {
      tibble::tibble(n = n, statistic_W = NA_real_, p_value = NA_real_)
    }
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

cat("Wilcoxon signed-rank, paired = TRUE\n")
print(wilcox.test(
  paired_complete$`duration_sec___numerical`,
  paired_complete$`duration_sec___spatial-color`,
  paired = TRUE
))
print(wilcox.test(
  paired_complete$`area_off_px2___numerical`,
  paired_complete$`area_off_px2___spatial-color`,
  paired = TRUE
))
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
