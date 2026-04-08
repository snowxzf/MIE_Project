# =============================================================================
# Post-experiment survey — qualitative summary charts (standalone)
# -----------------------------------------------------------------------------
# Aggregated counts hard-coded below (not read from CSV). Builds three ggplot
# panels (preference bar, learning pie, numerical sentiment bar) and saves:
#   qualitative_survey_results.png  (wd = code/ when run from project root)
# Run in RStudio or: Rscript post_survey_analysis.R
# =============================================================================

library(ggplot2)
library(patchwork)

# 1. Create Dataframes
pref_data <- data.frame(
  Modality = c("Spatial-Colour", "Numerical", "No Feedback", "Neutral"),
  Count = c(19, 7, 4, 2)
)

learn_data <- data.frame(
  Effect = c("Learned Curve", "Did Not Learn"),
  Count = c(22, 10)
)

sentiment_data <- data.frame(
  Sentiment = c("Distracting/Stressful", "Helpful/Clear", "Ignored/Neutral"),
  Count = c(16, 10, 8)
)

# 2. Theme Settings (Matching your report colors)
fill_colors <- c(
  "Spatial-Colour" = "#18a050", "Numerical" = "#4a36c0",
  "No Feedback" = "#3d4f73", "Neutral" = "#808080"
)

# 3. Bar Chart: Preference
p1 <- ggplot(pref_data, aes(x = reorder(Modality, -Count), y = Count, fill = Modality)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_manual(values = fill_colors) +
  labs(title = "User Preference by Modality", x = NULL, y = "Number of Participants") +
  theme_bw() + theme(legend.position = "none")

# 4. Pie Chart: Learning Effect
p2 <- ggplot(learn_data, aes(x = "", y = Count, fill = Effect)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(
    aes(label = paste0(round(Count / sum(Count) * 100), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(values = c("Learned Curve" = "#18a050", "Did Not Learn" = "#b22222")) +
  labs(title = "Perceived Learning Effect") +
  theme_void()

# 5. Bar Chart: Numerical Sentiment
p3 <- ggplot(sentiment_data, aes(x = reorder(Sentiment, -Count), y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity", fill = "#4a36c0", alpha = 0.7) +
  labs(title = "Sentiment: Numerical Feedback", x = NULL, y = "Count") +
  theme_bw()

# Combine using patchwork
qual_summary_plot <- (p1 + p2) / p3 +
  plot_annotation(
    title = "Iteration 2: Qualitative Survey Analysis",
    subtitle = "N=32 Engineering Science Participants"
  )

print(qual_summary_plot)
ggsave("qualitative_survey_results.png", qual_summary_plot, width = 8, height = 7)
