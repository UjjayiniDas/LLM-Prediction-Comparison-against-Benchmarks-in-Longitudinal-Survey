## Creating Plots for Paper ##

### Plot 1: Wave wise F1 trajectory for majority class (SNAP = No) for different models ###

library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)
font_add_google("Caladea", "caladea")
showtext_auto()

# Data 
df <- tibble::tribble(
  ~Wave, ~Model,                ~F1,
  1,     "Logistic Regression", 0.941,
  1,     "Random Forest",       0.938,
  1,     "XGBoost",             0.944,
  1,     "Qwen2.5",             0.940,
  2,     "Logistic Regression", 0.974,
  2,     "Random Forest",       0.959,
  2,     "XGBoost",             0.974,
  2,     "Qwen2.5",             0.940,
  3,     "Logistic Regression", 0.978,
  3,     "Random Forest",       0.974,
  3,     "XGBoost",             0.978,
  3,     "Qwen2.5",             0.980,
  4,     "Logistic Regression", 0.978,
  4,     "Random Forest",       0.976,
  4,     "XGBoost",             0.980,
  4,     "Qwen2.5",             0.980
)

# Fix factor order for legend
df$Model <- factor(df$Model,
                   levels = c("Qwen2.5",
                              "Logistic Regression",
                              "Random Forest",
                              "XGBoost"))

#  Palette
# Using manual colors from the Okabe-Ito palette
model_colors <- c(
  "Qwen2.5"             = "#E69F00",
  "Logistic Regression" = "#0072B2",
  "Random Forest"       = "#009E73",
  "XGBoost"             = "#D55E00"
)

model_shapes <- c(
  "Qwen2.5"             = 17,  # filled triangle
  "Logistic Regression" = 16,  # filled circle
  "Random Forest"       = 15,  # filled square
  "XGBoost"             = 18   # filled diamond
)

# Plot
p <- ggplot(df, aes(x = Wave, y = F1,
                    color = Model,
                    shape = Model,
                    group = Model)) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 1:4,
    labels = c("Wave 1\n(n = 9,894)", "Wave 2\n(n = 10,263)", "Wave 3\n(n = 7,337)", "Wave 4\n(n = 5,512)")
  ) +
  scale_y_continuous(
    limits = c(0.92, 0.99),
    breaks = seq(0.92, 0.99, by = 0.01),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  scale_color_manual(values = model_colors) +
  scale_shape_manual(values = model_shapes) +
  labs(
    x     = NULL,
    y     = "F1 score (SNAP = No)",
    color = "Model",
    shape = "Model"
  ) +
  theme_classic(base_size = 11, base_family = "caladea") +
  theme(
    legend.position      = "bottom",
    legend.title         = element_blank(),
    legend.text          = element_text(size = 10),
    legend.key.width     = unit(1, "cm"),
    axis.text.x = element_text(size = 10, lineheight = 1.2),
    axis.text.y          = element_text(size = 10),
    axis.title.y         = element_text(size = 11, margin = margin(r = 8)),
    panel.grid.major.y   = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.major.x   = element_blank()
  )

# Save
ggsave("majority_f1_trajectory.pdf", plot = p,
       width = 6, height = 4, units = "in", dpi = 300)

ggsave("majority_f1_trajectory.png", plot = p,
       width = 6, height = 4, units = "in", dpi = 300)


### Plot 2: Metrics trajectory for minority class over waves (imbalanced full model) ###

#  Data 
df2 <- tibble::tribble(
  ~Wave, ~Model,                  ~Precision, ~Recall, ~F1,
  1, "Logistic Regression",   0.618, 0.240, 0.345,
  2, "Logistic Regression",   0.844, 0.763, 0.802,
  3, "Logistic Regression",   0.840, 0.804, 0.821,
  4, "Logistic Regression",   0.834, 0.777, 0.805,
  1, "Random Forest",         0.691, 0.022, 0.042,
  2, "Random Forest",         0.887, 0.454, 0.601,
  3, "Random Forest",         0.854, 0.713, 0.778,
  4, "Random Forest",         0.843, 0.737, 0.787,
  1, "XGBoost",               0.662, 0.289, 0.402,
  2, "XGBoost",               0.851, 0.767, 0.807,
  3, "XGBoost",               0.830, 0.821, 0.825,
  4, "XGBoost",               0.839, 0.811, 0.825,
  1, "Qwen2.5 (Structured)",  0.620, 0.210, 0.320,
  2, "Qwen2.5 (Structured)",  0.750, 0.130, 0.220,
  3, "Qwen2.5 (Structured)",  0.810, 0.840, 0.830,
  4, "Qwen2.5 (Structured)",  0.860, 0.760, 0.810,
  1, "Qwen2.5 (Narrative)",   0.230, 0.290, 0.370,
  2, "Qwen2.5 (Narrative)",   0.840, 0.790, 0.810,
  3, "Qwen2.5 (Narrative)",   0.810, 0.840, 0.830,
  4, "Qwen2.5 (Narrative)",   0.840, 0.740, 0.790
)

model_levels <- c("Logistic Regression", "Random Forest", "XGBoost",
                  "Qwen2.5 (Structured)", "Qwen2.5 (Narrative)")

wave_labels <- c("Wave 1\n(n=1,332)", "Wave 2\n(n=1,440)",
                 "Wave 3\n(n=938)",   "Wave 4\n(n=655)")

df_long <- df2 %>%
  pivot_longer(cols = c(Precision, Recall, F1),
               names_to  = "Metric",
               values_to = "Value") %>%
  mutate(
    Model  = factor(Model,  levels = model_levels),
    Metric = factor(Metric, levels = c("Precision", "Recall", "F1"))
  )

#  Aesthetics 
model_colors <- c(
  "Logistic Regression"  = "#0072B2",
  "Random Forest"        = "#009E73",
  "XGBoost"              = "#D55E00",
  "Qwen2.5 (Structured)" = "#E69F00",
  "Qwen2.5 (Narrative)"  = "#CC79A7"
)
model_shapes <- c(
  "Logistic Regression"  = 16,
  "Random Forest"        = 15,
  "XGBoost"              = 17,
  "Qwen2.5 (Structured)" = 18,
  "Qwen2.5 (Narrative)"  = 8
)
model_linetypes <- c(
  "Logistic Regression"  = "solid",
  "Random Forest"        = "solid",
  "XGBoost"              = "solid",
  "Qwen2.5 (Structured)" = "dashed",
  "Qwen2.5 (Narrative)"  = "dotdash"
)

# --------------- F1 only --------------------------------------
df2_f1 <- df_long %>% filter(Metric == "F1")

p2_f1 <- ggplot(df2_f1,
               aes(x = Wave, y = Value,
                   color = Model, shape = Model,
                   linetype = Model, group = Model)) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 3.5) +
  scale_x_continuous(breaks = 1:4, labels = wave_labels) +
  scale_y_continuous(limits = c(0, 0.9),
                     breaks = seq(0, 0.9, 0.1),
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_color_manual(values = model_colors, breaks = model_levels) +
  scale_shape_manual(values = model_shapes, breaks = model_levels) +
  scale_linetype_manual(values = model_linetypes, breaks = model_levels) +
  labs(x = NULL, y = "F1 Score (SNAP = Yes)",
       color = NULL, shape = NULL, linetype = NULL) +
  theme_classic(base_size = 12, base_family = "caladea") +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 10),
    legend.key.width   = unit(1.5, "cm"),
    legend.spacing.x   = unit(0.3, "cm"),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.35),
    axis.text.x        = element_text(size = 9, lineheight = 1.3),
    axis.text.y        = element_text(size = 9),
    axis.title.y       = element_text(size = 10, margin = margin(r = 8))
  )

ggsave("f1_trajectory_main.pdf", plot = p2_f1,
       width = 7, height = 4.5, units = "in", dpi = 300)
ggsave("f1_trajectory_main.png", plot = p2_f1,
       width = 7, height = 4.5, units = "in", dpi = 300)


# ------ All three metrics — facet by Metric only, color by Model -----------------
p2_all <- ggplot(df_long,
             aes(x        = Wave,
                 y        = Value,
                 color    = Model,
                 shape    = Model,
                 linetype = Model,
                 group    = Model)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 3) +
  facet_wrap(~ Metric, ncol = 3, scales = "free_y") +
  scale_x_continuous(
    breaks = 1:4,
    labels = wave_labels
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_color_manual(values    = model_colors,    breaks = model_levels) +
  scale_shape_manual(values    = model_shapes,    breaks = model_levels) +
  scale_linetype_manual(values = model_linetypes, breaks = model_levels) +
  labs(
    x        = NULL,
    y        = "Score (SNAP = Yes)",
    color    = NULL,
    shape    = NULL,
    linetype = NULL
  ) +
  theme_classic(base_size = 11, base_family = "caladea") +
  theme(
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 11),
    legend.position    = "bottom",
    legend.text        = element_text(size = 10),
    legend.key.width   = unit(1.2, "cm"),
    panel.spacing      = unit(1.2, "lines"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.35),
    axis.text.x        = element_text(size = 8, lineheight = 1.2),
    axis.text.y        = element_text(size = 8),
    axis.title.y       = element_text(size = 9, margin = margin(r = 8))
  )
ggsave("metrics_trajectory_all_minority.pdf", plot = p2_all,
       width = 11, height = 4.5, units = "in", dpi = 300)
ggsave("metrics_trajectory_all.png", plot = p2_all,
       width = 11, height = 4.5, units = "in", dpi = 300)

### Plot 3: ECE plots ###


#  Data 
df3 <- data.frame(
  wave = c(
    1,1,1,1,
    2,2,2,2,2,2,2,
    3,3,3,3,3,3,3,
    4,4,4,4,4,4,4
  ),
  condition = c(
    "Imbalanced (Full Model)", "Oversampled", "Zero-shot", "Narrative Prompt",
    "Imbalanced (Full Model)", "Oversampled", "Demo Only",
    "Longitudinal Only", "Zero-shot", "Narrative Prompt", "Minimal Imbalanced",
    "Imbalanced (Full Model)", "Oversampled", "Demo Only",
    "Longitudinal Only", "Zero-shot", "Narrative Prompt", "Minimal Imbalanced",
    "Imbalanced (Full Model)", "Oversampled", "Demo Only",
    "Longitudinal Only", "Zero-shot", "Narrative Prompt", "Minimal Imbalanced"
  ),
  ECE = c(
    0.093, 0.064, 0.119, 0.085,
    0.107, 0.044, 0.107, 0.026, 0.096, 0.026, 0.018,
    0.018, 0.012, 0.068, 0.018, 0.007, 0.018, 0.018,
    0.025, 0.013, 0.091, 0.031, 0.013, 0.028, 0.032
  )
)

#  Facet assignment 
df3_f1 <- df3 |>
  filter(condition %in% c("Zero-shot", "Imbalanced (Full Model)")) |>
  mutate(facet = "Training Regime\n(Zero-shot vs. Fine-tuned)")

df3_f2 <- df3 |>
  filter(condition %in% c("Narrative Prompt", "Imbalanced (Full Model)")) |>
  mutate(facet = "Prompt Format\n(Structured vs. Narrative)")

df3_f3 <- df3 |>
  filter(condition %in% c("Demo Only", "Longitudinal Only",
                          "Imbalanced (Full Model)", "Minimal Imbalanced")) |>
  mutate(facet = "Feature Configuration")

# Wave 1: Demo Only = same ECE as Imbalanced (Full); Minimal = 0.118
# Longitudinal Only has no Wave 1 entry (starts Wave 2)
wave1_additions <- data.frame(
  wave      = c(1, 1),
  condition = c("Demo Only", "Minimal Imbalanced"),
  ECE       = c(0.093, 0.118),
  facet     = "Feature Configuration"   
)

df3_f3 <- bind_rows(df3_f3, wave1_additions)

df3_f4 <- df3 |>
  filter(condition %in% c("Imbalanced (Full Model)", "Oversampled")) |>
  mutate(facet = "Class Balance\n(Imbalanced vs. Oversampled)")

df3_plot <- bind_rows(df3_f1, df3_f2, df3_f3, df3_f4) |>
  mutate(
    facet = factor(facet, levels = c(
      "Training Regime\n(Zero-shot vs. Fine-tuned)",
      "Prompt Format\n(Structured vs. Narrative)",
      "Feature Configuration",
      "Class Balance\n(Imbalanced vs. Oversampled)"
    )),
    condition = factor(condition, levels = c(
      "Imbalanced (Full Model)", "Zero-shot", "Oversampled",
      "Narrative Prompt", "Demo Only", "Longitudinal Only", "Minimal Imbalanced"
    ))
  )

#  Colorblind-safe palette (Wong 2011) + matched linetypes 
pal <- c(
  "Imbalanced (Full Model)" = "#0072B2",
  "Zero-shot"               = "#D55E00",
  "Oversampled"             = "#009E73",
  "Narrative Prompt"        = "#CC79A7",
  "Demo Only"               = "#E69F00",
  "Longitudinal Only"       = "#56B4E9",
  "Minimal Imbalanced"      = "#999999"
)

lty <- c(
  "Imbalanced (Full Model)" = "solid",
  "Zero-shot"               = "dashed",
  "Oversampled"             = "dashed",
  "Narrative Prompt"        = "dashed",
  "Demo Only"               = "dotted",
  "Longitudinal Only"       = "dotdash",
  "Minimal Imbalanced"      = "longdash"
)

#  Plot 
p3 <- ggplot(df3_plot, aes(x = wave, y = ECE,
                           color = condition, linetype = condition,
                           group = condition)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red",
             linewidth = 0.6, alpha = 0.5) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 2.2, shape = 16) +
  scale_color_manual(values = pal, name = "Condition") +
  scale_linetype_manual(values = lty, name = "Condition") +
  scale_x_continuous(breaks = 1:4, labels = paste0("Wave ", 1:4)) +
  scale_y_continuous(limits = c(0, 0.135),
                     breaks = seq(0, 0.12, by = 0.03),
                     labels = scales::number_format(accuracy = 0.001)) +
  facet_wrap(~ facet, ncol = 2, scales = "fixed") +
  labs(
    x       = NULL,
    y       = "Expected Calibration Error (ECE)",
    title   = "LLM Calibration Across Waves by Experimental Conditions",
    caption = "Reference line at ECE = 0 denotes perfect calibration."
  ) +
  theme_bw(base_size = 11, base_family = "caladea") +
  theme(
    strip.background = element_rect(fill = "#f0f0f0", color = "grey60"),
    strip.text       = element_text(face = "bold", size = 9.5),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 9),
    legend.text      = element_text(size = 8.5),
    legend.key.width = unit(1.4, "cm"),
    axis.text.x      = element_text(angle = 30, hjust = 1, size = 8.5),
    axis.title.y     = element_text(size = 10),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.caption     = element_text(size = 8, color = "grey40", hjust = 0)
  ) +
  guides(
    color    = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

ggsave("ece_calibration_facets.pdf", plot = p3,
       width = 9, height = 7, dpi = 300, bg = "white")

### Plot 4: Imbalance Correction Comparison (Figure 4) ###
df4 <- data.frame(
  model = rep(c("Logistic Regression", "Random Forest", "XGBoost", "LLM"), each = 4),
  wave  = rep(c("Wave 1", "Wave 2", "Wave 3", "Wave 4"), times = 4),
  imbalanced = c(0.345, 0.802, 0.821, 0.805,
                 0.042, 0.601, 0.778, 0.787,
                 0.402, 0.807, 0.825, 0.825,
                 0.320, 0.220, 0.830, 0.810),
  corrected  = c(0.462, 0.743, 0.781, 0.788,
                 0.460, 0.702, 0.782, 0.795,
                 0.497, 0.752, 0.804, 0.802,
                 0.440, 0.540, 0.790, 0.800),
  delta      = c(+11.7, -5.9, -4.0, -1.6,
                 +41.8, +10.1, +0.5, +0.9,
                 +9.4,  -5.4, -2.2, -2.2,
                 +12.0, +32.0, -4.0, -1.0)
) |>
  mutate(
    wave  = factor(wave,  levels = c("Wave 1","Wave 2","Wave 3","Wave 4")),
    model = factor(model, levels = c("Logistic Regression", "Random Forest", "XGBoost", "LLM")),
    delta_label = ifelse(delta > 0, paste0("+", delta, "pp"), paste0(delta, "pp")),
    delta_sign  = ifelse(delta > 0, "positive", "negative"),
    y_top = pmax(imbalanced, corrected) + 0.055
  )

# Pivot to long for geom_col
df_long4 <- df4 |>
  pivot_longer(cols = c(imbalanced, corrected),
               names_to  = "condition",
               values_to = "F1") |>
  mutate(condition = factor(condition,
                            levels = c("imbalanced", "corrected"),
                            labels = c("Imbalanced", "Corrected")))


p4 <- ggplot(df_long4, aes(x = wave, y = F1, fill = condition)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +

  # Delta annotations
  geom_text(
    data = df4,
    aes(x = wave, y = y_top, label = delta_label, color = delta_sign),
    inherit.aes = FALSE,
    size = 3, fontface = "bold", hjust = 0.5
  ) +

  scale_fill_manual(
    values = c("Imbalanced" = "#a8c4e0", "Corrected" = "#1f6fa8"),
    name   = "Training condition"
  ) +
  scale_color_manual(
    values = c("positive" = "#1a7d3a", "negative" = "#c0392b"),
    guide  = "none"
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, by = 0.2),
    labels = scales::number_format(accuracy = 0.1)
  ) +

  facet_wrap(~ model, ncol = 2) +

  labs(
    x       = NULL,
    y       = "F1 Score",
    title   = "F1 Score: Imbalanced vs. Corrected Training by Model and Wave",
    caption = "Annotations show difference in F1 in percentage points.\nGreen = net gain from correction; red = imbalanced model yields higher F1.\nCorrection method: ROSE (LR, RF, LLM); class-weighted loss (XGBoost)."
  ) +

  theme_bw(base_size = 11, base_family = "caladea") +
  theme(
    strip.background  = element_rect(fill = "#f0f0f0", color = "grey60"),
    strip.text        = element_text(face = "bold", size = 10),
    legend.position   = "bottom",
    legend.title      = element_text(face = "bold", size = 9),
    legend.text       = element_text(size = 9),
    axis.text.x       = element_text(angle = 30, hjust = 1, size = 8.5),
    axis.title.y      = element_text(size = 10),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title        = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.caption      = element_text(size = 7.5, color = "black", hjust = 0)
  )

ggsave("imbalance_f1_facets.pdf", plot = p4,
       width = 8, height = 6, dpi = 300, bg = "white")
