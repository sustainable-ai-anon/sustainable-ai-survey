# ------------------------------------------------------------------------------
# Script: 01_figure3_likert_distribution.R
# Purpose: Creates Figure 3 (diverging Likert plot + no opinion panel)
# Input: data/Analysis for R.csv
# Output: Figure 3
# ------------------------------------------------------------------------------
# ---- Load necessary libraries ----
library(tidyverse)
library(patchwork) # For combining plots

# ---- Load your CSV ----
SusAIsurvey <- read.csv("Analysis for R.csv", dec = ".", fileEncoding = "latin1")

# ---- Rename columns ----
colnames(SusAIsurvey) <- c(
  "ID", "SustainableAI_Definition", "AI_EnvImpact_PublicDebate", "AI_NegEnvImpact_10yrs",
  "Awareness", "Incentive", "TeachEfficientCode", "LostInMetrics", "ConflictGoals",
  "RestrictedFreedom", "Optional_Comments", "Age", "Gender", "AI_Researcher_Job", "Title_Education"
)

# ---- Define order of questions and colors ----
likert_cols <- c(
  "RestrictedFreedom", "ConflictGoals", "LostInMetrics",
  "TeachEfficientCode", "Incentive", "Awareness"
)

colors <- c(
  "Strongly disagree" = "#d73027",
  "Disagree"          = "#fc8d59",
  "Agree"             = "#a6d96a",
  "Strongly agree"    = "#1a9850",
  "no opinion"        = "grey80"
)

question_labels <- c(
  Awareness         = "... would increase my awareness of the \nenvironmental impact of my work",
  Incentive         = "... would be an incentive to use less \ncomputing resources",
  TeachEfficientCode= "... would - over time - teach me how to \ncreate more energy efficient models",
  LostInMetrics     = "... would get lost among all the other metrics \nI receive after training a model",
  ConflictGoals     = "... would make me feel a conflict between energy efficiency \nand my personal or academic goals and tasks",
  RestrictedFreedom = "... would make me feel restricted in my \nacademic freedom"
)

# ---- Data Preparation (Factorizing and Pivoting) ----
likert_items <- SusAIsurvey[, likert_cols]
levels_order <- c("Strongly disagree", "Disagree", "no opinion", "Agree", "Strongly agree")
likert_items <- as.data.frame(
  lapply(likert_items, function(x) factor(x, levels = levels_order))
)

df_long <- likert_items %>%
  pivot_longer(
    cols = everything(),
    names_to = "Category",
    values_to = "Response"
  )

# ---- Calculate percentages for ALL categories ----
df_summary <- df_long %>%
  filter(!is.na(Response)) %>%
  group_by(Category, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Category) %>%
  mutate(Percent = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(Category = factor(Category,
                           levels = likert_cols,
                           labels = question_labels[likert_cols]))

# ---- Create the data for the DIVERGING plot ----
dat_diverging <- df_summary %>%
  filter(Response %in% c("Strongly disagree", "Disagree", "Agree", "Strongly agree")) %>%
  mutate(Response = factor(Response, levels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"))) %>%
  arrange(Category, Response) %>%
  group_by(Category) %>%
  mutate(
    disagree_width = sum(Percent[Response %in% c("Strongly disagree", "Disagree")]),
    lagged_percent = lag(Percent, default = 0),
    left = cumsum(lagged_percent) - disagree_width,
    right = cumsum(Percent) - disagree_width,
    middle_point = (left + right) / 2,
    width = right - left
  ) %>%
  ungroup()

# ---- Create the data for the "NO OPINION" plot ----
dat_no_opinion <- df_summary %>%
  filter(Response == "no opinion") %>%
  # Ensure ALL categories are present, even if their count is 0
  tidyr::complete(Category, fill = list(Percent = 0))


# ---- Build the Diverging Plot ----
bar_height <- 0.8
diverging_plot <- ggplot(dat_diverging) +
  geom_tile(aes(x = middle_point, y = Category, width = width, fill = Response),
            height = bar_height) +
  geom_text(aes(x = middle_point, y = Category,
                # Only show label if the segment is reasonably large
                label = if_else(Percent > 2, paste0(round(Percent), "%"), "")),
            color = "white", size = 3.5, family = "sans") +
  geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
  scale_fill_manual(values = colors, name = "Response") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# ---- Build the "No Opinion" Plot ----
no_opinion_plot <- ggplot(dat_no_opinion) +
  geom_col(aes(x = Percent, y = Category), fill = colors["no opinion"], width = bar_height) +
  geom_text(aes(x = Percent, y = Category,
                label = if_else(Percent > 0, paste0(round(Percent), "%"), "")),
            hjust = -0.2, color = "grey20", size = 3.5, family = "sans") +
  labs(x = NULL, y = NULL, title = "No Opinion") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(color = "grey20")
  )


# ---- Synchronize axis limits and combine plots ----
max_abs_value <- dat_diverging %>%
  summarise(max_abs = max(abs(right), abs(left))) %>%
  pull(max_abs)

axis_limit <- max_abs_value * 1.1

diverging_plot <- diverging_plot + scale_x_continuous(limits = c(-axis_limit, axis_limit))
no_opinion_plot <- no_opinion_plot + scale_x_continuous(limits = c(0, axis_limit),
                                                        expand = expansion(mult = c(0, 0.1)))

# ---- Assemble the final plot ----
final_plot <- diverging_plot + no_opinion_plot +
  plot_layout(widths = c(2.5, 0.7), guides = 'collect') & # Adjusted ratio slightly
  theme(legend.position = 'bottom')

# ---- Display the final plot ----
final_plot