# ------------------------------------------------------------------------------
# Script: 02_Appendix - faceted_demographics_analysis.R
# Purpose: Generates faceted diverging stacked bar plots of Likert-scale responses segmented by demographic variables (Gender, Age, Education).
# Description:
# - Processes survey data and computes response distributions per subgroup
# - Creates faceted visualizations for:
#     1) Gender groups
#     2) Age groups
#     3) Highest educational level
# - Excludes "no opinion" responses in subgroup analyses
# Input: data/Analysis for R.csv
# Output: Faceted plots displayed in R (optionally saved to /figures)
# ------------------------------------------------------------------------------
 # ---- Load packages ----
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tidyverse) # G: otherwise pipe doesn't work for me
  
  # ---- Load your CSV ----
  SusAIsurvey <- read.csv("Analysis for R.csv", dec = ".", fileEncoding = "latin1")
  
  # ---- Rename columns ----
  colnames(SusAIsurvey) <- c(
    "ID",
    "SustainableAI_Definition",
    "AI_EnvImpact_PublicDebate",
    "AI_NegEnvImpact_10yrs",
    "Awareness",
    "Incentive",
    "TeachEfficientCode",
    "LostInMetrics",
    "ConflictGoals",
    "RestrictedFreedom",
    "Optional_Comments",
    "Age",
    "Gender",
    "AI_Researcher_Job",
    "Title_Education"
  )
  
  # ---- Recode Education Levels ----
  SusAIsurvey <- SusAIsurvey %>%
    mutate(
      Title_Education = case_when(
        Title_Education %in% c("PhD and group leader (lo habil)", "PhD") ~ "PhD",
        Title_Education %in% c("Bachelor","Master") ~ "Bachelor / Master",
        TRUE ~ Title_Education
      ),
      # optional: enforce order of facets
      Title_Education = factor(
        Title_Education,
        levels = c("Bachelor / Master", "PhD", "Habilitation") # adjust if needed
      )
    )
  
  # ---- Likert items ---- # G: Here we can change the order of the questions
  likert_cols <- c(
    "RestrictedFreedom", 
    "ConflictGoals",
    "LostInMetrics",
    "TeachEfficientCode",
    "Incentive",
    "Awareness"
  )
  
  # ---- Colors for each Likert level ----
  colors <- c(
    "Strongly disagree" = "#d73027",
    "Disagree"          = "#fc8d59",
    "Agree"             = "#a6d96a",
    "Strongly agree"    = "#1a9850", 
    "no opinion"        = "grey" # G: We define the colour for no opinion
  )
  
  question_labels <- c(
    Awareness         = "... would increase my awareness of the \nenvironmental impact of my work",
    Incentive         = "... would be an incentive to use less \ncomputing resources",
    TeachEfficientCode= "... would - over time - teach me how to \ncreate more energy efficient models",
    LostInMetrics     = "... would get lost among all the other metrics \nI receive after training a model",
    ConflictGoals     = "... would make me feel a conflict between energy efficiency \nand my personal or academic goals and tasks",
    RestrictedFreedom = "... would make me feel restricted in my \nacademic freedom"
  )
  
  likert_items <- SusAIsurvey[, likert_cols]
  
  # ---- Factorize with levels ----
  levels_order <- c("Strongly disagree", "Disagree", "Strongly agree", "Agree", "No opinion") # T: Changed order
  likert_items <- as.data.frame(
    lapply(likert_items, function(x) factor(x, levels = levels_order))
  )
  likert_items$id <- 1:nrow(likert_items)
  
  
  # ---- Pivot to long format ----
  df_long <- likert_items %>%
    pivot_longer(
      cols = -id,
      names_to = "Category",
      values_to = "Response"
    )
  
  # ---- Exclude "No opinion" ----
  #df_long <- df_long %>% filter(Response != "No opinion") # G: skip this now
  
  # ---- Set negative / positive for diverging ----
  df_long <- df_long %>%
    mutate(
      ResponseGroup = case_when(
        Response %in% c("Strongly disagree", "Disagree") ~ "Disagree",
        Response %in% c("Agree", "Strongly agree") ~ "Agree"
      )
    )
  
  # ---- Count percentages ----
  df_plot <- df_long %>%
    group_by(Category, Response) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Category) %>%
    mutate(Percent = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(Percent = ifelse(Response %in% c("Strongly disagree", "Disagree"),
                            -Percent, Percent)) %>%
    mutate(Category = factor(Category,
                             levels = likert_cols,
                             labels = question_labels[likert_cols]))
  
  df_plot <- df_plot %>% filter(Response != "no opinion") # G: we now remove no opinion after including it in the calculation
  
  # ---- Plot diverging stacked bar chart ----
  ggplot(df_plot, aes(x = Category, y = Percent, fill = Response)) +
    geom_bar(stat = "identity", width = 0.6) +
    coord_flip() +
    scale_fill_manual(values = colors, breaks = levels_order) +
    scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
    labs(x = "", y = "Percentage", fill = "Response") +
    geom_text(aes(label = paste0(round(abs(Percent), 1), "%")),
              position = position_stack(vjust = 0.5),
              color = "white", size = 4) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y  = element_text(size = 13, lineheight = 1.1),
      axis.text.x  = element_text(size = 11),
      axis.title   = element_text(size = 13),
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 13),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank()
    )
  
  #############################################################################################################
  # FACETED PLOTS:
  
  # ---- DATA PREPARATION FOR FACETED PLOT (by Gender) ----
  faceted_data_gender <- SusAIsurvey %>%
    select(all_of(likert_cols), Gender) %>%
    filter(!is.na(Gender) & !(Gender %in% c("AI researcher", "Bayesian", "Non-binary")))
  
  df_long_gender <- faceted_data_gender %>%
    pivot_longer(
      cols = -Gender,
      names_to = "Category",
      values_to = "Response"
    ) %>%
    filter(!is.na(Response)) %>%
    mutate(
      Response_clean = tolower(str_trim(as.character(Response))),
      Response = dplyr::recode(Response_clean,
                               "strongly disagree" = "Strongly disagree",
                               "disagree"          = "Disagree",
                               "agree"             = "Agree",
                               "strongly agree"    = "Strongly agree",
                               "no opinion"        = NA_character_,
                               "no_opinion"        = NA_character_,
                               .default = NA_character_
      )
    ) %>%
    filter(!is.na(Response)) %>%
    mutate(Response = factor(Response, levels = levels_order))
  
  df_plot_gender <- df_long_gender %>%
    group_by(Gender, Category, Response) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Gender, Category) %>%
    mutate(Percent = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(
      Percent = ifelse(Response %in% c("Strongly disagree", "Disagree"), -Percent, Percent),
      Category = factor(Category, levels = likert_cols, labels = question_labels[likert_cols])
    )
  
  # ---- PLOT FACETED DIVERGING STACKED BAR CHART "Gender" ----
  ggplot(df_plot_gender, aes(x = Category, y = Percent, fill = Response)) +
    geom_bar(stat = "identity", width = 0.6) +
    coord_flip() +
    facet_wrap(~ Gender, nrow = 1) +
    scale_fill_manual(values = colors, limits = levels_order) +
    scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
    labs(
      title = "Responses Segmented by Gender",
      x = "", 
      y = "Percentage",
      fill = "Response"
    ) +
    geom_text(
      aes(label = paste0(round(abs(Percent), 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white", size = 3
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y  = element_text(size = 13, lineheight = 1.1),
      axis.text.x  = element_text(size = 11),
      axis.title   = element_text(size = 13),
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 13),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      strip.background = element_rect(fill="grey85", color = NA),
      strip.text = element_text(size = 12, face = "bold"),
      panel.spacing = unit(1.5, "lines")
    )
  
  # ---- DATA PREPARATION FOR FACETED PLOT (by Age group) ----
  faceted_data_age <- SusAIsurvey %>%
    select(all_of(likert_cols), Age) %>%
    filter(!is.na(Age) & Age != "NA")
  
  df_long_age <- faceted_data_age %>%
    pivot_longer(
      cols = -Age,
      names_to = "Category",
      values_to = "Response"
    ) %>%
    filter(!is.na(Response)) %>%
    # normalize text and map to canonical labels (case-insensitive)
    mutate(
      Response_clean = tolower(str_trim(as.character(Response))),
      Response = dplyr::recode(Response_clean,
                               "strongly disagree" = "Strongly disagree",
                               "disagree"          = "Disagree",
                               "agree"             = "Agree",
                               "strongly agree"    = "Strongly agree",
                               # map any variant of "no opinion" to NA so we drop them next
                               "no opinion"        = NA_character_,
                               "no_opinion"        = NA_character_,
                               .default = NA_character_
      )
    ) %>%
    filter(!is.na(Response)) %>%                # drop "no opinion" or unmatched
    mutate(Response = factor(Response, levels = levels_order)) # enforce order
  
  # now compute percentages (Response is already a factor with the right order)
  df_plot_age <- df_long_age %>%
    group_by(Age, Category, Response) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Age, Category) %>%
    mutate(Percent = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(
      Percent = ifelse(Response %in% c("Strongly disagree", "Disagree"), -Percent, Percent),
      Category = factor(Category, levels = likert_cols, labels = question_labels[likert_cols])
    )
  
  # quick sanity checks (optional)
  # print(levels(df_plot_age$Response))
  # print(table(df_plot_age$Response))
  
  # ---- PLOT FACETED DIVERGING STACKED BAR CHART "Age" ----
  ggplot(df_plot_age, aes(x = Category, y = Percent, fill = Response)) +
    geom_bar(stat = "identity", width = 0.6) +
    coord_flip() +
    facet_wrap(~ Age, nrow = 1) +
    scale_fill_manual(values = colors, limits = levels_order) +   # use limits so legend + stacking match
    scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
    labs(
      title = "Responses Segmented by Age Group",
      x = "", 
      y = "Percentage",
      fill = "Response"
    ) +
    geom_text(
      aes(label = paste0(round(abs(Percent), 1), "%")),
      position = position_stack(vjust = 0.5),
      color = "white", size = 3
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y  = element_text(size = 13, lineheight = 1.1),
      axis.text.x  = element_text(size = 11),
      axis.title   = element_text(size = 13),
      legend.text  = element_text(size = 11),
      legend.title = element_text(size = 13),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      strip.background = element_rect(fill="grey85", color = NA),
      strip.text = element_text(size = 12, face = "bold"),
      panel.spacing = unit(1.5, "lines")
    )
  
  
  # ----- PLOT FACETED DIVERGING STACKED BAR CHART "Education"  -----
  df_long_faceted <- faceted_data_prep %>%
    pivot_longer(
      cols = -Title_Education,
      names_to = "Category",
      values_to = "Response"
    ) %>%
    filter(!is.na(Response)) %>%
    # HERE: enforce the factor order used for stacking/legend
    mutate(Response = factor(Response, levels = levels_order))
  
  df_plot_faceted <- df_long_faceted %>%
    group_by(Title_Education, Category, Response) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Title_Education, Category) %>%
    mutate(Percent = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(
      Percent = ifelse(Response %in% c("Strongly disagree", "Disagree"), -Percent, Percent),
      Category = factor(Category, levels = likert_cols, labels = question_labels[likert_cols])
    ) %>%
    # filter using the exact string your data uses; here we assume lowercase "no opinion" is not present
    filter(!is.na(Response) & Response != "no opinion")
  
  
  # ----- PLOT faceted (EDUCATION) -----
  ggplot(df_plot_faceted, aes(x = Category, y = Percent, fill = Response)) +
    geom_bar(stat = "identity", width = 0.6) +
    coord_flip() +
    facet_wrap(~ Title_Education, nrow = 1) +
    # single, explicit scale that follows the canonical order
    scale_fill_manual(values = colors, limits = levels_order) +
    scale_y_continuous(labels = function(x) paste0(abs(x), "%")) +
    labs(title = "Responses Segmented by Highest Education Level", x = "", y = "Percentage", fill = "Response") +
    geom_text(aes(label = paste0(round(abs(Percent), 1), "%")),
              position = position_stack(vjust = 0.5), color = "white", size = 3) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 13, lineheight = 1.1),
      axis.text.x = element_text(size = 11),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank()
    )

  SusAIsurvey %>%
    count(Gender)
  
  SusAIsurvey %>%
    count(Age)
  
  SusAIsurvey %>%
    count(Title_Education)
  