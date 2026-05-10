# Load preprocessing and model objects for visualization.
source("scripts/pipelines/01_create_student_dataset.R")
source("scripts/analysis/02_logistic_persistence_model.R")
source("scripts/analysis/03_early_dropout_model.R")

library(tidyverse)
library(scales)


# Long-Term Persistence Visualizations


# Outcome distribution plot (standard palette)
student_level_data_no_mathgpa %>%
  mutate(outcome = if_else(persisted == 1, "Persisted", "Switched")) %>%
  count(outcome) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = outcome, y = n, fill = outcome)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = paste0(n, "\n(", scales::percent(pct, accuracy = 0.1), ")")),
    vjust = -0.5,
    size = 5,
    fontface = "bold",
    color = "#1a1a1a"
  ) +
  scale_fill_manual(values = c("Persisted" = "#378ADD", "Switched" = "#B4B2A9")) +
  scale_y_continuous(limits = c(0, 130), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Distribution of Outcomes Among Mathematics Majors",
    subtitle = "N = 170 students across seven academic years",
    x = NULL,
    y = "Number of Students"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray50", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 13, color = "#1a1a1a"),
    axis.title.y = element_text(size = 12, color = "gray40"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave("outcome_distribution.png", width = 7, height = 5, dpi = 300, bg = "white")

# Odds ratio forest plot for persistence predictors
or_data_rq1 <- data.frame(
  predictor = c("ACT Math Score", "Math Credit Hours", "First-Term GPA"),
  OR        = c(1.105, 1.191, 1.956),
  lo        = c(1.003, 1.063, 1.317),
  hi        = c(1.230, 1.344, 3.117),
  sig       = c("Marginal", "Significant", "Significant"),
  color     = c("#B4B2A9", "#5DCAA5", "#378ADD")
) %>%
  mutate(predictor = factor(predictor, levels = predictor))

ggplot(or_data_rq1, aes(x = OR, y = predictor, color = color)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray60", linewidth = 1) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.2, linewidth = 2) +
  geom_point(size = 8) +
  geom_text(
    aes(label = paste0("OR = ", round(OR, 2))),
    hjust = -0.3,
    size = 6,
    fontface = "bold",
    color = "#1a1a1a"
  ) +
  scale_color_identity() +
  scale_x_continuous(limits = c(0.8, 4.2), breaks = c(1, 1.5, 2, 2.5, 3)) +
  labs(
    title = "RQ1: Predictors of Persistence in the Mathematics Major",
    subtitle = "Odds ratios with 95% confidence intervals  |  Dashed line = no effect",
    x = "Odds Ratio",
    y = NULL
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(color = "gray50", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 18, face = "bold", color = "#1a1a1a"),
    axis.text.x = element_text(size = 14, color = "#1a1a1a"),
    axis.title.x = element_text(size = 14, color = "gray40"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 40, 20, 20)
  )

ggsave("rq1_forest_bold.png", width = 10, height = 5, dpi = 300, bg = "white")

# Outcome distribution plot (bold style)
student_level_data_no_mathgpa %>%
  mutate(outcome = if_else(persisted == 1, "Persisted", "Switched")) %>%
  count(outcome) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = outcome, y = n, fill = outcome)) +
  geom_col(width = 0.45) +
  geom_text(
    aes(label = paste0(n, " students")),
    vjust = -1.2,
    size = 8,
    fontface = "bold",
    color = "#1a1a1a"
  ) +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    vjust = -0.3,
    size = 6,
    color = "gray40"
  ) +
  scale_fill_manual(values = c("Persisted" = "#378ADD", "Switched" = "#5DCAA5")) +
  scale_y_continuous(limits = c(0, 145), expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Distribution of Outcomes Among Mathematics Majors",
    subtitle = "N = 170 students across seven academic years",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(color = "gray50", size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    axis.text.x = element_text(size = 20, face = "bold", color = "#1a1a1a"),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 40, 20, 20)
  )

ggsave("outcome_distribution_bold.png", width = 8, height = 6, dpi = 300, bg = "white")


# Early Dropout Visualizations


# Immediate-exit forest plot
or_data_early_dropout <- exp(cbind(
  OR = coef(model_early_dropout_risk),
  ci_early_dropout_risk
)) %>%
  as.data.frame() %>%
  rownames_to_column(var = "predictor") %>%
  filter(predictor != "(Intercept)") %>%
  mutate(
    predictor = recode(
      predictor,
      "age_T0"                                   = "Age at Entry",
      "math_act_T0"                              = "ACT Math Score",
      "genderM"                                  = "Gender: Male",
      "married_T0S"                              = "Marital Status: Single",
      "married_T0Unknown"                        = "Marital Status: Unknown",
      "transfer_T0Y"                             = "Transfer Student",
      "entry_action_T0Freshman -Grad HS 1-3 yrs" = "Freshman (HS Grad 1-3 yrs)",
      "entry_action_T0New First Time"            = "New First-Time Student",
      "entry_action_T0Returning SUU Student"     = "Returning SUU Student",
      "entry_action_T0Transfer 2 yr Institution" = "Transfer: 2-Year Institution",
      "entry_action_T0Transfer 4 yr Institution" = "Transfer: 4-Year Institution",
      "financial_aidY"                           = "Financial Aid Recipient"
    )
  ) %>%
  mutate(distance = abs(log(OR))) %>%
  arrange(distance) %>%
  mutate(predictor = factor(predictor, levels = predictor)) %>%
  mutate(highlight = predictor == "New First-Time Student")

ggplot(or_data_early_dropout, aes(x = OR, y = predictor)) +
  geom_errorbarh(
    aes(xmin = `2.5 %`, xmax = `97.5 %`),
    height = 0.25,
    color = "gray60"
  ) +
  geom_point(
    aes(color = highlight),
    size = 3
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "gray50"
  ) +
  scale_x_log10() +
  scale_color_manual(
    values = c(
      "FALSE" = "#4B5563",
      "TRUE"  = "#2563EB"
    )
  ) +
  labs(
    title = "Immediate Exit from the Mathematics Major After First Term (Q1b)",
    subtitle = "Odds ratios with 95% confidence intervals",
    x = "Odds Ratio (log scale)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40"),
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Immediate exit proportion plot
exit_plot_data <- student_level_data %>%
  count(drop_after_1) %>%
  mutate(
    pct = n / sum(n),
    label = if_else(drop_after_1 == 1,
                    "Exited After\n1 Term",
                    "Persisted\nBeyond 1 Term")
  )

ggplot(exit_plot_data, aes(x = label, y = pct)) +
  geom_col(width = 0.55, fill = "#1E3A8A") +
  geom_text(
    aes(label = percent(pct, accuracy = 0.1)),
    vjust = -0.6,
    size = 7,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(exit_plot_data$pct) * 1.15),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Immediate Exit from the Mathematics Major",
    subtitle = "Proportion of students exiting after their first math term",
    x = NULL,
    y = "Percentage of Students"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    plot.subtitle = element_text(size = 16, color = "gray40"),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Filtered forest plot of key early-exit predictors
or_data_clean <- or_data_early_dropout %>%
  filter(predictor %in% c(
    "ACT Math Score",
    "Transfer Student",
    "Financial Aid Recipient",
    "Freshman (HS Grad 1–3 yrs)",
    "New First-Time Student",
    "Returning SUU Student",
    "Transfer: 2-Year Institution",
    "Transfer: 4-Year Institution"
  )) %>%
  mutate(predictor = factor(predictor, levels = rev(unique(predictor))))

ggplot(or_data_clean, aes(x = OR, y = predictor, color = predictor)) +
  geom_errorbarh(
    aes(xmin = `2.5 %`, xmax = `97.5 %`),
    height = 0.3,
    linewidth = 1.5,
    alpha = 0.9
  ) +
  geom_point(size = 5) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "black",
    linewidth = 1
  ) +
  scale_x_log10(labels = label_number(accuracy = 0.01)) +
  scale_color_manual(values = c(
    "#2563EB", "#DC2626", "#059669", "#7C3AED",
    "#EA580C", "#0891B2", "#BE185D", "#65A30D"
  )) +
  labs(
    title = "Predictors of Immediate Exit from the Mathematics Major After First Term",
    subtitle = "Fully adjusted logistic regression model\nOdds ratios with 95% confidence intervals",
    x = "Odds Ratio (log scale)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 22) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 30, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 18, color = "gray30", margin = margin(b = 15)),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(20, 40, 20, 20)
  )

# Timing plot for switchers
student_level_data %>%
  mutate(
    group = if_else(drop_after_1 == 1,
                    "Left after\n1 term",
                    "Stayed for\n2+ terms")
  ) %>%
  count(group) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = group, y = n, fill = group)) +
  geom_col(width = 0.5) +
  geom_text(
    aes(label = paste0(n, " students\n", percent(pct, accuracy = 1))),
    vjust = -0.6,
    size = 7,
    fontface = "bold",
    color = "#1C2B3A"
  ) +
  scale_fill_manual(values = c(
    "Left after\n1 term"   = "#1E4D8C",
    "Stayed for\n2+ terms" = "#CBD5E1"
  )) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.05)),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = "When Do Switchers Leave?",
    subtitle = "Among the 106 students who switched out of the mathematics major",
    x = NULL,
    y = "Number of Students"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 22, color = "#1C2B3A"),
    plot.subtitle      = element_text(size = 15, color = "gray40", margin = margin(b = 15)),
    axis.text.x        = element_text(size = 16, face = "bold", color = "#1C2B3A"),
    axis.text.y        = element_text(size = 14, color = "gray50"),
    axis.title.y       = element_text(size = 15, color = "gray50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(20, 30, 20, 20)
  )
