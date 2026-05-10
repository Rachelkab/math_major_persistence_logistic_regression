# Load the shared preprocessing pipeline to create the student-level datasets.
source("scripts/pipelines/01_create_student_dataset.R")

# Early Dropout / Immediate Switching Logistic Regression
# Purpose:
#   Model whether a student exits after the first mathematics major term.
#
# Outcome: drop_after_1 (1 = exited after 1 term, 0 = stayed beyond 1 term)
# Data:    student_level_data
# Method:  Logistic regression (binomial, logit link)


# 1. Construct the immediate exit outcome

student_level_data <- student_level_data %>%
  mutate(
    drop_after_1 = if_else(math_term_count == 1, 1L, 0L)
  )


# 2. Logistic regression model for early dropout risk

# This model uses entry-level covariates only to predict whether
# a student leaves the mathematics major immediately after the
# first math term.
model_early_dropout_risk <- glm(
  drop_after_1 ~
    age_T0 +
    math_act_T0 +
    gender +
    married_T0 +
    transfer_T0 +
    entry_action_T0 +
    financial_aid,
  data   = student_level_data,
  family = binomial(link = "logit")
)
summary(model_early_dropout_risk)


# 3. Odds ratios with confidence intervals
 
ci_early_dropout_risk <- confint(model_early_dropout_risk)

odds_ratios_early_dropout_risk <- exp(cbind(
  OR = coef(model_early_dropout_risk),
  ci_early_dropout_risk
))
print(odds_ratios_early_dropout_risk)