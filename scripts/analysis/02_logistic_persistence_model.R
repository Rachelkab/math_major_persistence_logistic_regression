# Load the shared preprocessing pipeline to create the student-level datasets.
source("scripts/pipelines/01_create_student_dataset.R")

# Long-Term Persistence Logistic Regression
# Purpose:
#   Model persistence across the full observation period from entry
#   into the mathematics major through the final observed outcome.
#
# Outcome: persisted (1 = persisted, 0 = switched)
# Data:    student_level_data_no_mathgpa
# Method:  Logistic regression (binomial, logit link)

library(ResourceSelection)


# 1. FULL MODEL — ALL PREDICTORS

# Run everything first to see what the model can and can't
# separate given the sample size.
# Watch for: wide CIs, NA coefficients, separation warnings
model_long_term_persistence <- glm(
  persisted ~
    # age_T0 +
    # gender +
    # married_T0 +
    # transfer_T0 +
    # financial_aid +
    # full_time +
    # double_major +
    # resident_T0 +
    math_act_T0 +
    term_gpa +
    math_hrs,
  data   = student_level_data_no_mathgpa,
  family = binomial(link = "logit")
)

summary(model_long_term_persistence)


# 2. ODDS RATIOS WITH CONFIDENCE INTERVALS

ci_full <- confint(model_long_term_persistence)

exp(cbind(
  OR = coef(model_long_term_persistence),
  ci_full
))


# 3. HOSMER-LEMESHOW GOODNESS-OF-FIT TEST

hoslem.test(model_long_term_persistence$y, fitted(model_long_term_persistence), g = 10)
