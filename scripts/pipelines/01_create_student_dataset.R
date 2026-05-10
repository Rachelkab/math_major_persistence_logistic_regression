
# Shared student dataset construction pipeline

# Purpose:
#   Build student-level persistence datasets anchored to each
#   student's first mathematics major term.
#
# Outputs:
#   student_level_data             — full dataset, includes math_gpa
#   student_level_data_no_mathgpa  — math_gpa dropped; full sample
#
# Includes:
#   - data loading
#   - invalid enrollment cleaning
#   - secondary major cleaning
#   - math term reconstruction
#   - factor conversion
#   - anchoring variables to first math term
#   - math_hrs cleaning
#   - double_major construction
#   - student-level dataset creation
#   - persisted outcome construction
#   - imputation


library(tidyverse)
library(mice)


# 1. LOAD DATA

# This pipeline expects to be run from the repository root.
data <- read.csv(
  file.path("student_persistence", "shrt_vs_lg.csv"),
  header = TRUE,
  stringsAsFactors = FALSE
)


# 2. FIX INVALID ENROLLMENT ROWS

data <- data %>%
  mutate(
    primary_major = if_else(
      enrolled_suu == "N" & AttemptedHours == 0 & TermGPA == 0,
      "Not Enrolled (SUU)",
      primary_major
    ),
    secondary_major = if_else(
      enrolled_suu == "N" & AttemptedHours == 0 & TermGPA == 0,
      "Not Enrolled (SUU)",
      secondary_major
    ),
    enrollment_status = case_when(
      enrolled_suu == "N" & AttemptedHours == 0 & TermGPA == 0 ~ "Not Enrolled (SUU)",
      is.na(enrollment_status) | enrollment_status == ""             ~ "Unknown",
      TRUE                                                            ~ enrollment_status
    ),
    entry_action_T0 = case_when(
      enrolled_suu == "N" & AttemptedHours == 0 & TermGPA == 0 ~ "Not Enrolled (SUU)",
      is.na(entry_action_T0) | entry_action_T0 == ""             ~ "Unknown",
      TRUE                                                            ~ as.character(entry_action_T0)
    ),
    class_level = case_when(
      enrolled_suu == "N" & AttemptedHours == 0 & TermGPA == 0 ~ "Not Enrolled (SUU)",
      is.na(class_level) | class_level == ""                   ~ "Unknown",
      TRUE                                                       ~ as.character(class_level)
    )
  )


# 3. CLEAN SECONDARY MAJOR

data <- data %>%
  mutate(
    secondary_major = if_else(
      is.na(secondary_major) | secondary_major == "",
      "None",
      secondary_major
    )
  )


# 4. DEFINE MATH MAJORS

math_majors <- c("Mathematical Science", "Mathematics Education")


# 5. RECONSTRUCT MATH TERM COUNT

data <- data %>%
  mutate(
    is_math_term = primary_major %in% math_majors |
                   secondary_major %in% math_majors
  ) %>%
  group_by(RandomSID) %>%
  mutate(
    last_math_term = if (any(is_math_term)) {
      max(Term[is_math_term], na.rm = TRUE)
    } else {
      NA_integer_
    },
    math_term_count = if_else(
      is_math_term & Term == last_math_term,
      sum(is_math_term),
      NA_integer_
    )
  ) %>%
  ungroup() %>%
  select(-is_math_term, -last_math_term)


# 6. REMOVE KNOWN INVALID STUDENT

data <- data %>%
  filter(RandomSID != 135)


# 7. CONVERT CATEGORICAL VARIABLES TO FACTORS

data <- data %>%
  mutate(
    across(
      -c(
        RandomSID,
        Term,
        math_term_count,
        age_T0,
        math_act_T0,
        AttemptedHours,
        TermGPA,
        math_hrs,
        math_gpa
      ),
      as.factor
    ),
    financial_aid = factor(financial_aid)
  )


# 8. ANCHOR VARIABLES TO FIRST MATH TERM

all_vars_to_anchor <- c(
  "age_T0",
  "gender",
  "resident_T0",
  "married_T0",
  "transfer_T0",
  "entry_action_T0",
  "math_act_T0",
  "financial_aid",
  "enrollment_status",
  "class_level",
  "TermGPA",
  "math_gpa",
  "math_hrs",
  "AttemptedHours"
)

first_math_term <- data %>%
  group_by(RandomSID) %>%
  summarize(
    first_math_term = {
      math_rows <- which(
        primary_major %in% math_majors |
        secondary_major %in% math_majors
      )
      if (length(math_rows) == 0L) {
        NA_integer_
      } else {
        min(Term[math_rows], na.rm = TRUE)
      }
    },
    .groups = "drop"
  )

data <- data %>%
  left_join(first_math_term, by = "RandomSID") %>%
  group_by(RandomSID) %>%
  arrange(Term, .by_group = TRUE) %>%
  mutate(
    across(
      all_of(all_vars_to_anchor),
      ~ .[Term == first_math_term][1]
    )
  ) %>%
  ungroup() %>%
  select(-first_math_term)


# 9. CLEAN FACTOR LEVELS

data <- data %>%
  mutate(
    resident_T0 = na_if(as.character(resident_T0), ""),
    married_T0  = na_if(as.character(married_T0), "")
  ) %>%
  mutate(
    resident_T0 = factor(resident_T0),
    married_T0  = factor(married_T0)
  ) %>%
  mutate(
    married_T0 = forcats::fct_na_value_to_level(married_T0, "Unknown")
  )


# 10. CLEAN math_hrs AT FIRST MATH TERM

# - No attempted hours + GPA 0 → math_hrs = 0
# - Math GPA exists but math_hrs missing → keep NA (impute below)
# - Other missing math_hrs → set to 0
# This preserves the administrative structure of missing vs zero credits.
data <- data %>%
  mutate(
    math_hrs = case_when(
      AttemptedHours == 0 & TermGPA == 0     ~ 0,
      !is.na(math_gpa) & is.na(math_hrs)     ~ NA_real_,
      is.na(math_hrs)                         ~ 0,
      TRUE                                    ~ math_hrs
    )
  )


# 11. CONSTRUCT double_major AT FIRST MATH TERM

# A student is a double major at entry if they hold a second major
# that is neither None nor a second math major.
data <- data %>%
  group_by(RandomSID) %>%
  arrange(Term, .by_group = TRUE) %>%
  mutate(
    first_math_term = {
      math_rows <- which(
        primary_major %in% math_majors |
        secondary_major %in% math_majors
      )
      if (length(math_rows) == 0L) {
        NA_integer_
      } else {
        min(Term[math_rows], na.rm = TRUE)
      }
    },
    double_major = if_else(
      Term == first_math_term &
        (primary_major %in% math_majors | secondary_major %in% math_majors) &
        !primary_major %in% c("None", "Not Enrolled (SUU)") &
        !secondary_major %in% c("None", "Not Enrolled (SUU)") &
        !(primary_major %in% math_majors & secondary_major %in% math_majors),
      1L,
      0L
    )
  ) %>%
  mutate(
    double_major = max(double_major, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-first_math_term)


# 12. COLLAPSE TO STUDENT-LEVEL DATASET

student_level_data <- data %>%
  filter(!is.na(math_term_count)) %>%
  group_by(RandomSID) %>%
  summarise(
    age_T0          = first(age_T0),
    gender          = first(gender),
    resident_T0     = first(resident_T0),
    married_T0      = first(married_T0),
    transfer_T0     = first(transfer_T0),
    entry_action_T0 = first(entry_action_T0),
    math_act_T0     = first(math_act_T0),
    financial_aid   = first(financial_aid),
    enrollment_status = first(enrollment_status),
    class_level     = first(class_level),
    term_gpa        = first(TermGPA),
    math_gpa        = first(math_gpa),
    math_hrs        = first(math_hrs),
    attempted_hrs   = first(AttemptedHours),
    double_major    = first(double_major),
    math_term_count = math_term_count[!is.na(math_term_count)][1],
    math_status     = math_status[!is.na(math_term_count)][1],
    .groups = "drop"
  ) %>%
  mutate(
    double_major = factor(double_major)
  )


# 13. CONSTRUCT persisted OUTCOME

# math_status on last math term:
#   1 = still enrolled in math  → persisted
#   2 = graduated in math       → persisted
#   NA / missing                → switched out
student_level_data <- student_level_data %>%
  mutate(
    persisted = if_else(
      math_status %in% c(1, 2),
      1L,
      0L
    ),
    persisted = factor(persisted)
  )


# 14. IMPUTE MISSING math_hrs (student level)

imp_mathhrs_data <- student_level_data %>%
  select(math_hrs, math_gpa, term_gpa, attempted_hrs)

ini  <- mice(imp_mathhrs_data, maxit = 0)
meth <- ini$method
pred <- ini$predictorMatrix

meth["math_hrs"] <- "pmm"
meth[c("math_gpa", "term_gpa", "attempted_hrs")] <- ""

pred["math_hrs", ] <- 0
pred["math_hrs", c("math_gpa", "term_gpa", "attempted_hrs")] <- 1

set.seed(4700)
imp_mathhrs <- mice(
  imp_mathhrs_data,
  method = meth,
  predictorMatrix = pred,
  m = 5,
  maxit = 10,
  printFlag = FALSE
)

student_level_data <- student_level_data %>%
  mutate(
    math_hrs = if_else(
      is.na(math_hrs) & !is.na(math_gpa),
      complete(imp_mathhrs, 1)$math_hrs,
      math_hrs
    )
  )


# 15. IMPUTE MISSING age_T0 (student level)

imp_age_data <- student_level_data %>%
  select(
    age_T0,
    gender,
    resident_T0,
    married_T0,
    transfer_T0,
    entry_action_T0
  )

ini  <- mice(imp_age_data, maxit = 0)
meth <- ini$method
pred <- ini$predictorMatrix

meth["age_T0"] <- "pmm"
meth[c(
  "gender",
  "resident_T0",
  "married_T0",
  "transfer_T0",
  "entry_action_T0"
)] <- ""

pred["age_T0", ] <- 0
pred["age_T0", c(
  "gender",
  "resident_T0",
  "married_T0",
  "transfer_T0",
  "entry_action_T0"
)] <- 1

set.seed(4700)
imp_age <- mice(
  imp_age_data,
  method = meth,
  predictorMatrix = pred,
  m = 5,
  maxit = 10,
  printFlag = FALSE
)

student_level_data$age_T0 <- complete(imp_age, 1)$age_T0


# 16. BUILD full-sample dataset WITHOUT math_gpa

student_level_data_no_mathgpa <- student_level_data %>%
  select(-math_gpa, -math_status) %>%
  mutate(
    full_time = if_else(enrollment_status == "Full Time", 1, 0),
    full_time = factor(full_time)
  )
view(student_level_data_no_mathgpa)