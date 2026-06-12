# Math Major Persistence Logistic Regression

This project investigates long-term persistence and early dropout patterns among undergraduate mathematics majors using logistic regression and longitudinal student-level data.

The analysis was developed as part of a faculty-supervised undergraduate research project focused on understanding which factors are associated with persistence in the mathematics major and which factors are associated with immediate switching after the first mathematics term.

## Research Questions

1. Which academic and demographic factors are associated with long-term persistence in the mathematics major?

2. Which factors are associated with students leaving the mathematics major immediately after their first mathematics term?

## Methodology

The project uses:

- Logistic regression modeling
- Odds ratios and confidence intervals
- Hosmer-Lemeshow goodness-of-fit testing
- Longitudinal student-level preprocessing workflows
- Multiple imputation using `mice`

The workflow was modularized into reusable preprocessing, analysis, and visualization scripts to support reproducibility and maintainability.

## Project Structure

```text
scripts/
├── pipelines/
│   └── 01_create_student_dataset.R
│
├── analysis/
│   ├── 02_logistic_persistence_model.R
│   ├── 03_early_dropout_model.R
│   └── 04_visualizations.R
```

## Main Variables

Some variables used in the analyses include:

- `term_gpa`
- `math_hrs`
- `math_act_T0`
- `financial_aid`
- `transfer_T0`
- `entry_action_T0`

Outcomes include:
- long-term persistence
- immediate exit after first mathematics term

## Reproducibility

The repository uses a shared preprocessing pipeline to construct reusable student-level datasets across analyses.

All scripts are designed to run modularly using:

```r
source("scripts/pipelines/01_create_student_dataset.R")
```

## Data Privacy

The original student-level dataset is not included in this repository due to privacy and institutional data restrictions.

## Tools Used

- R
- tidyverse
- mice
- ggplot2
- ResourceSelection
- Git/GitHub
