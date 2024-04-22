
# cardim

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of cardim is to provide the user with tools to predict risk using the 
CARDIM-model and also tools for the possible external validation.

## Installation

You can install the development version of cardim from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("metodologianavarrabiomed/cardim-model")
```

## Example

This is a basic example which shows you how to solve a common problem:

Suppose we want to estimate the risk of a male patient that have the following 

|Variable|Value|
|--------|-----|
| Age, years | $71$ |
| Income less than 18000 € | yes |
| Diabetes duration, years | $24$ |
| hba1c, % | $9.9$ |
| Hypertension treatment at baseline | yes |
| Logarithm of albumine creatinine ratio, log(mg/g) | $4.577$ |
| Total cholesterol - hdl cholesterol, mmol/L | $2.793$ |
| Physical activity, inactive | no |
| Physical activity, partially active | no |
| Atrial fibrillation at baseline | no |
| Systolic blood pressure - diastolic blood pressure, mmHg | $67$ |
| Retinopathy at baseline | yes |
| Smoking status, smoker | no |
| Smoking status, ex-smoker | no |

The risk can be calculated by using the `predict_males` function as follows.

``` r
library(cardim)

male <- data.frame(
  age = 71, 
  income_less_18000 = 1, 
  diabetes_duration = 24, 
  hba1c = 9.9, 
  hypertension_treatment = 1, 
  log_albtocreatratio = 4.577, 
  non_hdl = 2.793, 
  physical_activity_inactive = 0, 
  physical_activity_partially_active = 0, 
  previous_atrial_f = 0, 
  pulse_pressure = 67, 
  retinopathy = 1, 
  smoking_status_smoker = 0, 
  smoking_status_ex_smoker = 0
)

cardim::predict_males(
  age = male$age, 
  diabetes_duration = male$diabetes_duration, 
  hba1c = male$hba1c, 
  hypertension_treatment = male$hypertension_treatment,
  income_less_18000 = male$income_less_18000,
  log_albtocreatratio = male$log_albtocreatratio, 
  non_hdl = male$non_hdl,
  physical_activity_inactive = male$physical_activity_inactive, 
  physical_activity_partially_active = male$physical_activity_partially_active,
  previous_atrial_f = male$previous_atrial_f, 
  pulse_pressure = male$pulse_pressure,
  retinopathy = male$retinopathy,
  smoking_status_smoker = male$smoking_status_smoker,
  smoking_status_ex_smoker = male$smoking_status_ex_smoker
)
```

For the sake of simplicity we assume a female patient with the same characteristics and calculate the risk using the function `predict_females`.

``` r
female <- data.frame(
  age = 71, 
  income_less_18000 = 1,
  diabetes_duration = 24, 
  hba1c = 9.9, 
  hypertension_treatment = 1, 
  log_albtocreatratio = 4.577, 
  non_hdl = 2.793, 
  physical_activity_inactive = 0,
  physical_activity_partially_active = 0, 
  previous_atrial_f = 0, 
  pulse_pressure = 67, 
  retinopathy = 1, 
  smoking_status_smoker = 0, 
  smoking_status_ex_smoker = 0
)

cardim::predict_females(
  age = female$age, 
  diabetes_duration = female$diabetes_duration, 
  hba1c = female$hba1c, 
  hypertension_treatment = female$hypertension_treatment,
  income_less_18000 = female$income_less_18000,
  log_albtocreatratio = female$log_albtocreatratio, 
  non_hdl = female$non_hdl,
  physical_activity_inactive = female$physical_activity_inactive, 
  physical_activity_partially_active = female$physical_activity_partially_active,
  previous_atrial_f = female$previous_atrial_f, 
  pulse_pressure = female$pulse_pressure,
  retinopathy = female$retinopathy,
  smoking_status_smoker = female$smoking_status_smoker,
  smoking_status_ex_smoker = female$smoking_status_ex_smoker
)
```

Normally the cohort data is stored as an unique table where there are represented males and females. It is possible to estimate the risk in all the cohort using the `cardim::predict` function. The argument of this function is a `data.frame` that have two added variables `id` and `sex`. 

| Variable | Value |
|----------|-------|
| id | string or numeric value that identifies the patient |
| sex | "Male" or "Female" |

```r
# If we want to estimate both of them as a cohort we need to generate
# a data.frame and add the id and sex variables
data <- rbind(male, female)
data$id <- c(1, 2)
data$sex <- c("Male", "Female")

# returns a data.frame with a new column pred with the predictions
cardim::predict(data)
```
