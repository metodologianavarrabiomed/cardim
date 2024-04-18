testthat::test_that("the function predicts properly", {
  # it is known that the first male from the `data` fixture should have
  # a prediction of 0.267384676
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  )

  testthat::expect_equal(prediction, 0.267384676)
})

testthat::test_that("checks the type of the `age` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = as.character(male$age),
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`age` parameter must be numeric"
  )
})

testthat::test_that("checks the type of the `diabetes_duration` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = as.character(male$diabetes_duration),
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`diabetes_duration` parameter must be numeric"
  )
})

testthat::test_that("checks the type of the `hba1c` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = as.character(male$hba1c),
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`hba1c` parameter must be numeric"
  )
})

testthat::test_that("checks the type of the `hypertension_treatment` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = as.character(male$hypertension_treatment),
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`hypertension_treatment` parameter must be numeric"
  )

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = 2,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`hypertension_treatment` parameter must be dichotomous"
  )
})

testthat::test_that("checks the type of the `log_albtocreatratio` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = as.character(male$log_albtocreatratio),
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`log_albtocreatratio` parameter must be numeric"
  )
})

testthat::test_that("checks the type of the `non_hdl` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = as.character(male$non_hdl),
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`non_hdl` parameter must be numeric"
  )
})

testthat::test_that("checks the type of the `income_less_18000` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = as.character(male$income_less_18000),
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`income_less_18000` parameter must be numeric"
  )

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = 2,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`income_less_18000` parameter must be dichotomous"
  )
})

testthat::test_that("checks the type of the `physical_activity_inactive` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = as.character(male$physical_activity_inactive),
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`physical_activity_inactive` parameter must be numeric"
  )

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = 2,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`physical_activity_inactive` parameter must be dichotomous"
  )
})

testthat::test_that("checks the type of the `physical_activity_partially_active` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = as.character(male$physical_activity_partially_active),
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`physical_activity_partially_active` parameter must be numeric"
  )

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = 2,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`physical_activity_partially_active` parameter must be dichotomous"
  )
})

testthat::test_that("checks the type of the `previous_atrial_f` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = as.character(male$previous_atrial_f),
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`previous_atrial_f` parameter must be numeric"
  )

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = 2,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`previous_atrial_f` parameter must be dichotomous"
  )
})

testthat::test_that("checks the type of the `pulse_pressure` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = as.character(male$pulse_pressure),
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`pulse_pressure` parameter must be numeric"
  )
})

testthat::test_that("checks the type of the `retinopathy` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = as.character(male$retinopathy),
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`retinopathy` parameter must be numeric"
  )

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = 2,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`retinopathy` parameter must be dichotomous"
  )
})

testthat::test_that("checks the type of the `smoking_status_smoker` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = as.character(male$smoking_status_smoker),
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`smoking_status_smoker` parameter must be numeric"
  )

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = 2,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`smoking_status_smoker` parameter must be dichotomous"
  )
})

testthat::test_that("checks the type of the `smoking_status_ex_smoker` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = as.character(male$smoking_status_ex_smoker)
  ),
  "`smoking_status_ex_smoker` parameter must be numeric"
  )

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = 2
  ),
  "`smoking_status_ex_smoker` parameter must be dichotomous"
  )
})

testthat::test_that("checks multiple non-numeric variables", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = as.character(male$age),
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = male$hypertension_treatment,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = as.character(male$non_hdl),
    income_less_18000 = male$income_less_18000,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = male$previous_atrial_f,
    pulse_pressure = as.character(male$pulse_pressure),
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`age`, `non_hdl`, and `pulse_pressure` parameters must be numeric"
  )
})

testthat::test_that("checks multiple non-dichotomous variables", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  male <- data[1, ]

  testthat::expect_error(prediction <- predict_males(
    age = male$age,
    diabetes_duration = male$diabetes_duration,
    hba1c = male$hba1c,
    hypertension_treatment = 5,
    log_albtocreatratio = male$log_albtocreatratio,
    non_hdl = male$non_hdl,
    income_less_18000 = 5,
    physical_activity_inactive = male$physical_activity_inactive,
    physical_activity_partially_active = male$physical_activity_partially_active,
    previous_atrial_f = 5,
    pulse_pressure = male$pulse_pressure,
    retinopathy = male$retinopathy,
    smoking_status_smoker = male$smoking_status_smoker,
    smoking_status_ex_smoker = male$smoking_status_ex_smoker
  ),
  "`hypertension_treatment`, `income_less_18000`, and `previous_atrial_f` parameters must be dichotomous"
  )
})
