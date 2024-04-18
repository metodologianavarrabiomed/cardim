testthat::test_that("the function predicts properly", {
  # it is known that the first female from the `data` fixture should have
  # a prediction of 0.198173845
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  )

  testthat::expect_equal(prediction, 0.198173845)
})

testthat::test_that("checks the type of the `age` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = as.character(female$age),
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )
})

testthat::test_that("checks the type of the `diabetes_duration` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = as.character(female$diabetes_duration),
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )
})

testthat::test_that("checks the type of the `hba1c` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = as.character(female$hba1c),
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )
})

testthat::test_that("checks the type of the `hypertension_treatment` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = as.character(female$hypertension_treatment),
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = 2,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "must be 0 or 1"
  )
})

testthat::test_that("checks the type of the `log_albtocreatratio` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = as.character(female$log_albtocreatratio),
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )
})

testthat::test_that("checks the type of the `non_hdl` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = as.character(female$non_hdl),
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )
})

testthat::test_that("checks the type of the `income_less_18000` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = as.character(female$income_less_18000),
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = 2,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "must be 0 or 1"
  )
})

testthat::test_that("checks the type of the `physical_activity_inactive` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = as.character(female$physical_activity_inactive),
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = 2,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "must be 0 or 1"
  )
})

testthat::test_that("checks the type of the `physical_activity_partially_active` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = as.character(female$physical_activity_partially_active),
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = 2,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "must be 0 or 1"
  )
})

testthat::test_that("checks the type of the `previous_atrial_f` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = as.character(female$previous_atrial_f),
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = 2,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "must be 0 or 1"
  )
})

testthat::test_that("checks the type of the `puls_pressure` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = as.character(female$pulse_pressure),
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )
})

testthat::test_that("checks the type of the `retinopathy` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = as.character(female$retinopathy),
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = 2,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "must be 0 or 1"
  )
})

testthat::test_that("checks the type of the `smoking_status_smoker` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = as.character(female$smoking_status_smoker),
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "all parameters must be numeric"
  )

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = 2,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "must be 0 or 1"
  )
})

testthat::test_that("checks the type of the `smoking_status_ex_smoker` variable", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = as.character(female$smoking_status_ex_smoker)
  ),
  "all parameters must be numeric"
  )

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = 2
  ),
  "must be 0 or 1"
  )
})
