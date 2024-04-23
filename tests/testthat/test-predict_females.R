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
  "`age` parameter must be numeric"
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
  "`diabetes_duration` parameter must be numeric"
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
  "`hba1c` parameter must be numeric"
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
  "`hypertension_treatment` parameter must be numeric"
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
  "`hypertension_treatment` parameter must be dichotomous"
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
  "`log_albtocreatratio` parameter must be numeric"
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
  "`non_hdl` parameter must be numeric"
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
  "`income_less_18000` parameter must be numeric"
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
  "`income_less_18000` parameter must be dichotomous"
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
  "`physical_activity_inactive` parameter must be numeric"
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
  "`physical_activity_inactive` parameter must be dichotomous"
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
  "`physical_activity_partially_active` parameter must be numeric"
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
  "`physical_activity_partially_active` parameter must be dichotomous"
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
  "`previous_atrial_f` parameter must be numeric"
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
  "`previous_atrial_f` parameter must be dichotomous"
  )
})

testthat::test_that("checks the type of the `pulse_pressure` variable", {
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
  "`pulse_pressure` parameter must be numeric"
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
  "`retinopathy` parameter must be numeric"
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
  "`retinopathy` parameter must be dichotomous"
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
  "`smoking_status_smoker` parameter must be numeric"
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
  "`smoking_status_smoker` parameter must be dichotomous"
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
  "`smoking_status_ex_smoker` parameter must be numeric"
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
  "`smoking_status_ex_smoker` parameter must be dichotomous"
  )
})

testthat::test_that("checks multiple non-numeric variables", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = as.character(female$age),
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = female$hypertension_treatment,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = as.character(female$non_hdl),
    income_less_18000 = female$income_less_18000,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = female$previous_atrial_f,
    pulse_pressure = as.character(female$pulse_pressure),
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "`age`, `non_hdl`, and `pulse_pressure` parameters must be numeric"
  )
})

testthat::test_that("checks multiple non-dichotomous variables", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_error(prediction <- predict_females(
    age = female$age,
    diabetes_duration = female$diabetes_duration,
    hba1c = female$hba1c,
    hypertension_treatment = 5,
    log_albtocreatratio = female$log_albtocreatratio,
    non_hdl = female$non_hdl,
    income_less_18000 = 5,
    physical_activity_inactive = female$physical_activity_inactive,
    physical_activity_partially_active = female$physical_activity_partially_active,
    previous_atrial_f = 5,
    pulse_pressure = female$pulse_pressure,
    retinopathy = female$retinopathy,
    smoking_status_smoker = female$smoking_status_smoker,
    smoking_status_ex_smoker = female$smoking_status_ex_smoker
  ),
  "`hypertension_treatment`, `income_less_18000`, and `previous_atrial_f` parameters must be dichotomous"
  )
})

testthat::test_that("substitutes the NA", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))
  female <- data[3, ]

  testthat::expect_no_error(
    predict_females(
      age = female$age,
      diabetes_duration = female$diabetes_duration,
      hba1c = NA,
      hypertension_treatment = female$hypertension_treatment,
      log_albtocreatratio = female$log_albtocreatratio,
      non_hdl = female$non_hdl,
      income_less_18000 = female$income_less_18000,
      physical_activity_inactive = NA,
      physical_activity_partially_active = female$physical_activity_partially_active,
      previous_atrial_f = female$previous_atrial_f,
      pulse_pressure = female$pulse_pressure,
      retinopathy = female$retinopathy,
      smoking_status_smoker = female$smoking_status_smoker,
      smoking_status_ex_smoker = female$smoking_status_ex_smoker
    )
  )

  testthat::expect_false(
    is.na(
      predict_females(
        age = female$age,
        diabetes_duration = female$diabetes_duration,
        hba1c = NA,
        hypertension_treatment = female$hypertension_treatment,
        log_albtocreatratio = female$log_albtocreatratio,
        non_hdl = female$non_hdl,
        income_less_18000 = female$income_less_18000,
        physical_activity_inactive = NA,
        physical_activity_partially_active = female$physical_activity_partially_active,
        previous_atrial_f = female$previous_atrial_f,
        pulse_pressure = female$pulse_pressure,
        retinopathy = female$retinopathy,
        smoking_status_smoker = female$smoking_status_smoker,
        smoking_status_ex_smoker = female$smoking_status_ex_smoker
      )
    )
  )
})
