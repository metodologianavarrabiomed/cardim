testthat::test_that("tests the parameters", {
  # should check that the data is a data frame and the variables are
  # correct.
  test_data <- tibble::tibble(
    pred = c(0.23, 0.45, 0.22, 0.15, 0.8),
    surv = survival::Surv(time = c(3, 2.4, 3.2, 4.1, 1.5), event = c(0, 1, 0, 0, 1))
  )

  expect_no_error(harrell_c_index(test_data))
  expect_no_error(harrell_c_index(test_data |> as.data.frame()))
  expect_error(harrell_c_index("error"), "must be a `data.frame` or a `tibble`")

  test_data_bad_pred <- test_data
  test_data_bad_pred$pred <- as.character(test_data_bad_pred$pred)
  expect_error(harrell_c_index(test_data_bad_pred), "`pred` must be numeric")

  test_data_bad_surv <- test_data
  test_data_bad_surv$surv <- as.character(test_data_bad_surv$surv)
  expect_error(harrell_c_index(test_data_bad_surv), "`surv` must be Surv")
})

testthat::test_that("generates the plot properly", {
  test_data <- tibble::tibble(
    pred = rep(c(0.23, 0.45, 0.22, 0.15, 0.8), 100),
    surv = survival::Surv(time = rep(c(3, 2.4, 3.2, 4.1, 1.5), 100), event = rep(c(0, 1, 0, 0, 1), 100))
  )

  testthat::expect_s3_class(calibration_plot(test_data), "ggplot")
})
