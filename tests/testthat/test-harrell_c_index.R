testthat::test_that("tests the parameters", {
  # should check that the data is a data frame and the variables are
  # correct.
  test_data <- tibble::tibble(
    pred = c(0.23, 0.45, 0.22, 0.15, 0.8),
    surv = survival::Surv(time = c(3, 2.4, 3.2, 4.1, 1.5), event = c(0, 1, 0, 0, 1))
  )

  expect_no_error(harrell_c_index(test_data))
  expect_no_error(harrell_c_index(test_data |> as.data.frame()))
  expect_error(harrell_c_index("error"), "the variable \\`data\\` must inherit from \\`data.frame\\`")

  test_data_bad_pred <- test_data
  test_data_bad_pred$pred <- as.character(test_data_bad_pred$pred)
  expect_error(harrell_c_index(test_data_bad_pred), "\\`pred\\` variable must be numeric")

  test_data_bad_surv <- test_data
  test_data_bad_surv$surv <- as.character(test_data_bad_surv$surv)
  expect_error(harrell_c_index(test_data_bad_surv), "\\`surv\\` variable must inherits from Surv")
})

testthat::test_that("calculates the result properly", {
  # extracted from the hmisc example w["C Index"] should contain the
  # value 0.98536516
  set.seed(123)
  for (i in 1:2) {
    n1 <- c(24, 138)[i]
    n0 <- c(62, 102)[i]
    y <- c(rep(0, n0), rep(1, n1))
    d <- 3
    x <- c(rnorm(n0, 0), rnorm(n1, d))
    w <- Hmisc::rcorr.cens(x, y)
    w["C Index"]
    w["S.D."] / 2
  }

  expect_equal(w["C Index"], 0.98536516, ignore_attr = TRUE)
})
