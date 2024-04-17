testthat::test_that("`data` parameter to inherit a `data.frame`", {
  # load example data
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))

  testthat::expect_error(cardim::predict(5))
  testthat::expect_no_error(cardim::predict(data = data %>% as.data.frame()))
  testthat::expect_no_error(cardim::predict(data = data))
})

testthat::test_that("`data` has the needed variables to estimate the risk", {
  # load example data
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))

  testthat::expect_error(cardim::predict(tibble::tibble(x = 1, y = 2)))
  testthat::expect_error(cardim::predict(tibble::tibble(age = 1, y = 2)))
  testthat::expect_no_error(cardim::predict(data = data))
})

testthat::test_that("`data` variables have the expected type", {
  # load example data
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))

  testthat::expect_no_error(cardim::predict(data))

  # modify some variables and check that fails
  data$hba1c <- data$hba1c |> as.character()
  testthat::expect_error(cardim::predict(data))
})
