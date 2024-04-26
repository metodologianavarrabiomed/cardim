testthat::test_that("`data` parameter inherits a `data.frame`", {
  # load example data
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))

  testthat::expect_error(cardim::predict_cardim(5), "the variable `data` must inherit from `data.frame`")
  testthat::expect_no_error(cardim::predict_cardim(data = data %>% as.data.frame()))
  testthat::expect_no_error(cardim::predict_cardim(data = data))
})

testthat::test_that("`data` has the needed variables to estimate the risk", {
  # load example data
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))

  testthat::expect_error(cardim::predict_cardim(tibble::tibble(x = 1, y = 2)))
  testthat::expect_error(cardim::predict_cardim(tibble::tibble(age = 1, y = 2)))
  testthat::expect_no_error(cardim::predict_cardim(data = data))
})

testthat::test_that("`data` variables have the expected type", {
  # load example data
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))

  testthat::expect_no_error(cardim::predict_cardim(data))

  # modify some variables and check that fails
  data$hba1c <- data$hba1c |> as.character()
  testthat::expect_error(cardim::predict_cardim(data), "the variable \\`hba1c\\` must be numeric")

  data$previous_atrial_f <- data$previous_atrial_f |> as.character()
  testthat::expect_error(cardim::predict_cardim(data), "the variable \\`previous_atrial_f\\` must be dichotomic \\(0, 1\\)")

  data$sex[2] <- "as"
  testthat::expect_error(cardim::predict_cardim(data), 'the variable \\`sex\\` must be of character type with categories \\(\\"Male\\", \\"Female\\"\\)')

  data$id <- data$id |> as.list()
  testthat::expect_error(cardim::predict_cardim(data), "the variable \\`id\\` must be numeric or character")

})

testthat::test_that("the `NA` substitution to the mean value works", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))

  # generate random indexes
  set.seed(123)
  num_nas <- 8
  rows <- sample(nrow(data), num_nas, replace = TRUE)
  cols <- sample(ncol(data), num_nas, replace = TRUE)

  for (idx in 1:num_nas) {
    data[rows[idx], cols[idx]] <- NA
  }

  testthat::expect_no_error(cardim::predict_cardim(data))

  res <- cardim::predict_cardim(data)
  testthat::expect_false(any(is.na(res$pred)))
})

testthat::test_that("not substituting the NA works", {
  data <- readRDS(testthat::test_path("fixtures", "data_example.rds"))

  # generate random indexes
  set.seed(123)
  num_nas <- 3
  rows <- sample(nrow(data), num_nas, replace = TRUE)
  cols <- sample(ncol(data), num_nas, replace = TRUE)

  for (idx in 1:num_nas) {
    data[rows[idx], cols[idx]] <- NA
  }

  testthat::expect_no_error(cardim::predict_cardim(data, FALSE))

  res <- cardim::predict_cardim(data, FALSE)
  testthat::expect_false(all(is.na(res$pred)))
})
