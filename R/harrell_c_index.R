#' this function is a wrapper for the `Hmisc::rcorr.cens` function of the `Hmisc`
#' package.
#'
#' @param data `tibble` or `data.frame` that contains the predicted values in a variable called `pred` and the survival object in a variable called `surv`. The survival outcome object must be created with the package `survival`
#'
#' @return the Harrell's c-index for the given data
#'
#' @importFrom Hmisc rcorr.cens
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' for (i in 1:2) {
#'   n1 <- c(24, 138)[i]
#'   n0 <- c(62, 102)[i]
#'   y <- c(rep(0, n0), rep(1, n1))
#'   times <- ifelse(y == 1, rnorm(2, 1), rnorm(4,1))
#'   y <- survival::Surv(time = times, event = y)
#'   d <- 3
#'   x <- c(rnorm(n0, 0), rnorm(n1, d))
#'   harrell_c_index(tibble::tibble(pred = x, surv = y))
#' }
harrell_c_index <- function(data) {
  # The data should contain pred and surv values for which is needed to calculate
  # the harrell's c-index

  # test variable types
  if (!inherits(data, "data.frame")) {
    cli::cli_abort(
      c(
        "the variable `data` must inherit from `data.frame`",
        "i" = "it is expected to be a `data.frame` or a `tibble`"
      )
    )
  }

  df_var <- c("pred", "surv")
  arg_in_data <- df_var %in% names(data)
  if (!all(arg_in_data)) {
    cli::cli_abort("{.arg {df_var[!arg_in_data]}} must be {?a/} variable{?s} of `data`")
  }

  variable_type_error <- NULL
  bad_type <- 0

  if (!is.numeric(data$pred)) {
    bad_type <- bad_type + 1
    variable_type_error <- c(variable_type_error, "*" = cli::format_error("{.arg pred} variable must be numeric"))
  }

  if (!inherits(data$surv, "Surv")) {
    bad_type <- bad_type + 1
    variable_type_error <- c(
      variable_type_error,
      c(
        "*" = "{.arg surv} variable must inherits from Surv",
        "i" = "{.arg surv} variable should be created using `survival::Surv` function"
        )
      )
  }

  if (!is.null(variable_type_error)) {
    error_message <- c(
      "{bad_type} argument{?s} do{?es/} not have the properly type",
      variable_type_error
    )
    cli::cli_abort(error_message)
  }

  # calculate the index
  return(Hmisc::rcorr.cens(data$pred, data$surv))
}
