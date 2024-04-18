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
harrell_c_index <- function(data) {
  # The data should contain pred and surv values for which is needed to calculate
  # the harrell's c-index

  # test variable types
  stopifnot("`data` must be a `data.frame` or a `tibble`" = inherits(data, "data.frame"))
  stopifnot("`pred` must be a variable in `data`" = "pred" %in% names(data))
  stopifnot("`pred` must be numeric" = is.numeric(data$pred))
  stopifnot("`surv` must be a variable in `data`" = "surv" %in% names(data))
  stopifnot("`surv` must be Surv" = inherits(data$surv, "Surv"))

  # calculate the index
  return(Hmisc::rcorr.cens(data$pred, data$surv))
}
