#' @title
#' Calibration plot
#'
#' @description
#' Generates the calibration plot for the given data. A calibration plot is a representation
#' of the predicted risk vs the observed. In survival analysis there is no observed risk in each
#' of the patients, we only know if they suffer an event or not.
#'
#' To estimate the observed risk this function group the patients by the estimated risk. Once they
#' are grouped we can estimate the observed risk in each group using a Kaplan-Meier estimator.
#' Finally we can plot the results, the mean of the predicted risk and the observed risk in each of
#' the groups. If the points are below the bisecting line means that the model is overpredicting.
#' On the other hand, if the points are above the bisecting line means that the model is
#' underpredicting.
#'
#' @param data `tibble` or `data.frame` that contains the predicted values in a variable called `pred` and the survival object in a variable called `surv`. The survival outcome object must be created with the package `survival`
#' @param time time for the observed risk estimation, should be the same of the predictions.
#' @param n_bins number of groups in the calibration plot
#' @param xlim limit in the x-axis
#' @param ylim limit in the y-axis
#'
#' @return a calibration plot for the given data
#'
#' @importFrom dplyr group_by mutate n distinct arrange bind_rows
#' @importFrom tibble tibble
#' @importFrom survival survfit Surv
#'
#' @export
calibration_plot <- function(data, time = 5, n_bins = 10, xlim = 1, ylim = 1) {
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

  # generate the calibration plot data
  df_deciles <- data.frame(predictions = data$pred, outcomes = data$surv[, "status"], time = time) %>%
    dplyr::mutate(bin = dplyr::ntile(predictions, n_bins))

  df_predicted <- df_deciles |>
    dplyr::group_by(bin) |>
    dplyr::mutate(
      n = dplyr::n(),
      bin_pred = mean(predictions, na.rm = TRUE),
      mean_bin_obs = mean(outcomes, na.rm = TRUE),
      se = sqrt((bin_pred * (1 - bin_pred)) / n)
    ) |>
    dplyr::distinct(bin, n, bin_pred, se, mean_bin_obs) |>
    dplyr::arrange(bin)

  df_observed <- df_deciles |>
    dplyr::group_by(bin) |>
    dplyr::group_map(~ {
      .x$surv_obj <- survival::Surv(.x$time, .x$outcomes)
      km <- survival::survfit(surv_obj ~ 1, data = .x)
      return(
        tibble::tibble(
          bin = .y$bin,
          # TODO: do it with competing risks?
          observed = 1 - km$surv[length(km$surv)],
          se_observed = km$std.err[length(km$std.err)]
        )
      )
    }) |>
    dplyr::bind_rows()
  out <- dplyr::left_join(df_predicted, df_observed, by = "bin")
  names(out) <- c("bin", "n", "predicted", "se", "mean_bin_obs", "observed", "se_observed")

  out$ll <- out$observed - 1.96 * out$se_observed
  out$ul <- out$observed + 1.96 * out$se_observed

  my_theme <- function() {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(colour = "gray90"),
        panel.grid.minor = ggplot2::element_line(colour = "gray90", linetype = "blank"),
        axis.title.x = ggplot2::element_text(
          size = 20,
          face = "italic",
          margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)
        ),
        axis.title.y = ggplot2::element_text(
          size = 20,
          face = "italic",
          margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)
        ),
        axis.text = ggplot2::element_text(size = 16, color = "gray25"),
        plot.title = ggplot2::element_text(size = 20, hjust = 0.25),
        plot.background = ggplot2::element_rect(fill = "white", color = "white")
      )
  }

  gg <- out %>%
    ggplot2::ggplot() +
    ggplot2::scale_y_continuous(limits = c(0, xlim)) +
    ggplot2::scale_x_continuous(limits = c(0, ylim)) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::geom_abline(linetype = "dashed") +
    ggplot2::geom_point(
      ggplot2::aes(x = predicted, y = observed),
      size = 2.5,
      color = "#00BFC4",
      stroke = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(x = predicted, y = observed, ymin = ll, ymax = ul),
      linewidth = 1,
      color = "#00BFC4",
      width = 0.01
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(x = predicted, y = observed),
      weight = 0.5,
      linewidth = 0.6,
      color = "#00BFC4",
      se = FALSE,
      method = "loess",
      fullrange = FALSE
    ) +
    ggplot2::xlab("Predicted Risk") +
    ggplot2::ylab("Observed Risk") +
    ggplot2::geom_vline(xintercept = 0, color = "black") +
    ggplot2::geom_hline(yintercept = 0, color = "black") +
    my_theme()

  return(gg)
}
