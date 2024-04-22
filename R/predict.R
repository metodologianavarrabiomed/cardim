#' @title
#' calculates the cardim model predictions
#'
#' @description
#' Calculates the cardim model predictions by using the [predict_males()] and [predict_females()] functions.
#' The `data.frame` must have the following structure.
#'
#' | **Variable** | **Type** |
#' |--------|-----|
#' | Id | numeric or string  |
#' | sex | "Male" or "Female"  |
#' | Age, years | numeric  |
#' | Income less than \eqn{18000}€ | dichotomic |
#' | Diabetes duration, years | numeric |
#' | hba1c, \eqn{\%} | numeric |
#' | Hypertension treatment at baseline | dichotomic |
#' | Logarithm of albumine creatinine ratio, log(mg/g) | numeric |
#' | \eqn{\text{Total cholesterol} - \text{hdl cholesterol}}, mmol/L | numeric |
#' | Physical activity, inactive | dichotomic |
#' | Physical activity, partially active | dichotomic |
#' | Atrial fibrillation at baseline | dichotomic |
#' | \eqn{\text{Systolic blood pressure} - \text{diastolic blood pressure}}, mmHg | numeric |
#' | Retinopathy at baseline | dichotomic |
#' | Smoking status, smoker | dichotomic |
#' | Smoking status, ex-smoker | dichotomic |
#'
#' @md
#'
#' @param data `data.frame` or `tibble` where all the patients data is stored
#'
#' @return cardim predictions for the given cohort
#'
#' @importFrom cli format_message cli_abort format_error
#' @importFrom dplyr select left_join all_of
#'
#' @export
#'
#' @examples
#'
#' predict(
#'   tibble::tibble(
#'   id = c(1,2,3),
#'   sex = c("Male", "Female", "Female"),
#'   age = c(60, 65, 45),
#'   diabetes_duration = c(6, 10, 2),
#'   hba1c = c(9.8, 6.7, 8.5),
#'   hypertension_treatment = c(1, 1, 0),
#'   income_less_18000 = c(1, 1, 1),
#'   log_albtocreatratio = c(2.25, 1.84, 2.36),
#'   non_hdl = c(3.95, 4.3, 3.4),
#'   pulse_pressure = c(70, 74, 65),
#'   physical_activity_inactive = c(0, 0, 0),
#'   physical_activity_partially_active = c(1, 0, 1),
#'   previous_atrial_f = c(0, 0, 0),
#'   retinopathy = c(1, 1, 0),
#'   smoking_status_smoker = c(0, 0, 1),
#'   smoking_status_ex_smoker = c(0, 0, 0)
#'   )
#' )
predict <- function(data) {
  # checks that the data argument is a data.frame/tibble
  if (!inherits(data, "data.frame")) {
    cli::cli_abort(
      c(
        "the variable `data` must inherit from `data.frame`",
        "i" = "it is expected to be a `data.frame` or a `tibble`"
      )
    )
  }

  # defines all the checks for each model variable
  is_dichotomic <- \(x) is.numeric(x) && all(x %in% c(0, 1))
  dichotomic_error <- \(x) cli::format_message("the variable {.arg {x}} must be dichotomic (0, 1)")
  numeric_error <- \(x) cli::format_message("the variable {.arg {x}} must be numeric")

  variable_validators <- list(
    id = list(validator = \(x) is.numeric(x) || is.character(x), error_message = "the variable `id` must be numeric or character"),
    sex = list(
      validator = \(x) is.character(x) && all(x %in% c("Male", "Female")),
      error_message = 'the variable `sex` must be of character type with categories ("Male", "Female")'
    ),
    age = list(validator = is.numeric, error_message = numeric_error("age")),
    diabetes_duration = list(validator = is.numeric, error_message = numeric_error("diabetes_duration")),
    hba1c = list(validator = is.numeric, error_message = numeric_error("hba1c")),
    hypertension_treatment = list(validator = is_dichotomic, error_message = dichotomic_error("hypertension_treatment")),
    log_albtocreatratio = list(validator = is.numeric, error_message = numeric_error("log_albtocreatratio")),
    non_hdl = list(validator = is.numeric, error_message = numeric_error("non_hdl")),
    income_less_18000 = list(validator = is_dichotomic, error_message = dichotomic_error("income_less_18000")),
    physical_activity_inactive = list(validator = is_dichotomic, error_message = dichotomic_error("physical_activity_inactive")),
    physical_activity_partially_active = list(validator = is_dichotomic, error_message = dichotomic_error("physical_activity_partially_active")),
    previous_atrial_f = list(validator = is_dichotomic, error_message = dichotomic_error("previous_atrial_f")),
    pulse_pressure = list(validator = is.numeric, error_message = numeric_error("pulse_pressure")),
    retinopathy = list(validator = is_dichotomic, error_message = dichotomic_error("retinopathy")),
    smoking_status_smoker = list(validator = is_dichotomic, error_message = dichotomic_error("smoking_status_smoker")),
    smoking_status_ex_smoker = list(validator = is_dichotomic, error_message = dichotomic_error("smoking_status_ex_smoker"))
  )

  # tests that the variables exists
  exist_variables <- names(variable_validators) %in% names(data)
  if (!all(exist_variables)) {
    not_included_variables <- names(variable_validators)[!exist_variables]
    error_message <- cli::format_error("{.arg {not_included_variables}} argument{?s} must be in `data`")
    cli::cli_abort(error_message)
  }

  # tests the variables types
  is_error_type <- sapply(
    names(variable_validators),
    \(name, data, variable_validators) {
      variable_validators[[name]]$validator(data[[name]])
    },
    data = data,
    variable_validators = variable_validators
  )
  if (!all(is_error_type)) {
    vector_of_errors <- sapply(variable_validators[!is_error_type], \(x) x$error_message)
    errors <- c("x" = cli::format_message("{sum(!is_error_type)} argument{?s} do{?es/} not have the properly type"))
    names(vector_of_errors) <- rep("*", sum(!is_error_type))
    cli::cli_abort(c(errors, vector_of_errors))
  }

  # calculates the predictions
  # TODO:
  #   - [ ] what do we do with NA?
  #     - replace them with the mean value in the variable?
  #     - ignore that patient and return NA in its prediction
  #     - allow the user to decide?
  #
  # To start we will set the mean value of the patient but will be executed
  # inside the prediction function for males and females. Therefore no need to
  # be worried at this point.
  #
  # for each row we need to estimate the prediction
  data_males <- subset(data, data$sex == "Male")
  data_females <- subset(data, data$sex == "Female")

  data_males$pred <- predict_males(
      age = data_males$age,
      diabetes_duration = data_males$diabetes_duration,
      hba1c = data_males$hba1c,
      hypertension_treatment = data_males$hypertension_treatment,
      log_albtocreatratio = data_males$log_albtocreatratio,
      non_hdl = data_males$non_hdl,
      income_less_18000 = data_males$income_less_18000,
      physical_activity_inactive = data_males$physical_activity_inactive,
      physical_activity_partially_active = data_males$physical_activity_partially_active,
      previous_atrial_f = data_males$previous_atrial_f,
      pulse_pressure = data_males$pulse_pressure,
      retinopathy = data_males$retinopathy,
      smoking_status_smoker = data_males$smoking_status_smoker,
      smoking_status_ex_smoker = data_males$smoking_status_ex_smoker,
      .test_variables = FALSE
    )

  pred_males <- data_males |> dplyr::select(dplyr::all_of(c("id", "pred")))

  data_females$pred <- predict_females(
      age = data_females$age,
      diabetes_duration = data_females$diabetes_duration,
      hba1c = data_females$hba1c,
      hypertension_treatment = data_females$hypertension_treatment,
      log_albtocreatratio = data_females$log_albtocreatratio,
      non_hdl = data_females$non_hdl,
      income_less_18000 = data_females$income_less_18000,
      physical_activity_inactive = data_females$physical_activity_inactive,
      physical_activity_partially_active = data_females$physical_activity_partially_active,
      previous_atrial_f = data_females$previous_atrial_f,
      pulse_pressure = data_females$pulse_pressure,
      retinopathy = data_females$retinopathy,
      smoking_status_smoker = data_females$smoking_status_smoker,
      smoking_status_ex_smoker = data_females$smoking_status_ex_smoker,
      .test_variables = FALSE
    )

  pred_females <- data_females |> dplyr::select(dplyr::all_of(c("id", "pred")))

  # merge males and females into one table
  pred_results <- rbind(pred_females, pred_males)

  # returns the original data table with the predictions joined
  return(data |> dplyr::left_join(pred_results, by = "id"))
}
