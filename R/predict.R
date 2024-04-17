#' predicts the cardim model predictions for the given data
#'
#' @param data `data.frame` or `tibble` where all the patients data is stored
#'
#' @return cardim predictions
#' @export
#'
#' @examples
#'
#' predict(
#'   tibble::tibble(
#'     id = 1,
#'     age = 50,
#'     sex = "Male"
#'   )
#' )
predict <- function(data) {
  # TODO:
  #   - [ ] calculate the predictions
  #   - [ ] generate the return value
  #     - should it be a tibble with only predictions?
  #     - should it be appended to the data?
  #   - [ ] custom error messages

  # checks that the data argument is a data.frame/tibble
  stopifnot(inherits(data, "data.frame"))

  # defines all the checks for each model variable
  variable_validators <- list(
    id = \(x) is.numeric(x) || is.character(x),
    sex = is.character,
    age = is.numeric,
    diabetes_duration = is.numeric,
    hba1c = is.numeric,
    hypertension_treatment = is.numeric,
    log_albtocreatratio = is.numeric,
    non_hdl = is.numeric,
    income_less_18000 = is.numeric,
    physical_activity_inactive = is.numeric,
    physical_activity_partially_active = is.numeric,
    previous_atrial_f = is.numeric,
    pulse_pressure = is.numeric,
    retinopathy = is.numeric,
    smoking_status_smoker = is.numeric,
    smoking_status_ex_smoker = is.numeric
  )

  # tests that the variables exists
  stopifnot(all(names(variable_validators) %in% names(data)))

  # tests the variables types
  types_checked_results <- sapply(
    names(variable_validators),
    \(name, data, variable_validators) {
      variable_validators[[name]](data[[name]])
    },
    data = data,
    variable_validators = variable_validators
  )
  stopifnot(all(types_checked_results))

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

  # calculate the predictions in males
  pred_males <- data_males %>%
    dplyr::mutate(pred = predict_males(
      age = age,
      diabetes_duration = diabetes_duration,
      hba1c = hba1c,
      hypertension_treatment = hypertension_treatment,
      log_albtocreatratio = log_albtocreatratio,
      non_hdl = non_hdl,
      income_less_18000 = income_less_18000,
      physical_activity_inactive = physical_activity_inactive,
      physical_activity_partially_active = physical_activity_partially_active,
      previous_atrial_f = previous_atrial_f,
      pulse_pressure = pulse_pressure,
      retinopathy = retinopathy,
      smoking_status_smoker = smoking_status_smoker,
      smoking_status_ex_smoker = smoking_status_ex_smoker
    )
    ) %>%
    dplyr::select(id, pred)

  # calculate the prediction in females
  pred_females <- data_females %>%
    dplyr::mutate(pred = predict_females(
      age = age,
      diabetes_duration = diabetes_duration,
      hba1c = hba1c,
      hypertension_treatment = hypertension_treatment,
      log_albtocreatratio = log_albtocreatratio,
      non_hdl = non_hdl,
      income_less_18000 = income_less_18000,
      physical_activity_inactive = physical_activity_inactive,
      physical_activity_partially_active = physical_activity_partially_active,
      previous_atrial_f = previous_atrial_f,
      pulse_pressure = pulse_pressure,
      retinopathy = retinopathy,
      smoking_status_smoker = smoking_status_smoker,
      smoking_status_ex_smoker = smoking_status_ex_smoker
    )
    ) %>%
    dplyr::select(id, pred)

  # merge males and females into one table
  pred_results <- dplyr::bind_rows(pred_females, pred_males)

  # returns the original data table with the predictions joined
  return(data %>% dplyr::left_join(pred_results, by = "id"))
}
