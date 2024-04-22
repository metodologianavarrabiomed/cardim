#' @title
#' calculates the cardim females model prediction
#'
#' @description
#' calculates the model prediction for the given parameters. The risk predictions
#' are calculated as follows.
#'
#' \deqn{
#' \text{prediction} = 1 - S_0(t)^{exp(\beta X)}
#' }
#'
#' where \eqn{\beta} is the model coefficients and \eqn{S_0(t)} is \eqn{1 - \lambda_0(t)} that
#' is the cumulative incidence at the time \eqn{t}.
#'
#' @param age Age in years
#' @param diabetes_duration Diabetes duration in years
#' @param hba1c HbA1c in \%
#' @param hypertension_treatment if the patient has an active hypertension treatment at baseline, 1 = yes, 0 = no
#' @param log_albtocreatratio logarithm of the albumine creatinine ratio in log(mg/g)
#' @param non_hdl \eqn{\text{total cholesterol} - \text{hdl cholesterol}} in mmol/L
#' @param income_less_18000 if the patient is in the category `income_less_18000`, 1 = yes, 0 = no
#' @param physical_activity_inactive if the patient is in the category `inactive`, 1 = yes, 0 = no
#' @param physical_activity_partially_active if the patient is in the category `partially_inactive`, 1 = yes , 0 = no
#' @param previous_atrial_f if the patient had atrial fibrillation at baseline, 1 = yes, 0 = no
#' @param pulse_pressure \eqn{\text{systolic blood pressure} - \text{diastolic blood pressure}} in mmHg
#' @param retinopathy if the patient had retinopathy at baseline, 1 = yes, 0 = no
#' @param smoking_status_smoker if the patient is in the category `smoker`, 1 = yes, 0 = no
#' @param smoking_status_ex_smoker if the patient is in the category `ex-smoker`, 1 = yes, 0 = no
#'
#' @return model prediction for the given parameters
#'
#' @export
#'
#' @examples
#' predict_females(
#'    age = 71,
#'    diabetes_duration = 24,
#'    hba1c = 9.9,
#'    hypertension_treatment = 1,
#'    log_albtocreatratio = 4.577,
#'    non_hdl = 2.793,
#'    income_less_18000 = 1,
#'    physical_activity_inactive = 0,
#'    physical_activity_partially_active = 0,
#'    previous_atrial_f = 0,
#'    pulse_pressure = 67,
#'    retinopathy = 1,
#'    smoking_status_smoker = 0,
#'    smoking_status_ex_smoker = 0
#' )
predict_females <- function(
    age,
    diabetes_duration,
    hba1c,
    hypertension_treatment,
    log_albtocreatratio,
    non_hdl,
    income_less_18000,
    physical_activity_inactive,
    physical_activity_partially_active,
    previous_atrial_f,
    pulse_pressure,
    retinopathy,
    smoking_status_smoker,
    smoking_status_ex_smoker) {
  args <- match.call() |>
    as.list() |>
    names()
  args <- args[-1]

  # test that all parameters are numeric
  are_numeric <- sapply(args, \(v, envir) is.numeric(get(v, envir)), envir = environment())
  if (!all(are_numeric)) {
    not_numeric <- sapply(args[!are_numeric], \(x) paste("`", x, "`", sep = ""))
    error_message <- cli::format_error(message = "{.pkg {not_numeric}} parameter{?s} must be numeric")
    stop(error_message)
  }

  # test that variables are dichotomous
  dichotomous_variables <- c("hypertension_treatment", "income_less_18000", "physical_activity_inactive", "physical_activity_partially_active", "previous_atrial_f", "retinopathy", "smoking_status_smoker", "smoking_status_ex_smoker")
  are_dichotomous <- sapply(dichotomous_variables, function(v, envir) all(get(v, envir) %in% c(0, 1)), envir = environment())
  if (!all(are_dichotomous)) {
    not_dichotomous <- sapply(dichotomous_variables[!are_dichotomous], \(x) paste("`", x, "`", sep = ""))
    error_message <- cli::format_error(message = "{.pkg {not_dichotomous}} parameter{?s} must be dichotomous")
    stop(error_message)
  }

  # firstly we center the values with their mean
  mean_values <- list(age = 57.318, diabetes_duration = 7.608, hba1c = 7.017, hypertension_treatment = 0.556, log_albtocreatratio = 1.934, non_hdl = 3.775, income_less_18000 = 0.792, physical_activity_inactive = 0.574, physical_activity_partially_active = 0.313, previous_atrial_f = 0.037, pulse_pressure = 56.489, retinopathy = 0.16, smoking_status_smoker = 0.091, smoking_status_ex_smoker = 0.131)

  model_coefficients = list(age = 0.058866, income_less_18000 = 0.406477, diabetes_duration = 0.022072, hba1c = 0.1382, hypertension_treatment = 0.271602, log_albtocreatratio = 0.098929, non_hdl = 0.176775, physical_activity_inactive = 0.254179, physical_activity_partially_active = 0.26858, previous_atrial_f = 0.344456, pulse_pressure = 0.010361, retinopathy = 0.44503, smoking_status_smoker = -0.059986 , smoking_status_ex_smoker = 0.66877, age_hba1c = -0.004711, age_hypertension_treatment = -0.020774, age_smoking_status_ex_smoker = 0.00483, age_smoking_status_smoker = -0.050457)

  betax <- ((age - mean_values[["age"]]) * model_coefficients[["age"]]) +
      ((diabetes_duration - mean_values[["diabetes_duration"]]) * model_coefficients[["diabetes_duration"]]) +
      ((hba1c - mean_values[["hba1c"]]) * model_coefficients[["hba1c"]]) +
      ((hypertension_treatment - mean_values[["hypertension_treatment"]]) * model_coefficients[["hypertension_treatment"]]) +
      ((log_albtocreatratio - mean_values[["log_albtocreatratio"]]) * model_coefficients[["log_albtocreatratio"]]) +
      ((non_hdl - mean_values[["non_hdl"]]) * model_coefficients[["non_hdl"]]) +
      ((income_less_18000 - mean_values[["income_less_18000"]]) * model_coefficients[["income_less_18000"]]) +
      ((physical_activity_inactive - mean_values[["physical_activity_inactive"]]) * model_coefficients[["physical_activity_inactive"]]) +
      ((physical_activity_partially_active - mean_values[["physical_activity_partially_active"]]) * model_coefficients[["physical_activity_partially_active"]]) +
      ((previous_atrial_f - mean_values[["previous_atrial_f"]]) * model_coefficients[["previous_atrial_f"]]) +
      ((pulse_pressure - mean_values[["pulse_pressure"]]) * model_coefficients[["pulse_pressure"]]) +
      ((retinopathy - mean_values[["retinopathy"]]) * model_coefficients[["retinopathy"]]) +
      ((smoking_status_smoker - mean_values[["smoking_status_smoker"]]) * model_coefficients[["smoking_status_smoker"]]) +
      ((smoking_status_ex_smoker - mean_values[["smoking_status_ex_smoker"]]) * model_coefficients[["smoking_status_ex_smoker"]]) +
      ((age - mean_values[["age"]]) * (hba1c - mean_values[["hba1c"]]) * model_coefficients[["age_hba1c"]]) +
      ((age - mean_values[["age"]]) * (hypertension_treatment - mean_values[["hypertension_treatment"]]) * model_coefficients[["age_hypertension_treatment"]]) +
      ((age - mean_values[["age"]]) * (smoking_status_ex_smoker - mean_values[["smoking_status_ex_smoker"]]) * model_coefficients[["age_smoking_status_ex_smoker"]]) +
      ((age - mean_values[["age"]]) * (smoking_status_smoker - mean_values[["smoking_status_smoker"]]) * model_coefficients[["age_smoking_status_smoker"]])

  return(1 - 0.9626656^exp(betax))
}
