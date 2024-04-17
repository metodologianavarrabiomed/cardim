#' calculates the cardim model prediction for males given the needed parameters
#'
#' @param age Age in years
#' @param diabetes_duration Diabetes duration in years
#' @param hba1c HbA1c in %
#' @param hypertension_treatment if the patient has an active hypertension treatment at baseline, 1 = yes, 0 = no
#' @param log_albtocreatratio logarithm of the albumine creatinine ratio in log(mg/g)
#' @param non_hdl $total cholesterol - hdl cholesterol$ in mmol/L
#' @param income_less_18000 if the patient is in the category `income_less_18000`, 1 = yes, 0 = no
#' @param physical_activity_inactive if the patient is in the category `inactive`, 1 = yes, 0 = no
#' @param physical_activity_partially_active if the patient is in the category `partially_inactive`, 1 = yes , 0 = no
#' @param previous_atrial_f if the patient had atrial fibrillation at baseline, 1 = yes, 0 = no
#' @param pulse_pressure $systolic blood pressure - diastolic blood pressure$ in mmHg
#' @param retinopathy if the patient had retinopathy at baseline, 1 = yes, 0 = no
#' @param smoking_status_smoker if the patient is in the category `smoker`, 1 = yes, 0 = no
#' @param smoking_status_ex_smoker if the patient is in the category `ex-smoker`, 1 = yes, 0 = no
#'
#' @return model prediction for the given parameters
#' @export
#'
#' @examples
#' predict_males(
#'   age = 71,
#'   diabetes_duration = 24,
#'   hba1c = 9.9,
#'   hypertension_treatment = 1,
#'   log_albtocreatratio = 4.577,
#'   non_hdl = 2.793,
#'   income_less_18000 = 1,
#'   physical_activity_inactive = 0,
#'   physical_activity_partially_active = 0,
#'   previous_atrial_f = 0,
#'   pulse_pressure = 67,
#'   retinopathy = 1,
#'   smoking_status_smoker = 0,
#'   smoking_status_ex_smoker = 0
#' )
predict_males <- function(age, diabetes_duration, hba1c, hypertension_treatment, log_albtocreatratio, non_hdl, income_less_18000, physical_activity_inactive, physical_activity_partially_active, previous_atrial_f, pulse_pressure, retinopathy, smoking_status_smoker, smoking_status_ex_smoker) {
  # firstly we center the values with their mean
  mean_values <- list(age = 55.269, diabetes_duration = 7.048, hba1c = 6.962, hypertension_treatment = 0.526, log_albtocreatratio = 2.073, non_hdl = 3.722, income_less_18000 = 0.573, physical_activity_inactive = 0.671, physical_activity_partially_active = 0.246, previous_atrial_f = 0.05, pulse_pressure = 56.463, retinopathy = 0.151, smoking_status_smoker = 0.333, smoking_status_ex_smoker = 0.318)

  model_coefficients = list(age = 0.044741, income_less_18000 = 0.269727, diabetes_duration = 0.013302, hba1c = 0.108134, hypertension_treatment = 0.132284, log_albtocreatratio = 0.122076, non_hdl = 0.123658, physical_activity_inactive = 0.319014, physical_activity_partially_active = 0.197057, previous_atrial_f = 0.244639, pulse_pressure = 0.004444, retinopathy = 0.31149, smoking_status_smoker = -0.014711, smoking_status_ex_smoker = 0.42625, age_hba1c = 0.003214, age_hypertension_treatment = -0.005613, age_smoking_status_ex_smoker = 0.000011, age_smoking_status_smoker = -0.007295)

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

  # print(((age - mean_values[["age"]]) * (smoking_status_ex_smoker - mean_values[["smoking_status_ex_smoker"]]) * model_coefficients[["age_smoking_status_ex_smoker"]]))
  # print((age - mean_values[["age"]]))
  # print((smoking_status_ex_smoker - mean_values[["smoking_status_ex_smoker"]]))
  # print(model_coefficients[["age_smoking_status_ex_smoker"]])

  print(((age - mean_values[["age"]]) * (smoking_status_smoker - mean_values[["smoking_status_smoker"]]) * model_coefficients[["age_smoking_status_smoker"]]))
  print((age - mean_values[["age"]]))
  print((smoking_status_smoker - mean_values[["smoking_status_smoker"]]))
  print(model_coefficients[["age_smoking_status_smoker"]])
  return(1 - 0.9429132^exp(betax))
}
