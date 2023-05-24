#' Get the Hill slope from a `drda` model
#'
#' @param model A logistic model of the type returned by [drda::drda()]
#'
#' @return The Hill slope (growth rate) of the model. Unsigned (always
#'   positive).
#' @export
#'
get_hill_slope <- function(model){
  stats::coefficients(model)["eta"]
}

#' Get effective log10 dose for a response level from a drda model
#'
#' @inheritParams get_hill_slope
#' @param level The desired response level in percent. Percent of the way from
#'   the minimum-dose response to the maximum-dose response, regardless of
#'   whether the response is positive or negative. Default is 50, giving the
#'   classic EC50.
#' @return The effective dose in log10 units.
#' @export
get_EC_logM <- function(model, level=50){
  assertthat::assert_that(0<level && level<100,
                          msg="level must be between 0 and 100")
  # effective_dose expects level from 0 to 1
  drda::effective_dose(model, level/100)[1]
}

#' Get effective nanomolar dose for a response level from a drda model
#'
#' @inheritParams get_EC_logM
#' @return The effective dose in nanomolar units.
#' @export
get_EC_nM <- function(model, level=50){
  get_EC_logM(model, level) |> logM_to_nM()
}

#' Estimate EC50 from one dose-response point and given model parameters
#'
#' @param alpha The alpha parameter from a `drda` model.
#' @param delta The delta parameter from a `drda` model.
#' @param eta The eta parameter from a `drda` model.
#' @param log_dose The dose of the single point measurement in log units
#' @param activity The activity (NOT percent inhibition) observed at the tested
#'   dose
#' @return The estimated EC50 in log10 units
#' @export
EC50_logM_from_point_params <- function(alpha, delta, eta, log_dose, activity){
  # 4-parameter logistic equation solved for phi
  log(delta/(activity-alpha)-1)/eta + log_dose |> unname()
}

#' Estimate log EC50 from one dose-response point and 3 parameters of a model
#'
#' @inheritParams get_hill_slope
#' @inherit EC50_logM_from_point_params params return
#' @export
EC50_logM_from_point_model <- function(model, log_dose, activity){
  c <- stats::coefficients(model)
  EC50_logM_from_point_params(c["alpha"], c["delta"], c["eta"],
                              log_dose, activity)
}


#' Estimate nM EC50 from one dose-response point and 3 parameters of a model
#'
#' @inherit EC50_logM_from_point_model params return
#' @param dose_nM The dose of the single point measurement in nanomolar units
#' @export
EC50_nM_from_point_model <- function(model, dose_nM, activity){
  EC50_logM_from_point_model(model, nM_to_logM(dose_nM), activity) |>
    logM_to_nM()
}
