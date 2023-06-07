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

#' Get effective log10 dose for an exact response level from a drda model
#'
#' This function returns the dose for which a dose-response model
#' gives an exact desired response level. For instance, the IC50 is the dose
#' that gives a response level of exactly 50. This is not to be confused with
#' the EC50, which is the dose that gives 50% of the maximum response and is
#' given by `get_EC_log()`.
#' @inheritParams get_hill_slope
#' @param level The desired exact response level.
#' @return The effective dose in log10 units.
#' @export
get_IC_log <- function(model, level = 50){
  IC <- drda::effective_dose(model, level, type = "absolute")[1]
  if(is.na(IC)){warning("Unable to fit an IC50")}
  return(IC)
}

#' Get effective log10 dose for a percent of maximum response from a drda model
#'
#' This function returns the dose for which a dose-response model
#' gives a desired fraction of the maximum response. For instance, the EC50 is
#' the dose that gives 50% of the maximum response, regardless of the direction
#' or magnitude of the response. This is not to be confused with the IC50, which
#' is the dose that gives a response of exactly 50 and is given by
#' `get_IC_log()`.
#' @inheritParams get_hill_slope
#' @inherit get_IC_log return
#' @param level The desired response in percent. Percent of the way from the
#'   minimum-dose response to the maximum-dose response, regardless of whether
#'   the response is positive or negative. Default is 50, giving the classic
#'   EC50.
#' @export
get_EC_log <- function(model, level=50){
  assertthat::assert_that(0<level && level<100,
                          msg="level must be between 0 and 100")
  # effective_dose expects level from 0 to 1
  drda::effective_dose(model, level/100, type = "relative")[1]
}


#' Get effective nanomolar dose for an exact response level from a drda model
#'
#' This function returns the dose for which a dose-response model gives an exact
#' desired response level. For instance, the IC50 is the dose that gives a
#' response level of exactly 50. This is not to be confused with the EC50, which
#' is the dose that gives 50% of the maximum response and is given by
#' `get_EC_nM()`.
#' @inheritParams get_IC_log
#' @return The effective dose in nanomolar units.
#' @export
get_IC_nM <- function(model, level=50){
  get_IC_log(model, level) |> logM_to_nM()
}

#' Get effective nanomolar dose for a percent of maximum response from a drda
#' model
#'
#' This function returns the dose for which a dose-response model gives a
#' desired fraction of the maximum response. For instance, the EC50 is the dose
#' that gives 50% of the maximum response, regardless of the direction or
#' magnitude of the response. This is not to be confused with the IC50, which is
#' the dose that gives a response of exactly 50 and is given by `get_IC_nM()`.
#' @inheritParams get_EC_log
#' @inherit get_IC_nM return
#' @export
get_EC_nM <- function(model, level=50){
  get_EC_log(model, level) |> logM_to_nM()
}

#' Estimate log10 EC50 from one dose-response point and given model parameters
#'
#' @param alpha The alpha parameter from a `drda` model.
#' @param delta The delta parameter from a `drda` model.
#' @param eta The eta parameter from a `drda` model.
#' @param log_dose The dose of the single point measurement in log units
#' @param activity The activity (NOT percent inhibition) observed at the tested
#'   dose
#' @return The estimated EC50 in log10 units
#' @export
EC50_log_from_point_params <- function(alpha, delta, eta, log_dose, activity){
  # 4-parameter logistic equation solved for phi
  log(delta/(activity-alpha)-1)/eta + log_dose |> unname()
}

#' Estimate log10 EC50 from one dose-response point and 3 parameters of a model
#'
#' @inheritParams get_hill_slope
#' @inherit EC50_log_from_point_params params return
#' @export
EC50_log_from_point_model <- function(model, log_dose, activity){
  c <- stats::coefficients(model)
  EC50_log_from_point_params(c["alpha"], c["delta"], c["eta"],
                              log_dose, activity)
}


#' Estimate nM EC50 from one dose-response point and 3 parameters of a model
#'
#' @inherit EC50_log_from_point_model params return
#' @param dose_nM The dose of the single point measurement in nanomolar units
#' @export
EC50_nM_from_point_model <- function(model, dose_nM, activity){
  EC50_log_from_point_model(model, nM_to_logM(dose_nM), activity) |>
    logM_to_nM()
}
