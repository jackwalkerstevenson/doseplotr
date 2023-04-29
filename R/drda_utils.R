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

#' Get effective logmolar concentration for a response level from a drda model
#'
#' @param model A logistic model of the type returned by [drda::drda()]
#' @param level The desired response level in percent. Percent of the way from
#'   the minimum-dose response to the maximum-dose response, regardless of
#'   whether the response is positive or negative. Default is 50, giving the
#'   classic EC50.
#' @return The effective concentration in log10(molar) units.
#' @export
get_EC_logM <- function(model, level=50){
  assertthat::assert_that(0<level && level<100,
                          msg="level must be between 0 and 100")
  # effective_dose expects level from 0 to 1
  drda::effective_dose(model, level/100)[1]
}

#' Get effective nanomolar concentration for a response level from a drda model
#'
#' @inheritParams get_EC_logM
#' @return The effective concentration in nanomolar units.
#' @export
get_EC_nM <- function(model, level=50){
  assertthat::assert_that(0<level && level<100,
                          msg="level must be between 0 and 100")
  # effective_dose expects level from 0 to 1
  drda::effective_dose(model, level/100)[1] |> logM_to_nM()
}

#' Estimate EC50 from one dose-response point and given model parameters
#'
#' @param alpha The alpha parameter from a `drda` model.
#' @param delta The delta parameter from a `drda` model.
#' @param eta The eta parameter from a `drda` model.
#' @param x The dose of the single point measurement in log(molar) units
#' @param y The activity (NOT percent inhibition) of the single point
#'   measurement
#'
#' @return The estimated EC50 in log10(molar) units
#' @export
#'
EC50_logM_from_point_params <- function(alpha, delta, eta, x, y){
  # 4-parameter logistic equation solved for phi
  unname(log(delta/(y-alpha)-1)/eta + x)
}

#' Estimate EC50 from one dose-response point and 3 parameters of a given model
#'
#' @inheritParams get_hill_slope
#' @inherit EC50_logM_from_point_params params return
#' @export
EC50_logM_from_point_model <- function(model, x, y){
  c <- stats::coefficients(model)
  EC50_logM_from_point_params(c["alpha"], c["delta"], c["eta"], x, y)
}
