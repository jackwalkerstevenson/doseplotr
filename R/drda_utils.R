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
#' @param model A model of type `drda`.
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

#' Estimate the EC50 of a treatment on a target from a single measurement
#'
#' @param alpha The alpha parameter from a `drda` model.
#' @param delta The delta parameter from a `drda` model.
#' @param eta The eta parameter from a `drda` model.
#' @param x The dose of the single point measurement in log(molar) units
#' @param y The response of the single point measurement
#'
#' @return The estimated EC50 in log10(molar) units
#' @export
#'
EC50_logM_from_point <- function(alpha, delta, eta, x, y){
  # 4-parameter logistic equation solved for phi
  log(delta/(y-alpha)-1)/eta + x
}
