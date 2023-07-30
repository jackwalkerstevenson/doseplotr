#' Get the Hill slope from a `drda` model
#'
#' @param model A logistic model of the type returned by [drda::drda()]
#' @return The Hill slope (growth rate) of the model. Unsigned (always
#'   positive).
#' @export
get_hill_slope <- function(model){
  if(!"drda" %in% class(model)){
    warning("get_IC_log received no model. Returning NA")
    return(NA)
  }
  stats::coefficients(model)["eta"]
}

#' Get the low-dose asymptote from a `drda` model
#'
#' @inheritParams get_hill_slope
#' @return The level of the low-dose asymptote. The value of the model when dose
#'   -> -Inf.
#' @export
get_low_dose_asymptote <- function(model){
  if(!"drda" %in% class(model)){
    warning("get_IC_log received no model. Returning NA")
    return(NA)
  }
  stats::coefficients(model)["alpha"]
}

#' Get the high-dose asymptote from a `drda` model
#'
#' @inheritParams get_hill_slope
#' @return The level of the high-dose asymptote. The value of the model when
#'   dose -> Inf.
#' @export
get_high_dose_asymptote <- function(model){
  if(!"drda" %in% class(model)){
    warning("get_IC_log received no model. Returning NA")
    return(NA)
  }
  stats::coefficients(model)["alpha"] + stats::coefficients(model)["delta"]
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
  if(!"drda" %in% class(model)){
    warning("get_IC_log received no model. Returning NA")
    return(NA)
  }
  IC <- drda::effective_dose(model, level, type = "absolute")[1]
  if(is.na(IC)){warning("Unable to fit an IC50. Returning NA")}
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
  if(!"drda" %in% class(model)){
    warning("get_IC_log received no model. Returning NA")
    return(NA)
  }
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
#' @param response The response (NOT percent inhibition) observed at the tested
#'   dose
#' @return The estimated EC50 in log10 units
#' @export
EC50_log_from_point_params <- function(alpha, delta, eta, log_dose, response){
  # 4-parameter logistic equation solved for phi
  (log(delta/(response-alpha)-1)/eta + log_dose) |> unname()
}

#' Estimate log10 EC50 from one dose-response point and 3 parameters of a model
#'
#' @inheritParams get_hill_slope
#' @inherit EC50_log_from_point_params params return
#' @export
EC50_log_from_point_model <- function(model, log_dose, response){
  c <- stats::coefficients(model)
  EC50_log_from_point_params(c["alpha"], c["delta"], c["eta"],
                              log_dose, response)
}


#' Estimate nM EC50 from one dose-response point and 3 parameters of a model
#'
#' @inherit EC50_log_from_point_model params return
#' @param dose_nM The dose of the single point measurement in nanomolar units
#' @export
EC50_nM_from_point_model <- function(model, dose_nM, response){
  EC50_log_from_point_model(model, nM_to_logM(dose_nM), response) |>
    logM_to_nM()
}


#' Get a dataframe of the predictions of a single dose-response model
#'
#' `predict_helper()` is a helper function for `get_predictions()`. Its logic
#' only depends on `model` and `dose_seq`, but it also takes `trt`, `tgt` and
#' `response_col` just so that the returned dataframe will have proper column
#' names.
#' @param model The model to use to predict responses
#' @param trt Name of the treatment used to fit the model
#' @param tgt Name of the target used to fit the model
#' @param min_log10_dose Minimum dose in the data in log10 units. Lower limit of
#'   predictions
#' @param max_log10_dose Maximum dose in the data in log10 units. Upper limit of
#'   predictions
#' @param response_col Name of the column containing predicted responses
#' @return A dataframe containing the predictions of the model. Has columns:
#' * treatment
#' * target
#' * log_dose
#' * response (or other column name specified in response_col)
#'   If no model is passed, returns null.
predict_helper <- function(model,
                           min_log10_dose, max_log10_dose,
                           trt, tgt, response_col){
  if("drda" %in% class(model)){
    dose_seq <- seq(min_log10_dose, max_log10_dose, length.out = 100)
    data.frame(
      treatment = trt,
      target = tgt,
      log_dose = dose_seq,
      response = stats::predict(model,
                                newdata = data.frame(log_dose = dose_seq))) |>
      # overbuilt but works: rename column from argument
      dplyr::rename_with(\(col){ifelse(col == "response", response_col, col)})
  }
}

#' Get a dataframe of predictions from desired doses and a dataframe of models
#'
#' @param models_df Dataframe containing models and information about them. Has columns:
#' * "treatment": treatment for which the model was fit
#' * "target": target for which the model was fit
#' * "model": dose-response model fit for the specified treatment and target
#' @param response_col Name for the column of predicted responses
#' @return A dataframe containing predicted values. Has columns:
#' * treatment
#' * target
#' * log_dose
#' * response (or other column name specified in response_col)
#' @export
get_predictions <- function(models_df, response_col = "mean_response"){
  mapply(predict_helper,
         models_df$model,
         models_df$min_log10_dose,
         models_df$max_log10_dose,
         models_df$treatment,
         models_df$target,
         response_col = response_col,
         SIMPLIFY = FALSE) |>
    dplyr::bind_rows() # bind list of dataframes into one dataframe
}
