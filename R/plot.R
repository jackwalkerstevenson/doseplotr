#' Summarize replicate data for plotting
#' @param df Dataframe containing the data to plot
#' @param response_col Name of the column containing response data to summarize
#' @return A dataframe containing the data summarized by `dplyr::summarize()`.
#'   Columns are:
#' * "sem": the standard error of the mean of the response values
#' * "mean_response": the mean of the response values
#' * "w": the width values for error bars.
#'   This is a workaround for `ggplot2::ggplot()` behavior where the width of
#'   error bars depends on the size of the group.
#' @export
summarize_response <- function(df, response_col = "response"){
  dplyr::summarize(df,
                   # standard error for error bars = standard deviation / sqrt n
                   sem = stats::sd(.data[[response_col]],
                            na.rm = TRUE) / sqrt(dplyr::n()),
                   # get mean normalized readout value for plotting
                   mean_response = mean(.data[[response_col]]),
                   w = 0.06 * dplyr::n() # for consistent error bar widths
  )}

#' Add ggplot objects common to all dose-response plots
#'
#' @param plot ggplot2 plot to which to add objects
#' @param x_limits Limits of the x axis. Vector of form c(start, end)
#' @param font_base_size Font size in points. Default is 14 from theme_prism
#' @param xlab The label for the x axis
#' @param ylab The label for the y axis
#' @param legend Whether or not the plot should have a legend (default is TRUE)
#' @return The plot with added objects
#' @importFrom ggprism guide_prism_offset_minor
#' @export
base_dose_response <- function(plot, x_limits, font_base_size = 14,
                               xlab = "log10[treatment] (M)",
                               ylab = "percent of untreated response",
                               legend = TRUE){
  x_min <- x_limits[1]
  x_max <- x_limits[2]
  # manually construct minor ticks
  minor_x <- log10(rep(1:9, x_max - x_min) * (10^rep(x_min:(x_max - 1),
                                                     each = 9)))
  p <- plot +
    # ggprism guide to end at last tick
    ggplot2::scale_x_continuous(guide =ggprism::guide_prism_offset_minor(),
                       breaks = scales::breaks_width(1),
                       minor_breaks = minor_x) +
    # ggprism guide to end at last tick
    ggplot2::scale_y_continuous(guide = ggprism::guide_prism_offset(),
                       breaks = c(0,25,50,75,100)) + # y axis ticks 0 to 100
    ggplot2::coord_cartesian(xlim = x_limits, ylim = c(0,NA)) + # y 0 to max
    ggprism::theme_prism(base_size = font_base_size) + # prism theme
    ggplot2::theme(plot.background = ggplot2::element_blank()) + # transparent
    {if(!legend) ggplot2::theme(legend.position = "none")} + # legend option
    ggplot2::geom_errorbar(ggplot2::aes(ymax = .data$mean_response + .data$sem,
                                        ymin = .data$mean_response - .data$sem,
                                        width = .data$w)) +
    ggplot2::labs(x = xlab, y = ylab)
  return(p)
}

#' Save a plot as an image with predictable plot dimensions
#'
#' `save_plot()` takes a ggplot2 object and saves it as an image.
#' [ggplot2::ggsave()] allows the dimensions of the output image to be specified
#' but does not offer logic to make the actual plot portion of the image a
#' constant size. `save_plot()` tries to set the exported image dimensions such
#' that the plot portion of the image is a similar size across exports, even
#' when plots have legends of different sizes, using parameters empirically
#' optimized for dose-response curve plots.
#' @inheritParams base_dose_response
#' @param plot A ggplot2 object
#' @param filename Path to the file to be saved
#' @param legend_len Length of the longest piece of text in the legend
#' @param font_base_size Size (point units) of font in plots. 14 is theme_prism
#'   default
#' @param nrow Number of rows of facets in the plot
#' @param ncol Number of columns of facets in the plot
#' @param width Width of the image to be saved
#' @param height Height of the image to be saved
#' @export
save_plot <- function(plot, filename,
                      legend = TRUE, legend_len = NULL, font_base_size = 14,
                      nrow = 1, ncol = 1,
                      width = NULL, height = NULL){
  facet_size <- 4 # base measurement of both width and height before legend
  legend_pad <- 0.3 # extra width for legend icon
  text_factor <- font_base_size / 120 # approx width per character of longest legend text
  # if legend length is not provided, take a wild guess
  if(is.null(legend_len)) legend_len <- 15
    # if width is not provided, calculate width from length of legend text
    if(is.null(width)){
      if(legend){
        width <- ncol * facet_size + legend_pad + legend_len * text_factor}
      else{width <- ncol * facet_size}
    }
  if(is.null(height)){
    height <- nrow * facet_size
  }
  ggplot2::ggsave(filename, plot, bg = "transparent", width = width, height = height)}

#' Plot dose-response effects of one treatment on all targets
#' @param df Dataframe containing data to plot. Must contain columns:
#' * treatment
#' * target
#' * log_dose
#' * response column, specified in response_col argument
#' @param trt Name of treatment to plot
#' @param rigid Whether to set rigid value for low-dose asymptote. 100 if
#'   decreasing, 0 if increasing.
#' @param response_col Name of column containing response data
#' @param x_limits Limits for x axis of plot. Optional: if not provided, will be
#'   calculated automatically.
#' @param ... Extra parameters to pass to `base_dose_response()`
#'
#' @return A ggplot2 plot of the dose-response effect of the specified treatment
#'   on all targets
#' @export
plot_treatment <- function(df, trt,
                           rigid = FALSE,
                           response_col = "response",
                           x_limits = NULL,
                           ...){
  # get data for specified treatment
  data <- df |> filter_trt_tgt(trt = trt)
  # calculate x limits if not specified
  if(is.null(x_limits)){
    x_min <- floor(min(data$log_dose))
    x_max <- ceiling(max(data$log_dose))
    x_limits <- c(x_min, x_max)}
  # summarize actual data to plot points and error bars
  data_summary <- data |>
    dplyr::group_by(.data$target, .data$log_dose) |> # group by target
    summarize_response(response_col = response_col)
  # fit models to data to plot model prediction curves
  model_predictions <- data |>
    dplyr::group_by(.data$target, .data$log_dose) |>
    summarize_models(response_col = response_col, rigid = rigid) |>
    get_predictions(response_col = "mean_response") # name for plotting
  # set up color parameters based on number of targets
  num_targets <- length(unique(data_summary$target))
  vr <- viridis_range(num_targets)
  vr_begin <- vr[[1]]
  vr_end <- vr[[2]]
  vr_option <- vr[[3]]
  # plot points and error bars from the summarized data
  p <- {ggplot2::ggplot(data_summary,
                        ggplot2::aes(x = .data$log_dose,
                                     y = .data$mean_response,
                                     color = .data$target)) +
      ggplot2::geom_point(ggplot2::aes(shape = .data$target), size = 3) +
      viridis::scale_color_viridis(discrete = TRUE, option = vr_option,
                                   begin = vr_begin, end = vr_end) +
      ggplot2::labs(title = trt)
  } |>
    base_dose_response(x_limits = x_limits, ...)
  # plot model predictions for models that were fit successfully
  p <- p +
    ggplot2::geom_line(data = model_predictions, linewidth = .75, alpha = 0.8)
  return(p)
}

#' Plot dose-response effects of one target on all treatments
#' @inheritParams plot_treatment
#' @param tgt Name of target to plot
#'
#' @return A ggplot2 plot of the dose-response effect of the specified target on
#'   all treatments
#' @export
plot_target <- function(df, tgt,
                           rigid = FALSE,
                           response_col = "response",
                           x_limits = NULL,
                           ...){
  # get data for specified target
  data <- df |> filter_trt_tgt(tgt = tgt)
  # calculate x limits if not specified
  if(is.null(x_limits)){
    x_min <- floor(min(data$log_dose))
    x_max <- ceiling(max(data$log_dose))
    x_limits <- c(x_min, x_max)}
  # summarize actual data to plot points and error bars
  data_summary <- data |>
    dplyr::group_by(.data$treatment, .data$log_dose) |> # group by treatment
    summarize_response(response_col = response_col)
  # fit models to data to plot model prediction curves
  model_predictions <- data |>
    dplyr::group_by(.data$treatment, .data$log_dose) |>
    summarize_models(response_col = response_col, rigid = rigid) |>
    get_predictions(response_col = "mean_response") # name for plotting
  # set up color parameters based on number of treatments
  num_treatments <- length(unique(data_summary$treatment))
  vr <- viridis_range(num_treatments)
  vr_begin <- vr[[1]]
  vr_end <- vr[[2]]
  vr_option <- vr[[3]]
  # plot points and error bars from the summarized data
  p <- {ggplot2::ggplot(data_summary,
                        ggplot2::aes(x = .data$log_dose,
                                     y = .data$mean_response,
                                     color = .data$treatment)) +
      ggplot2::geom_point(ggplot2::aes(shape = .data$treatment), size = 3) +
      viridis::scale_color_viridis(discrete = TRUE, option = vr_option,
                                   begin = vr_begin, end = vr_end) +
      ggplot2::labs(title = tgt)
  } |>
    base_dose_response(x_limits = x_limits, ...)
  # plot model predictions for models that were fit successfully
  p <- p +
    ggplot2::geom_line(data = model_predictions, linewidth = .75, alpha = 0.8)
  return(p)
}
# plot_target <- function(df, tgt,
#                            response_col = "response",
#                            x_limits = NULL,
#                            ...){
#   summary <- df |>
#     filter_trt_tgt(tgt = tgt) |>
#     dplyr::group_by(.data$treatment, .data$log_dose) |>
#     summarize_response(response_col = response_col)
#   # set up color parameters based on number of treatments
#   num_treatments <- length(unique(summary$treatment))
#   vr <- viridis_range(num_treatments)
#   vr_begin <- vr[[1]]
#   vr_end <- vr[[2]]
#   vr_option <- vr[[3]]
#   # calculate x limits if not specified
#   if(is.null(x_limits)){
#     x_min <- floor(min(summary$log_dose))
#     x_max <- ceiling(max(summary$log_dose))
#     x_limits <- c(x_min, x_max)
#   }
#   p <- {ggplot2::ggplot(summary,
#                         ggplot2::aes(x = .data$log_dose,
#                                      y = .data$mean_response,
#                                      color = .data$treatment)) +
#       ggplot2::geom_point(ggplot2::aes(shape = .data$treatment), size = 3) +
#       viridis::scale_color_viridis(discrete = TRUE, option = vr_option,
#                                    begin = vr_begin, end = vr_end) +
#       ggplot2::labs(title = tgt)
#   } |>
#     base_dose_response(x_limits = x_limits, ...)
#   return(p)
# }
