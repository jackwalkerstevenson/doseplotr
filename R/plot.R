#' Summarize replicate data for plotting
#' @param df Dataframe containing the data to plot
#' @param response_col Name of the column containing response data to summarize
#' @return A dataframe containing the summarized data
#' @export
summarize_response <- function(df, response_col = "response"){
  dplyr::summarize(df,
                   # standard error for error bars = standard deviation / sqrt n
                   sem = stats::sd(.data[[response_col]],
                            na.rm = TRUE) / sqrt(dplyr::n()),
                   # get mean normalized readout value for plotting
                   mean_read = mean(.data[[response_col]]),
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
                               xlab = "log10[compound] (M)",
                               ylab = "percent of untreated response",
                               legend = TRUE){
  x_min <- x_limits[1]
  x_max <- x_limits[2]
  minor_x <- log10(rep(1:9, x_max - x_min)*(10^rep(x_min:(x_max - 1), each = 9)))
  plot +
    ggplot2::scale_x_continuous(guide = ggprism::guide_prism_offset_minor(), # end at last tick
                       breaks = scales::breaks_width(1),
                       minor_breaks = minor_x) + # manual minor ticks
    ggplot2::scale_y_continuous(guide = ggprism::guide_prism_offset(),  # end at last tick
                       breaks = c(0,25,50,75,100)) + # y axis ticks 0 to 100
    ggplot2::coord_cartesian(xlim = x_limits,
                    ylim = c(0,NA)) + # set y axis zoom locally
    ggprism::theme_prism(base_size = font_base_size) + # make it look fancy like prism
    ggplot2::theme(plot.background = ggplot2::element_blank()) + # need for transparent background
    {if(!legend) ggplot2::theme(legend.position = "none")} +
    ggplot2::geom_errorbar(ggplot2::aes(ymax = .data$mean_read + .data$sem,
                               ymin = .data$mean_read - .data$sem,
                               width = .data$w)) +
    # use drm method from drc package to plot dose response curve
    # todo: replace this with same drda method that fits EC50s
    ggplot2::geom_smooth(method = "drm", method.args = list(fct = drc::L.4()), se = FALSE, linewidth = 1) +
    ggplot2::labs(x = xlab, y = ylab)
}

#' Plot dose-response effects of one treatment on all targets
#' @param df Dataframe containing data to plot. Must contain columns:
#'
#' * treatment
#' * target
#' * log_dose
#' * response column, specified in response_col argument
#' @param trt Name of treatment to plot
#' @param response_col Name of column containing response data
#' @param ... Extra parameters to pass to `base_dose_response()`
#'
#' @return A ggplot2 plot of the dose-response effect of the specified treatment
#'   on all targets
#' @export
plot_treatment <- function(df, trt, response_col = "response", ...){
  # set up color parameters based on number of targets
  num_targets <- length(unique(df$target))
  if (num_targets > 8) color_scale <- "turbo" else color_scale <- "viridis"
  vr <- viridis_range(num_targets)
  viridis_begin <- vr[1]
  viridis_end <- vr[2]
  summary <- df |>
    filter_trt_tgt(trt = trt) |>
    dplyr::group_by(.data$target, .data$log_dose) |>
    summarize_response(response_col = response_col)
  p <- {ggplot2::ggplot(summary,
                        ggplot2::aes(x = .data$log_dose,
                                     y = .data$mean_read, # later take from argument
                                     color = .data$target)) +
      ggplot2::geom_point(ggplot2::aes(shape = .data$target), size = 3) +
      viridis::scale_color_viridis(option = color_scale,
                          discrete = TRUE,
                          begin = viridis_begin, end = viridis_end) +
      ggplot2::labs(title = trt)
  } |>
    base_dose_response(...)
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
