#' Summarize replicate data for plotting
#' @param df Dataframe containing the data to plot
#' @param response_col Name of the column containing response data to summarize
#' @return A dataframe containing the summarized data
#' @export
summarize_response <- function(df, response_col = "response"){
  dplyr::summarize(df,
                   # standard error for error bars = standard deviation / sqrt n
                   sem = sd(.data[[response_col]],
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
#' @param legend Whether or not the plot should have a legend (default is TRUE)
#' @return The plot with added objects
#' @export
base_dose_response <- function(plot, x_limits, font_base_size = 14,
                               legend = TRUE){
  plot +
    scale_x_continuous(guide = "prism_offset_minor", # end at last tick
                       breaks = scales::breaks_width(1),
                       minor_breaks = minor_x) + # manual minor ticks
    scale_y_continuous(guide = "prism_offset",  # end at last tick
                       breaks = c(0,25,50,75,100)) + # y axis ticks 0 to 100
    coord_cartesian(xlim = x_limits,
                    ylim = c(0,NA)) + # set y axis zoom locally
    theme_prism(base_size = font_base_size) + # make it look fancy like prism
    theme(plot.background = element_blank()) + # need for transparent background
    {if(!legend) theme(legend.position = "none")}
    # can't figure out how to make 10 subscript and still bold
    # labs(x = "log10[compound] (M)",
         # y = "relative cell viability (%)")
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
#' @param ... Other parameters to be passed to ggsave
#' @export
save_plot <- function(plot, filename,
                      legend = TRUE, legend_len = 0, font_base_size = 14,
                      nrow = 1, ncol = 1,
                      width = NULL, height = NULL, ...){
  facet_size <- 4 # base measurement of both width and height before legend
  legend_pad <- 0.3 # extra width for legend icon
  text_factor <- font_base_size / 120 # approx width per character of longest legend text
  # if width is not provided, calculate width from length of legend text
  if(is.null(width)){
    if(legend){
      width <- ncol * facet_size + legend_pad + legend_len * text_factor}
    else{width <- ncol * facet_size}
  }
  if(is.null(height)){
    height <- nrow * facet_size
  }
  ggplot2::ggsave(filename, plot, bg = "transparent", width = width, height = height, ...)}
