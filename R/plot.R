#' Save a plot as an image with predictable plot dimensions
#'
#' `save_plot()` takes a ggplot2 object and saves it as an image.
#' [ggplot2::ggsave()] allows the dimensions of the output image to be specified
#' but does not offer logic to make the actual plot portion of the image a
#' constant size. `save_plot()` tries to set the exported image dimensions such
#' that the plot portion of the image is a similar size across exports, even
#' when plots have legends of different sizes, using parameters empirically
#' optimized for dose-response curve plots.
#' @param plot A ggplot2 object
#' @param filename Path to the file to be saved
#' @param legend Boolean: whether or not the plot has a legend
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
