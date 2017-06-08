#' Create a climate heatmap.
#'
#' Creates a ggplot2 heatmap with temperature on the x axis, preciptation on the
#' y axis, and a given metric on the z axis.
#'
#' In order for this function to work, you must have a column named "temp" and
#' another named "precip". Beyond that, you can have as many columns as you'd
#' like.
#'
#' Because this is a ggplot2 object, you should be able to overwrite many of
#' settings defined here.
#'
#' @param data The data. dataframe, data table, named matrix, etc.
#' @param metric character; The metric to display in the z axis. There should be
#'   a corresponding column name.
#' @param ascending logical; Do increasing values in the metric indicate
#'   increasingly acceptable performance?
#' @param metric_col character; the name of the column where the metric resides
#'   in \code{data}. This defaults to \code{metric}.
#' @param num_bins Either a single number, or a vector of length 2, where the
#'   first number is the number of bins below the threshold (inclusive), and the
#'   second number is the number of bins above the treshold. Each number counts
#'   the midpoint as its own bin, so really you're going to have one less bin
#'   than the sum of numbers you put here.
#' @param midpoint Sets the value where the color scale diverges. If NULL,
#'   defaults to middle of metric range.
#' @param range A vector of length two that defines the max and min value of the
#'   scale.
#' @param color_scale Provide a list representing a color scale. Sould be length
#'   4, in the format c(lowest, midpoint, one-bin-above-midpoint, highest).
#' @return A ggplot2 object representing the heatmap.
#' @examples
#' df <- expand.grid(temp=0:8,precip=seq(0.7,1.3,by=0.1))
#' df$rel <- seq(40,100,length=63)
#' climate_heatmap_continuous(df,"reliability", metric_col = "rel")
#'
#' @importFrom magrittr "%>%"
#' @export
climate_heatmap_continuous <- function(data,
  metric,
  metric_col = metric,
  num_bins = NULL,
  midpoint = NULL,
  range = NULL,
  color_scale = NULL,
  to_percent = c(FALSE, TRUE)) {

  try({ #catch errors in input
    names <- names(data)
    if (!( ("temp" %in% names) & ("precip" %in% names))){
      stop("named 'temp' and 'precip' columns are required ",
        "for wrviz::climate_heatmap")
    }
    stopifnot(is.character(metric),
      is.character(metric_col))
  })

  if (is.null(range)) {
    range <- get_range(data, metric)
  }

  if (is.null(midpoint)) {
    midpoint <- round( (range[1] + range[2]) / 2)
  }

  x <- bin_color_continuous(data,
    by = metric_col,
    ascending = ascending,
    num_bins = num_bins,
    midpoint = midpoint,
    range = range,
    scale = color_scale)

  data   <- x$data
  colors <- x$colors

  plot <- build_plot(
    data,
    colors,
    to_percent = to_percent
  )

  plot
}
