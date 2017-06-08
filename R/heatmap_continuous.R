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
#' @param bins Either a single number indicating the number of bins, or a vector
#'   that specifies where the bins will be cut.
#' @param range A vector of length two that defines the max and min value of the
#'   scale.
#' @param colors Provide a list representing a color scale.
#' @param to_percent list; List of length two that says whether or not (temp,
#'   precip) data should be treated as percent changes (e.g. 1 means 0% change,
#'   0.9 means -10% change, 1.2 means 20% change...)
#' @return A ggplot2 object representing the heatmap.
#' @examples
#' df <- expand.grid(temp=0:8,precip=seq(0.7,1.3,by=0.1))
#' df$rel <- seq(40,100,length=63)
#' climate_heatmap_continuous(df,"reliability")
#'
#' @importFrom magrittr "%>%"
#' @export
climate_heatmap_continuous <- function(data,
  metric,
  bins = 7,
  ascending = TRUE,
  range = NULL,
  colors = NULL,
  to_percent = c(FALSE, TRUE)) {

  try({ #catch errors in input
    names <- names(data)
    if (!( ("temp" %in% names) & ("precip" %in% names))){
      stop("named 'temp' and 'precip' columns are required ",
        "for wrviz::climate_heatmap")
    }
    stopifnot(is.character(metric))
  })

  if (is.null(range)) {
    range <- get_range(data, metric)
  }

  # if (is.null(midpoint)) {
  #   midpoint <- round( (range[1] + range[2]) / 2)
  # }

  x <- bin_color_continuous(data,
    by = metric,
    ascending = ascending,
    bins = bins,
    #midpoint = midpoint,
    range = range,
    scale = colors)

  data   <- x$data
  colors <- x$colors

  plot <- build_plot(
    data,
    colors,
    to_percent = to_percent
  )

  plot
}
