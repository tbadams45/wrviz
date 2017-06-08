#' Create a binary climate heatmap.
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
#' @param data The data. Could be dataframe, data table, named matrix, etc.
#' @param metric character; The metric to display in the z axis. There should be
#'   a corresponding column name.
#' @param threshold numeric; Determines when
#'   performance changes from acceptable to unacceptable.
#' @param ascending logical; Do increasing values in the metric indicate
#'   increasingly acceptable performance?
#' @param metricCol character; the name of the column where the metric resides
#'   in \code{data}. This defaults to \code{metric}.
#' @param colorScale Provide a vector of length 2 representing a color scale.
#' @return A ggplot2 object representing the heatmap.
#' @examples
#' df <- expand.grid(temp=0:8,precip=seq(0.7,1.3,by=0.1))
#' df$rel <- seq(40,100,length=63)
#' climate_heatmap_binary(df,"rel",80)
#'
#' #don't use these colors in an actual plot
#' climate_heatmap_binary(df,"rel", colorScale = c("green","orange"))
#'
#' @importFrom magrittr "%>%"
#' @export
climate_heatmap_binary <- function(data,
                            metric,
                            threshold = NULL,
                            ascending = TRUE,
                            colorScale = NULL,
                            metricCol = metric){
  try({ #catch errors in input
    names <- names(data)
    if (!(("temp" %in% names) & ("precip" %in% names))){
      stop("named 'temp' and 'precip' columns are required ",
           "for wrviz::climate_heatmap")
    }
    stopifnot(is.character(metric),
              is.character(metricCol))
  })

  x   <- bin_binary(data,
                    by = metricCol,
                    threshold = threshold,
                    reverse = !ascending,
                    scale = colorScale)
  data <- x$data
  colors <- x$colors

  plot <- build_plot(
    data,
    colors
  )

  plot
}
