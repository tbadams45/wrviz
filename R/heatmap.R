#' Create a climate heatmap.
#'
#' Creates a ggplot2 heatmap with temperature on the x axis, preciptation on the
#' y axis, and a given metric on the z axis.
#'
#' In order for this function to work, you must have a column named "temp" and
#' another named "precip". Beyond that, you can have as many columns as you'd
#' like.
#'
#' We expect the temp column to represent number of degrees celsius from the
#' baseline. The precip column should have decimal numbers that indicate the
#' percentage precipiation change from the baseline. For precip, "1" would
#' indicate no change in precipitation, while "0.7" would indicate a -30% change
#' and "1.2" a +20% change.
#'
#' Because this is a ggplot2 object, you should be able to overwrite many of
#' settings defined here.
#'
#' @param data The data. dataframe, data table, named matrix, etc.
#' @param metric character; The metric to display in the z axis. There should be
#'   a corresponding column name.
#' @param threshold numeric; for binary color scales. Determines when
#'   performance changes from acceptable to unacceptable.
#' @param binary logical; Should the data be represented by a binary color
#'   scale?
#' @param ascending logical; Do increasing values in the metric indicate
#'   increasingly acceptable performance?
#' @param metricCol character; the name of the column where the metric resides
#'   in \code{data}. This defaults to \code{metric}.
#' @param bins Only used if binary == FALSE. Provide (a) a sequence that will be
#'   used to bin the metric, and (b) a color scale of the same length as this
#'   sequence. If null, this function will generate bins and an accompanying
#'   color scale for your metric based on a best guess.
#' @param colorScale Provide a list representing a color scale. If binary ==
#'   TRUE, this list should be of length 2. Otherwise, the length should equal
#'   the length of bins.
#' @return A ggplot2 object representing the heatmap.
#' @examples
#' df <- expand.grid(temp=0:8,precip=seq(0.7,1.3,by=0.1))
#' df$rel <- seq(40,100,length=63)
#' climate_heatmap(df,"rel",80)
#' climate_heatmap(df,"reliability", binary = FALSE, metricCol = "rel")
#'
#' #don't use these colors in an actual plot
#' climate_heatmap(df,"rel", colorScale = c("green","orange"))
#'
#' @importFrom magrittr "%>%"
#' @export
climate_heatmap <- function(data,
                            metric,
                            threshold = 90,
                            binary = TRUE,
                            ascending = TRUE,
                            metricCol = metric,
                            bins = NULL,
                            colorScale = NULL){
  try({ #catch errors in input
    names <- names(data)
    if (!(("temp" %in% names) & ("precip" %in% names))){
      stop("named 'temp' and 'precip' columns are required ",
           "for wrviz::climate_heatmap")
    }
    stopifnot(is.character(metric),
              is.numeric(threshold),
              is.logical(binary),
              is.character(metricCol))
  })

  if (binary == TRUE){
      x   <- bin_binary(data,
                        by = metricCol,
                        threshold = threshold,
                        reverse = !ascending,
                        scale = colorScale)
      data <- x$data
      colors <- x$colors
  } else { # continuous
      x      <- bin_color_continuous(data,
                                     by = metricCol,
                                     ascending = ascending,
                                     metric = metric,
                                     customBins = bins,
                                     scale = colorScale)
      data   <- x$data
      colors <- x$colors
  }

  tick   <- list(x = seq(min(data$temp), max(data$temp), 1),
                 y = seq(min(data$precip), max(data$precip), 0.1))
  label  <- list(x = expression("Temperature change (" * degree * C *")"),
                 y = paste("Precipitation change (%)"))

  ggplot2::ggplot(data, ggplot2::aes(x = temp, y = precip)) +
    ggplot2::geom_tile(ggplot2::aes(fill = bins), color = "gray60") +
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = tick$x) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
                                breaks = tick$y,
                                labels = to_percent_change(tick$y)) +
    ggplot2::scale_fill_manual(name = "Range", values = colors, drop = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 2,
                                                 keyheight = 1.5,
                                                 keywidth  = 1.5)) +
    ggplot2::labs(x = label$x, y = label$y)
}
