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
#' @param metric The metric name to display in the z axis.
#' @param threshold Either (a) the minimum acceptable performance level for a
#'   binary heatmap, or (b) the value where positive/negative values will
#'   diverge from on the color scale.
#' @param binary Display with a binary or diverging color scale?
#' @return A ggplot2 object representing the heatmap.
#' @examples
#' df <- expand.grid(temp=0:8,precip=seq(0.7,1.3,by=0.1))
#' df$rel <- seq(40,100,length=63)
#' climate_heatmap(df,"rel",80)
#' climate_heatmap(df,"rel",40)
#'
#' @export
climate_heatmap <- function(data, metric, threshold, binary=TRUE){
  try({
    names <- names(data)
    if (!(("temp" %in% names) & ("precip" %in% names))){
      stop("named 'temp' and 'precip' columns are required ",
           "for wrviz::climate_heatmap")
    }
  })

  if (binary == TRUE){
    data   <- bin_binary(data, by = metric, threshold = threshold)
    colors <- get_colors_binary()
  } else {
    data   <- bin_continuous(data, by = metric, threshold=threshold)
    colors <- get_colors_continuous(baseline = threshold, diverging == TRUE)
  }

  tick   <- list(x = seq(min(data$temp),max(data$temp),1),
                 y = seq(min(data$precip),max(data$precip),0.1))
  label  <- list(x = expression("Temperature change (" * degree * C *")"),
                 y = paste("Precipitation change (%)"))

  ggplot2::ggplot(data, ggplot2::aes(x = temp, y = precip)) +
    ggplot2::geom_tile(ggplot2::aes(fill = bins), color = "gray60") +
    ggplot2::scale_x_continuous(expand=c(0,0), breaks = tick$x) +
    ggplot2::scale_y_continuous(expand=c(0,0),
                                breaks = tick$y,
                                labels = to_percent_change(tick$y)) +
    ggplot2::scale_fill_manual(name = "Range", values = colors, drop = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 2,
                                                 keyheight = 1.5,
                                                 keywidth  = 1.5)) +
    ggplot2::labs(x = label$x, y = label$y)

}
