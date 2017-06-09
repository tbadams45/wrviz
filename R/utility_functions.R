#' Convert decimals difference to percent differences.
#'
#' Converts a list of decimal numbers (e.g. 0.7, 1.0, 1.3) to their
#' corresponding percent difference representation. The representation will
#' depend on what value is selected as the baseline, i.e. 0\% percent change.
#'
#' This function uses round() to avoid any weird floating point representation
#' errors. Because of this, the best precision you can obtain is integers for
#' your percent differences (e.g. can only get "52%", not "52.3%")
#'
#' @param decimal_list The list to convert.
#' @param baseline The value that represents 0\% percent change.
#' @return A list of the same length as decimal_list.
#' @examples
#' \dontrun{to_percent_change(seq(0.5,1.5,by=0.1))
#' to_percent_change(seq(-0.3,0.7,by=0.1),baseline=0)
#' to_percent_change(seq(0.512,1.512,by=0.1)) # answer is rounded}
to_percent_change <- function(decimal_list, baseline = 1){
  sapply(decimal_list, function(x) round( (x - baseline) * 100))
}

#' Build plot with defaults
#'
#' @param data data frame with at least four columns: "temp", "precip", your
#'   output column, and a "bins" column which indicates which bin the
#'   observation falls into.
#' @param colors vector containing your color scale
#' @param to_percent vector of length two: do you want your (temp, precip) data
#'   to be interpreted as percentage changes around 1? (e.g 0.9 is -10\%
#'   change, 1.2 is 20\% change)
#' @param z_axis_title Title of Z axis (represented by color).
#' @return ggplot2 plot
build_plot <- function(
  data,
  colors,
  to_percent = c(FALSE, TRUE),
  z_axis_title = "Range") {
  # dynamically determine tick marks
  t <- unique(data$temp) # remove elements that do not represent 'steps'
  t <- abs(t[2] - t[1]) # find length of one step
  p <- unique(data$precip)
  p <- abs(p[2] - p[1])

  tick   <- list(x = seq(min(data$temp), max(data$temp), t),
    y = seq(min(data$precip), max(data$precip), p))

  if (to_percent[1]) {
    x_tick_label <- to_percent_change(tick$x)
  }
  else x_tick_label <- tick$x

  if (to_percent[2]) {
    y_tick_label <- to_percent_change(tick$y)
  }
  else y_tick_label <- tick$y

  axis_labels  <- list(x = expression("Temperature change (" * degree * C * ")"),
    y = paste("Precipitation change (%)"))

  ggplot2::ggplot(data, ggplot2::aes(x = temp, y = precip)) +
    ggplot2::geom_tile(ggplot2::aes(fill = bins), color = "gray60") +
    ggplot2::scale_x_continuous(expand = c(0, 0),
      breaks = tick$x,
      labels = x_tick_label) +
    ggplot2::scale_y_continuous(expand = c(0, 0),
      breaks = tick$y,
      labels = y_tick_label) +
    ggplot2::scale_fill_manual(name = z_axis_title, values = colors, drop = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 2,
      keyheight = 1.5,
      keywidth  = 1.5)) +
    ggplot2::labs(x = axis_labels$x, y = axis_labels$y)
}

#' Finds the min and max of the metric.
#'
#' @param data The dataframe containing your metric
#' @param metric Character; the name of your metric
#' @return vector containing (min_range, max_range)
get_range <- function(data, metric) {
  metric_min <- min(data[metric])
  metric_max <- max(data[metric])

  range <- c(metric_min, metric_max)
  range
}
