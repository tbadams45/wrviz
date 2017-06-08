#' Bin data in a binary fashion.
#'
#' Adds a "bin" column to a dataframe or related data strucutre which indicates
#' whether or not another column meets a given threshold.
#'
#' @param data The dataframe or related data structure containing the data.
#' @param by The column that we will evaluate against the threshold. Pass as
#'   string.
#' @param threshold The value we evaluate our "by" column with.
#' @param levels The values that will be used to fill the "bin" column.
#' @param reverse logical; Should values below or equal to the threshold be considered
#'   good?
#' @param scale A list (of length 2) of custom colors to use for the plot. First
#'   element is for values that meet threshold, last element for those that fail
#'   to meet the threshold.
#' @return The same dataframe, but with our additional column indicating the
#'   bins our data falls into.
bin_binary <- function(data,
                       by,
                       threshold,
                       levels = c("Acceptable", "Not Acceptable"),
                       reverse = FALSE){

  stopifnot(is.character(by), length(levels) == 2, is.logical(reverse))

  if (is.null(threshold)) {
    metric_min <- min(data[by])
    metric_max <- max(data[by])
    threshold <- round( (metric_min + metric_max) / 2)
  }

  # this final working solution using lazyeval was found on
  # http://www.r-bloggers.com/using-mutate-from-dplyr-inside-a-function-getting-around-non-standard-evaluation/
  # Potentially helpful vignettes at
  # https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html#fnref1
  # and https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
  if (reverse == FALSE){
    mutate_call <- lazyeval::interp(~ b >= t,
                                   b = as.name(by),
                                   t = threshold)
  } else {
    mutate_call <- lazyeval::interp(~ b <= t,
                                   b = as.name(by),
                                   t = threshold)
  }

  # default new col name to "bins" since I don't know how to call data$col_name
  # on user-given col name
  data <- data %>%
    dplyr::mutate_(.dots = setNames(list(mutate_call), c("bins")))
  data$bins[data$bins == TRUE]  <- levels[1]
  data$bins[data$bins == FALSE] <- levels[2]
  data <- data %>%
    dplyr::mutate(bins = factor(bins, levels = levels, labels = levels))

  return(data)
}

#' Bin data and get corresponding colors.
#'
#' Bins data for according to a given list.
#'
#' Currently provides scales and colors for reliability, safeyield, resilience,
#' and vulnerability. To access, use \code{metric="reliability"},
#' \code{metric="safeyield"}, etc.
#'
#' @param data The data to bin. Generally a data table.
#' @param by Character; The name of of our output column that we are binning.
#' @param ascending logical; do increasing values indicate a positive trend?
#' @param bin_name Character; Name of column to be created that contains bin data
#' @param num_bins Vector; Provide a vector of length 2, where the first number
#'   is the number of bins below the threshold (inclusive), and the second
#'   number is the number of bins above the treshold.
#' @param midpoint Sets the value where the color scale diverges. If NULL,
#'   defaults to middle of metric range.
#' @param range A vector of length two that defines the max and min value of the
#'   scale.
#' @param scale List; should be length 4, in the format c(lowest, midpoint (white),
#'   one-bin-above-midpoint, highest).
#' @return a list \code{x}. \code{x$data} returns the data frame with a new
#'   column containing the bins, \code{x$colors} contains the corresponding
#'   color scale.
#' @importFrom grDevices "colorRampPalette"
#' @importFrom stats "setNames"
#' @examples
#' \dontrun{df <- expand.grid(temp = 0:8, precip = seq(0.7, 1.3, by = 0.1))
#' df$rel <- seq(40, 100, length=63)
#' df2 <- bin_color_continuous(df, by = "rel", metric = "reliability")}
bin_color_continuous <- function(data,
                                 by,
                                 range,
                                 bins,
                                 scale = NULL,
                                 ascending = TRUE,
                                 bin_name = "bins"){
  stopifnot(is.character(by),
            is.character(bin_name))

  if (is.null(range)) {
    range <- get_range(data, by)
  }

  if (length(bins) == 1) {
    # they gave us the number of bins they want
    b <- round(seq(floor(range[1]), ceiling(range[2]), length.out = bins+1))
    colors <- brewer.pal(bins, "RdBu")

    if(ascending == FALSE) {
      colors <- rev(colors)
    }
  } else {
    # they gave us where they want the bins cut, and they gave us a color scale
    b <- bins
    colors <- scale
  }


  # # create color scale
  # lower_bins <- round(seq(range[1], midpoint, length.out = 5)) # 5 bins.
  # upper_bins <- round(seq(midpoint, range[2], length.out = 4 + 2)) # 4 bins.
  # if (!is.null(num_bins)) { # user defines num_bins
  #   lower_bins <- round(seq(range[1], midpoint, length.out = num_bins[1]))
  #   upper_bins <- round(seq(midpoint, range[2], length.out = num_bins[2] + 2))
  # }
  # b <- c(lower_bins, upper_bins)
  # b <- unique(b)
  #
  # if (!is.null(scale)){ # user defines color scale
  #   col1 <- colorRampPalette(c(scale[[1]], scale[[2]]))(length(lower_bins))
  #   col2 <- colorRampPalette(c(scale[[3]], scale[[4]]))(length(upper_bins))
  # } else { # best guess
  #   col1 <- colorRampPalette(c("firebrick2", "white"))(length(lower_bins))
  #   col2 <- colorRampPalette(c("lightsteelblue1", "royalblue4"))(length(upper_bins))
  # }
  # colors  <- c(col1, col2)

  dots <- list(lazyeval::interp(~cut(x, b, dig.lab = 5, include.lowest = TRUE),
                 x = as.name(by)))
  df <- data %>%
    dplyr::mutate_(.dots = setNames(dots, c(bin_name)))


  list(data = df, colors = colors)
}

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
#' @param data data frame with at least four columns: "temp", "precip", your output column, and a "bins" column which indicates which bin the observation falls into.
#' @param colors vector containing your color scale
#' @param to_percent vector of length two: do you want your (temp, precip) data to be interpreted as percentage changes around 1? (e.g 0.9 is -10% change, 1.2 is 20% change)
#' @return ggplot2 plot
build_plot <- function(
  data,
  colors,
  to_percent = c(FALSE, TRUE)) {
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
    ggplot2::scale_fill_manual(name = "Range", values = colors, drop = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 2,
      keyheight = 1.5,
      keywidth  = 1.5)) +
    ggplot2::labs(x = axis_labels$x, y = axis_labels$y)
}

#' Finds the min and max of the metric.
#'
#' @return vector containing (min_range, max_range)
get_range <- function(data, metric) {
  metric_min <- min(data[metric])
  metric_max <- max(data[metric])

  range <- c(metric_min, metric_max)
  range
}
