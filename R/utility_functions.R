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
#' @param reverse Should values below or equal to the threshold be considered
#'   good?
#' @return The same dataframe, but with our additional column indicating the
#'   bins our data falls into.
bin_binary <- function(data,
                       by,
                       threshold,
                       levels=c("Acceptable", "Not Acceptable"),
                       reverse=FALSE){
  stopifnot(is.character(by), length(levels) == 2, is.logical(reverse))

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
#' @param by Character; The name of the column to bin by.
#' @param bins A list that defines the bins you would like to create.
#' @param ascending logical; do increasing values indicate a positive trend?
#' @param metric character; The name of the metric we want to create a color
#'   scale for.
#' @param binName Character; Name of column to be created that contains bin data
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
                                 ascending = T,
                                 metric = NULL,
                                 binName = "bins"){
  stopifnot(is.character(by),
            is.character(binName),
            !is.null(metric))

  bins <- get_bins(metric)
  b <- c(bins[[1]],bins[[2]])
  dots <- list(lazyeval::interp(~cut(x, b, dig.lab = 5, include.lowest = TRUE),
                 x = as.name(by)))
  df <- data %>%
    dplyr::mutate_(.dots = setNames(dots, c(binName)))

  if (metric == "reliability"){
    col1 <- colorRampPalette(c("firebrick2", "white"))(length(bins[[1]]))
    col2 <- colorRampPalette(c("lightsteelblue1", "royalblue4"))(length(bins[[2]]))
  } else if (metric == "safeyield"){
    col1 <- colorRampPalette(c("firebrick2", "white"))(length(bins[[1]]))
    col2 <- colorRampPalette(c("lightsteelblue1", "royalblue4"))(length(bins[[2]]))
  } else if (metric == "resilience"){
    col1 <- colorRampPalette(c("firebrick2", "white"))(length(bins[[1]]))
    col2 <- colorRampPalette(c("lightsteelblue1", "royalblue4"))(length(bins[[2]]))
  } else if (metric == "vulnerability"){
    col1 <- colorRampPalette(c("firebrick2", "white"))(length(bins[[1]]))
    col2 <- colorRampPalette(c("lightsteelblue1", "royalblue4"))(length(bins[[2]]))
  }

  colors  <- c(col1, col2)
  list(data = df, colors = colors)
}

#' Get colors for binary visualizations.
#'
#' Returns a list of two colors to be used for binary visualizations.
#'
#' The first value in the list is analagous to "good", "true", etc. The second
#' is analagous to "bad", "false", etc. Currently this is hardcoded to return
#' \code{c("royalblue4","firebrick2")}.
#'
#' @return A list of two colors.
get_colors_binary <- function(){
  return(c("royalblue4", "firebrick2"))
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
  sapply(decimal_list, function(x) round((x - baseline) * 100))
}


#' Get bins for metrics.
#'
#' Generates a sequence to be used for binning, for a number of different water
#' resources metrics.
#'
#' Currently supported metrics: reliability, safeyield.
#'
#' @param metric character; The name of the metric.
#' @return A list containing two sequences. These should be concatenated before
#'   binning, but are provided separately in order to enable more advanced color
#'   scales.
get_bins <- function(metric){

  if (metric == "reliability"){
    s1 <- c(seq(40, 90, 10), 95)
    s2 <- seq(96, 100, 1)
  }
  else if (metric == "safeyield"){
    s1 <- seq(30, 80, 10)
    s2 <- seq(90, 130, 10)
  }
  else if (metric == "resilience") { # values range from 0 to 1
    s1 <- c(seq(0.10, 0.60, 0.10))
    s2 <- c(seq(seq(0.70, 1, 0.05)))
  }
  else if (metric == "vulnerability") { # values range from 0 to 1
    s1 <- c(seq(0.10, 0.60, 0.10))
    s2 <- c(seq(seq(0.70,1,0.05)))
  } else {
    stop("Please enter a valid metric.")
  }
  list(s1,s2)
}
