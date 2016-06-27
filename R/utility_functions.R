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
#' @param col_name The name of the column to be added.
#' @param reverse Should values below or equal to the threshold be considered
#'   good?
#' @return The same dataframe, but with our additional column indicating the
#'   bins our data falls into.
#'
#' @export
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

  # default new col to "bins" since idk how to call data$col_name on user given
  # col name
  data <- data %>%
    dplyr::mutate_(.dots = setNames(list(mutate_call), c("bins")))
  data$bins[data$bins == TRUE]  <- levels[1]
  data$bins[data$bins == FALSE] <- levels[2]
  data <- data %>%
    dplyr::mutate(bins = factor(bins, levels = levels, labels = levels))

  return(data)
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
#'
#' @export
get_colors_binary <- function(){
  return(c("royalblue4", "firebrick2"))
}

#' Get colors for continuous visualizations.
#'
#' Returns a list of colors to be used for continuous visualizations.
#'
#'
#'
#' @param data
#' @param baseline a baseline
#' @param diverging logical; does this scale diverge in two directions from the baseline? If scale only increases in one direction, choose TRUE
#' @param upward logical; do increasing values indicate a positive trend?
#' @return A list of colors.
#'
#' @export
get_colors_continuous <- function(data,
                                  baseline,
                                  diverging = TRUE,
                                  upward = NULL){

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
#' to_percent_change(seq(0.5,1.5,by=0.1))
#' to_percent_change(seq(-0.3,0.7,by=0.1),baseline=0)
#' to_percent_change(seq(0.512,1.512,by=0.1)) # answer is rounded
#'
#' @export
to_percent_change <- function(decimal_list, baseline = 1){
  sapply(decimal_list, function(x) round((x-baseline)*100))
}

