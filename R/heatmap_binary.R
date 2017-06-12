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
#' @param threshold numeric; Determines when performance changes from acceptable
#'   to unacceptable.
#' @param ascending logical; Do increasing values in the metric indicate
#'   increasingly acceptable performance?
#' @param color_scale Provide a vector of length 2 representing a color scale.
#' @return A ggplot2 object representing the heatmap.
#' @param metric_col character; the name of the column where the metric resides
#'   in \code{data}. This defaults to \code{metric}.
#' @param to_percent list; List of length two that says whether or not (temp,
#'   precip) data should be treated as percent changes (e.g. 1 means 0% change,
#'   0.9 means -10\% change, 1.2 means 20\% change...)
#' @param z_axis_title Title of Z axis (represented by color)
#' @examples
#' df <- expand.grid(temp=0:8,precip=seq(0.7,1.3,by=0.1))
#' df$rel <- seq(40,100,length=63)
#' climate_heatmap_binary(df,"rel",80)
#'
#' climate_heatmap_binary(df,"rel", color_scale = c("firebrick2","deepskyblue2")) +
#'  ggplot2::theme(text = ggplot2::element_text(size = 18))
#'
#' @importFrom magrittr "%>%"
#' @export
climate_heatmap_binary <- function(data,
  metric,
  threshold = NULL,
  ascending = TRUE,
  color_scale = NULL,
  metric_col = metric,
  to_percent = c(FALSE, TRUE),
  z_axis_title = "Range"){
  try({
    names <- names(data)
    if (!( ("temp" %in% names) & ("precip" %in% names))){
      stop("named 'temp' and 'precip' columns are required ",
        "for wrviz::climate_heatmap")
    }
    stopifnot(is.character(metric),
      is.character(metric_col))
  })

  data   <- bin_binary(data,
    by = metric_col,
    threshold = threshold,
    reverse = !ascending)

  if (is.null(color_scale)) {
    colors <- c("#2E2ECC", "#CC2E2E") # from http://colorbrewer2.org/#type=diverging&scheme=RdBu&n=3
  }
  else {
    colors <- color_scale
  }

  plot <- build_plot(
    data,
    colors,
    to_percent = to_percent,
    z_axis_title = z_axis_title
  )

  plot
}

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
#' @return The same dataframe, but with an additional column indicating the
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
    dplyr::mutate_(.dots = stats::setNames(list(mutate_call), c("bins")))
  data$bins[data$bins == TRUE]  <- levels[1]
  data$bins[data$bins == FALSE] <- levels[2]
  data <- data %>%
    dplyr::mutate(bins = factor(bins, levels = levels, labels = levels))

  return(data)
}
