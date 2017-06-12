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
#'   0.9 means -10\% change, 1.2 means 20\% change...)
#' @param z_axis_title Title of Z axis (represented by color)
#' @return A ggplot2 object representing the heatmap.
#' @examples
#' df <- expand.grid(temp=0:8,precip=seq(0.7,1.3,by=0.1))
#' df$rel <- seq(40,100,length=63)
#' climate_heatmap_continuous(df,"rel")
#' climate_heatmap_continuous(df,"rel", bins = 4)
#' climate_heatmap_continuous(df,"rel", bins = c(20, 30, 50, 80, 100), range = c(20, 100))
#'
#' colors <- grDevices::colorRampPalette(c("firebrick2", "deepskyblue2"))(7)
#' climate_heatmap_continuous(df, "rel", bins = 7, colors = colors) +
#'   ggplot2::theme(text = ggplot2::element_text(size = 18))
#'
#' @importFrom magrittr "%>%"
#' @export
climate_heatmap_continuous <- function(data,
  metric,
  bins = 7,
  ascending = TRUE,
  range = NULL,
  colors = NULL,
  to_percent = c(FALSE, TRUE),
  z_axis_title = "Range") {

  try({
    names <- names(data)
    if (!( ("temp" %in% names) & ("precip" %in% names))){
      stop("named 'temp' and 'precip' columns are required ",
        "for wrviz::climate_heatmap_continuous")
    }
    stopifnot(is.character(metric))
  })

  if (is.null(range)) {
    range <- get_range(data, metric)
  }

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
    to_percent = to_percent,
    z_axis_title = z_axis_title
  )

  plot
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
#' @param range A vector of length two that defines the max and min value of the
#'   scale.
#' @param bins Vector; Either a number indicating the number of bins, or a
#'   vector containing the locations where the bins should be cut.
#' @param scale List; should be length 4, in the format c(lowest, midpoint
#'   (white), one-bin-above-midpoint, highest).
#' @param ascending logical; do increasing values indicate a positive trend?
#' @param bin_name Character; Name of column to be created that contains bin
#'   data
#' @return a list \code{x}. \code{x$data} returns the data frame with a new
#'   column containing the bins, \code{x$colors} contains the corresponding
#'   color scale.
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
    b <- round(seq(floor(range[1]), ceiling(range[2]), length.out = bins + 1))

    if (is.null(scale)) {
      colors <- RColorBrewer::brewer.pal(bins, "RdBu")
    } else {
      colors <- scale
    }

    if (ascending == FALSE) {
      colors <- rev(colors)
    }

  } else {
    # they gave us where they want the bins cut, and they gave us a color scale
    b <- bins
    if (is.null(scale)) {
      colors <- RColorBrewer::brewer.pal(length(bins), "RdBu")
    } else {
      colors <- scale
    }
  }

  dots <- list(lazyeval::interp(~cut(x, b, dig.lab = 5, include.lowest = TRUE),
    x = as.name(by)))
  df <- data %>%
    dplyr::mutate_(.dots = stats::setNames(dots, c(bin_name)))


  list(data = df, colors = colors)
}
