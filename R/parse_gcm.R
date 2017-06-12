#' Parse GCM data
#'
#' Takes GCM data from Sunwook Wi's GCM analysis tool, and converts it to a data
#' frame that can be used to overlay GCM information on top of a climate
#' heatmap.
#'
#' Sunwook's tool computes monthly averages of temperature and precipitation
#' change. We average these out to compute a yearly average.
#'
#' @param excel_filepath path to excel file
#'
#' @return A dataframe containing four columns (model, scenario, temp, and
#'   precip), which detail the gcm model, the scenario (e.g. RCP 2.6), the
#'   temperature change in absolute units, and the preciptation change in
#'   percentges (where 1 == 0\% change, and 0.9 == -10\% percentage change)
#' @importFrom magrittr "%>%"
#' @export
parse_gcm <- function(excel_filepath) {
  result <- dplyr::tibble(model = character(0),
                          scenario = character(0))

  # these are dependent on how Sunwook currently sends his data and will break
  # if he changes his output format
  remove <- c("Sheet1", "pr_historical", "tas_historical")

  sheet_names <- readxl::excel_sheets(excel_filepath)
  sheet_names <- sheet_names[!sheet_names %in% remove]

  gcm_data <- dplyr::tibble("model" = character(0),
                         "scenario" = character(0),
                         "temp" = numeric(0),
                         "precip" = c(numeric(0)))
  for (sheet_name in sheet_names) {
  #for (sheet_name in c("pr_rcp26", "tas_rcp26", "pr_rcp45", "tas_rcp45")) {
    sheet <- readxl::read_excel(excel_filepath, sheet = sheet_name) %>%
      tidyr::gather(key = months, value = value, -GCM) %>%
      tidyr::spread(key = GCM, value = value) %>%
      dplyr::select(-months)

    sheet_mean <- dplyr::summarise_all(sheet, mean)
    sheet_mean <- t(sheet_mean)
    models <- rownames(sheet_mean)
    model_info <- parse_sheet_name(sheet_name)

    sheet_result <- dplyr::tibble(model   = models,
                           scenario = model_info$scenario,
                           variable = model_info$variable,
                           value    = dplyr::as_tibble(sheet_mean)$V1) %>%
      tidyr::spread(key = variable, value = value)

    result <- dplyr::bind_rows(result, sheet_result)
  }

  # get rid of rows where we have neither a temp or precip value
  result <- dplyr::filter(result, !is.na(temp) | !is.na(precip))

  # we now have double the amount of rows we need: one set has NA for temp, and
  # and the other has NA for precip. This merges those two rows.
  # From https://stackoverflow.com/questions/14268814/merge-two-rows-in-one-dataframe-when-the-rows-are-disjoint-and-contain-nulls
  result <- stats::aggregate(x = result[c("temp", "precip")],
                             by = list(model = result$model, scenario = result$scenario),
                             min,
                             na.rm = TRUE)
  result <- dplyr::mutate(result, precip = (precip / 100) + 1)
  result
}

#' Parse GCM sheet names
#'
#' Takes potential sheet names from Sunwook Wi's GCM analysis tool, and returns
#' a list with the variable in question ("temp" or "precip") and the scenario
#' (e.g. "RCP 2.6").
#'
#' @param sheet_name string of sheet name
#' @example parse_sheet_name("tas_rcp45")
#'
#' @return list(variable, scenario)
parse_sheet_name <- function(sheet_name) {
  result <- list()
  split_name <- strsplit(sheet_name, "_")
  if (split_name[[1]][1] == "tas") {
    result$variable <- "temp"
  } else if (split_name[[1]][1] == "pr") {
    result$variable <- "precip"
  } else {
    stop(paste("unexpected sheet_name: expected tas_... or pr_..., got", sheet_name))
    return(NULL)
  }

  if (split_name[[1]][2] == "rcp26") {
    result$scenario <- "RCP 2.6"
  } else if (split_name[[1]][2] == "rcp45") {
    result$scenario <- "RCP 4.5"
  } else if (split_name[[1]][2] == "rcp60") {
    result$scenario <- "RCP 6.0"
  } else if (split_name[[1]][2] == "rcp85") {
    result$scenario <- "RCP 8.5"
  } else {
    print(split_name[[1]][2])
    stop(paste("Unexpected sheet_name, expected ..._rcp26 or ..._rcp45 or ..._rcp60 or ..._rcp85, but got", sheet_name))
  }

  result
}

