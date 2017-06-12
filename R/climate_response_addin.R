#' Climate Response Gadget
#'
#' Used to easily create climate response surfaces. Can be used on the console,
#' or via the addins toolbar.
#'
#' @return A list containing the plot (x$plot), and the data used to create it
#'   (x$metrics and x$gcm)
#' @export
climateResponseCreator <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Climate Response Creator"),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Upload", icon = shiny::icon("upload"),
        miniUI::miniContentPanel(
          shiny::fillCol(
                shiny::tagList(
                  shiny::p("Upload a CSV containing a temperature change column, precipitation change column, and however many output metric columns you'd like. Temperature and precipitation columns must be named \"temp\" and \"precip\", respectively. If you intend to overlay GMC data, then your temperature column should be in absolute units, and your precipitation should be in percentage change, where 1 == 0% change, 0.9 represents -10% change, 1.2 represents 20% change, etc."),
                shiny::p("To include GCM data, upload the raw excel file output from Sungwook's GCM analysis tool. The file name will probably look something like", shiny::em("ProjectName_ClimatolChange_Hist(aaaa-bbbb)_RCP(cccc-dddd).xlsx"), "."),
                shiny::p("When you're done, hit the done button (yes, really!), and your plot and the data used to build it will be stored in a list. Even if you didn't assign the output to a variable in the console, you can access it using R's built in .Last.value variable."),
                shiny::uiOutput("upload_complete_notification"),
                shiny::fileInput("data_file", "Upload temp, precip, metric data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values",
                    "text/tab-separated-values",
                    "text/plain",
                    ".csv",
                    ".tsv"
                  )
                ),
                shiny::fileInput("gcm_file", "Upload GCM data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values",
                    "text/tab-separated-values",
                    "text/plain",
                    ".csv",
                    ".tsv",
                    ".xlsx",
                    ".xls"
                  )
                )
                )
          )
        ) # close miniContentPanel
      ), # close miniTabPanel

      miniUI::miniTabPanel("Edit", icon = shiny::icon("pencil"),
        miniUI::miniContentPanel(
          shiny::fillCol(flex = c(2,3),
            shiny::fillRow(width = "100%", height = "85%", # plot
              shiny::plotOutput("plot", height = "300px")
            ),

            shiny::fillRow( # outputs
              shiny::fillCol(width = "95%", shiny::tagList(
                shiny::uiOutput("output_column_controls"),
                shiny::uiOutput("eval_type_option"),
                shiny::uiOutput("gcm_option"),
                shiny::uiOutput("ascending_option"),
                shiny::uiOutput("format_as_percentage_controls")
              )),
              shiny::fillCol(width = "95%", shiny::tagList(
                shiny::uiOutput("range_controls"),
                shiny::uiOutput("eval_type_specific_controls")
              )),
              shiny::fillCol(width = "95%", shiny::tagList(
                shiny::uiOutput("title_controls")
              ))
            )
            )

        ) # close miniContentPanel
      ) # close miniTabPanel
    ) # close miniTabstripPanel
  ) # close miniPage

  server <- function(input, output, session) {
    data <- shiny::reactive({

      in_file <- input$data_file

      if (is.null(in_file)) {
        return(NULL)
      }

      csv <- utils::read.csv(in_file$datapath, header = TRUE)

      csv
    })

    gcm_data <- shiny::reactive({
      in_file <- input$gcm_file

      if (is.null(in_file)) {
        return(NULL)
      }

      extension <- strsplit(in_file$name, "[.]")[[1]][2] # split on dot.
      file.rename(in_file$datapath, paste(in_file$datapath, ".", extension, sep = ""))
      parsed_data <- parse_gcm(paste(in_file$datapath, ".", extension, sep = ""))

      parsed_data
    })

    plot <- shiny::reactive({
      if (is.null(input$output_columns) ||
          is.null(input$is_continuous_scale) ||
          is.null(input$range_min) ||
          is.null(input$ascending) ||
          is.null(input$bins) ||
          is.null(input$to_percent_x) ||
          is.null(input$to_percent_y) ||
          is.null(input$range_max)) {
        return(NULL)
      }

      if (input$is_continuous_scale == TRUE) {
        bins <- as.numeric(unlist(strsplit(input$bins, split = ",")))

        if (input$colors != ""){
          colors <- unlist(strsplit(input$colors, split = ","))
        } else {
          colors <- NULL
        }

        temp_plot <- climate_heatmap_continuous(
          data(),
          metric = input$output_columns,
          bins = bins,
          ascending = input$ascending,
          range = c(input$range_min, input$range_max),
          colors = colors,
          to_percent = c(input$to_percent_x, input$to_percent_y),
          z_axis_title = input$z_axis_title
        )

      } else {
        if (input$colors != ""){
          colors <- unlist(strsplit(input$colors, split = ","))
        } else {
          colors <- NULL
        }

        if (is.null(input$threshold)) {
          return(NULL)
        }

        temp_plot <- climate_heatmap_binary(
          data(),
          metric = input$output_columns,
          threshold = as.numeric(input$threshold),
          ascending = input$ascending,
          color_scale = colors,
          to_percent = c(input$to_percent_x, input$to_percent_y),
          z_axis_title = input$z_axis_title
        )
      }

      if (input$gcm) {
        temp_plot <- temp_plot +
          ggplot2::geom_point(
            data = gcm_data(),
            ggplot2::aes(x = temp, y = precip, shape = scenario),
            size = 2,
            stroke = 1.5) +
          ggplot2::scale_shape_manual(name = "Scenarios",
                                      values = c(21, 22, 23, 24))
      }

      if (input$x_axis_units == "%") {
        x_lab <- paste(input$x_axis_title, " (%)")
      } else {
        x_lab <- paste(input$x_axis_title, " (\u00B0", input$x_axis_units, ")",
                      sep = "")
      }

      y_lab <- paste(input$y_axis_title, " (", input$y_axis_units, ")", sep = "")

      temp_plot <- temp_plot +
        ggplot2::labs(x = x_lab, y = y_lab) +
        ggplot2::theme(text = ggplot2::element_text(size = input$text_size))

      temp_plot
    })

    output$plot <- shiny::renderPlot({
      plot()
    })

    output$upload_complete_notification <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      shiny::tagList(
        shiny::tags$p(shiny::tags$b("Your upload is complete!
          Go to the edit tab to edit your plot. You can also upload your GCM
          data if you'd like."))
      )
    })

    output$output_column_controls <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      col_names <- colnames(data())
      col_names <- col_names[!col_names %in% c("temp", "precip")] # remove temp and precip columns, so we only have output columns
      shiny::selectInput("output_columns", "Display Variable", col_names)
    })

    output$eval_type_option <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      shiny::radioButtons("is_continuous_scale",
        "Evaluation Type",
        c("Continuous" = TRUE, "Binary" = FALSE),
        selected = TRUE,
        inline = TRUE)
    })

    output$gcm_option <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      shiny::checkboxInput("gcm", "Show GCMs", value = FALSE)
    })

    output$ascending_option <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      shiny::checkboxInput("ascending", "Ascending", value = TRUE)
    })

    output$format_as_percentage_controls <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      shiny::tagList(
        shiny::checkboxInput("to_percent_x",
                             "Format temp as percentage change",
                             value = FALSE),
        shiny::checkboxInput("to_percent_y",
                             "Format precip as percentage change",
                             value = TRUE)
      )
    })

    output$range_controls <- shiny::renderUI({
      if (is.null(input$output_columns) ||
          input$is_continuous_scale == FALSE) {
        return(NULL)
      }
      selected <- data()[input$output_columns]

      shiny::tagList(
        shiny::numericInput("range_min", "Range Minimum", value = floor(min(selected))),
        shiny::numericInput("range_max", "Range Maximum", value = ceiling(max(selected)))
      )
    })

    output$eval_type_specific_controls <- shiny::renderUI({
      if (is.null(data()) ||
          is.null(input$range_min) ||
          is.null(input$range_max)) {
        return(NULL)
      }

      if (input$is_continuous_scale == TRUE) {
        shiny::tagList(
          shiny::textInput("bins",
                           "Bins (either a number, OR a list e.g, 20, 30,
                            40, 50 for bins [20,30], (30,40], (40,50])",
            value = "7"),
          shiny::textInput("colors", "Custom colors (must equal number of bins) \u2014 separate values by commas but no spaces",
            value = "",
            placeholder = "#EF8A62,#F7F7F7,#67A9CF")
        )
      }
      else {
        selected <- data()[input$output_columns]

        range_min <- floor(min(selected))
        range_max <- ceiling(max(selected))

        shiny::tagList(
          shiny::sliderInput("threshold", "Threshold", min = range_min, max = range_max,
            value = (range_max + range_min) / 2, round = TRUE),
          shiny::textInput("colors", "Custom colors (must have 2) \u2014 separate values by commas but no spaces",
            value = "",
            placeholder = "#2E2ECC,#CC2E2E")
        )
      }
    })

    output$title_controls <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      shiny::tagList(
        shiny::textInput("x_axis_title", "X Axis Name", value = "Temperature Change"),
        shiny::selectInput("x_axis_units", "X Axis Units", choices = c("Fahrenheit" = "F", "Celsius" = "C", "%")),
        shiny::textInput("y_axis_title", "Y Axis Name", value = "Precipitaton Change"),
        shiny::textInput("y_axis_units", "Y Axis Units", value = "%"),
        shiny::numericInput("text_size", "Text Size", value = 20, min = 6, max = 100),
        shiny::textInput("z_axis_title", "Z Axis Title", value = "Range")
      )
    })

    shiny::observeEvent(input$done, {
      val <- list(metrics = data(), gcm = gcm_data(), plot = plot())
      print("Note: if you forgot to save your results in a variable, they're stored in .Last.value")

      shiny::stopApp(val)
    })
  }

  shiny::runGadget(ui,
                   server,
                   viewer = shiny::paneViewer(minHeight = "maximize"))
                   # viewer = shiny::dialogViewer("Climate Reponse Explorer",
                   #                              width = 800,
                   #                              height = 1300))
}
