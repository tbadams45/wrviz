#' Climate Response Gadget
#'
#' Used to easily create climate response surfaces. Can be used on the console,
#' or via the addins toolbar.
#'
#' @return A list containing the plot (x$plot), and the data used to create it
#'   (x$metrics and x$gcm)
#' @export
climateResponseCreator <- function() {
  ALL_SCENARIOS <- c("RCP 2.6", "RCP 4.5", "RCP 6.0", "RCP 8.5")

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
              ),
              shiny::actionButton("demo_data", "Use Demo Data")
            )
          )
        ) # close miniContentPanel
      ), # close miniTabPanel

      miniUI::miniTabPanel("Edit Chart", icon = shiny::icon("area-chart"),
        miniUI::miniContentPanel(
          shiny::fillRow(flex = c(3, 2),

            shiny::fillRow( # outputs
              shiny::fillCol(width = "95%", shiny::tagList(
                shiny::uiOutput("output_column_controls"),
                shiny::uiOutput("eval_type_option"),
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
            ),

            shiny::fillRow(width = "100%", height = "85%", # plot
              shiny::plotOutput("plot", height = "400px")
            )
          )
        ) # close miniContentPanel
      ), # close miniTabPanel

      miniUI::miniTabPanel("Add GCMS", icon = shiny::icon("table"),
        miniUI::miniContentPanel(
          shiny::fillRow(flex = c(3, 2),
            shiny::fillCol(shiny::tagList(
              shiny::uiOutput("gcm_option"),
              shiny::uiOutput("scenario_selection_option"),
              shiny::actionButton("deselect_all", "Deselect all models"),
              shiny::actionButton("select_all", "Select all models"),
              DT::dataTableOutput("selected_models")
            )),
            shiny::fillRow(
              shiny::plotOutput("plot2", height = "400px")
            )
          )
        )
      )
    ) # close miniTabstripPanel
  ) # close miniPage

  server <- function(input, output, session) {
    demo_data <- shiny::reactive({
      if (input$demo_data == 0) {
        return(NULL)
      }

      file_name <- system.file("extdata/stresstest_ffd.csv", package = "wrviz")
      utils::read.csv(file_name, header = TRUE)
    })


    data <- shiny::reactive({
      if (!is.null(demo_data())) {
        return(demo_data())
      }

      in_file <- input$data_file

      if (is.null(in_file)) {
        return(NULL)
      }

      csv <- utils::read.csv(in_file$datapath, header = TRUE)

      csv
    })

    demo_gcm_data <- shiny::reactive({
      if (input$demo_data == 0) {
        return(NULL)
      }
      file_name <- system.file("extdata/raw_cmip5.xlsx", package = "wrviz")
      parsed_data <- parse_gcm(file_name)
      parsed_data
    })


    # raw GCM data from file/demo data
    gcm_data <- shiny::reactive({
      if (!is.null(demo_gcm_data())) {
        result <- demo_gcm_data()
      } else {
        in_file <- input$gcm_file

        if (is.null(in_file)) {
          return(NULL)
        }

        extension <- strsplit(in_file$name, "[.]")[[1]][2] # split on dot.
        file.rename(in_file$datapath, paste(in_file$datapath, ".", extension, sep = ""))
        result <- parse_gcm(paste(in_file$datapath, ".", extension, sep = ""))
      }

      result
    })

    # selected models from table, used to generate plot
    gmc_data_selected_models <- shiny::reactive({
      result <- gcm_data()
      result <- result[input$selected_models_rows_selected, ]
      result <- dplyr::filter(result, !is.na(temp)) # get rid of rows that are NA
    })

    # basic plot does not include GCMs.
    basic_plot <- shiny::reactive({
      if (is.null(input$output_columns) ||
          is.null(input$is_continuous_scale) ||
          is.null(input$range_min) ||
          is.null(input$ascending) ||
          is.null(input$bins) ||
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
          to_percent = c(FALSE, input$to_percent_y),
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
          to_percent = c(FALSE, input$to_percent_y),
          z_axis_title = input$z_axis_title
        )
      }

      if (input$x_axis_units == "%") {
        x_lab <- paste(input$x_axis_title, " (%)")
      } else {
        x_lab <- paste(input$x_axis_title, " (\u00B0", input$x_axis_units, ")",
                      sep = "")
      }

      y_lab <- input$y_axis_title

      temp_plot <- temp_plot +
        ggplot2::labs(x = x_lab, y = y_lab) +
        ggplot2::theme(text = ggplot2::element_text(size = input$text_size))

      temp_plot
    })

    gcm_plot <- shiny::reactive({
      if (is.null(gcm_data()) ||
          is.null(input$show_gcm)) {
        return(NULL)
      }

      if (input$show_gcm == FALSE) {
        temp_plot <- basic_plot()
      } else {
        data <- gmc_data_selected_models()

        temp_plot <- basic_plot() + ggplot2::geom_point(
          data = data,
          ggplot2::aes(x = temp, y = precip, shape = scenario),
          size = 2,
          stroke = 1.5) +
          ggplot2::scale_shape_manual(name = "Scenarios",
            values = c(21, 22, 23, 24))
      }

      temp_plot
    })

    output$plot <- shiny::renderPlot({
      if (is.null(input$show_gcm) ||
          input$show_gcm == FALSE) {
        temp_plot <- basic_plot()
      } else {
        temp_plot <- gcm_plot()
      }
      temp_plot
    })

    output$plot2 <- shiny::renderPlot({
      if (is.null(input$show_gcm) ||
          input$show_gcm == FALSE) {
        temp_plot <- basic_plot()
      } else {
        temp_plot <- gcm_plot()
      }
      temp_plot
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
        return(shiny::p("Please upload your metric data on the upload tab."))
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

    output$ascending_option <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      shiny::checkboxInput("ascending", "Invert colors", value = TRUE)
    })

    output$format_as_percentage_controls <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      shiny::tagList(
        shiny::checkboxInput("to_percent_y",
                             "Format precipitation as percentage change",
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
                           "Bins",
            value = "7"),
          p("You can also enter a list to specify where to cut custom bins. E.g., the list \"20, 30, 40, 50\" will create the bins [20,30], (30,40], (40, 50]."),
          shiny::textInput("colors", "Custom colors (must equal number of bins)",
            value = "",
            placeholder = "#EF8A62,#F7F7F7,#67A9CF"),
          p("Separate values by commas but no spaces.")
        )
      }
      else {
        selected <- data()[input$output_columns]

        range_min <- floor(min(selected))
        range_max <- ceiling(max(selected))

        shiny::tagList(
          shiny::sliderInput("threshold", "Threshold", min = range_min, max = range_max,
            value = (range_max + range_min) / 2, round = TRUE),
          shiny::textInput("colors", "Custom colors (must have 2)",
            value = "",
            placeholder = "#2E2ECC,#CC2E2E"),
          p("Separate values by commas but no spaces.")
        )
      }
    })

    output$title_controls <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      shiny::tagList(
        shiny::textInput("x_axis_title", "X Axis Title", value = "Temperature Change"),
        shiny::selectInput("x_axis_units", "X Axis Units", choices = c("Fahrenheit" = "F", "Celsius" = "C", "%"), selected = "C"),
        shiny::textInput("y_axis_title", "Y Axis Title", value = "Precipitaton Change (%)"),
        shiny::textInput("z_axis_title", "Z Axis Title", value = "Range"),
        shiny::numericInput("text_size", "Text Size", value = 20, min = 6, max = 100)
      )
    })

    output$gcm_option <- shiny::renderUI({
      if (is.null(data())) {
        return(shiny::p("Please upload your metric and GCM data on the upload tab."))
      }

      if (is.null(gcm_data())) {
        view <- shiny::checkboxInput("show_gcm", "Show GCMs", value = FALSE)
      } else {
        view <- shiny::checkboxInput("show_gcm", "Show GCMs", value = TRUE)
      }

      view
    })

    output$scenario_selection_option <- shiny::renderUI({
      gcms <- gcm_data()
      scenarios <- ALL_SCENARIOS
      shiny::checkboxGroupInput("selected_scenarios",
                                "Filter by scenarios",
                                choices = scenarios,
                                selected = scenarios,
                                inline = TRUE)
    })


    output$selected_models <- DT::renderDataTable(
      DT::datatable(gcm_data(),
                      selection = list(mode = "multiple",
                                       selected = 1:nrow(gcm_data())),
                      extensions = "Scroller",
                      options = list(
                        deferRender = TRUE,
                        scrollY = 400,
                        scroller = TRUE)) %>%
          DT::formatRound("temp", 3) %>%
          DT::formatRound("precip", 3)
      )


    dt_proxy <- DT::dataTableProxy("selected_models")
    shiny::observeEvent(input$deselect_all, {
      dt_proxy %>% DT::selectRows(numeric(0))
      shiny::updateCheckboxGroupInput(session, "selected_scenarios", selected = character(0))
    })

    shiny::observeEvent(input$select_all, {
      dt_proxy %>% DT::selectRows(1:nrow(gcm_data()))
      shiny::updateCheckboxGroupInput(session, "selected_scenarios", selected = ALL_SCENARIOS)
    })

    shiny::observeEvent(input$selected_scenarios, {
      currently_selected <- tibble::rowid_to_column(gcm_data())

      print(currently_selected)

      print(input$selected_scenarios)
      print(currently_selected$scenario %in% input$selected_scenarios)
      rows_to_keep <- currently_selected[currently_selected$scenario %in% input$selected_scenarios,]

      dt_proxy %>% DT::selectRows(rows_to_keep$rowid)
    })

    shiny::observeEvent(input$done, {
      if (is.null(input$show_gcm) ||
          input$show_gcm == FALSE) {
        plot <- basic_plot()
      } else {
        plot <- gcm_plot()
      }

      val <- list(metrics = data(), gcm = gmc_data_selected_models(), plot = plot)
      print("Note: if you forgot to save your results in a variable, they're stored in .Last.value")

      shiny::stopApp(val)
    })
  }

  shiny::runGadget(ui,
                   server,
                   viewer = shiny::dialogViewer("Climate Reponse Explorer",
                                                width = 1200,
                                                height = 750))
}
