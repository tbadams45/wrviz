#' Climate Response Gadget
#'
#' Used to easily create climate response surfaces. Can be used on the console,
#' or via the addins toolbar.
#'
#' @return A list containing the plot (x$plot) and the data used to create it
#'   (x$data)
#' @export
climateResponseAddin <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Climate Response Creator"),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Upload", icon = shiny::icon("upload"),
        miniUI::miniContentPanel(
          shiny::p("Upload a CSV. Temperature column should be called 'temp', and precipitation should be 'precip'. You can have any number of columns with output variables."),
          shiny::uiOutput('uploadCompleteNotification'),
          shiny::fileInput('file', 'Choose file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv'
            )
          ),
          shiny::tags$hr(),
          shiny::checkboxInput('header', 'Header', TRUE),
          shiny::radioButtons('sep', 'Separator',
            c(Comma=',',
              Semicolon=';',
              Tab='\t'),
            ','),
          shiny::radioButtons('quote', 'Quote',
            c(None='',
              'Double Quote'='"',
              'Single Quote'="'"),
            '"')
        ) # close miniContentPanel
      ), # close miniTabPanel

      miniUI::miniTabPanel("Edit", icon = shiny::icon("pencil"),
        miniUI::miniContentPanel(
          shiny::fillRow(width = "100%", height = "65%", # plot
            shiny::plotOutput('plot', height = "400px")
          ),
          shiny::fillRow( # outputs
            shiny::fillCol(width = "95%", shiny::tagList(
              shiny::uiOutput('outputColumnControls'),
              shiny::uiOutput('evalTypeOption'),
              shiny::uiOutput('ascendingOption'),
              shiny::uiOutput('formatAsPercentageControls')
            )),
            shiny::fillCol(width = "95%", shiny::tagList(
              shiny::uiOutput('rangeControls'),
              shiny::uiOutput('evalTypeSpecificControls')
            )),
            shiny::fillCol(width = "95%", shiny::tagList(
              shiny::uiOutput('titleControls')
            ))
          )
        ) # close miniContentPanel
      ) # close miniTabPanel
    ) # close miniTabstripPanel
  ) # close miniPage

  server <- function(input, output, session) {
    data <- shiny::reactive({

      inFile <- input$file

      if (is.null(inFile)) {
        return(NULL)
      }

      csv <- utils::read.csv(inFile$datapath, header = input$header,
        sep = input$sep, quote = input$quote)

      shiny::updateCheckboxInput(session, 'fileOpt', value = FALSE)
      csv
    })

    plot <- shiny::reactive({
      if (is.null(input$outputColumns) ||
          is.null(input$isContinuousScale) ||
          is.null(input$rangeMin) ||
          is.null(input$ascending) ||
          is.null(input$bins) ||
          is.null(input$toPercentX) ||
          is.null(input$toPercentY) ||
          is.null(input$rangeMax)) {
        return(NULL)
      }

      if(input$isContinuousScale == TRUE) {
        bins <- as.numeric(unlist(strsplit(input$bins, split=",")))

        if(input$colors != ''){
          colors <- unlist(strsplit(input$colors, split=","))
        } else {
          colors <- NULL
        }

        temp_plot <- climate_heatmap_continuous(
          data(),
          metric = input$outputColumns,
          bins = bins,
          ascending = input$ascending,
          range = c(input$rangeMin, input$rangeMax),
          colors = colors,
          to_percent = c(input$toPercentX, input$toPercentY),
          z_axis_title = input$zAxisTitle
        )

      } else {
        if(input$colors != ''){
          colors <- unlist(strsplit(input$colors, split=","))
        } else {
          colors <- NULL
        }

        if (is.null(input$threshold)) {
          return(NULL)
        }

        temp_plot <- climate_heatmap_binary(
          data(),
          metric = input$outputColumns,
          threshold = as.numeric(input$threshold),
          ascending = input$ascending,
          color_scale = colors,
          to_percent = c(input$toPercentX, input$toPercentY),
          z_axis_title = input$zAxisTitle
        )
      }

      if(input$xAxisUnits == "%") {
        xLab <- paste(input$xAxisTitle, " (%)")
      } else {
        xLab <- paste(input$xAxisTitle, " (\u00B0", input$xAxisUnits, ")", sep = '')
      }

      yLab <- paste(input$yAxisTitle, " (", input$yAxisUnits, ")", sep = '')

      temp_plot +
        ggplot2::labs(x = xLab, y = yLab) +
        ggplot2::theme(text = ggplot2::element_text(size = input$textSize))
    })

    output$plot <- shiny::renderPlot({
      plot()
    })

    output$uploadCompleteNotification <- shiny::renderUI({
      if(is.null(data())) {
        return(NULL)
      }
      shiny::tagList(
        shiny::tags$p(shiny::tags$b("Your upload is complete! Go to the edit tab."))
      )
    })

    output$outputColumnControls <- shiny::renderUI({
      if(is.null(data())) {
        return(NULL)
      }
      colNames <- colnames(data())
      colNames <- colNames[!colNames %in% c('temp', 'precip')] # remove temp and precip columns, so we only have output columns
      shiny::selectInput('outputColumns', 'Display Variable', colNames)
    })

    output$evalTypeOption <- shiny::renderUI({
      if (is.null(data())) {
        return(NULL)
      }
      shiny::radioButtons('isContinuousScale',
        'Evaluation Type',
        c('Continuous' = TRUE, 'Binary' = FALSE),
        selected = TRUE,
        inline = TRUE)
    })

    output$ascendingOption <- shiny::renderUI({
      if(is.null(data())) {
        return(NULL)
      }
      shiny::checkboxInput('ascending', 'Ascending', value = TRUE)
    })

    output$formatAsPercentageControls <- shiny::renderUI({
      if(is.null(data())) {
        return(NULL)
      }
      shiny::tagList(
        shiny::checkboxInput('toPercentX', "Format temp as percentage change", value = FALSE),
        shiny::checkboxInput('toPercentY', "Format precip as percentage change", value = TRUE)
      )
    })

    output$rangeControls <- shiny::renderUI({
      if(is.null(input$outputColumns) ||
          input$isContinuousScale == FALSE) {
        return(NULL)
      }
      selected <- data()[input$outputColumns]

      shiny::tagList(
        shiny::numericInput('rangeMin', 'Range Minimum', value = floor(min(selected))),
        shiny::numericInput('rangeMax', 'Range Maximum', value = ceiling(max(selected)))
      )
    })

    output$evalTypeSpecificControls <- shiny::renderUI({
      if(is.null(data()) ||
          is.null(input$rangeMin) ||
          is.null(input$rangeMax)) {
        return(NULL)
      }

      if (input$isContinuousScale == TRUE) {
        shiny::tagList(
          shiny::textInput('bins', 'Bins (either a number, OR a list. e.g, 20, 30, 40, 50 for bins [20,30], (30,40], (40,50])',
            value = "7"),
          shiny::textInput('colors', 'Custom colors (must equal number of bins)',
            value = '',
            placeholder = "#EF8A62,#F7F7F7,#67A9CF")
        )
      }
      else {
        selected <- data()[input$outputColumns]

        rangeMin <- floor(min(selected))
        rangeMax <- ceiling(max(selected))

        shiny::tagList(
          shiny::sliderInput('threshold', 'Threshold', min = rangeMin, max = rangeMax,
            value = (rangeMax + rangeMin) / 2, round = TRUE),
          shiny::textInput('colors', 'Custom colors (must have 2)',
            value = '',
            placeholder = '#2E2ECC,#CC2E2E')
        )
      }
    })

    output$titleControls <- shiny::renderUI({
      if(is.null(data())) {
        return(NULL)
      }
      shiny::tagList(
        shiny::textInput('xAxisTitle', 'X Axis Name', value = 'Temperature Change'),
        shiny::selectInput('xAxisUnits', 'X Axis Units', choices = c("Fahrenheit" = "F", "Celsius" = "C", "%")),
        shiny::textInput('yAxisTitle', 'Y Axis Name', value = 'Precipitaton Change'),
        shiny::textInput('yAxisUnits', 'Y Axis Units', value = "%"),
        shiny::numericInput('textSize', 'Text Size', value = 20, min = 6, max = 100),
        shiny::textInput('zAxisTitle', 'Z Axis Title', value = "Range")
      )
    })

    shiny::observeEvent(input$done, {
      val <- list(data = data(), plot = plot())

      shiny::stopApp(val)
    })
  }

  shiny::runGadget(ui,
                   server,
                   viewer = shiny::dialogViewer("Climate Reponse Explorer",
                                                width = 800,
                                                height = 1300))

}
