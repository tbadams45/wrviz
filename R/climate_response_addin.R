#' @export
climateResponseAddin <- function() {

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Climate Response Creator")

  )

  server <- function(input, output, session) {

    shiny::observeEvent(input$done, {

      shiny::stopApp("you're done!")
    })
  }

  shiny::runGadget(ui, server)

}
