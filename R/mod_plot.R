#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("plot_button"), "Generate plot"),
    plotOutput(ns("plot"))
  )
}

#' plot Server Functions
#'
#' @noRd
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_point
mod_plot_server <- function(id, dataset, selections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plot_obj <- eventReactive(input$plot_button, {
      .generate_plot(dataset(), selections$x_var(), selections$y_var(), selections$group_var())
    })

    output$plot <- renderPlot({
      plot_obj()
    })
  })
}

## To be copied in the UI
# mod_plot_ui("plot_ui_1")

## To be copied in the server
# mod_plot_server("plot_ui_1")
