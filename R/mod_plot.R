#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
mod_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("plot_button"), "Generate plot"),
    plotlyOutput(ns("plot"))
  )
}

#' plot Server Functions
#'
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @noRd
mod_plot_server <- function(id, dataset, selections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plot_obj <- eventReactive(input$plot_button, {
      x_var <- .check_truthiness(selections$x_var())
      y_var <- .check_truthiness(selections$y_var())
      group_var <- .check_truthiness(selections$group_var())
      facet_var <- .check_truthiness(selections$facet_var())

      .generate_plot(
        dataset(),
        x_var,
        y_var,
        group_var = group_var,
        facet_var = facet_var,
        hexbin = selections$hexbin(),
        abline = selections$abline(),
        loess = selections$loess(),
        lm = selections$lm(),
        yintercept = selections$yintercept()
      )
    })

    output$plot <- renderPlotly({
      req(plot_obj)
      plot_obj() %>% ggplotly() %>% plotly::layout(boxmode = 'group')
    })
  })
}

## To be copied in the UI
# mod_plot_ui("plot_ui_1")

## To be copied in the server
# mod_plot_server("plot_ui_1")
