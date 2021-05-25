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
    plotOutput(ns("plot"))
  )
}

#' plot Server Functions
#'
#' @noRd
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_point
mod_plot_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plot_obj <- reactive({
      dat <- r$data_loader$dataset
      x_var <- r$var_selector$x_var
      y_var <- r$var_selector$y_var

      p <- ggplot(dat, aes_string(x = as.name(x_var), y = as.name(y_var))) +
        geom_point()
      return(p)
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
