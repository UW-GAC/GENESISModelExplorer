#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  r <- reactiveValues()

  # Your application server logic
  mod_data_loader_server("data_loader_ui_1", r)
  mod_var_selector_server("var_selector_ui_1", r)
  mod_plot_server("plot_ui_1", r)
}
