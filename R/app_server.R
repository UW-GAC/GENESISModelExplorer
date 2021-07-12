#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  r <- reactiveValues()

  # Your application server logic
  mod_overview_server("overview_ui_1", parent_session = session)
  dataset <- mod_data_loader_server("data_loader_ui_1", parent_session = session)
  mod_plot_server("plot_ui_1", dataset)
}
