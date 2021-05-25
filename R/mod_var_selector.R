#' var_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_var_selector_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Plot setup"),
    selectInput(ns("x"), label = "x axis", choices = NULL),
    selectInput(ns("y"), label = "y axis", choices = NULL)
  )
}

#' var_selector Server Functions
#'
#' @noRd
mod_var_selector_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r$var_selector <- reactiveValues(
      x = NULL,
      y = NULL
    )
    # Update x and y axis selections based on loaded data.
    observe({
      updateSelectInput(session, "x", choices = .get_variable_names(r$data_loader$dataset))
    })

    observe({
      updateSelectInput(session, "y", choices = .get_variable_names(r$data_loader$dataset))
    })

  })
}
