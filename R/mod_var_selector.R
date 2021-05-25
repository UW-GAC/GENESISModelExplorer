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
mod_var_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
