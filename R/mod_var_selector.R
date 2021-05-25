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
