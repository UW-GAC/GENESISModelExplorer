#' data_loader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_loader_ui <- function(id){
  ns <- NS(id)
  tagList(
      h2("Load data"),
      fileInput("null_model_file", label = "Null model file", accept = ".RData"),
      fileInput("phenotype_file", label = "Phenotype file", accept = ".RData"),
      # Grey this out until both files are uploaded?
      actionButton("load_data", "Load data")
  )
}

#' data_loader Server Functions
#'
#' @noRd
mod_data_loader_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_data_loader_ui("data_loader_ui_1")

## To be copied in the server
# mod_data_loader_server("data_loader_ui_1")
