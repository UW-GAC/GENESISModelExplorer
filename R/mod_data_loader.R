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
      fileInput(ns("null_model_file"), label = "Null model file", accept = ".RData"),
      fileInput(ns("phenotype_file"), label = "Phenotype file", accept = ".RData"),
      # TODO: Grey this out until both files are uploaded?
      actionButton(ns("load_data_button"), "Load data"),

      tableOutput(ns("data_head"))
  )
}

#' data_loader Server Functions
#'
#' @noRd
mod_data_loader_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Report if button was pressed in the console.
    observe({
        print(input$load_data_button)
    })

    # Load the data when the user selects a null model and phenotype file.
    data_reactive <- eventReactive(input$load_data_button, {

      print('checking data reactive')
      null_model_file <- input$null_model_file
      phenotype_file <- input$phenotype_file

      if (!is.null(null_model_file) & !is.null(phenotype_file)) {
        print('loading data')
        .load_data(null_model_file$datapath, phenotype_file$datapath)
      } else {
        return(NULL)
      }
    })

    output$data_head <- renderTable({
      head(data_reactive())
    })

  })
}

## To be copied in the UI
# mod_data_loader_ui("data_loader_ui_1")

## To be copied in the server
# mod_data_loader_server("data_loader_ui_1")
