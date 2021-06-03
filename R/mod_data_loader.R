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
      checkboxInput(ns("use_example_data"), label = "Use example data?"),
      fileInput(ns("null_model_file"), label = "Null model file", accept = ".RData"),
      fileInput(ns("phenotype_file"), label = "Phenotype file", accept = ".RData"),
      # TODO: Grey this out until both files are uploaded?
      actionButton(ns("load_data_button"), "Load data"),
      textOutput(ns("data_loaded_message"))
  )
}

#' data_loader Server Functions
#'
#' @noRd
mod_data_loader_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_reactive <- eventReactive(input$load_data_button, {
    # data_reactive <- reactive({
      print("reading in dataset")
      if (input$use_example_data) {
        print("loading example data")
        # TODO: May need to change this when deployed.
        null_model_file = system.file("extdata", "null_model.RData", package = "shinyNullModel")

        phenotype_file = system.file("extdata", "phenotype.RData", package = "shinyNullModel")
        print(file.exists(null_model_file))
        print(file.exists(phenotype_file))
      } else if (!is.null(input$null_model_file) & !is.null(input$phenotype_file)) {
        print("loading user data")
        null_model_file <- input$null_model_file$datapath
        phenotype_file <- input$phenotype_file$datapath
      } else {
        print("inputs are wrong")
        return(NULL)
      }

      print('loading data')
      .load_data(null_model_file, phenotype_file)
    })

    output$data_loaded_message <- renderText({
      sprintf("%s samples loaded", nrow(data_reactive()))
    })

    # Return the reactive dataset.
    return(data_reactive)

  })
}

## To be copied in the UI
# mod_data_loader_ui("data_loader_ui_1")

## To be copied in the server
# mod_data_loader_server("data_loader_ui_1")
