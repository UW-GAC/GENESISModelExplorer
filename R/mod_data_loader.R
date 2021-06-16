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

    tags$head(
      tags$style(
        HTML(".shiny-notification {
                height: 100px;
                width: 400px;
                position:fixed;
                top: calc(50% - 50px);;
                left: calc(50% - 400px);;
              }
             "
        )
      )
    ),

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
mod_data_loader_server <- function(id, parent_session = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_reactive <- eventReactive(input$load_data_button, {

      if (input$use_example_data) {
        # TODO: May need to change this when deployed.
        null_model_file = system.file("extdata", "null_model.RData", package = "shinyNullModel")

        phenotype_file = system.file("extdata", "phenotype.RData", package = "shinyNullModel")
      } else if (!is.null(input$null_model_file) & !is.null(input$phenotype_file)) {
        null_model_file <- input$null_model_file$datapath
        phenotype_file <- input$phenotype_file$datapath
      } else {
        return(NULL)
      }

      # Set up progress reporting
      # From this url: https://shiny.rstudio.com/articles/progress.html
      progress <- shiny::Progress$new()
      progress$set(message = "Loading data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())

      # Create a callback function to update progress.
       # Each time this is called:
       # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
       #   distance. If non-NULL, it will set the progress to that value.
       # - It also accepts optional detail text.
       updateProgress <- function(value = NULL, detail = NULL) {
         if (is.null(value)) {
           value <- progress$getValue()
           value <- value + (progress$getMax() - value) / 3
         }
         progress$set(value = value, detail = detail)
       }

      tryCatch({
        dat <- .load_data(null_model_file, phenotype_file, updateProgress = updateProgress)
      },
      error = function(err) {
        validate(err$message)
      })

      # Switch to the plotting tab.
      if (!is.null(parent_session)) {
        updateNavbarPage(parent_session, "navbar", selected="Plot")
      }

      return(dat)
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
