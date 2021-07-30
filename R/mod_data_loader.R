#' data_loader UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyFiles shinyFilesButton
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
      conditionalPanel(
        condition = sprintf("input['%s'] == false", ns("use_example_data")),
        p(
          strong("Null model file"),
          fluidRow(
            column(1,
              shinyFilesButton(ns("null_model_file"), label = "Select", title = 'Please select a null model file', multiple = FALSE)
            ),
            column(11,
              textOutput(ns("selected_null_model_file"))
            )
          )
        ),
        helpText("The null model file should be a GENESIS null model. It is suggested to use the \"reportonly\" file, which is smaller but contains all data necessary for this app."),
        p(
          strong("Phenotype file"),
          fluidRow(
            column(1,
              shinyFilesButton(ns("phenotype_file"), label = "Select", title = 'Please select a phenotype file', multiple = FALSE)
            ),
            column(11,
              textOutput(ns("selected_phenotype_file"))
            )
          )
        ),
        helpText("The phenotype file should be the phenotype file used to fit the GENESIS null model, or a modified version of it with extra columns. It must contain all the sample ids in the null model."),
        p(
          strong("Genotype file (optional)"),
          fluidRow(
            column(1,
              shinyFilesButton(ns("genotype_file"), label = "Select", title = 'Please select a genotype file', multiple = FALSE)
            ),
            column(11,
              textOutput(ns("selected_genotype_file"))
            )
          ),
          helpText("The genotype file should contain all the samples in the null model in the sample.id column. Other columns must contain variants. This file can be generated with the XXX app.")
        )
      ),
      # TODO: Grey this out until both files are uploaded?
      actionButton(ns("load_data_button"), "Load data", class = "btn-primary"),
      textOutput(ns("data_loaded_message"))
  )
}

#' data_loader Server Functions
#'
#' @noRd
#' @importFrom shinyFiles shinyFileChoose
#' @importFrom shinyFiles parseFilePaths
mod_data_loader_server <- function(id, parent_session = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    roots <- c(wd = ".")
    shinyFileChoose(input, 'null_model_file', root=roots, filetypes=c('', 'RData'), session = session)
    shinyFileChoose(input, 'phenotype_file', root=roots, filetypes=c('', 'RData'), session = session)
    shinyFileChoose(input, 'genotype_file', root=roots, filetypes=c('', 'rds'), session = session)

    selected_null_model_file <- reactive({
      if (input$use_example_data) {
        # TODO: May need to change this when deployed.
        null_model_file = system.file("extdata", "null_model.RData", package = "shinyNullModel")
      } else if (!is.null(input$null_model_file)) {
        null_model_file <- parseFilePaths(roots, input$null_model_file)$datapath
      } else {
        return(NULL)
      }
      null_model_file
    })

    selected_phenotype_file <- reactive({
      if (input$use_example_data) {
        # TODO: May need to change this when deployed.
        phenotype_file = system.file("extdata", "phenotype.RData", package = "shinyNullModel")
      } else if (!is.null(input$phenotype_file)) {
        phenotype_file <- parseFilePaths(roots, input$phenotype_file)$datapath
      } else {
        return(NULL)
      }
      phenotype_file
    })

    selected_genotype_file <- reactive({
      if (input$use_example_data) {
        # TODO: May need to change this when deployed.
        genotype_file = system.file("extdata", "genotypes.rds", package = "shinyNullModel")
      } else if (!is.null(input$genotype_file)) {
        genotype_file <- parseFilePaths(roots, input$genotype_file)$datapath
      } else {
        return(NULL)
      }
      genotype_file
    })

    data_reactive <- eventReactive(input$load_data_button, {

      validate(
        need(selected_null_model_file(), "Please select a null model file."),
        need(selected_phenotype_file(), "Please select a phenotype file.")
      )

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
        dat <- .load_data(
          selected_null_model_file(),
          selected_phenotype_file(),
          genotype_filename = selected_genotype_file(),
          updateProgress = updateProgress
        )
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

    output$selected_null_model_file <- renderText({
      sprintf("Selected: %s", selected_null_model_file())
    })

    output$selected_phenotype_file <- renderText({
      sprintf("Selected: %s", selected_phenotype_file())
    })

    output$selected_genotype_file <- renderText({
      sprintf("Selected: %s", selected_genotype_file())
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
