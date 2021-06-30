#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Welcome to Shiny Null Model!"),
    p(
      "This app allows you to interactively explore GENESIS null models that you have fit on a set of samples. It can help you determine whether a model fit looks reasonable or identify outliers in your model."
    ),
    p(
      "You will need access to:"
    ),
    tags$ol(
      tags$li(HTML("The <b>GENESIS null model file</b> in RData format.")),
      tags$li(HTML("A <b>phenotype file</b> in RData format. This can be the same file you used to fit the null model, or a new file where you have added additional columns. It must contain all the samples included in your null model file."))
    ),
    p(
      "First, you will load these files into the app. After loading your data, you will be able to generate plots of different phenotype or model variables. You can plot these variables by themselves or against each other. You can also color the plots or created faceted plots using these variables."
    ),
    p("placeholder button")
  )
}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # No server code yet, since this is intended to provide general information
    # about using the app.
  })
}
