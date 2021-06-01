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
    selectInput(ns("y"), label = "y axis", choices = NULL),
    selectInput(ns("group"), label = "group by", choices = NULL)
  )
}

#' var_selector Server Functions
#'
#' @noRd
mod_var_selector_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # Update x and y axis selections based on loaded data.
    observe({
      updateSelectInput(session, "x", choices = .get_variable_names(dataset()))
      updateSelectInput(session, "y", choices = .get_variable_names(dataset()))
      # group by categorical variables only.
      var_types <- sapply(dataset(), .detect_variable_type)
      categorical_variables <- names(var_types)[var_types == CATEGORICAL]
      updateSelectInput(session, "group", choices = categorical_variables)
    })

    # observeEvent(input$x, {
    #   print(input$x)
    #   r$var_selector$x_var <- input$x
    # })
    #
    # observeEvent(input$y, {
    #   print(input$y)
    #   r$var_selector$y_var <- input$y
    # })


    return(
      list(
        x_var = reactive({ input$x }),
        y_var = reactive({ input$y })
      )
    )
  })
}
