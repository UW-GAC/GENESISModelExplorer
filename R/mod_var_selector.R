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
      # Get variable types.
      var_types <- sapply(dataset() %>% dplyr::select(-.data$sample.id), .detect_variable_type)

      possible_variables <- setdiff(names(var_types), "sample.id")

      updateSelectInput(session, "x", choices = possible_variables)
      updateSelectInput(session, "y", choices = possible_variables)

      # group by categorical variables only.
      categorical_variables <- names(var_types)[var_types == CATEGORICAL]
      updateSelectInput(session, "group", choices = categorical_variables)
    })

    return(
      list(
        x_var = reactive({ input$x }),
        y_var = reactive({ input$y })
      )
    )
  })
}
