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
    column(4,
      h3("Main selections"),
      selectInput(ns("x"), label = "x axis", choices = NULL),
      selectInput(ns("y"), label = "y axis", choices = NULL, selectize = FALSE),
      selectInput(ns("group"), label = "group by", choices = NULL, selectize = FALSE),
      selectInput(ns("facet"), label = "facet by", choices = NULL, selectize = FALSE),
    ),
    column(4,
      h3("Scatterplot options"),
      checkboxInput(ns("hexbin"), label = "Hexbin instead of scatterplot?"),
      checkboxInput(ns("abline"), label = "Add x = y line?"),
      checkboxInput(ns("loess"), label = "Add loess smooth line?"),
      checkboxInput(ns("lm"), label = "Add lm line?"),
      h3("Boxplot options"),
      checkboxInput(ns("violin"), label = "Violin plot instead of boxplot?"),
      h3("Other options"),
      checkboxInput(ns("yintercept"), label = "Add y = 0 line?")
    )
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
      var_types <- sapply(dataset(), .detect_variable_type)
      var_types <- var_types[names(var_types) != "sample.id"]

      updateSelectInput(session, "x", choices = names(var_types))
      updateSelectInput(session, "y", choices = c("---" = "", names(var_types)))

      # group by categorical variables only.
      categorical_variables <- names(var_types)[var_types == CATEGORICAL]
      updateSelectInput(session, "group", choices = c("---" = "", categorical_variables))
      updateSelectInput(session, "facet", choices = c("---" = "", categorical_variables))
    })

    return(
      list(
        x_var = reactive({ input$x }),
        y_var = reactive({ input$y }),
        group_var = reactive({ input$group }),
        facet_var = reactive({ input$facet }),
        hexbin = reactive({ input$hexbin }),
        abline = reactive({ input$abline }),
        loess = reactive({ input$loess }),
        lm = reactive({ input$lm }),
        yintercept = reactive({ input$yintercept }),
        violin = reactive({ input$violin })
      )
    )
  })
}
