#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_ui <- function(id){
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
      h3("General options"),
      checkboxInput(ns("yintercept"), label = "Add y = 0 line?"),
      numericInput(ns("nbins"), label = "Number of bins for histograms or hexbin plots", value = 30, step = 1, min = 2, max = 100),
      checkboxInput(ns("hide_legend"), label = "Hide legend?")
    ),
    column(4,
      h3("Scatterplot options"),
      checkboxInput(ns("hexbin"), label = "Hexbin instead of scatterplot?"),
      checkboxInput(ns("abline"), label = "Add x = y line?"),
      checkboxInput(ns("loess"), label = "Add loess smooth line?"),
      checkboxInput(ns("lm"), label = "Add lm line?"),
      h3("Boxplot options"),
      checkboxInput(ns("violin"), label = "Violin plot instead of boxplot?"),
      h3("Histogram options"),
      checkboxInput(ns("density"), label = "Density plot instead of histogram?"),
      checkboxInput(ns("proportion"), label = "Show proportion instead of counts?")
    ),
    actionButton(ns("plot_button"), "Generate plot"),
    plotOutput(ns("plot"))
  )
}

#' plot Server Functions
#'
#' @noRd
mod_plot_server <- function(id, dataset, selections){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plot_obj <- eventReactive(input$plot_button, {
      x_var <- .check_truthiness(selections$x_var())
      y_var <- .check_truthiness(selections$y_var())
      group_var <- .check_truthiness(selections$group_var())
      facet_var <- .check_truthiness(selections$facet_var())

      .generate_plot(
        dataset(),
        x_var,
        y_var,
        group_var = group_var,
        facet_var = facet_var,
        hexbin = selections$hexbin(),
        abline = selections$abline(),
        loess = selections$loess(),
        lm = selections$lm(),
        yintercept = selections$yintercept(),
        violin = selections$violin(),
        nbins = selections$nbins(),
        density = selections$density(),
        hide_legend = selections$hide_legend(),
        proportion = selections$proportion()
      )
    })

    output$plot <- renderPlot({
      plot_obj()
    })
  })
}

## To be copied in the UI
# mod_plot_ui("plot_ui_1")

## To be copied in the server
# mod_plot_server("plot_ui_1")
