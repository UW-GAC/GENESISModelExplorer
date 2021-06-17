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
      h3("Main input"),
      selectInput(ns("x"), label = "x axis", choices = NULL),
      selectInput(ns("y"), label = "y axis", choices = NULL, selectize = FALSE),
      selectInput(ns("group"), label = "group by", choices = NULL, selectize = FALSE),
      selectInput(ns("facet"), label = "facet by", choices = NULL, selectize = FALSE),
    ),
    column(4,
      h3("General options"),
      checkboxInput(ns("yintercept"), label = "Add y = 0 line?"),
      numericInput(ns("nbins_histogram"), label = "Number of bins for histograms", value = 30, step = 1, min = 2, max = 100),
      numericInput(ns("nbins_hexbin"), label = "Number of bins for hexbin plot", value = 30, step = 1, min = 2, max = 100),
      checkboxInput(ns("hide_legend"), label = "Hide legend?")
    ),
    column(4,
      h3("Scatterplot options"),
      checkboxInput(ns("hexbin"), label = "Hexbin instead of scatterplot?"),
      checkboxInput(ns("abline"), label = "Add x = y line?"),
      checkboxInput(ns("smooth_line"), label = "Add smooth line?"),
      checkboxInput(ns("lm"), label = "Add lm line?"),
      h3("Boxplot options"),
      checkboxInput(ns("violin"), label = "Violin plot instead of boxplot?"),
      h3("Histogram options"),
      checkboxInput(ns("density"), label = "Density plot instead of histogram?"),
      checkboxInput(ns("proportion"), label = "Show proportion instead of counts?")
    ),
    actionButton(ns("plot_button"), "Generate plot", class = "btn-primary"),
    plotOutput(ns("plot"))
  )
}

#' plot Server Functions
#'
#' @noRd
mod_plot_server <- function(id, dataset){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Update x and y axis selections based on loaded data.
    observe({
      # Get variable types.
      these_types <- var_types()

      updateSelectInput(session, "x", choices = names(these_types))
      updateSelectInput(session, "y", choices = c("---" = "", names(these_types)))

      # group by categorical variables only.
      categorical_variables <- names(these_types)[these_types == CATEGORICAL]
      updateSelectInput(session, "group", choices = c("---" = "", categorical_variables))
      updateSelectInput(session, "facet", choices = c("---" = "", categorical_variables))
    })

    var_types <- reactive({
      tmp <- sapply(dataset(), .detect_variable_type)
      tmp[names(tmp) != "sample.id"]
    })

    plot_type <- reactive({
      req(input$x)
      x_type <- var_types()[input$x]
      y_type <- .check_truthiness(var_types()[input$y])
      tryCatch({
        .get_plot_type(x_type, y_type = y_type, density = input$density,
                       hexbin = input$hexbin, violin = input$violin)
      },
      error = function(err) {
        validate(err$message)
      })
    })

    observe({
      req(plot_type())
      shiny::updateActionButton(session, "plot_button", label = sprintf("Generate %s", plot_type()))
    })

    plot_obj <- eventReactive(input$plot_button, {
      x_var <- .check_truthiness(input$x)
      y_var <- .check_truthiness(input$y)
      group_var <- .check_truthiness(input$group)
      facet_var <- .check_truthiness(input$facet)

      .generate_plot(
        dataset(),
        x_var,
        y_var,
        group_var = group_var,
        facet_var = facet_var,
        hexbin = input$hexbin,
        abline = input$abline,
        smooth_line = input$smooth_line,
        lm = input$lm,
        yintercept = input$yintercept,
        violin = input$violin,
        nbins_histogram = input$nbins_histogram,
        nbins_hexbin = input$nbins_hexbin,
        density = input$density,
        hide_legend = input$hide_legend,
        proportion = input$proportion
      )
    })

    output$plot_type <- renderText({
      plot_type()
    })
    outputOptions(output, "plot_type", suspendWhenHidden = FALSE)

    output$plot <- renderPlot({
      plot_obj()
    })
  })
}

## To be copied in the UI
# mod_plot_ui("plot_ui_1")

## To be copied in the server
# mod_plot_server("plot_ui_1")
