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
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("plot_button"), "Generate plot", class = "btn-primary"),
        h3("Variable selection"),
        helpText(
          "Only the x-axis variable is required.
          Variables names are prefixed by \"Phenotype: \" or \"Model: \" based on which file they were originally in.
          The plot type will be selected based on the types of the selected variables."
        ),
        selectInput(ns("x"), label = "x axis", choices = NULL),
        selectInput(ns("y"), label = "y axis", choices = NULL, selectize = FALSE),
        selectInput(ns("plot_type"), label = "plot type", choices = NULL),
        selectInput(ns("group"), label = "group by", choices = NULL, selectize = FALSE),
        helpText("The group variable can only be a categorical variable."),
        selectInput(ns("facet"), label = "facet by", choices = NULL, selectize = FALSE),
        helpText("The facet variable can only be a categorical variable."),

        h3("Additional options"),
        helpText("Different options will be displayed based on the plot type."),
        checkboxInput(ns("hide_legend"), label = "Hide legend"),
        conditionalPanel(
          condition = sprintf("output['%s']", ns("show_options_1d")),
          # Consider making this show up only if group_by is selected.
          checkboxInput(ns("proportion"), label = "Show proportion instead of counts"),
          conditionalPanel(
            condition = sprintf("output['%s']", ns("show_options_1d_quant"))
          ),
          conditionalPanel(
            condition = sprintf("output['%s'] == '%s'", ns("plot_type"), HISTOGRAM),
            numericInput(ns("nbins_histogram"), label = "Number of bins for histograms", value = 30, step = 1, min = 2, max = 100),
          )
        ),

        conditionalPanel(
          condition = sprintf("output['%s']", ns("show_options_2d")),
          checkboxInput(ns("yintercept"), label = "Add y = 0 line"),
          conditionalPanel(
            condition = sprintf("output['%s']", ns("show_options_2d_quant")),
            checkboxInput(ns("abline"), label = "Add x = y line"),
            checkboxInput(ns("smooth_line"), label = "Add smooth line"),
            checkboxInput(ns("lm"), label = "Add lm line"),
            conditionalPanel(
              condition = sprintf("output['%s'] == '%s'", ns("plot_type"), HEXBIN),
              numericInput(ns("nbins_hexbin"), label = "Number of bins for hexbin", value = 30, step = 1, min = 2, max = 100),
            )
          ),
          conditionalPanel(
            condition = sprintf("output['%s']", ns("show_options_2d_cat"))
          )
        )
      ),
      mainPanel(
        plotOutput(ns("plot"), height = "600px")
      )
    )
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

    # Update plot type based on selected variables.
    observe({
      req(input$x)
      x_type <- var_types()[input$x]
      y_type <- .check_truthiness(var_types()[input$y])
      allowed_plot_types <- .get_allowed_plot_types(x_type, y_type = y_type)
      if (length(allowed_plot_types) == 0) allowed_plot_types <- ""
      updateSelectInput(session, "plot_type", choices = allowed_plot_types)
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
        hexbin = input$plot_type == HEXBIN,
        abline = input$abline,
        smooth_line = input$smooth_line,
        lm = input$lm,
        yintercept = input$yintercept,
        violin = input$plot_type == VIOLIN,
        nbins_histogram = input$nbins_histogram,
        nbins_hexbin = input$nbins_hexbin,
        density = input$plot_type == DENSITY,
        hide_legend = input$hide_legend,
        proportion = input$proportion
      )
    })

    # For showing or hiding certain options.
    output$show_options_1d <- reactive({!isTruthy(input$y)})
    outputOptions(output, "show_options_1d", suspendWhenHidden = FALSE)

    output$show_options_1d_quant <- reactive({plot_type() %in% c(HISTOGRAM, DENSITY)})
    outputOptions(output, "show_options_1d_quant", suspendWhenHidden = FALSE)

    output$show_options_2d <- reactive({isTruthy(input$y)})
    outputOptions(output, "show_options_2d", suspendWhenHidden = FALSE)

    output$show_options_2d_quant <- reactive({plot_type() %in% c(SCATTERPLOT, HEXBIN)})
    outputOptions(output, "show_options_2d_quant", suspendWhenHidden = FALSE)

    output$show_options_2d_cat <- reactive({plot_type() %in% c(VIOLIN, BOXPLOT)})
    outputOptions(output, "show_options_2d_cat", suspendWhenHidden = FALSE)

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
