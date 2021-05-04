library(shiny)
library(Biobase)
library(dplyr)
library(ggplot2)
library(shinyWidgets)

source("helpers.R")

# Set a global theme for all plots.
theme_set(
  theme(legend.position = "top")
)

ui <- fluidPage(
  sidebarPanel(
    h2("Load data"),
    materialSwitch("use_example_data", label = "Load example data?", value = TRUE, status = "primary"),
    conditionalPanel(
      condition = "input.use_example_data == false",
      fileInput("null_model_file", label = "null model file", accept = ".RData"),
      fileInput("phenotype_file", label = "phenotype file", accept = ".RData")
    )
  ),
  sidebarPanel(
    h2("Plot setup"),
    selectInput("x", label = "x axis", choices = "None"),
    selectInput("y", label = "y axis", choices = c("None")),
    selectInput("group", label = "group by", choices = c("None"))
  ),
  mainPanel(
    h2("data plot"),
    plotOutput("plot")
  ),
  mainPanel(
    h2("data preview"),
    tableOutput("data")
  )
)

server <- function(input, output, session) {

  # Load the data when the user selects a null model and phenotype file.
  data_reactive <- reactive({

    null_model_file <- input$null_model_file
    phenotype_file <- input$phenotype_file

    if (input$use_example_data) {
      print("using example data")
      .load_data(
        null_model_file = "testdata/null_model.RData",
        phenotype_file = "testdata/1KG_phase3_subset_annot.RData"
      )
    } else if (!is.null(null_model_file) & !is.null(phenotype_file)) {
      .load_data(null_model_file$datapath, phenotype_file$datapath)
    } else {
      return(NULL)
    }
  })

  # Update dropdown choices when data changes?
  observe({
    data_names <- setdiff(names(data_reactive()), "sample.id")
    print(head(data_names))
    updateSelectInput(session, "x", choices = data_names)
    updateSelectInput(session, "y", choices = c("None", data_names))
    updateSelectInput(session, "group", choices = c("None", data_names))
  })

  output$plot <- renderPlot({

    req(data_reactive())

    # Obtain the data for plotting.
    dat <- data_reactive()
    type_x <- .detect_variable_type(dat[[input$x]])

    group <- if (input$group == "None") NULL else as.name(input$group)
    if (input$y == "None") {
      # 1-d plot.
      p <- ggplot(dat, aes_string(x = as.name(input$x)))
      if (type_x == QUANTITATIVE) {
        p <- p + geom_histogram(aes_string(fill = group))
      } else if (type_x == CATEGORICAL) {
        p <- p + geom_bar(aes_string(fill = group))
      }

    } else {
      # 2-d plot.
      type_y <- .detect_variable_type(dat[[input$y]])
      print(type_y)
      p <- ggplot(dat, aes_string(x = as.name(input$x), y = as.name(input$y)))
      if (type_x == QUANTITATIVE & type_y == QUANTITATIVE) {
        # Show a scatterplot.
        print("scatterplot")
        p <- p + geom_point(aes_string(color = group))
      } else if (type_x == QUANTITATIVE & type_y == CATEGORICAL) {
        # Show a flipped boxplot.
        # We have to recreate p because we need to use coord_flip.
        p <- ggplot(dat, aes_string(y = as.name(input$x), x = as.name(input$y))) +
          geom_boxplot(aes_string(fill = group)) +
          coord_flip()
      } else if (type_x == CATEGORICAL & type_y == QUANTITATIVE) {
        # Show a boxplot.
        print("boxplot")
        p <- p + geom_boxplot(aes_string(fill = group))
      } else if (type_y == CATEGORICAL & type_y == CATEGORICAL) {
        # Maybe we don't want to allow this?
        stop("Cannot plot two categorical variables against each other.")
      } else {
        stop("Unknown variable type.")
      }
    }
    p
  })

  # Save the head of the data file to the outputs to display as a table.
  output$data <- renderTable({
    req(data_reactive())
    data_reactive() %>% head()
  })

}

shinyApp(ui, server)
