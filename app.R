library(shiny)
library(Biobase)
library(dplyr)
library(ggplot2)

source("helpers.R")

# Set a global theme for all plots.
theme_set(
  theme(legend.position = "top")
)

ui <- fluidPage(
  fileInput("null_model_file", label = "null model file", accept = ".RData"),
  fileInput("phenotype_file", label = "phenotype file", accept = ".RData"),
  selectInput("x", label = "x axis", choices = "None"),
  selectInput("y", label = "y axis", choices = c("None")),
  selectInput("group", label = "group by", choices = c("None")),
  tableOutput("data"),
  plotOutput("plot")
)

server <- function(input, output, session) {

  # Load the data when the user selects a null model and phenotype file.
  data_reactive <- reactive({
    null_model_file <- input$null_model_file
    phenotype_file <- input$phenotype_file

    if (!is.null(null_model_file) & !is.null(phenotype_file)) {
      .load_data(null_model_file$datapath, phenotype_file$datapath)
    } else {
      return(NULL)
    }
  })

  # Save the head of the data file to the outputs to display as a table.
  output$data <- renderTable({data_reactive() %>% head()})

  # Update dropdown choices when data changes?
  observe({
    data_names <- setdiff(names(data_reactive()), "sample.id")
    print(head(data_names))
    updateSelectInput(session, "x", choices = data_names)
    updateSelectInput(session, "y", choices = c("None", data_names))
    updateSelectInput(session, "group", choices = c("None", data_names))
  })

  output$plot <- renderPlot({

    dat <- data_reactive()
    type_x <- .detect_variable_type(dat[[input$x]])
    print(type_x)

    group <- if (input$group == "None") NULL else as.name(input$group)
    print(group)
    if (input$y == "None") {
      # 1-d plot.
      print("1d plot")
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

}

shinyApp(ui, server)
