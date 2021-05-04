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
  selectInput("x", label = "x axis", choices = names(dat)),
  selectInput("y", label = "y axis", choices = c("None", names(dat))),
  selectInput("group", label = "group by", choices = c("None", names(dat)[sapply(dat, .detect_variable_type) == CATEGORICAL])),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({

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
        stop("unknown")
      }
    }
    p
  })
}

shinyApp(ui, server)
