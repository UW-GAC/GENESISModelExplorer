library(shiny)
library(Biobase)
library(dplyr)
library(ggplot2)

source("helpers.R")

ui <- fluidPage(
  selectInput("x", label = "x axis", choices = names(dat)),
  selectInput("y", label = "y axis", choices = names(dat)),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    print(input$x)
    print(input$y)
    ggplot(dat, aes_string(x = as.name(input$x), y = as.name(input$y))) +
      geom_point()
  })
}

shinyApp(ui, server)
