library(shiny)
library(Biobase)
library(dplyr)
library(ggplot2)

# Hard code data loading statements here for now.
nullmod <- get(load("testdata/null_model.RData"))
phen <- get(load("testdata/1KG_phase3_subset_annot.RData")) %>% pData()
dat <- nullmod$fit %>%
  inner_join(phen, by = "sample.id", suffix = c(".model", ".phen"))

ui <- fluidPage(
  selectInput("y", label = "y axis", choices = names(dat)),
  selectInput("x", label = "x axis", choices = names(dat)),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(dat, aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
}

shinyApp(ui, server)
