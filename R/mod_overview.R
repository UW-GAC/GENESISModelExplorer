#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(10, offset = 1,
          h1("Welcome to GENESIS Model Explorer!"),
          p(
            "The GENESIS Model Explorer App is a Shiny application developed by the Genetic Analysis Center at the University of Washington in collaboration with SevenBridges.",
            "It enables users to visualize and explore the results of the", a("GENESIS Null Model workflow", href="https://platform.sb.biodatacatalyst.nhlbi.nih.gov/public/apps/admin/sbg-public-data/null-model", target="_blank", .noWS="after"), ".",
            "It is meant to provide an intuitive interface for researchers to easily select, visualize, and explore phenotype variables, genotypes, and GENESIS model results interactively with no prior R programming knowledge.",
            "Researchers can use the app to explore their own data or use example data provided with the app."
          ),
          p(
            "The GENESIS Model Explorer app is in beta version. If you are having any difficulty using the app please email",
            a("support@sevenbridges.com", href="mailto:support@sevenbridges.com", .noWS="after"),
            "."
          ),
          p(
            "To use the app with your own data, you will need access to:"
          ),
          tags$ol(
            tags$li(HTML("The <b>GENESIS null model file</b> in RData format.")),
            tags$li(HTML("A <b>phenotype file</b> in RData format. This can be the same file you used to fit the null model, or a new file where you have added additional columns. It must contain all the samples included in your null model file in a column named \"sample.id\", with additional columns containing phenotype variables.")),
            tags$li(HTML("(optional) A <b>genotype file</b> in rds format containing variants of interest. It must contain a column named \"sample.id\" with all the samples in your null model file, with additional columns containing variant dosages. This file can be generated from an existing GDS file with the"), a("GDS Genotype Extractor", href = "https://platform.sb.biodatacatalyst.nhlbi.nih.gov/u/smgogarten/uw-gac-commit/apps/#smgogarten/uw-gac-commit/gds-genotype-extractor"), HTML(" app."))
          ),
          p("The app contains three different tabs:"),
          tags$ol(
            tags$li(HTML("<b>Overview</b>: Read basic information about the app (this tab).")),
            tags$li(HTML("<b>Load data</b>: Load your own data or indicate that you will use example data.")),
            tags$li(HTML("<b>Plot</b>: Plot your data. Note that data must be loaded in the <b>Load data</b> tab before using this tab."))
          )
        ),
      ),
      fluidRow(
        column(10, align="center", offset = 1,
          actionButton(ns("button"), "Get started!", class = "btn-primary")
        )
      ),
      hr(),
      fluidRow(
        column(10, offset = 1,
          p(
            "
            The GENESIS Model Explorer app was developed at the University of Washington", a("Genetic Analysis Center", href="https://www.biostat.washington.edu/research/centers/gac", target="_blank"), "in collaboration with the NHLBI Biodata Catalyst powered by Seven Bridges team and uses the GENESIS R package (", a("Gogarten et al. 2019", href="https://pubmed.ncbi.nlm.nih.gov/31329242/", target="_blank", .noWS="outside"), ").
            The source code is available in a", a("GitHub repository", href="https://github.com/UW-GAC/shinyNullModel", target="_blank", .noWS="after"), ".
            "
          )
        )
      )
    )
  )
}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id, parent_session = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$button, {
      # Switch to the data loader tab.
      if (!is.null(parent_session)) {
        updateNavbarPage(parent_session, "navbar", selected="Load data")
      }
    })
  })
}
