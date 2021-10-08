#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      "GENESIS Model Explorer",
      id = 'navbar',
      tabPanel(
        "Overview",
        id = "overview",
        mod_overview_ui("overview_ui_1")
      ),
      tabPanel(
        "Load data",
        id = "load",
        mod_data_loader_ui("data_loader_ui_1")
      ),
      tabPanel(
        "Plot",
        id = "plot",
        mod_plot_ui("plot_ui_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'GENESISModelExplorer'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
