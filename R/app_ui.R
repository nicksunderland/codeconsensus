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
    fluidPage(
      titlePanel("Heart failure phenotyping"),
      h5("nicholas.sunderland@bristol.ac.uk"),
      navlistPanel(
        "Phenotype",
        tabPanel("Heart failure syndrome",  DT::dataTableOutput("checkbox_table_1")),
        tabPanel("Cardiomyopathy", DT::dataTableOutput("checkbox_table_2")),
        tabPanel("DCM",  DT::dataTableOutput("checkbox_table_3"))
      ),
      plotOutput("plot")
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "hfphenotyping"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
