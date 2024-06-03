#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyjs
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Initialize shinyjs
    shinyjs::useShinyjs(),
    # Your application UI logic
    fluidPage(
      title = "CodeConsensus",
      # top bar
      titlePanel("CodeConsensus"),
      fluidRow(column(6, h6("bug reports: nicholas.sunderland@bristol.ac.uk")),
               column(5, uiOutput("user_info", class = "text-right")),
               column(1, actionLink("login_page", "Login"))),
      hr(),
      # login screen
      div(id = "login_screen",
          fluidRow(
            column(12, offset = 0, align = "center",
                   tags$style(HTML("
                      /* Custom CSS styles for radio buttons */
                      #project {
                        border: 1px solid #ccc;
                        border-radius: 10px;
                        padding: 10px;
                        background-color: #f5f5f5;
                        text-align: center;
                        margin-bottom: 10px;
                      }
                    ")),
                   radioButtons("project", "Project",
                                inline = TRUE,
                                choices = list("ESC computable guideline" = "esc_guideline",
                                               "NIH cardiomyopathy" = "nih_cardiomyopathy",
                                               "Cardiovascular initiative" = "cv_initiative")),
                   hr(),
                   actionButton("enter_btn", "Enter (view only)"),
                   hr(),
                   p("or"),
                   hr(),
                   textInput("username", NULL, width = "25%", placeholder = "username"),
                   passwordInput("password", NULL, width = "25%", placeholder = "password"),
                   actionButton("login_btn", "Login")
            )
          )
      ),
      # placeholder for dynamic UI
      div(id = "main_content", uiOutput("main_ui"), style = "display:none;")
    )
  )
}

# app_ui <- shinymanager::secure_app(app_ui, theme = "cerulean")



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
