#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinymanager
#' @import data.table
#' @importFrom config get
#' @importFrom yaml read_yaml
#' @importFrom RJDBC JDBC

#' @noRd
app_server <- function(input, output, session) {

  # --------------------------
  # Reactive values
  # --------------------------
  user <- reactiveValues(username = NULL, is_rater = NULL)


  # --------------------------
  # Enter (view only) button
  # --------------------------
  observeEvent(input$enter_btn, {

    render_main_ui(input$project)

  })


  # --------------------------
  # Login button
  # --------------------------
  observeEvent(input$login_btn, {
    req(input$username, input$password)

    # authentication logic
    db_user_data <- query_db("SELECT USERNAME, IS_RATER FROM USERS WHERE USERNAME = ? AND PASSWORD = ?", type = "get", value = list(input$username, input$password))

    # enter main ui if validated
    if (nrow(db_user_data > 0)) {

      # store username
      user[["username"]] <- db_user_data$USERNAME
      user[["is_rater"]] <- as.logical(db_user_data$IS_RATER)

      # render main ui
      render_main_ui(input$project)

    } else {

      showNotification("Invalid login details...", type = "error", duration = NULL)

    }

  })


  # --------------------------
  # Return to login button
  # --------------------------
  observeEvent(input$login_page, {

    # remove user info
    user[["username"]] <- NULL
    user[["is_rater"]] <- NULL

    # hide login and show main
    shinyjs::hide("main_content")
    shinyjs::show("login_screen")

  })


  # print out user
  output$user_info <- renderUI({
    div(paste0("User: ", user[["username"]]))
  })

  # --------------------------
  # Render UI function
  # --------------------------
  render_main_ui <- function(project) {

    # render
    output$main_ui <- renderUI({

      # get the concepts for this project
      project_config <- system.file("projects", paste0(project, ".yaml"),  package = "hfphenotyping")
      config         <- yaml::read_yaml(project_config)
      concept_dir    <- system.file("concepts", package = "hfphenotyping")
      concepts_files <- lapply(config$concepts, function(x) file.path(concept_dir, x))
      concepts       <- lapply(concepts_files, function(x) yaml::read_yaml(x))

      # create the home ui
      home <- mod_home_ui("home")
      mod_home_server("home", concept_names = sapply(concepts, function(x) x$name))

      # create the list of diagnosis concepts uis
      diagnosis_ui_list <- list()
      procedure_ui_list <- list()
      derived_ui_list   <- list()

      for (x in concepts) {

        # create the diagnosis and procedure concepts
        if (x$domain != "Derived") {

          m <- mod_concept_ui(id                  = clean_id(x$id, check = TRUE),
                              title               = x$name,
                              definition          = x$definition,
                              pmid                = x$pmid,
                              domain              = x$domain,
                              terminology         = x$terminology,
                              concept_term        = x$concept_term,
                              regexes             = x$regexes)

          mod_concept_server(id                   = clean_id(x$id, check = TRUE),
                             concept_name         = x$name,
                             regexes              = x$regexes,
                             user                 = user)

          if (x$domain == "Procedure") {

            procedure_ui_list <- c(procedure_ui_list, list(m))

          } else {

            diagnosis_ui_list <- c(diagnosis_ui_list, list(m))

          }

          # create the derived phenotypes uis
        } else {

          m <- mod_derived_ui("derived")

          mod_derived_server("derived")

          derived_ui_list <- c(derived_ui_list, list(m))

        }

      }

      # unpack the list into the menu panel
      menu <- navlistPanel(home,
                           "Diseases / syndromes",
                           !!!diagnosis_ui_list,
                           "Procedures",
                           !!!procedure_ui_list,
                           "Derived",
                           !!!derived_ui_list,
                           widths = c(3, 9))

      # return the menu panel
      return(menu)
    })

    # hide login and show main
    shinyjs::hide("login_screen")
    shinyjs::show("main_content")
  }

}
