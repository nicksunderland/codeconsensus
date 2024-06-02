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
  # user information
  user <- reactiveValues(username = NULL, is_rater = NULL)

  # lists of the different types of ui
  rv <- reactiveValues(home_ui       = list(),
                       diagnosis_uis = list(),
                       procedure_uis = list(),
                       derived_uis   = list())

  # --------------------------
  # Enter (view only) button
  # --------------------------
  observeEvent(input$enter_btn, {

    # (remake) main uis
    make_uis(input$project)

    # hide login and show main
    shinyjs::hide("login_screen")
    shinyjs::show("main_content")

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

      # (remake) main uis
      make_uis(input$project)

      # hide login and show main
      shinyjs::hide("login_screen")
      shinyjs::show("main_content")

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
  # Make UIs function
  # --------------------------
  make_uis <- function(project) {

    # delete old uis if present
    if (any(sapply(reactiveValuesToList(rv), length) > 0)) {
      print("cleaning up / deleting old modules")

      # for each type of module
      for (v in names(rv)) {

        # for each concept module
        for (id in names(rv[[v]])) {

          # remove the UI
          removeUI(selector = paste0("#", id))

          # remove the inputs
          invisible(lapply(grep(id, names(input), value = TRUE), function(i) { .subset2(input, "impl")$.values$remove(i) }))

          # remove the observers, names still exist so must clear the list after this nested loop
          lapply(session$userData$observer_store, function(i) { i$destroy() })

        }

        # empty the module type list
        rv[[v]] <- list()

      }

      # clear the observer store list
      session$userData$observer_store <- NULL

    }

    # get the concepts for this project
    project_config <- system.file("projects", paste0(project, ".yaml"),  package = "hfphenotyping")
    config         <- yaml::read_yaml(project_config)
    concept_dir    <- system.file("concepts", package = "hfphenotyping")
    concepts_files <- lapply(config$concepts, function(x) file.path(concept_dir, x))
    concepts       <- lapply(concepts_files, function(x) yaml::read_yaml(x))

    # create the home ui
    rv$home_ui <- c(rv$home_ui, list(home = mod_home_ui("home")))
    mod_home_server("home", concept_names = sapply(concepts, function(x) x$name))

    for (x in concepts) {

      # create the diagnosis and procedure concepts
      m <- mod_concept_ui(id                  = clean_id(x[["id"]], check = TRUE),
                          title               = x[["name"]],
                          definition          = x[["definition"]],
                          pmid                = x[["pmid"]],
                          domain              = x[["domain"]],
                          terminology         = x[["terminology"]],
                          concept_term        = x[["concept_term"]],
                          regexes             = x[["regexes"]])
      setNames(m, clean_id(x$id, check = TRUE))

      mod_concept_server(id                   = clean_id(x$id, check = TRUE),
                         concept_name         = x[["name"]],
                         include              = x[["include"]],
                         exclude              = x[["exclude"]],
                         user                 = user)

      if (x$domain == "Procedure") {

        rv$procedure_uis <- c(rv$procedure_uis, list(m))

      } else if (x$domain == "Derived") {

        rv$derived_uis <- c(rv$derived_uis, list(m))

      } else {

        rv$diagnosis_uis <- c(rv$diagnosis_uis, list(m))

      }

    } # end loop concept configs

  } # end make_uis


  # --------------------------
  # Render UI function
  # --------------------------
  output$main_ui <- renderUI({

    req(rv$home_ui, rv$diagnosis_uis, rv$derived_uis)

    # unpack the list into the menu panel
    menu <- navlistPanel(!!!unname(rv$home_ui),
                         "Diseases / syndromes",
                         !!!unname(rv$diagnosis_uis),
                         "Procedures",
                         !!!unname(rv$procedure_uis),
                         "Derived",
                         !!!unname(rv$derived_uis),
                         widths = c(3, 9))

    # return the menu panel
    return(menu)
  })


}
