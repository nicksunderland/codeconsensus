#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
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
  # Create porject radiobuttons
  # --------------------------
  output$project_radiobuttons <- renderUI({
    project_configs <- list.files(system.file("projects", package = "hfphenotyping"), pattern = ".ya?ml$", full.names = TRUE)
    project_names   <- sapply(project_configs, function(x) yaml::read_yaml(paste0(sub(".yaml$", "", x), ".yaml"))$name)
    project_ids     <- lapply(project_configs, function(x) yaml::read_yaml(paste0(sub(".yaml$", "", x), ".yaml"))$id)
    names(project_ids) <- project_names
    radioButtons("project", "Project", inline = TRUE, choices = project_ids)
  })


  # --------------------------
  # Enter (view only) button
  # --------------------------
  observeEvent(input$enter_btn, {

    # (re)make main uis
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

      # (re)make the main uis
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

    # hide main and show login
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

    # delete old uis if present - this happens if switching projects or users via the initial landing page
    if (any(sapply(reactiveValuesToList(rv), length) > 0)) {
      print("cleaning up / deleting old modules")

      # for each type of module
      for (v in names(rv)) {

        module_ids <- names(rv[[v]])

        # remove all the UI elements
        lapply(module_ids, function(id) {
          removeUI(selector = paste0("#", id))
          invisible(lapply(grep(id, names(input), value = TRUE), function(i) {
            .subset2(input, "impl")$.values$remove(i)
          }))
        })

        # empty the module type list
        rv[[v]] <- list()
      }

      # remove all the observers
      if (!is.null(session$userData$observer_store)) {
        lapply(session$userData$observer_store, function(i) { i$destroy() })
        session$userData$observer_store <- NULL
      }

    } # end cleaning up old UIs

    # get the concepts for this project
    project_config <- system.file("projects", paste0(project, ".yaml"),  package = "hfphenotyping")
    config         <- yaml::read_yaml(project_config)
    concept_dir    <- system.file("concepts", package = "hfphenotyping")
    concepts_files <- lapply(config$concepts, function(x) file.path(concept_dir, paste0(x, ".yaml")))
    concepts       <- lapply(concepts_files, function(x) yaml::read_yaml(x))

    # create the home ui
    rv$home_ui <- c(rv$home_ui, list(home = mod_home_ui("home")))
    mod_home_server("home", concepts = sapply(concepts, function(x) x$id))

    # preallocate lists for different UI/concept types
    diagnosis_uis <- list()
    procedure_uis <- list()
    derived_uis   <- list()

    for (x in concepts) {

      # create the concept module
      m <- mod_concept_ui(id                  = x[["id"]],
                          title               = x[["name"]],
                          definition          = x[["definition"]],
                          pmid                = x[["pmid"]],
                          domain              = x[["domain"]],
                          terminology         = x[["terminology"]],
                          concept_term        = x[["concept_term"]],
                          regexes             = x[["regexes"]])
      setNames(m, clean_id(x$id, check = TRUE))

      mod_concept_server(id                   = x[["id"]],
                         include              = x[["include"]],
                         exclude              = x[["exclude"]],
                         user                 = user,
                         derived              = x[["domain"]] == "Derived")

      # categorize UIs based on type/domain
      if (x$domain == "Procedure") {
        procedure_uis <- c(procedure_uis, list(m))
      } else if (x$domain == "Derived") {
        derived_uis <- c(derived_uis, list(m))
      } else {
        diagnosis_uis <- c(diagnosis_uis, list(m))
      }

    } # end loop concept configs

    # update reactive values outside the loop
    rv$procedure_uis <- procedure_uis
    rv$derived_uis   <- derived_uis
    rv$diagnosis_uis <- diagnosis_uis

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

    # return the navlistPanel
    return(menu)
  })


}
