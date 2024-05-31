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

  # the login details
  # user_info        <- query_db("SELECT * FROM USERS")
  # names(user_info) <- c("uid", "user", "password")
  # creds            <- shinymanager::check_credentials(user_info)
  # res_auth         <- shinymanager::secure_server(check_credentials = creds)
  res_auth = list(user = "default")

  # create user details
  output$user_info <- renderUI({
    fluidRow(column(12, div(paste0("user: ", res_auth[["user"]]), class = "text-right")))
  })

  # get the concepts
  concept_dir    <- system.file("concepts", package = "hfphenotyping")
  concepts_files <- list.files(concept_dir, pattern = ".*\\.(yaml|yml)$", full.names = TRUE)
  concepts_files <- concepts_files[!concepts_files %in% list.dirs(concept_dir)]
  concepts       <- lapply(concepts_files, function(x) yaml::read_yaml(x))

  # create the concept UI elements
  output$menu <- renderUI({

    # create the home ui
    home <- mod_home_ui("home")

    # create the list of diagnosis concepts uis
    diagnosis_ui_list <- list()
    procedure_ui_list <- list()

    for (x in concepts) {

      m <- mod_concept_ui(id                  = clean_id(x$id, check = TRUE),
                          title               = x$name,
                          definition          = x$definition,
                          pmid                = x$pmid,
                          domain              = x$domain,
                          terminology         = x$terminology,
                          concept_term        = x$concept_term,
                          regexes             = x$regexes)

      if (x$domain != "Procedure") {

        diagnosis_ui_list <- c(diagnosis_ui_list, list(m))

      } else {

        procedure_ui_list <- c(procedure_ui_list, list(m))

      }

    }

    # create the derived phenotypes uis
    derived <- mod_derived_ui("derived")

    # unpack the list into the menu panel
    menu <- navlistPanel(home,
                         "Diseases / syndromes",
                         !!!diagnosis_ui_list,
                         "Procedures",
                         !!!procedure_ui_list,
                         "Derived",
                         derived,
                         widths = c(3, 9))

    # return the menu panel
    return(menu)
  })

  # initialise the home UI element server
  mod_home_server("home", username = res_auth[["user"]])

  # initialise the concept UI element server functions
  lapply(concepts, function(x) mod_concept_server(id               = clean_id(x$id, check = TRUE),
                                                  concept_name     = x$name,
                                                  regexes          = x$regexes,
                                                  username         = res_auth[["user"]]))

  # initialise the derived phenotype UI servers
  mod_derived_server("derived")

}
