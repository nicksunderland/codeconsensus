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
#' @importFrom DBI dbConnect dbDisconnect

#' @noRd
app_server <- function(input, output, session) {

  # the login details
  user_info        <- query_db("SELECT * FROM USERS")
  names(user_info) <- c("uid", "user", "password")
  creds            <- shinymanager::check_credentials(user_info)
  res_auth         <- shinymanager::secure_server(check_credentials = creds)
  # res_auth = list(user = "test")

  # get the concepts
  concept_dir    <- system.file("concepts", package = "hfphenotyping")
  concepts_files <- list.files(concept_dir, full.names = TRUE)
  concepts_files <- concepts_files[!concepts_files %in% list.dirs(concept_dir)]
  concepts       <- lapply(concepts_files, function(x) yaml::read_yaml(x))

  # id cleaning (only allowed letters, numbers, underscores)
  clean_id <- function(str) {
    gsub("[^A-Za-z0-9_]+", "_", str)
  }

  # create user details
  output$user_info <- renderUI({
    fluidRow(column(12, div(paste0("user: ", res_auth[["user"]]), class = "text-right")))
  })

  # create the concept UI elements
  output$concept_menu <- renderUI({

    concept_iu_list <- lapply(concepts, function(x) {

      mod_concept_ui(id                  = clean_id(x$name),
                     title               = x$name,
                     definition          = x$definition,
                     pmid                = x$pmid,
                     domain              = x$domain,
                     terminology         = x$terminology,
                     concept_term        = x$concept_term,
                     valueset_definition = x$valueset_definition,
                     regexes             = x$regexes)

    })

    # unpack the list into the menu panel
    menu <- navlistPanel("Concept", !!!concept_iu_list, widths = c(3, 9))

    # return the menu panel
    return(menu)
  })

  # initialise the concept UI element server functions
  lapply(concepts, function(x) mod_concept_server(id = clean_id(x$name), regexes = x$regexes, user = res_auth[["user"]]))

}
