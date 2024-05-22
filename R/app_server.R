# the database driver
drv <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = system.file("database", "ojdbc8.jar", package = "hfphenotyping"))
#https://download.oracle.com/otn-pub/otn_software/jdbc/1923/ojdbc8.jar
#https://www.java.com/en/download/


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DT
#' @importFrom config get
#' @importFrom yaml read_yaml
#' @importFrom RJDBC JDBC
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom shinymanager secure_server
#' @noRd
app_server <- function(input, output, session) {

  # connect to the database
  config <- config::get(file = system.file("database", "db_config.yaml", package = "hfphenotyping"))
  con <- DBI::dbConnect(drv, paste0("jdbc:oracle:thin:@", config[["connection_str"]]), config[["username"]], config[["password"]])

  # the users
  user_table <- DBI::dbGetQuery(con, "SELECT * FROM USERS")
  names(user_table) <- c("uid", "user", "password")

  # disconnect
  DBI::dbDisconnect(con)

  # check login details (user name in res_auth[["user"]])
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(user_table)
  )

  # get the concepts
  config_files <- list.files(system.file("concepts", package = "hfphenotyping"), full.names = TRUE)
  concepts <- lapply(config_files, function(x) yaml::read_yaml(x))

  # create the concept UI elements
  output$concept_menu <- renderUI({

    concept_iu_list <- lapply(concepts, function(x) {

      mod_concept_ui(id                  = x$name,
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
  lapply(concepts, function(x) mod_concept_server(id = x$name))

}
