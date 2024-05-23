# the database driver
#https://download.oracle.com/otn-pub/otn_software/jdbc/1923/ojdbc8.jar
#https://www.java.com/en/download/

#' @title database connection and query function
#' @param query_str string, valid SQL (or name of dt if 'write' option)
#' @param type string, either get or send
#' @param dtname name of data.table name
#' @param dtvalue data.table
#' @return a data.table
#' @export
#'
query_db <- function(query_str = NULL, type = "get", ...) {

  type <- match.arg(type, choices = c("get", "send", "write"))

  # driver and local database config file (connection details)
  java_file <- system.file("database", "ojdbc8.jar", package = "hfphenotyping")
  db_drv    <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = java_file)
  config    <- config::get(file = system.file("database", "db_config.yaml", package = "hfphenotyping"))

  # makeshift injection protection (not going to take external input anyway)
  safe_pattern <- "^[a-zA-Z0-9_;\\s\\,\\.\\*\\=\\<\\>\\'\\%\\(\\)]+$"
  if (!is.null(query_str) && !grepl(safe_pattern, query_str, perl = TRUE)) {
    stop("Invalid characters in query string")
  }

  # make the connection
  con <- DBI::dbConnect(db_drv, paste0("jdbc:oracle:thin:@", config[["connection_str"]]), config[["username"]], config[["password"]])

  # get the results
  if (type == "get") {

    results <- DBI::dbGetQuery(con, query_str) |> data.table::as.data.table()

  } else if (type == "send") {

    results <- DBI::dbSendStatement(con, query_str)

  } else if (type == "write") {

    results <- DBI::dbWriteTable(con, ...)

  }

  # close the connection
  DBI::dbDisconnect(con)

  # return
  return(results)
}

## To be copied in the UI
# mod_database_ui("database_1")

## To be copied in the server
# mod_database_server("database_1")
