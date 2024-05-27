# the database driver
#https://download.oracle.com/otn-pub/otn_software/jdbc/1923/ojdbc8.jar
#https://www.java.com/en/download/

#' @title database connection and query function
#' @param query_str string, valid SQL (or name of dt if 'write' option)
#' @param type string, either get or send
#' @param table database table name (for read and write options)
#' @param value values (for update and write options)
#' @return a data.table
#' @import RJDBC
#' @export
#'
query_db <- function(query_str = NULL, type = "get", table = NULL, value = NULL) {

  type <- match.arg(type, choices = c("get", "read", "update", "write", "send"))

  # driver and local database config file (connection details)
  java_file <- system.file("database", "ojdbc8.jar", package = "hfphenotyping")
  db_drv    <- RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = java_file)
  config    <- config::get(file = system.file("database", "db_config.yaml", package = "hfphenotyping"))

  # make the connection
  con <- RJDBC::dbConnect(db_drv, paste0("jdbc:oracle:thin:@", config[["connection_str"]]), config[["username"]], config[["password"]])

  # get the results
  if (type == "get") {

    results <- RJDBC::dbGetQuery(con, query_str)
    results <- data.table::as.data.table(results)

  } else if (type == "read") {

    results <- RJDBC::dbReadTable(con, name = table)
    results <- data.table::as.data.table(results)

  } else if (type == "update") {

    results <- do.call(RJDBC::dbSendUpdate, c(list(con, query_str), unname(value)))

  } else if (type == "write") {

    results <- RJDBC::dbWriteTable(con, name = table, value = value)

  } else if (type == "send") {

    results <- RJDBC::dbSendQuery(con, query_str)

  }

  # close the connection
  RJDBC::dbDisconnect(con)

  # return
  return(results)
}
