# the database driver
#https://download.oracle.com/otn-pub/otn_software/jdbc/1923/ojdbc8.jar
#https://www.java.com/en/download/

# -----------------------
# GLOBAL CONNECTION POOL
# -----------------------
config <- config::get(file = system.file("database", "db_config.yaml", package = "hfphenotyping"))

con <- pool::dbPool(
  drv      = RJDBC::JDBC(driverClass = "oracle.jdbc.OracleDriver", classPath = system.file("database", "ojdbc8.jar", package = "hfphenotyping")),
  url      = paste0("jdbc:oracle:thin:@", config[["connection_str"]]),
  user     = config[["username"]],
  password = config[["password"]]
)

onStop(function() {
  pool::poolClose(con)
})


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

  # get the results
  if (type == "get") {

    if (is.null(value)) {
      results <- RJDBC::dbGetQuery(con, query_str)
    } else {
      results <- do.call(RJDBC::dbGetQuery, c(list(con, query_str), unname(value)))
    }
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

  # return
  return(results)
}
