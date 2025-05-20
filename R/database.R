# the database driver
#https://download.oracle.com/otn-pub/otn_software/jdbc/1923/ojdbc8.jar
#https://www.java.com/en/download/

# global environment to hold the connection
# https://r-pkgs.org/data.html#sec-data-state
con_env <- new.env(parent = emptyenv())

#' @title make_connection
#' @return a connection object
#' @importFrom DBI dbConnect
#' @importFrom RMySQL MySQL
#' @export
#'
make_connection <- function() {

  config <- config::get(file = system.file("database", "db_config.yaml", package = "codeconsensus"))

  con_env$con <- DBI::dbConnect(
    drv      = RPostgres::Postgres(),
    dbname   = config[["dbname"]],
    user     = config[["user"]],
    password = config[["password"]],
    host     = config[["host"]],
    port     = config[["port"]])

  return(con_env$con)
}


#' @title database connection and query function
#' @param query_str string, valid SQL (or name of dt if 'write' option)
#' @param type string, either get or send
#' @param table database table name (for read and write options)
#' @param value values (for update and write options)
#' @param ... optionally can pass the con object f not declared globally
#' @return a data.table
#' @importFrom DBI dbConnect dbGetQuery dbReadTable dbExecute dbSendQuery dbWriteTable
#' @export
#'
query_db <- function(query_str = NULL, type = "get", table = NULL, value = NULL, ...) {

  type <- match.arg(type, choices = c("get", "read", "update", "write", "send", "execute"))

  # if con is not global, needs to be passed in ...
  args <- list(...)
  if ("con" %in% names(args)) {
    con <- args$con
  } else {
    con <- con_env$con
  }

  # get the results
  if (type == "get") {

    if (is.null(value)) {
      results <- RPostgres::dbGetQuery(con, query_str)
    } else {
      results <- do.call(RPostgres::dbGetQuery, c(list(con, query_str), !!!unname(value)))
    }
    results <- data.table::as.data.table(results)

  } else if (type == "read") {

    results <- RPostgres::dbReadTable(con, name = table)
    results <- data.table::as.data.table(results)

  } else if (type == "update") {

    RPostgres::dbExecute(con, query_str, unname(value))

  } else if (type == "write") {

    results <- RPostgres::dbWriteTable(con, name = table, value = value)

  } else if (type == "send") {

    results <- RPostgres::dbSendQuery(con, query_str)

  } else if (type == "execute") {

    if (is.null(value)) {
      results <- RPostgres::dbExecute(con, query_str)
    } else {
      results <- RPostgres::dbExecute(con, query_str, unname(value))
    }

  }

  # return
  return(results)
}
