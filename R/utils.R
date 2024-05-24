#' @title Clean ID string
#' @description id cleaning (only allowed letters, numbers, underscores)
#' @param str a string to clean
#' @return a clean string
#' @export
#'
clean_id <- function(str) {
  gsub("[^A-Za-z0-9_]+", "_", str)
}
