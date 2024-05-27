#' @title Clean ID string
#' @description id cleaning (only allowed letters, numbers, underscores)
#' @param str a string to clean
#' @param check a logical, whether to use as a checking function
#' @return a clean string
#' @export
#'
clean_id <- function(str, check = FALSE, to_lower = TRUE) {
  if (check) stopifnot("str must only be characters, numbers and underscores" = !grepl("[^A-Za-z0-9_]+", str))
  str <- gsub("[^A-Za-z0-9_]+", "_", str)
  if (to_lower) {
    return(tolower(str))
  } else {
    return(str)
  }
}


#' @ Extract code from tree label
#' @param label string label
#' @return a string, the code
#' @export
#'
code_from_label <- function(label) {
  code <- sub("^(.*) \\| .*", "\\1", label)
  clean_id(code, check = FALSE, to_lower = FALSE)
}
