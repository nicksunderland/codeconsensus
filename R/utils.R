#' @title Clean ID string
#' @description id cleaning (only allowed letters, numbers, underscores)
#' @param str a string to clean
#' @param check a logical, whether to use as a checking function
#' @param to_lower logical, whether to convert to lowercase
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


#' @title Get tree attributes
#' @param tree a js tree object
#' @param attribute_name str, the attribute name (label, code, code_type, description, stselected, stopened)
#' @param result for recursive use
#' @return a list of name = attribute
#' @export
#'
tree_attributes <- function(tree, attribute_name, result = list()) {

  if ("x" %in% names(tree)) {
    tree <- tree$x$data
  }

  for (i in seq_along(tree)) {

    if (attribute_name %in% names(tree[[i]]$data)) {

      if (is.null(tree[[i]]$data[[attribute_name]])) {
        result[[tree[[i]]$text]] <- NA
      } else {
        result[[tree[[i]]$text]] <- tree[[i]]$data[[attribute_name]]
      }

    } else if (attribute_name %in% names(tree[[i]]$state)) {

      if (is.null(tree[[i]]$state[[attribute_name]]) && attribute_name %in% c("selected", "disabled")) {
        attribute_value <- FALSE
      } else {
        attribute_value <- tree[[i]]$state[[attribute_name]]
      }

      if (is.null(attribute_value)) {
        result[[tree[[i]]$text]] <- NA
      } else {
        result[[tree[[i]]$text]] <- attribute_value
      }

    }

    result <- tree_attributes(tree[[i]]$children, attribute_name, result)
  }

  return(result)
}



