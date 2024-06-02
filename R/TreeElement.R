#' @title TreeElement S3 class
#'
#' @param code string, the code e.g. I510
#' @param code_type string, the code tyoe e.g. "ICD10"
#' @param description string, the code's description
#' @param stselected logical
#' @param stopened logical
#' @param children empty list for recursive use (will potentially be filled with other TreeElements)
#'
#' @return a TreeElement S3 object
#' @export
#'
TreeElement <- function(code, code_type, description, sticon = NULL, stselected = FALSE, stopened = TRUE, stdisabled = FALSE, children = list()) {
  s <- structure(
    .Data      = children,
    class      = "TreeElement",
    code       = code,
    code_type  = code_type,
    description= description,
    stselected = stselected,
    stopened   = stopened,
    sticon     = sticon
  )
}
