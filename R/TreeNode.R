NodeTypes <- list(
  root = list(
    icon    = "fa fa-folder",
    a_attr  = list(),
    li_attr = list()
  ),
  chapter = list(
    icon    = "fa fa-folder-o",
    a_attr  = list(),
    li_attr = list()
  ),
  code = list(
    icon    = "fa fa-clone",
    a_attr  = list(),
    li_attr = list()
  )
)

#' @title JS TreeNode S3 class
#'
#' @param text string, the tree label
#' @param code_type string, a valid NodeTypes (see above)
#' @param data list, any required data attributes
#' @param checked logical
#' @param selected logical
#' @param opened logical
#' @param disabled logical
#' @param children empty list (to be filled with other TreeNodes if needed)
#'
#' @return a TreeNode S3 object
#' @export
#'
TreeNode <- function(text, type = "code", data = list(), checked = FALSE, selected = FALSE, opened = TRUE, disabled = FALSE, children = list()) {

  type <- match.arg(type, choices = names(NodeTypes))

  structure(
    .Data = list(
      text     = text,
      type     = type,
      data     = data,
      state    = list(opened   = opened,
                      selected = selected,
                      disabled = disabled,
                      checked  = checked),
      children = children
    ),
    class      = "list"
  )
}
