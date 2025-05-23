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

    } else if (attribute_name %in% names(tree[[i]])) {

      if (is.null(tree[[i]][[attribute_name]])) {
        result[[tree[[i]]$text]] <- NA
      } else {
        result[[tree[[i]]$text]] <- tree[[i]][[attribute_name]]
      }

    }

    result <- tree_attributes(tree[[i]]$children, attribute_name, result)
  }

  return(result)
}



parse_config_files <- function(file) {

  if (file.exists(file)) {
    config_paths <- file
  } else if (dir.exists(file)) {
    config_paths <- list.files(file, pattern = ".ya?ml$", full.names = TRUE, recursive = TRUE)
  } else if (file %in% list.files(system.file("concepts", package = "codeconsensus"))) {
    config_dir   <- system.file("concepts", file, package = "codeconsensus")
    config_paths <- list.files(config_dir, pattern = ".ya?ml$", full.names = TRUE)
  } else {
    stop("incorrect input")
  }

  # get the configs
  concept_ids  <- sapply(config_paths, function(x) yaml::read_yaml(x)$id)

  # set the structure of the config
  conf_struc <- structure(
    .Data = list(
      id           = NULL,
      name         = NULL,
      definition   = NULL,
      reference    = NULL,
      domain       = NULL,
      terminology  = NULL,
      perferred_term = list(),
      regexes      = list(),
      include      = NULL,
      exclude      = NULL
    ),
    class = "ConfigStructure"
  )

  # populate the structure and check, for each config
  for (path in config_paths) {

    yml    <- yaml::read_yaml(path)
    config <- utils::modifyList(conf_struc, yml)

    # print
    cat(basename(path), "\n")

    # check each element
    stopifnot("id must be a character" = !is.null(config$id) && is.character(config$id))
    stopifnot("name must be a character" = !is.null(config$name) && is.character(config$name))
    stopifnot("definition must be a character" = !is.null(config$definition) && is.character(config$definition))
    stopifnot("reference must be a valid URL" = !is.null(config$reference) && grepl("^https?://\\S+$", config$reference))
    stopifnot("domain must be in c('Measure', 'Disorder', 'Procedure', 'Derived')" = !is.null(config$domain) && config$domain %in% c('Observable entity', 'Disorder', 'Procedure', 'Derived'))
    stopifnot("terminology must one or more of c('SNOMED', 'SNOMED_procedure', 'ICD10', 'ICD11', 'OPCS4', 'CPT4', 'ICD9', 'ICD9_procedure')" = !any(is.na(config$terminology)) && all(config$terminology %in% c('SNOMED', 'SNOMED_procedure', 'ICD10', 'ICD11', 'CPT4', 'OPCS4', 'ICD9', 'ICD9_procedure')))
    for (term in c("all", config$terminology)) {
      if (!term %in% names(config$regexes)) config$regexes[[term]] <- list(NULL)
    }
    for (term in names(config$regexes)) {
      if (!term %in% c("all", config$terminology)) config$regexes[[term]] <- NULL
    }
    for (term in config$terminology) {
      if (!term %in% names(config$perferred_term)) config$perferred_term[[term]] <- list(code = NULL, desc = NULL)
    }
    for (term in names(config$perferred_term)) {
      if (!term %in% config$terminology || grepl("ICD9", term)) config$perferred_term[[term]] <- NULL
    }
    stopifnot("perferred_term must be a list with all terminologies present"  = is.list(config$perferred_term) && length(config$perferred_term) > 0 && all(config$terminology[!grepl("ICD9", config$terminology)] %in% names(config$perferred_term)))
    stopifnot("regexes must be a list with all terminologies must be present" = is.list(config$regexes)        && length(config$regexes) > 0        && all(config$terminology %in% names(config$regexes)))
    stopifnot("include must be length > 0 and be recognised concept_ids" = !any(is.null(config$include)) && length(config$include) > 0 && all(config$include %in% concept_ids))
    stopifnot("exclude must be recognised concept_ids" = if(!all(is.null(config$exclude))) all(config$exclude %in% concept_ids) else TRUE)

    # resave in standard format
    yaml::write_yaml(config, path)
    # yaml::write_yaml(config, "/Users/xx20081/Desktop/Untitled.yaml")

  }

  cat("\n** configs passed checks **\n")
}

