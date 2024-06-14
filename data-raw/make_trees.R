#' @title SNOMED tree
#' @param hierarch_codes output from Rdiagnosislist::showCodelistHierarchy
#' @return a nested list
#' @export
#'
snomed_tree <- function(hierarch_codes, concept_id) {

  # nested list of the row index within hierarch_codes
  build_nested_list <- function(hierarch_codes, row_id) {
    children <- hierarch_codes[rowid == row_id, childrowid[[1]]]
    if (length(children) == 0) return(list(rowid = row_id, children = list()))
    list(rowid = row_id, children = lapply(children, function(child) build_nested_list(hierarch_codes, child)))
  }

  # Build the nested list starting from the top-level parents
  nested_list <- lapply(hierarch_codes[is.na(parentrowid), rowid], function(parent) build_nested_list(hierarch_codes, parent))

  # convert to TreeElements
  convert_to_tree_element <- function(nested_list, hierarch_codes, concept_id) {
    result <- list()

    for (i in seq_along(nested_list)) {

      x           <- nested_list[[i]]
      concept_row <- hierarch_codes[rowid == x$rowid]
      code        <- as.character(concept_row$conceptId)
      desc        <- concept_row$term

      if (!is.null(x$children) && length(x$children) > 0) {
        children <- convert_to_tree_element(x$children, hierarch_codes, concept_id)
        element <- TreeNode(text = paste0(code, " | ", desc),
                            type = "code",
                            data = list(code       = code,
                                        code_type  = "SNOMED",
                                        desc       = desc,
                                        concept_id = concept_id),
                            checked  = FALSE,
                            selected = FALSE,
                            opened   = FALSE,
                            disabled = FALSE,
                            children = children)

      } else {
        element <- TreeNode(text = paste0(code, " | ", desc),
                            type = "code",
                            data = list(code       = code,
                                        code_type  = "SNOMED",
                                        desc       = desc,
                                        concept_id = concept_id),
                            checked  = FALSE,
                            selected = FALSE,
                            opened   = FALSE,
                            disabled = FALSE,
                            children = list())
      }

      result <- c(result, list(element))
    }

    return(result)
  }

  # convert
  tree <- TreeNode(text = "SNOMED",
                   type = "root",
                   data = list(code       = "SNOMED",
                               code_type  = "SNOMED",
                               desc       = NULL,
                               concept_id = concept_id),
                   checked  = FALSE,
                   selected = FALSE,
                   opened   = FALSE,
                   disabled = TRUE,
                   children = convert_to_tree_element(nested_list, hierarch_codes, concept_id))


  return(tree)
}


# # cat("[i] Extracting SNOMED\n")
# library(Rdiagnosislist)
# dir <- "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping"
# SNOMED_RDS <- file.path(dir, "SNOMED.RDS")
# # load the SNOMED dictionaries
# if (!file.exists(SNOMED_RDS)) {
#   SNOMED <- loadSNOMED(list.dirs(file.path(dir, "uk_sct2cl_38.0.0_20240410000001Z")))
#   saveRDS(SNOMED, file = SNOMED_RDS)
# } else {
#   SNOMED <- readRDS(SNOMED_RDS)
# }
# concept        <- SNOMEDconcept("heart failure", SNOMED = SNOMED, exact = FALSE)
# hierarch_codes <- showCodelistHierarchy(concept)
# snomed_tree2    <- snomed_tree(hierarch_codes)
#







#' @title ICD10 tree
#' @param icd10 list output from xml2::read_xml -> xml2::as_list (https://www.cms.gov/files/zip/2024-code-tables-tabular-and-index-updated-02/01/2024.zip)
#' @param regex regular expression to search for
#' @return a nested list
#' @export
#'
# base tree node
make_icd10_tree <- function(icd10, concept_id, regex = ".") {

  result <- TreeNode(text = "ICD10-CM",
                     type = "root",
                     data = list(code       = "ICD10-CM",
                                 code_type  = "ICD10",
                                 desc       = NULL,
                                 concept_id = concept_id),
                     checked  = FALSE,
                     selected = FALSE,
                     opened   = FALSE,
                     disabled = TRUE,
                     children = list())

  for (i in seq_along(icd10)) {

    if (names(icd10)[i] %in% c("chapter", "section", "diag") | all(names(icd10)[i] == c("name", "desc", "inclusionTerm"))) {

      desc      <- icd10[[i]][["desc"]][[1]]
      diag_code <- sub("\\.", "", icd10[[i]][["name"]][[1]])

      if (length(diag_code) == 0) {
        diag_code <- sub(".*\\((.*)\\).*", "\\1", desc)
      }

      nested_result           <- make_icd10_tree(icd10[[i]], concept_id = concept_id, regex = regex)
      nested_result$data$code <- diag_code
      nested_result$data$desc <- desc
      nested_result$text      <- paste0(c(diag_code, desc), collapse = " | ")

      if (grepl(regex, desc, perl = TRUE) | grepl(regex, diag_code, perl = TRUE) | length(nested_result$children) > 0) {
        if (names(icd10)[i] %in% c("chapter", "section")) {
          nested_result$type            <- "chapter"
          nested_result$state$disabled  <- FALSE
          nested_result$state$opened    <- FALSE
        } else {
          nested_result$type            <- "code"
          nested_result$state$disabled  <- FALSE
        }

        result$children <- c(result$children, list(nested_result))
      }
    }
  }
  return(result)
}

# dir <- "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping"
# icd10_xml_file <- file.path(dir, "FY24-CMS-1785-F-ICD-10-Table-Index", "icd10cm_tabular_2024.xml")
# icd10 <- xml2::read_xml(icd10_xml_file)
# icd10 <- xml2::as_list(icd10)
# icd10 <- icd10[[1]]
# icd10_tree <- icd10_tree(icd10, regex = "heart")






#' @title OPCS tree
#' @param opcs_dt a data.table of OPCS codes with columns CODE and DESCRIPTION
#' @param regex regular expression to search for
#' @return a nested list
#' @export
#'
opcs_tree <- function(opcs_dt, concept_id, regex = ".") {

  OPCS <- TreeNode(text = "OPCS4",
                   type = "root",
                   opened   = FALSE,
                   checked  = FALSE,
                   selected = FALSE,
                   disabled = TRUE,
                   data = list(code       = "OPCS4",
                               code_type  = "OPCS4",
                               desc       = NULL,
                               concept_id = concept_id))

  current_chapter_name <- NULL
  current_chapter_element <- NULL

  for (i in seq_len(nrow(opcs_dt))) {
    code <- opcs_dt$CODE[i]
    description <- opcs_dt$DESCRIPTION[i]

    clean_code <- sub("\\.", "", code)
    element <- TreeNode(text = paste0(c(clean_code, description), collapse = " | "),
                        type = "code",
                        data = list(code       = clean_code,
                                    code_type  = "OPCS4",
                                    desc       = description,
                                    concept_id = concept_id),
                        checked  = FALSE,
                        selected = FALSE,
                        opened   = FALSE,
                        disabled = FALSE,
                        children = list())

    if (!grepl("\\.", code, perl = TRUE)) {
      # Chapter level code
      chapter_idx <- length(OPCS$children) + 1
      current_chapter_element <- element
      current_chapter_element$type <- "chapter"

    } else {

      # skip if fails regex
      if (!grepl(regex, clean_code) & !grepl(regex, description)) {
        next
      } else {
        # add the chapter
        if (!is.null(current_chapter_element)) {
          OPCS$children <- c(OPCS$children, list(current_chapter_element))
          current_chapter_element <- NULL
        }
        # add the procedure
        OPCS$children[[chapter_idx]]$children <- c(OPCS$children[[chapter_idx]]$children, list(element))

      }
    }
  }

  return(OPCS)
}


# dir <- "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping"
# opcs <- data.table::fread(file.path(dir, "OPCS410 Data files txt", "OPCS410 CodesAndTitles Nov 2022 V1.0.txt"), col.names = c("CODE", "DESCRIPTION"), header = FALSE)
# opcs_tree <- opcs_tree(opcs, regex = "pacemaker")
# tree[[attr(opcs_tree, "name")]] <- opcs_tree




#' @title ICD9 tree
#' @param icd9_dt a data.table of ICD9 codes with columns CODE and DESCRIPTION
#' @param regex regular expression to search for
#' @return a nested list
#' @export
#'
icd9_tree <- function(icd9_dt, concept_id, regex = ".") {

  ICD9 <- TreeNode(text = "ICD9",
                   type = "root",
                   opened   = FALSE,
                   checked  = FALSE,
                   selected = FALSE,
                   disabled = TRUE,
                   data = list(code       = "ICD9",
                               code_type  = "ICD9",
                               desc       = NULL,
                               concept_id = concept_id))

  current_chapter_name <- NULL
  current_chapter_element <- NULL

  for (i in seq_len(nrow(icd9_dt))) {
    code <- icd9_dt$CODE[i]
    description <- icd9_dt$DESCRIPTION[i]

    element <- TreeNode(text = paste0(c(code, description), collapse = " | "),
                        type = "code",
                        data = list(code       = code,
                                    code_type  = "ICD9",
                                    desc       = description,
                                    concept_id = concept_id),
                        checked  = FALSE,
                        selected = FALSE,
                        opened   = FALSE,
                        disabled = FALSE,
                        children = list())

    if (grepl("^Chapter", code)) {
      # Chapter level code
      chapter_idx <- length(ICD9$children) + 1
      current_chapter_element <- element
      current_chapter_element$type <- "chapter"
      current_chapter_element$state$disabled <- TRUE

    } else {

      # skip if fails regex
      if (!grepl(regex, code) & !grepl(regex, description)) {
        next
      } else {
        # add the chapter
        if (!is.null(current_chapter_element)) {
          ICD9$children <- c(ICD9$children, list(current_chapter_element))
          current_chapter_element <- NULL
        }
        # add the procedure
        ICD9$children[[chapter_idx]]$children <- c(ICD9$children[[chapter_idx]]$children, list(element))

      }
    }
  }

  return(ICD9)
}


#' @title ICD9 procedure tree
#' @param icd9_proc_dt a data.table of ICD9 procedure codes with columns CODE and DESCRIPTION
#' @param regex regular expression to search for
#' @return a nested list
#' @export
#'
icd9_procedure_tree <- function(icd9_proc_dt, concept_id, regex = ".") {

  ICD9 <- TreeNode(text = "ICD9_procedures",
                   type = "root",
                   opened   = FALSE,
                   checked  = FALSE,
                   selected = FALSE,
                   disabled = TRUE,
                   data = list(code       = "ICD9_procedures",
                               code_type  = "ICD9_procedures",
                               desc       = NULL,
                               concept_id = concept_id))

  current_chapter_name <- NULL
  current_chapter_element <- NULL

  for (i in seq_len(nrow(icd9_proc_dt))) {
    code <- icd9_proc_dt$CODE[i]
    description <- icd9_proc_dt$DESCRIPTION[i]

    element <- TreeNode(text = paste0(c(code, description), collapse = " | "),
                        type = "code",
                        data = list(code       = code,
                                    code_type  = "ICD9_procedures",
                                    desc       = description,
                                    concept_id = concept_id),
                        checked  = FALSE,
                        selected = FALSE,
                        opened   = FALSE,
                        disabled = FALSE,
                        children = list())

    if (grepl("^Chapter", code)) {
      # Chapter level code
      chapter_idx <- length(ICD9$children) + 1
      current_chapter_element <- element
      current_chapter_element$state$disabled <- TRUE
      current_chapter_element$type <- "chapter"

    } else {

      # skip if fails regex
      if (!grepl(regex, code) & !grepl(regex, description)) {
        next
      } else {
        # add the chapter
        if (!is.null(current_chapter_element)) {
          ICD9$children <- c(ICD9$children, list(current_chapter_element))
          current_chapter_element <- NULL
        }
        # add the procedure
        ICD9$children[[chapter_idx]]$children <- c(ICD9$children[[chapter_idx]]$children, list(element))

      }
    }
  }

  return(ICD9)
}


#' @title Annotate tree
#' @param tree a tree
#' @param annot_dt a data.table, data to use for annotation
#' @param value_col a string, column name in `annot_dt` with the annotation values.
#' @param on a named character vector, how to join the data e.g. c("annot_dt_col" = "node$data$name"), can have multiple conditions e.g. c(code=code, code_type=code_type)
#' @param annot_name new attribute name (inserted into node$data)
#' @param no_match any value, what to annotate with if there is no join data
#' @return a nested list, a tree
#' @export
#'
annotate_tree <- function(tree, annot_dt, value_col = "count", on = c("code" = "code", "code_type" = "code_type"), annot_name = "value", no_match = 0) {

  for (i in seq_along(tree)) {

    filter_condition <- sapply(names(on), function(key) {
      attr_value <- tree[[i]]$data[[on[[key]]]]
      paste0(key, " == '", as.character(attr_value), "'")
    })
    filter_condition <- paste(filter_condition, collapse = " & ")
    annot_data <- annot_dt[eval(parse(text = filter_condition)), ..value_col]

    if (tree[[i]]$type != "code") {
      annot_data <- NULL
    } else if (nrow(annot_data) == 0 || all(is.na(annot_data))) {
      annot_data <- no_match
    } else if (nrow(annot_data) > 1) {
      annot_data <- annot_data[[1]]
      warning(paste0("Multiple values found for element ", element_name, ". Taking first `[[1]]`. "))
    } else {
      annot_data <- annot_data[[1]]
    }
    tree[[i]]$data[[annot_name]] <- annot_data

    # Recursively annotate the subtree
    if (length(tree[[i]]$children) > 0) {
      tree[[i]]$children <- annotate_tree(tree[[i]]$children, annot_dt, value_col, on, annot_name, no_match)
    }

  }
  return(tree)
}

