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
TreeElement <- function(code, code_type, description, stselected = FALSE, stopened = TRUE, children = list()) {
  s <- structure(
    .Data      = children,
    class      = "TreeElement",
    code       = code,
    code_type  = code_type,
    description= description,
    stselected = stselected,
    stopened   = stopened
  )
}


#' @title SNOMED tree
#' @param hierarch_codes output from Rdiagnosislist::showCodelistHierarchy
#' @return a nested list
#' @export
#'
snomed_tree <- function(hierarch_codes) {

  # nested list of the row index within hierarch_codes
  build_nested_list <- function(hierarch_codes, row_id) {
    children <- hierarch_codes[rowid == row_id, childrowid[[1]]]
    if (length(children) == 0) return(list(rowid = row_id, children = list()))
    list(rowid = row_id, children = lapply(children, function(child) build_nested_list(hierarch_codes, child)))
  }

  # Build the nested list starting from the top-level parents
  nested_list <- lapply(hierarch_codes[is.na(parentrowid), rowid], function(parent) build_nested_list(hierarch_codes, parent))

  # convert to TreeElements
  convert_to_tree_element <- function(nested_list, hierarch_codes) {
    result <- list()

    for (i in seq_along(nested_list)) {

      x           <- nested_list[[i]]
      concept_row <- hierarch_codes[rowid == x$rowid]
      code        <- as.character(concept_row$conceptId)
      desc        <- concept_row$term
      label       <- paste0(code, " | ", desc)

      if (!is.null(x$children) && length(x$children) > 0) {
        children <- convert_to_tree_element(x$children, hierarch_codes)
        element <- TreeElement(code, "SNOMED", desc, children = children)
      } else {
        element <- TreeElement(code, "SNOMED", desc)
      }

      result[[label]] <- element
    }

    return(result)
  }

  # convert
  tree <- TreeElement("SNOMED", "SNOMED", "coding system", children = convert_to_tree_element(nested_list, hierarch_codes))

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
icd10_tree <- function(icd10, regex = ".") {

  result <- TreeElement("ICD10", "ICD10", "coding system")

  for (i in seq_along(icd10)) {

    if (names(icd10)[i] %in% c("chapter", "section", "diag") | all(names(icd10)[i] == c("name", "desc", "inclusionTerm"))) {

      desc      <- icd10[[i]][["desc"]][[1]]
      diag_code <- sub("\\.", "", icd10[[i]][["name"]][[1]])

      if (length(diag_code) == 0) {
        diag_code <- clean_id(sub(".*\\((.*)\\).*", "\\1", desc), to_lower = FALSE)
      }

      nested_result <- icd10_tree(icd10[[i]], regex)
      attr(nested_result, "code") <- diag_code
      attr(nested_result, "code_type") <- "ICD10"
      attr(nested_result, "description") <- desc

      # print(nested_result)

      # If there are nested results or the current name matches the regex
      if (grepl(regex, desc, perl = TRUE) | grepl(regex, diag_code, perl = TRUE) | length(nested_result) > 0) {
        if (names(icd10)[i] %in% c("chapter", "section")) {
          attr(nested_result, "stselected") <- FALSE
          attr(nested_result, "stdisabled") <- TRUE
        }
        label <- paste0(attr(nested_result, "code"), " | ", attr(nested_result, "description"))
        result[[label]] <- nested_result
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
opcs_tree <- function(opcs_dt, regex = ".") {
  OPCS <- TreeElement("OPCS", "OPCS", "coding system")
  current_chapter_name <- NULL
  current_chapter_element <- NULL

  for (i in seq_len(nrow(opcs_dt))) {
    code <- opcs_dt$CODE[i]
    description <- opcs_dt$DESCRIPTION[i]

    clean_code <- sub("\\.", "", code)
    element <- TreeElement(code        = clean_code,
                           code_type   = "OPCS4",
                           description = description)

    if (!grepl("\\.", code, perl = TRUE)) {
      # Chapter level code
      current_chapter_element <- element
      attr(current_chapter_element, "stselected") <- FALSE
      attr(current_chapter_element, "stdisabled") <- TRUE
      current_chapter_name <- paste0(attr(element, "code"), " | ", attr(element, "description"))

    } else {

      # skip if fails regex
      if (!grepl(regex, clean_code) && !grepl(regex, description)) {
        next
      } else {
        # add the chapter
        if (!is.null(attr(current_chapter_element, "code"))) {
          OPCS[[current_chapter_name]] <- current_chapter_element
          current_chapter_element <- NULL
        }
        # add the procedure
        label <- paste0(attr(element, "code"), " | ", attr(element, "description"))
        OPCS[[current_chapter_name]][[label]] <- element

      }

    }
  }

  return(OPCS)
}


# dir <- "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping"
# opcs <- data.table::fread(file.path(dir, "OPCS410 Data files txt", "OPCS410 CodesAndTitles Nov 2022 V1.0.txt"), col.names = c("CODE", "DESCRIPTION"), header = FALSE)
# opcs_tree <- opcs_tree(opcs, regex = "pacemaker")
# tree[[attr(opcs_tree, "name")]] <- opcs_tree



#' @title Annotate tree
#' @param tree a tree
#' @param annot_dt a data.table, data to use for annotation
#' @param value_col a string, column name in `annot_dt` with the annotation values. The new attribute name is taken from the value column name.
#' @param on a named character vector, how to join the data e.g. c("annot_dt_col" = "tree_attribute_name"), can have multiple conditions e.g. c(code=code, code_type=code_type)
#' @param no_match any value, what to annotate with if there is no join data
#' @param result for recursive use
#' @return a nested list, a tree
#' @export
#'
annotate_tree <- function(tree, annot_dt, value_col = "count", on = c("icd10" = "code"), no_match = NA) {

  for (element_name in names(tree)) {
    element    <- tree[[element_name]]

    filter_condition <- sapply(names(on), function(key) {
      attr_value <- attr(element, on[[key]])
      paste0(key, " == '", attr_value, "'")
    })
    filter_condition <- paste(filter_condition, collapse = " & ")
    annot_data <- annot_dt[eval(parse(text = filter_condition)), ..value_col]

    if (nrow(annot_data) == 0) {
      annot_data <- no_match
    } else if (nrow(annot_data) > 1) {
      annot_data <- annot_data[[1]]
      warning(paste0("Multiple values found for element ", element_name, ". Taking first `[[1]]`. "))
    } else {
      annot_data <- annot_data[[1]]
    }
    attr(element, value_col) <- annot_data

    # Recursively annotate the subtree
    if (length(element) > 0) {
      element <- annotate_tree(element, annot_dt, value_col, on, no_match)
    }

    tree[[element_name]] <- element

  }
  return(tree)
}

