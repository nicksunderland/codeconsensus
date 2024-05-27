#' @title TreeElement S3 class
#'
#' @param name the label
#' @param db_name the database column name
#' @param stselected logical
#' @param stopened logical
#' @param data empty character ""
#'
#' @return a TreeElement S3 object
#' @export
#'
TreeElement <- function(name, db_name, stselected = FALSE, stopened = TRUE, children = list()) {
  s <- structure(
    .Data      = children,
    class      = "TreeElement",
    name       = name,
    stselected = stselected,
    stopened   = stopened,
    db_name    = db_name
  )
}


#' @title SNOMED tree
#' @param hierarch_codes output from Rdiagnosislist::showCodelistHierarchy
#' @param row_id NA for recursive use
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
      concept_id  <- concept_row$conceptId
      term        <- concept_row$term
      name        <- paste0(concept_id, " | ", term)
      db_name     <- paste0("SNOMED_", concept_id)

      if (!is.null(x$children) && length(x$children) > 0) {
        children <- convert_to_tree_element(x$children, hierarch_codes)
        element <- TreeElement(name, db_name, children = children)
      } else {
        element <- TreeElement(name, db_name)
      }

      result[[name]] <- element
    }

    return(result)
  }

  # convert
  tree <- TreeElement("SNOMED", "SNOMED", children = convert_to_tree_element(nested_list, hierarch_codes))

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
#' @param icd_xml list output from xml2::read_xml -> xml2::as_list (https://www.cms.gov/files/zip/2024-code-tables-tabular-and-index-updated-02/01/2024.zip)
#' @param regex regular expression to search for
#' @return a nested list
#' @export
#'
icd10_tree <- function(icd10, regex = ".") {

  result <- TreeElement("ICD10", "ICD10")

  for (i in seq_along(icd10)) {

    if (names(icd10)[i] %in% c("chapter", "section", "diag") | all(names(icd10)[i] == c("name", "desc", "inclusionTerm"))) {

      name      <- icd10[[i]][["desc"]][[1]]
      diag_code <- sub("\\.", "", icd10[[i]][["name"]][[1]])

      if (length(diag_code) > 0) {
        name <- paste0(diag_code, " | ", name)
      } else {
        name <- paste0(sub(".*\\((.*)\\).*", "\\1", name), " | ", name)
        diag_code <- clean_id(sub(".*\\((.*)\\).*", "\\1", name), to_lower = FALSE)
      }

      nested_result <- icd10_tree(icd10[[i]], regex)
      attr(nested_result, "name") <- name
      attr(nested_result, "db_name") <- paste0("ICD10_", diag_code)

      # print(nested_result)

      # If there are nested results or the current name matches the regex
      if (grepl(regex, name, perl = TRUE) | length(nested_result) > 0) {
         result[[attr(nested_result, "name")]] <- nested_result
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
#' @return a nested list
#' @export
#'
opcs_tree <- function(opcs_dt, regex = ".") {
  OPCS <- TreeElement("OPCS", "OPCS")
  current_chapter_name <- NULL
  current_chapter_element <- NULL

  for (i in seq_len(nrow(opcs_dt))) {
    code <- opcs_dt$CODE[i]
    description <- opcs_dt$DESCRIPTION[i]

    clean_code <- sub("\\.", "", code)
    element <- TreeElement(name    = paste0(clean_code, " | ", description),
                           db_name = paste0("OPCS4_", clean_code))

    if (!grepl("\\.", code, perl = TRUE)) {
      # Chapter level code
      current_chapter_element <- element
      current_chapter_name <- attr(element, "name")

    } else {

      # skip if fails regex
      if (!grepl(regex, code) && !grepl(regex, description)) {
        next
      } else {
        # add the chapter
        if (!is.null(attr(current_chapter_element, "name"))) {
          OPCS[[current_chapter_name]] <- current_chapter_element
          current_chapter_element <- NULL
        }
        # add the procedure
        OPCS[[current_chapter_name]][[attr(element, "name")]] <- element

      }

    }
  }

  return(OPCS)
}


# dir <- "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping"
# opcs <- data.table::fread(file.path(dir, "OPCS410 Data files txt", "OPCS410 CodesAndTitles Nov 2022 V1.0.txt"), col.names = c("CODE", "DESCRIPTION"), header = FALSE)
# opcs_tree <- opcs_tree(opcs, regex = "pacemaker")
# tree[[attr(opcs_tree, "name")]] <- opcs_tree






#' @title Get tree attributes
#' @param tree a tree
#' @param attribute_name str, the attribute name
#' @param result for recursive use
#' @return a list of name = attribute
#' @export
#'
tree_attributes <- function(tree, attribute_name, result = list()) {

  for (element_name in names(tree)) {
    element                <- tree[[element_name]]
    attribute_value        <- attr(element, attribute_name)
    if (is.null(attribute_value) && attribute_name == "stselected") {
      attribute_value <- FALSE
    }
    result[[element_name]] <- attribute_value
    result                 <- tree_attributes(element, attribute_name, result)
  }
  return(result)
}

