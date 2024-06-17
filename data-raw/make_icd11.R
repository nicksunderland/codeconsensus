



icd11_tree <- function(icd11_dt, concept_id, regex = ".") {

  ICD11 <- TreeNode(text = "ICD11",
                    type = "root",
                    opened   = FALSE,
                    checked  = FALSE,
                    selected = FALSE,
                    disabled = TRUE,
                    data = list(code       = "ICD11",
                                code_type  = "ICD11",
                                desc       = NULL,
                                concept_id = concept_id))

  add_element <- function(result, row, indices) {

    if (length(indices) > 1) {

      result$children[[indices[1]]] <- add_element(result$children[[indices[1]]], row, indices[-1])

    } else {

      code <- if(row$Code == "") NULL else row$Code
      desc <- gsub("- ?", "", row$Title)

      cat(paste0(c(code, desc), collapse = " | "), "\n")

      el <- TreeNode(text = paste0(c(code, desc), collapse = " | "),
                     type = ifelse(row$ClassKind != "category", "code", "chapter"),
                     opened   = FALSE,
                     checked  = FALSE,
                     selected = FALSE,
                     disabled = TRUE,
                     data = list(code       = code,
                                 code_type  = "ICD11",
                                 desc       = desc,
                                 concept_id = concept_id))

      result$children <- c(result$children, list(el))

    }

    return(result)
  }

  cpt <- 0
  blk <- c()
  cat <- c()

  while(nrow(icd11_dt) > 0) {

    row <- icd11_dt[1, ]

    print(row$Title)

    if (row$ClassKind == "chapter") {
      cpt <- cpt + 1
      blk <- c()
      cat <- c()

    } else if (row$ClassKind == "block") {

      cat <- c()
      skip_lvl <- row$DepthInKind - length(blk) - 1
      skip_lvl <- if(skip_lvl < 0) 0 else skip_lvl
      if (skip_lvl > 0) {
        blk <- c(blk, rep(1, skip_lvl), 0)
      } else if (length(blk) < row$DepthInKind) {
        blk <- c(blk, 0)
      } else if (length(blk) > row$DepthInKind) {
        blk <- blk[1:row$DepthInKind]
      }
      blk[row$DepthInKind] <- blk[row$DepthInKind] + 1


    } else if (row$ClassKind == "category") {

      skip_lvl <- row$DepthInKind - length(cat) - 1
      skip_lvl <- if(skip_lvl < 0) 0 else skip_lvl
      if (skip_lvl > 0) {
        cat <- c(cat, rep(1, skip_lvl), 0)
      } else if (length(cat) < row$DepthInKind) {
        cat <- c(cat, 0)
      } else if (length(cat) > row$DepthInKind) {
        cat <- cat[1:row$DepthInKind]
      }
      cat[row$DepthInKind] <- cat[row$DepthInKind] + 1

    }

    if (grepl(regex, row$Title, perl = TRUE) | grepl(regex, row$Code, perl = TRUE)) {
      ICD11 <- add_element(ICD11, row, c(cpt, blk, cat))
    }

    icd11_dt <- icd11_dt[-1, ]

  }

  return(ICD11)
}

devtools::load_all()
# cols <- data.table::fread("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/ICD11/SimpleTabulation-ICD-11-MMS-en.txt", nrows = 0, sep = "\t")
# icd11_dt <- data.table::fread("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/ICD11/SimpleTabulation-ICD-11-MMS-en.txt", skip = 18445, sep = "\t")
# names(icd11_dt) <- names(cols)

icd11_dt <- data.table::fread("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/ICD11/SimpleTabulation-ICD-11-MMS-en.txt", nrows = 27842, sep = "\t")
icd11 <- icd11_tree(icd11_dt, "test", regex = "heart failure")





