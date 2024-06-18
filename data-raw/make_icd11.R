
icd11_dt <- data.table::fread("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/ICD11/SimpleTabulation-ICD-11-MMS-en.txt", sep = "\t")

cat_lvls <- data.table::copy(icd11_dt)[ClassKind == "category"]
cat_lvls[, L0 := data.table::rleid(DepthInKind)]
max_depth <- max(cat_lvls$DepthInKind)
for (i in seq_len(max_depth)) {
  cat_lvls[DepthInKind == i, paste0("L", i) := Title, by = "L0"]
}

icd11_dt[cat_lvls, on = .(Title), paste0("L", 1:8) := mget(paste0("L", 1:8))]

for (i in seq_len(max_depth)) {

  icd11_dt[, lst_group := rowSums(!is.na(.SD)) == 0, .SDcols = paste0("L", i:max_depth)]
  icd11_dt[, paste0("L", i) := zoo::na.locf(get(paste0("L", i)), na.rm = FALSE)]
  icd11_dt[lst_group == TRUE, paste0("L", i) := NA_character_]
  icd11_dt[is.na(get(paste0("L", i))), paste0("L", i) := ""]

}





split_blocks <- function(icd11_dt, by = "chapter", level = 0) {



  if (by == "chapter") {
    cat("[i] chapter\n")

    result <- split(icd11_dt, by = "ChapterNo")
    result <- lapply(result, function(x) split_blocks(x, by = "block", level = 1))

  } else if (by == "block" && level < 5 && !all(icd11_dt[, .SD == "", .SDcols = paste0("Grouping", level)])) {
    cat("[i] block\n")

    result <- split(icd11_dt[!ClassKind %in% c("block", "chapter")], by = paste0("Grouping", level))
    result <- lapply(result, function(x) split_blocks(x, by = "block", level = level + 1))

  } else if (all(icd11_dt[, !is.na(L1)]) && by != "category") {
    cat("[i] category with", unique(icd11_dt$DepthInKind), "levels\n")

    result <- split(icd11_dt, by = "L1")
    result <- lapply(result, function(x) split_blocks(x, by = "category", level = 1 + 1))

  } else if (by == "category" && level < 8 && !all(icd11_dt[, .SD == "", .SDcols = paste0("L", level)])) {

    result <- split(icd11_dt, by = paste0("L", level))
    result <- lapply(result, function(x) split_blocks(x, by = "category", level = level + 1))

  } else {

    result <- icd11_dt

  }

  return(result)

}

foo <- split_blocks(icd11_dt)















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





