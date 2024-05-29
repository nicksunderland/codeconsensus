## code to prepare the `concepts` datasets goes here

# requirements
library(Rdiagnosislist)
library(data.table)
library(xml2)
devtools::load_all()

OVERWRITE = FALSE

# first we need the data sources, which are the SNOMED, ICD-10, and OPCS codes from the NHS
# TRUD website (https://isd.digital.nhs.uk/trud). You will need an account set up.
# These should be downloaded into directories outside this package (because they are large)
dir <- "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping"
SNOMED_RDS <- file.path(dir, "SNOMED.RDS")

# load the SNOMED dictionaries
if (!file.exists(SNOMED_RDS)) {
  SNOMED <- loadSNOMED(list.dirs(file.path(dir, "uk_sct2cl_38.0.0_20240410000001Z")))
  saveRDS(SNOMED, file = SNOMED_RDS)
} else {
  SNOMED <- readRDS(SNOMED_RDS)
}

# read the config files
configs <- list.files("/Users/xx20081/git/hfphenotyping/inst/concepts", pattern = ".*\\.(yaml|yml)$", full.names = TRUE)
configs <- configs[!file.info(configs)$isdir]

# produce the trees for each config
for (config in configs) {

  conf    <- yaml::read_yaml(config)
  outfile <- file.path(dirname(config), paste0(conf$id, ".RDS"))
  regex   <- paste0(conf$regexes, collapse = "|")


  if (!OVERWRITE & file.exists(outfile)) next


  cat("Extracting with regex: `", regex, "`\n")

  # tree
  tree <- TreeElement("tree", "tree", "tree")

  # extract the concept
  cat("[i] Extracting SNOMED\n")
  concept        <- SNOMEDconcept(regex, SNOMED = SNOMED, exact = FALSE)
  hierarch_codes <- showCodelistHierarchy(concept)
  snomed_tree    <- snomed_tree(hierarch_codes)
  label          <- paste0(attr(snomed_tree, "code"), " | ", attr(snomed_tree, "description"))
  tree[[label]] <- snomed_tree

  # read the ICD10
  cat("[i] Extracting ICD-10\n")
  icd10_xml_file <- file.path(dir, "FY24-CMS-1785-F-ICD-10-Table-Index", "icd10cm_tabular_2024.xml")
  icd10 <- xml2::read_xml(icd10_xml_file)
  icd10 <- xml2::as_list(icd10)
  icd10 <- icd10[[1]]
  icd10_tree <- icd10_tree(icd10, regex = regex)
  label <- paste0(attr(icd10_tree, "code"), " | ", attr(icd10_tree, "description"))
  tree[[label]] <- icd10_tree

  # read the OPCS (NHS TRUD)
  cat("[i] Extracting OPCS-4\n")
  opcs <- fread(file.path(dir, "OPCS410 Data files txt", "OPCS410 CodesAndTitles Nov 2022 V1.0.txt"), col.names = c("CODE", "DESCRIPTION"), header = FALSE)
  opcs_tree <- opcs_tree(opcs, regex = regex)
  label <- paste0(attr(opcs_tree, "code"), " | ", attr(opcs_tree, "description"))
  tree[[label]] <- opcs_tree

  # save
  cat("[i] saving .RDS file\n")
  saveRDS(tree, outfile)

  # populate the CONCEPTS database
  sql <- glue::glue("SELECT * FROM CONCEPTS WHERE CONCEPT_NAME = '{conf$name}' AND CONCEPT_CODE = '{conf$concept_id}'")
  this_concept <- query_db(sql, type = "get")
  if (nrow(this_concept) == 0) {
    sql     <- "SELECT MAX(CONCEPT_ID) AS max_concept_id FROM CONCEPTS"
    max_id  <- query_db(sql, type = "get")$MAX_CONCEPT_ID
    entry   <- list(CONCEPT_NAME = conf$name,
                    CONCEPT_ID   = if (is.na(max_id)) 1 else max_id + 1,
                    CONCEPT_CODE = conf$concept_id)
    cols <- paste0(names(entry), collapse = ", ")
    placeholders <- paste0(rep('?', length(entry)), collapse = ", ")
    sql <- glue::glue("INSERT INTO CONCEPTS ({cols}) VALUES ({placeholders})")
    query_db(query_str = sql, type = "update", value = entry)
  }

  # populate the CODE database
  these_codes <- data.table::data.table(CODE_DESC = as.character(tree_attributes(tree, "description")),
                                        CODE      = as.character(tree_attributes(tree, "code")),
                                        CODE_TYPE = as.character(tree_attributes(tree, "code_type")))
  saved_codes <- query_db(type = "read", table = "CODES")
  max_id <- if (is.infinite(max(saved_codes$CODE_ID))) 0 else max(saved_codes$CODE_ID)
  saved_codes[, CODE_ID := NULL]
  missing <- data.table::fsetdiff(these_codes, saved_codes)
  if (nrow(missing) > 0) {
    missing[, CODE_ID := .I + max_id]
    sql <- glue::glue("INSERT INTO CODES ({paste(names(missing), collapse = ', ')}) VALUES ({paste0(rep('?', ncol(missing)), collapse = ', ')})")
    query_db(query_str = sql, type = "update", value = as.list(missing))
  }

}


