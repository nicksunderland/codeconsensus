## code to prepare the `concepts` datasets goes here

# requirements
library(Rdiagnosislist)
library(data.table)
library(xml2)
devtools::load_all()
source(system.file("data-raw", "make_trees.R", package = "hfphenotyping"))

OVERWRITE = F

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

# NHS digital & UKBB ICD10, OPCS4 and SNOMED counts
nhs_counts  <- nhs_counts
ukbb_counts <- ukbb_counts

# produce the trees for each config
for (config in configs) {

  conf    <- yaml::read_yaml(config)
  outfile <- file.path(dirname(config), paste0(conf$id, ".RDS"))
  regex   <- paste0("(", conf$regexes, ")", collapse = "|")


  # populate the CONCEPTS database
  sql <- glue::glue("SELECT * FROM CONCEPTS WHERE CONCEPT = '{conf$id}' AND CONCEPT_CODE = '{conf$concept_id}'")
  this_concept <- query_db(sql, type = "get")
  if (nrow(this_concept) == 0) {
    sql     <- "SELECT MAX(CONCEPT_ID) AS max_concept_id FROM CONCEPTS"
    max_id  <- query_db(sql, type = "get")$MAX_CONCEPT_ID
    entry   <- list(CONCEPT      = conf$id,
                    CONCEPT_ID   = if (is.na(max_id)) 1 else max_id + 1,
                    CONCEPT_CODE = conf$concept_id)
    cols <- paste0(names(entry), collapse = ", ")
    placeholders <- paste0(rep('?', length(entry)), collapse = ", ")
    sql <- glue::glue("INSERT INTO CONCEPTS ({cols}) VALUES ({placeholders})")
    query_db(query_str = sql, type = "update", value = entry)
  }

  # if a derived concept, exit here
  if (conf$domain == "Derived") next

  # produce the tree
  if (!OVERWRITE & file.exists(outfile)) {

    tree <- readRDS(outfile)

  } else {

    cat("Extracting with regex: `", regex, "`\n")

    # extract the concept
    cat("[i] Extracting SNOMED\n")
    concept        <- SNOMEDconcept(regex, SNOMED = SNOMED, exact = FALSE)
    hierarch_codes <- showCodelistHierarchy(concept)
    snomed         <- snomed_tree(hierarch_codes)

    # read the ICD10
    cat("[i] Extracting ICD-10\n")
    icd10_xml_file <- file.path(dir, "FY24-CMS-1785-F-ICD-10-Table-Index", "icd10cm_tabular_2024.xml")
    icd10 <- xml2::read_xml(icd10_xml_file)
    icd10 <- xml2::as_list(icd10)
    icd10 <- icd10[[1]]
    icd10 <- make_icd10_tree(icd10, regex = regex)

    # read the OPCS (NHS TRUD)
    cat("[i] Extracting OPCS-4\n")
    opcs <- fread(file.path(dir, "OPCS410 Data files txt", "OPCS410 CodesAndTitles Nov 2022 V1.0.txt"), col.names = c("CODE", "DESCRIPTION"), header = FALSE)
    opcs <- opcs_tree(opcs, regex = regex)

    # annotate the tree with data
    tree <- annotate_tree(tree = list(snomed, icd10, opcs), annot_dt = nhs_counts, annot_name = "nhs_count", value_col = "count", on = c("code" = "code", "code_type" = "code_type"), no_match = 0)
    tree <- annotate_tree(tree = tree, annot_dt = ukbb_counts, annot_name = "ukbb_count", value_col = "count", on = c("code" = "code", "code_type" = "code_type"), no_match = 0)

    # save
    cat("[i] saving .RDS file\n")
    saveRDS(tree, outfile)
  }

  # populate the CODE database
  these_codes <- data.table::data.table(CODE_DESC = as.character(tree_attributes(tree, "desc")),
                                        CODE      = as.character(tree_attributes(tree, "code")),
                                        DISABLED  = unlist(tree_attributes(tree, "disabled")),
                                        CODE_TYPE = as.character(tree_attributes(tree, "code_type")))
  these_codes <- these_codes[DISABLED == FALSE, ]
  these_codes[, DISABLED := NULL]
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




