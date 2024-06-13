## code to prepare the `concepts` datasets goes here

# requirements
library(Rdiagnosislist)
library(data.table)
library(xml2)
library(furrr)
library(future)
devtools::load_all()


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
plan(multisession, workers = parallel::detectCores() - 1)
options(future.globals.maxSize = 90 * 1024^3)  # 2 GiB
future_walk(configs, function(config) {

  library(hfphenotyping)
  library(Rdiagnosislist)
  library(data.table)
  library(xml2)

  # source(system.file("data-raw", "make_trees.R", package = "hfphenotyping"))
  source("/Users/xx20081/git/hfphenotyping/data-raw/make_trees.R")

  conf    <- yaml::read_yaml(config)
  outfile <- file.path(dirname(config), paste0(conf$id, ".RDS"))
  regex   <- conf$regexes[!sapply(conf$regexes, is.null)]
  regex   <- lapply(regex, function(x) paste0("(", x, ")", collapse = "|"))
  terminologies <- conf$terminology


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
  if (conf$domain == "Derived") return(NULL)


  cat("Extracting `", conf$id, "`\n")

  # -------------------------
  #          SNOMED
  # -------------------------
  snomed_regex <- paste0(c(regex$all, regex$SNOMED), collapse = "|")
  concept      <- SNOMEDconcept(snomed_regex, SNOMED = SNOMED, exact = FALSE, active_only = FALSE)

  if ("SNOMED" %in% terminologies) {
    cat("[i] Extracting SNOMED\n")

    print(unique(semanticType(concept)))

    filt <- c("disorder", "finding", "observable entity", "situation", "event")
    hierarch_codes <- showCodelistHierarchy(description(concept)[grepl(paste0(filt, collapse = "|"), term), conceptId])
    snomed         <- snomed_tree(hierarch_codes, concept_id = conf$id)

  } else {

    snomed <- NULL

  }


  if ("SNOMED_procedure" %in% terminologies) {
    cat("[i] Extracting SNOMED procdures\n")

    filt <- c("procedure")
    hierarch_codes <- showCodelistHierarchy(description(concept)[grepl(paste0(filt, collapse = "|"), term), conceptId])
    snomed_procedure <- snomed_tree(hierarch_codes, concept_id = conf$id)

  } else {

    snomed_procedure <- NULL

  }


  # -------------------------
  #          ICD10
  # -------------------------
  if ("ICD10" %in% terminologies) {

    cat("[i] Extracting ICD-10\n")
    icd10_xml_file <- file.path(dir, "FY24-CMS-1785-F-ICD-10-Table-Index", "icd10cm_tabular_2024.xml")
    icd10 <- xml2::read_xml(icd10_xml_file)
    icd10 <- xml2::as_list(icd10)
    icd10 <- icd10[[1]]
    icd10_regex <- paste0(c(regex$all, regex$ICD10), collapse = "|")
    icd10 <- make_icd10_tree(icd10, concept_id = conf$id, regex = icd10_regex)

  } else {

    icd10 <- NULL

  }


  # -------------------------
  #          OPCS4
  # -------------------------
  if ("OPCS4" %in% terminologies) {

    cat("[i] Extracting OPCS-4\n")
    opcs <- fread(file.path(dir, "OPCS410 Data files txt", "OPCS410 CodesAndTitles Nov 2022 V1.0.txt"), col.names = c("CODE", "DESCRIPTION"), header = FALSE)
    opcs_regex <- paste0(c(regex$all, regex$OPCS4), collapse = "|")
    opcs4 <- opcs_tree(opcs, concept_id = conf$id, regex = opcs_regex)

  } else {

    opcs4 <- NULL

  }


  # -------------------------
  #          ICD9
  # -------------------------
  if ("ICD9" %in% terminologies) {

    cat("[i] Extracting ICD9\n")
    icd9 <- fread(file.path(dir, "ICD9", "icd9.tsv"))
    icd9_regex <- paste0(c(regex$all, regex$ICD9), collapse = "|")
    icd9 <- icd9_tree(icd9, concept_id = conf$id, regex = icd9_regex)

  } else {

    icd9 <- NULL

  }


  # -------------------------
  #          ICD9
  # -------------------------
  if ("ICD9_procedure" %in% terminologies) {

    # read the ICD 9 procedures
    cat("[i] Extracting ICD9 procedures\n")
    icd9_procedures <- fread(file.path(dir, "ICD9", "icd9_procedures.tsv"))
    icd9_procedures_regex <- paste0(c(regex$all, regex$ICD9_procedure), collapse = "|")
    icd9_procedures <- icd9_procedure_tree(icd9_procedures, concept_id = conf$id, regex = icd9_procedures_regex)

  } else {

    icd9_procedures <- NULL

  }


  # -------------------------
  # combine & annotate counts
  # -------------------------
  # NHS digital & UKBB ICD10, OPCS4 and SNOMED counts saved in package internal data objects `nhs_counts` and `ukbb_counts`
  tree <- list(snomed, snomed_procedure, icd10, opcs4, icd9, icd9_procedures)
  tree <- tree[!sapply(tree, is.null)]
  tree <- annotate_tree(tree = tree, annot_dt = nhs_counts, annot_name = "nhs_count_per100k_episodes", value_col = "per_100k_episodes", on = c("code" = "code", "code_type" = "code_type"), no_match = 0)
  tree <- annotate_tree(tree = tree, annot_dt = ukbb_counts, annot_name = "ukbb_count_per100k_episodes", value_col = "per_100k_episodes", on = c("code" = "code", "code_type" = "code_type"), no_match = 0)
  tree <- annotate_tree(tree = tree, annot_dt = nhs_counts, annot_name = "gp_count_per100k_patients", value_col = "per_100k_patients", on = c("code" = "code", "code_type" = "code_type"), no_match = 0)


  # -------------------------
  #   Final concept tree
  # -------------------------
  concept_tree <- TreeNode(text = conf$name,
                           type = "root",
                           data = list(code       = conf$name,
                                       code_type  = "concept",
                                       desc       = NULL,
                                       concept_id = conf$id),
                           checked  = FALSE,
                           selected = FALSE,
                           opened   = TRUE,
                           disabled = TRUE,
                           children = tree)

  # save RDS
  saveRDS(concept_tree, outfile)


  # populate the CODE database
  these_codes <- data.table::data.table(CODE_DESC = as.character(tree_attributes(list(concept_tree), "desc")),
                                        CODE      = as.character(tree_attributes(list(concept_tree), "code")),
                                        DISABLED  = unlist(tree_attributes(list(concept_tree), "disabled")),
                                        CODE_TYPE = as.character(tree_attributes(list(concept_tree), "code_type")))
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

}, .env_globals = environment()) # end future loop




