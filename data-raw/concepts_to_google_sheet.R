## code to prepare the `concepts` datasets goes here

# requirements
library(Rdiagnosislist)
library(data.table)
library(xml2)
library(googlesheets4)
library(googledrive)
devtools::load_all()
# source(system.file("data-raw", "make_trees.R", package = "hfphenotyping"))
source("/Users/xx20081/git/hfphenotyping/data-raw/make_trees.R")


# google drive
drive_auth()


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
project_files <- list.files("/Users/xx20081/git/hfphenotyping/inst/concepts", pattern = ".*esc_guideline\\.(yaml|yml)$", full.names = TRUE)
configs <- list.files("/Users/xx20081/git/hfphenotyping/inst/concepts", pattern = ".*\\.(yaml|yml)$", recursive = TRUE, full.names = TRUE)
configs <- configs[!file.info(configs)$isdir & !configs %in% project_files]
configs <- configs[grepl("esc_", configs)]

# produce the trees for each config
for (config in configs[[5]]) {

  # read the config data
  conf          <- yaml::read_yaml(config)
  outfile       <- file.path(dirname(config), paste0(conf$id, ".RDS"))
  regex         <- conf$regexes[!sapply(conf$regexes, function(x) is.null(x[[1]]))]
  regex         <- lapply(regex, function(x) paste0("(", x, ")", collapse = "|"))
  terminologies <- conf$terminology

  # if a derived concept, exit here
  if (conf$domain == "Derived") next

  # extract
  cat("Extracting `", conf$id, "`\n")
  tree <- list(snomed           = NULL,
               snomed_procedure = NULL,
               icd11            = NULL,
               icd10            = NULL,
               opcs4            = NULL,
               cpt4             = NULL,
               icd9             = NULL,
               icd9_procedures  = NULL)


  snomed_regex <- paste0(c(regex$all, regex$SNOMED), collapse = "|")
  concept      <- SNOMEDconcept(snomed_regex, SNOMED = SNOMED, exact = FALSE, active_only = FALSE)

  if ("SNOMED" %in% terminologies) {
    cat("[i] Extracting SNOMED\n")
    filt             <- c("disorder", "finding", "observable entity", "situation", "event")
    hierarch_codes   <- showCodelistHierarchy(description(concept)[grepl(paste0(filt, collapse = "|"), term), conceptId])
    tree[["snomed"]] <- snomed_tree(hierarch_codes, concept_id = conf$id)
  }

  if ("SNOMED_procedure" %in% terminologies) {
    cat("[i] Extracting SNOMED procdures\n")
    filt                       <- c("procedure")
    hierarch_codes             <- showCodelistHierarchy(description(concept)[grepl(paste0(filt, collapse = "|"), term), conceptId])
    tree[["snomed_procedure"]] <- snomed_tree(hierarch_codes, concept_id = conf$id)
  }

  if ("ICD10" %in% terminologies) {
    cat("[i] Extracting ICD-10\n")
    icd10_xml_file  <- file.path(dir, "FY24-CMS-1785-F-ICD-10-Table-Index", "icd10cm_tabular_2024.xml")
    icd10           <- xml2::read_xml(icd10_xml_file)
    icd10           <- xml2::as_list(icd10)
    icd10           <- icd10[[1]]
    icd10_regex     <- paste0(c(regex$all, regex$ICD10), collapse = "|")
    tree[["icd10"]] <- make_icd10_tree(icd10, concept_id = conf$id, regex = icd10_regex)
  }

  if ("ICD11" %in% terminologies) {
    cat("[i] Extracting ICD-11\n")
    icd11_path      <- file.path(dir, "ICD11", "SimpleTabulation-ICD-11-MMS-en.txt") # think I removed the datetime stamp from the header row for reading columns correctly
    icd11_dt        <- data.table::fread(icd11_path)
    icd11_regex     <- paste0(c(regex$all, regex$ICD11), collapse = "|")
    tree[["icd11"]] <- make_icd11_tree(icd11_dt, concept_id = conf$id, regex = icd11_regex)
  }

  if ("OPCS4" %in% terminologies) {
    cat("[i] Extracting OPCS-4\n")
    opcs            <- fread(file.path(dir, "OPCS410 Data files txt", "OPCS410 CodesAndTitles Nov 2022 V1.0.txt"), col.names = c("CODE", "DESCRIPTION"), header = FALSE)
    opcs_regex      <- paste0(c(regex$all, regex$OPCS4), collapse = "|")
    tree[["opcs4"]] <- opcs_tree(opcs, concept_id = conf$id, regex = opcs_regex)
  }

  if ("ICD9" %in% terminologies) {
    cat("[i] Extracting ICD9\n")
    icd9           <- fread(file.path(dir, "ICD9", "icd9.tsv"))
    icd9_regex     <- paste0(c(regex$all, regex$ICD9), collapse = "|")
    tree[["icd9"]] <- icd9_tree(icd9, concept_id = conf$id, regex = icd9_regex)
  }

  if ("ICD9_procedure" %in% terminologies) {
    cat("[i] Extracting ICD9 procedures\n")
    icd9_procedures           <- fread(file.path(dir, "ICD9", "icd9_procedures.tsv"))
    icd9_procedures_regex     <- paste0(c(regex$all, regex$ICD9_procedure), collapse = "|")
    tree[["icd9_procedures"]] <- icd9_procedure_tree(icd9_procedures, concept_id = conf$id, regex = icd9_procedures_regex)
  }

  if ("CPT4" %in% terminologies) {
    cat("[i] Extracting CPT4 procedures\n")
    #cpt4_dt <- data.table::as.data.table(readxl::read_xlsx(file.path(dir, "CPT4", "cpt-pcm-nhsn.xlsx"), sheet = "ALL 2024 CPT Codes"))[, .(code = `CPT Codes`, desc = `Procedure Code Descriptions`, code_type = "CPT4", chapter = `Procedure Code Category`)]
    cpt4_athena_codes <- data.table::fread(file.path(dir, "CPT4", "athena_vocabulary_download_v5", "CONCEPT_CPT4.csv"))
    cpt4_athena_desc  <- data.table::fread(file.path(dir, "CPT4", "athena_vocabulary_download_v5", "CONCEPT_SYNONYM.csv"))
    cpt4_athena_codes[cpt4_athena_desc, desc := i.concept_synonym_name, on = "concept_id"]
    cpt4_dt        <- cpt4_athena_codes[!is.na(desc)][, list(code = concept_code, desc = desc, code_type = "CPT4", chapter = "Procedures")]
    cpt4_regex     <- paste0(c(regex$all, regex$CPT4), collapse = "|")
    tree[["cpt4"]] <- cpt4_tree(cpt4_dt, concept_id = conf$id, regex = cpt4_regex)
  }


  # to data.tables
  tree <- tree[!sapply(tree, is.null)]
  pheno_tree <- lapply(seq_along(tree), function(i) {
    data.table(concept     = unlist(tree_attributes(tree[i], "concept_id"))[-1],
               source      = unlist(tree_attributes(tree[i], "code_type"))[-1],
               code        = unlist(tree_attributes(tree[i], "code"))[-1],
               description = unlist(tree_attributes(tree[i], "desc"))[-1])
  }) |> rbindlist()


  # write googlesheet
  pheno_gdoc  <- gs4_find("chadsva_phenotyping_codeconsensus")
  gdoc_sheets <- sheet_names(pheno_gdoc)
  if (pheno_tree$concept[1] %in% gdoc_sheets) pheno_gdoc |> sheet_delete(sheet = pheno_tree$concept[1])
  pheno_gdoc |>
    sheet_add(sheet = pheno_tree$concept[1]) |>
    sheet_write(data = pheno_tree, sheet = pheno_tree$concept[1])

}




