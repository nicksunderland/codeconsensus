## code to prepare the `concepts` datasets goes here

# requirements
library(Rdiagnosislist)
library(data.table)
library(xml2)
library(openxlsx)
library(parallel)
devtools::load_all()


###########################
# EDIT HERE: settings
sub_study   <- "nih_outcomes"
dict_dir    <- "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping"
output_xlsx <- "/Users/xx20081/Library/CloudStorage/OneDrive-SharedLibraries-VUMC/Shaffer, Lauren L - Phenotype Workstream/phenotyping_code_consensus/nih_outcome_phenotyping.xlsx"
###########################


# checks
concept_dir     <- system.file("concepts", package = "hfphenotyping")
sub_studies     <- sub("\\.ya?ml$", "", list.files(concept_dir, pattern = "\\.yaml$"))
sub_study       <- match.arg(sub_study, choices = sub_studies)
sub_concept_dir <- file.path(concept_dir, sub_study)
func_script     <- system.file("data-raw", "make_trees.R", package = "hfphenotyping")
source(func_script)

# first we need the data sources, which are the SNOMED, ICD-10, and OPCS codes from the NHS
# TRUD website (https://isd.digital.nhs.uk/trud). You will need an account set up.
# These should be downloaded into directories outside this package (because they are large)
SNOMED_RDS <- file.path(dict_dir, "SNOMED.RDS")

# load the SNOMED dictionaries
if (!file.exists(SNOMED_RDS)) {
  SNOMED <- loadSNOMED(list.dirs(file.path(dir, "uk_sct2cl_38.0.0_20240410000001Z")))
  saveRDS(SNOMED, file = SNOMED_RDS)
} else {
  SNOMED <- readRDS(SNOMED_RDS)
}

# read the config files
configs  <- list.files(sub_concept_dir, pattern = ".*\\.ya?ml$", full.names = TRUE)


# produce the trees for each config
# plan(multisession, workers = parallel::detectCores() - 1)
# options(future.globals.maxSize = 90 * 1024^3)  # 2 GiB
# future_walk(configs, function(config) {

dts <- mclapply(configs, function(config) {

  # read the config
  conf    <- yaml::read_yaml(config)
  outfile <- file.path(dirname(config), paste0(conf$id, ".RDS"))
  regex   <- conf$regexes[!sapply(conf$regexes, function(x) is.null(x[[1]]))]
  regex   <- lapply(regex, function(x) paste0("(", x, ")", collapse = "|"))
  terminologies <- conf$terminology


  # if a derived concept, exit here
  if (conf$domain == "Derived") next


  # extract from dictionaries
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
    icd10_xml_file <- file.path(dict_dir, "FY24-CMS-1785-F-ICD-10-Table-Index", "icd10cm_tabular_2024.xml")
    icd10 <- xml2::read_xml(icd10_xml_file)
    icd10 <- xml2::as_list(icd10)
    icd10 <- icd10[[1]]
    icd10_regex <- paste0(c(regex$all, regex$ICD10), collapse = "|")
    icd10 <- make_icd10_tree(icd10, concept_id = conf$id, regex = icd10_regex)

  } else {

    icd10 <- NULL

  }


  # -------------------------
  #          ICD11
  # -------------------------
  if ("ICD11" %in% terminologies) {

    cat("[i] Extracting ICD-11\n")
    icd11_path <- file.path(dict_dir, "ICD11", "SimpleTabulation-ICD-11-MMS-en.txt") # think I removed the datetime stamp from the header row for reading columns correctly
    icd11_dt <- data.table::fread(icd11_path)
    icd11_regex <- paste0(c(regex$all, regex$ICD11), collapse = "|")
    icd11 <- make_icd11_tree(icd11_dt, concept_id = conf$id, regex = icd11_regex)

  } else {

    icd11 <- NULL

  }



  # -------------------------
  #          OPCS4
  # -------------------------
  if ("OPCS4" %in% terminologies) {

    cat("[i] Extracting OPCS-4\n")
    opcs <- fread(file.path(dict_dir, "OPCS410 Data files txt", "OPCS410 CodesAndTitles Nov 2022 V1.0.txt"), col.names = c("CODE", "DESCRIPTION"), header = FALSE)
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
    icd9 <- fread(file.path(dict_dir, "ICD9", "icd9.tsv"))
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
    icd9_procedures <- fread(file.path(dict_dir, "ICD9", "icd9_procedures.tsv"))
    icd9_procedures_regex <- paste0(c(regex$all, regex$ICD9_procedure), collapse = "|")
    icd9_procedures <- icd9_procedure_tree(icd9_procedures, concept_id = conf$id, regex = icd9_procedures_regex)

  } else {

    icd9_procedures <- NULL

  }


  # -------------------------
  #          CPT4
  # -------------------------
  if ("CPT4" %in% terminologies) {

    cat("[i] Extracting CPT4 procedures\n")
    #cpt4_dt <- data.table::as.data.table(readxl::read_xlsx(file.path(dir, "CPT4", "cpt-pcm-nhsn.xlsx"), sheet = "ALL 2024 CPT Codes"))[, .(code = `CPT Codes`, desc = `Procedure Code Descriptions`, code_type = "CPT4", chapter = `Procedure Code Category`)]

    cpt4_athena_codes <- data.table::fread(file.path(dict_dir, "CPT4", "athena_vocabulary_download_v5", "CONCEPT_CPT4.csv"))
    cpt4_athena_desc  <- data.table::fread(file.path(dict_dir, "CPT4", "athena_vocabulary_download_v5", "CONCEPT_SYNONYM.csv"))
    cpt4_athena_codes[cpt4_athena_desc, desc := i.concept_synonym_name, on = "concept_id"]
    cpt4_dt <- cpt4_athena_codes[!is.na(desc)][, list(code = concept_code, desc = desc, code_type = "CPT4", chapter = "Procedures")]

    cpt4_regex <- paste0(c(regex$all, regex$CPT4), collapse = "|")
    cpt4 <- cpt4_tree(cpt4_dt, concept_id = conf$id, regex = cpt4_regex)

  } else {

    cpt4 <- NULL

  }

  # -------------------------
  # combine & annotate counts
  # -------------------------
  # NHS digital & UKBB ICD10, OPCS4 and SNOMED counts saved in package internal data objects `nhs_counts` and `ukbb_counts`
  tree <- list(snomed, snomed_procedure, icd11, icd10, opcs4, cpt4, icd9, icd9_procedures)
  tree <- tree[!sapply(tree, is.null)]
  tree <- annotate_tree(tree = tree, annot_dt = nhs_counts,   annot_name = "nhs_count_per100k_episodes",  value_col = "per_100k_episodes", on = c("code" = "code", "code_type" = "code_type"), no_match = 0)
  tree <- annotate_tree(tree = tree, annot_dt = ukbb_counts,  annot_name = "ukbb_count_per100k_episodes", value_col = "per_100k_episodes", on = c("code" = "code", "code_type" = "code_type"), no_match = 0)
  tree <- annotate_tree(tree = tree, annot_dt = nhs_counts,   annot_name = "gp_count_per100k_patients",   value_col = "per_100k_patients", on = c("code" = "code", "code_type" = "code_type"), no_match = 0)
  tree <- annotate_tree(tree = tree, annot_dt = nhs_counts,   annot_name = "nhs_count",                   value_col = "count",             on = c("code" = "code", "code_type" = "code_type"), no_match = 0)
  tree <- annotate_tree(tree = tree, annot_dt = ukbb_counts,  annot_name = "ukbb_count",                  value_col = "count",             on = c("code" = "code", "code_type" = "code_type"), no_match = 0)
  tree <- annotate_tree(tree = tree, annot_dt = biovu_counts, annot_name = "biovu_count",                 value_col = "count",             on = c("code" = "code", "code_type" = "code_type"), no_match = 0)


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

  # to data.tables
  pheno_tree_dt <- lapply(seq_along(tree), function(i) {
    data.table(concept     = unlist(tree_attributes(tree[i], "concept_id"))[-1],
               source      = unlist(tree_attributes(tree[i], "code_type"))[-1],
               type        = unlist(tree_attributes(tree[i], "type"))[-1],
               code        = unlist(tree_attributes(tree[i], "code"))[-1],
               description = unlist(tree_attributes(tree[i], "desc"))[-1])
  }) |> rbindlist()

  # only codes
  pheno_tree_dt <- pheno_tree_dt[type=="code"][, type := NULL]

  # return
  pheno_tree_dt
})


# save workbook
excel_wb <- createWorkbook()
for (t in dts) {
  t[, c("LL","QW","NS","TL","Consensus","Comments") := NA_character_]
  sheet_name <- sub("^nih_outcome_", "", t$concept[1])
  addWorksheet(excel_wb, sheet_name)
  writeData(excel_wb, sheet_name, t)
}
saveWorkbook(excel_wb, file = output_xlsx, overwrite = TRUE)
