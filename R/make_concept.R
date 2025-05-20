#' Make Concept Code List
#'
#' @param concept_yml the file path to the concept yaml
#' @param output_dir the output directory to save to
#' @param ehr_dict_root the directory containing the EHR dictionaries
#' @param format the output format one or most of c("list", "excel", "tree")
#'
#' @importFrom Rdiagnosislist loadSNOMED SNOMEDconcept showCodelistHierarchy
#' @importFrom yaml read_yaml
#' @importFrom xml2 read_xml as_list
#' @import data.table
#' @importFrom openxlsx createWorkbook loadWorkbook addWorksheet writeData saveWorkbook
#'
#' @returns codes lists in the specified formats
#' @export
#'
make_concept <- function(concept_yml, output_dir, ehr_dict_root = "", format=c("list", "excel", "tree")) {

  # testing
  if (FALSE) {
    concept_yml = "/Users/xx20081/git/codeconsensus/inst/concepts/esc_guideline/esc_angina_pectoris.yaml"
    ehr_dict_root = "/Users/xx20081/Library/CloudStorage/OneDrive-SharedLibraries-UniversityCollegeLondon/IHI.CompMed - Documents/PHENOTYPING/ehr_dictionaries"
    output_dir = "/Users/xx20081/Desktop/testing_codeconsensus"
    overwrite = TRUE
    format = c("list", "excel", "tree")
  }

  # checks
  format <- match.arg(format, choices = c("list", "tree", "excel"), several.ok = TRUE)
  stopifnot("`ehr_dict_root` must be a valid directory" = dir.exists(ehr_dict_root))
  stopifnot("`concept_yml` must be a valid yaml file" = file.exists(concept_yml) && grepl("\\.ya?ml$", concept_yml))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)


  # first we need the data sources, which are the SNOMED, ICD-10, and OPCS codes from the NHS
  # TRUD website (https://isd.digital.nhs.uk/trud). You will need an account set up.
  # These should be downloaded into directories outside this package (because they are large)
  SNOMED_RDS <- file.path(ehr_dict_root, "SNOMED.RDS")

  # load the SNOMED dictionaries
  if (!file.exists(SNOMED_RDS)) {
    SNOMED <- Rdiagnosislist::loadSNOMED(list.dirs(file.path(dir, "uk_sct2cl_38.0.0_20240410000001Z")))
    saveRDS(SNOMED, file = SNOMED_RDS)
  } else {
    SNOMED <- readRDS(SNOMED_RDS)
  }
  assign("SNOMED", SNOMED, envir = globalenv()) # hack to force Rdiagnosislist to work in a function


  # read the config
  conf          <- yaml::read_yaml(concept_yml)
  regex         <- conf$regexes[!sapply(conf$regexes, function(x) is.null(x[[1]]))]
  regex         <- lapply(regex, function(x) paste0("(", x, ")", collapse = "|"))
  terminologies <- conf$terminology


  # if a derived concept, exit here
  if (conf$domain == "Derived") next


  # extract from dictionaries
  cat("Extracting `", conf$id, "`\n")


  # -------------------------
  #          SNOMED
  # -------------------------
  snomed_regex <- paste0(c(regex$all, regex$SNOMED), collapse = "|")
  concept      <- Rdiagnosislist::SNOMEDconcept(snomed_regex, SNOMED = SNOMED, exact = FALSE, active_only = FALSE)

  if ("SNOMED" %in% terminologies) {
    cat("[i] Extracting SNOMED\n")

    filt <- c("disorder", "finding", "observable entity", "situation", "event")
    snomed_desc    <- Rdiagnosislist::description(concept, SNOMED = SNOMED)
    snomed_desc    <- snomed_desc[grepl(paste0(filt, collapse = "|"), term), conceptId]
    hierarch_codes <- Rdiagnosislist::showCodelistHierarchy(snomed_desc, SNOMED = SNOMED)
    snomed         <- snomed_tree(hierarch_codes, concept_id = conf$id)

  } else {

    snomed <- NULL

  }


  if ("SNOMED_procedure" %in% terminologies) {
    cat("[i] Extracting SNOMED procdures\n")

    filt <- c("procedure")
    hierarch_codes   <- Rdiagnosislist::showCodelistHierarchy(Rdiagnosislist::description(concept, SNOMED = SNOMED)[grepl(paste0(filt, collapse = "|"), term), conceptId], SNOMED = SNOMED)
    snomed_procedure <- snomed_tree(hierarch_codes, concept_id = conf$id)

  } else {

    snomed_procedure <- NULL

  }


  # -------------------------
  #          ICD10
  # -------------------------
  if ("ICD10" %in% terminologies) {

    cat("[i] Extracting ICD-10\n")
    icd10_xml_file <- file.path(ehr_dict_root, "FY24-CMS-1785-F-ICD-10-Table-Index", "icd10cm_tabular_2024.xml")
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
    icd11_path <- file.path(ehr_dict_root, "ICD11", "SimpleTabulation-ICD-11-MMS-en.txt") # think I removed the datetime stamp from the header row for reading columns correctly
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
    opcs <- fread(file.path(ehr_dict_root, "OPCS410 Data files txt", "OPCS410 CodesAndTitles Nov 2022 V1.0.txt"), col.names = c("CODE", "DESCRIPTION"), header = FALSE)
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
    icd9 <- fread(file.path(ehr_dict_root, "ICD9", "icd9.tsv"))
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
    icd9_procedures <- fread(file.path(ehr_dict_root, "ICD9", "icd9_procedures.tsv"))
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

    cpt4_athena_codes <- data.table::fread(file.path(ehr_dict_root, "CPT4", "athena_vocabulary_download_v5", "CONCEPT_CPT4.csv"))
    cpt4_athena_desc  <- data.table::fread(file.path(ehr_dict_root, "CPT4", "athena_vocabulary_download_v5", "CONCEPT_SYNONYM.csv"))
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


  # saving tree
  if ("tree" %in% format) {
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
    outfile_tree  <- file.path(output_dir, paste0(conf$id, ".RDS"))
    saveRDS(concept_tree, outfile_tree)
  }

  # saving flat list
  if ("list" %in% format | "excel" %in% format) {
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

    if ("list" %in% format) {
      outfile_list <- file.path(output_dir, paste0(conf$id, ".tsv"))
      data.table::fwrite(pheno_tree_dt, outfile_list, sep = "\t")
    }

    if ("excel" %in% format) {

      # save workbook
      outfile_excel <- file.path(output_dir, "concepts.xlsx")
      if (file.exists(outfile_excel)) {
        excel_wb <- openxlsx::loadWorkbook(outfile_excel)
      } else {
        excel_wb <- openxlsx::createWorkbook()
      }
      openxlsx::addWorksheet(excel_wb, conf$id)
      openxlsx::writeData(excel_wb, conf$id, pheno_tree_dt)
      openxlsx::saveWorkbook(excel_wb, file = outfile_excel, overwrite = TRUE)
    }
  }

}
