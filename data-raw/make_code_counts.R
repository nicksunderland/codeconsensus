# -------------------------
# UKBB CODE COUNTS
# -------------------------

# UKBB ICD10
ukbb_icd10 <- data.table::fread("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/UKBB_codes/hesin_diag.txt")
n_episodes <- nrow(ukbb_icd10)
ukbb_icd10 <- ukbb_icd10[, .(code = diag_icd10, desc = NA, code_type = "ICD10", count = .N, per_100k_episodes = round(.N / (n_episodes/100000))), by = .(diag_icd10)][, diag_icd10 := NULL]
data.table::fwrite(ukbb_icd10, "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/UKBB_codes/icd10_counts.tsv", sep = "\t")

# UKBB OPCS4
ukbb_opcs4 <- data.table::fread("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/UKBB_codes/hesin_oper.txt")
ukbb_opcs4 <- ukbb_opcs4[, .(code = oper4, desc = NA, code_type = "OPCS4", count = .N, per_100k_episodes = round(.N / (n_episodes/100000))), by = .(oper4)][, oper4 := NULL]
data.table::fwrite(ukbb_opcs4, "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/UKBB_codes/opcs4_counts.tsv", sep = "\t")

# combine and save
ukbb_counts <- rbind(ukbb_icd10, ukbb_opcs4)
usethis::use_data(ukbb_counts, overwrite = TRUE)


# -------------------------
# NHS DIGITAL CODE COUNTS
# -------------------------

# UK HES ICD10
# https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2022-23
nhs_icd10 <- rbind(
  data.table::as.data.table(readxl::read_xlsx("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/NHS_digital_code_counts/hosp-epis-stat-admi-diag-2022-23-tab_V2.xlsx", sheet = "All Diagnoses 4 Character", range = "A12:H11337"))[, .(code = `...1`, desc = `...2`, code_type = "ICD10", count = `...8`)],
  data.table::as.data.table(readxl::read_xlsx("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/NHS_digital_code_counts/hosp-epis-stat-admi-diag-2022-23-tab_V2.xlsx", sheet = "All Diagnoses 3 Character", range = "A12:I2051"))[, .(code = `...2`, desc = `...3`, code_type = "ICD10", count = `...9`)]
)
nhs_icd10[, code := sub("\\.", "", code)]
n_episodes <- 20028271
nhs_icd10[, per_100k_episodes := round(count / (n_episodes/100000))]
data.table::fwrite(nhs_icd10, "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/NHS_digital_code_counts/icd10_counts.tsv", sep = "\t")

# UK HES OPCS
# https://digital.nhs.uk/data-and-information/publications/statistical/hospital-admitted-patient-care-activity/2022-23
nhs_opcs4 <- rbind(
  data.table::as.data.table(readxl::read_xlsx("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/NHS_digital_code_counts/hosp-epis-stat-admi-proc-2022-23-tab-V2.xlsx", sheet = "All Procedure 4 Character", range = "A11:H9034"))[, .(code = `...1`, desc = `...2`, code_type = "OPCS4", count = `...8`)],
  data.table::as.data.table(readxl::read_xlsx("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/NHS_digital_code_counts/hosp-epis-stat-admi-proc-2022-23-tab-V2.xlsx", sheet = "All Procedure 3 Character", range = "A11:H1562"))[, .(code = `...1`, desc = `...2`, code_type = "OPCS4", count = `...8`)]
)
nhs_opcs4[, code := sub("\\.", "", code)]
n_episodes <- 20028271
nhs_icd10[, per_100k_episodes := round(count / (n_episodes/100000))]
data.table::fwrite(nhs_opcs4, "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/NHS_digital_code_counts/opcs4_counts.tsv", sep = "\t")

# UK GP SNOMED
# https://digital.nhs.uk/data-and-information/publications/statistical/mi-snomed-code-usage-in-primary-care/2022-23
gp_snomed <- data.table::fread("/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/NHS_digital_code_counts/SNOMED_code_usage_2022-23.txt")
gp_snomed <- gp_snomed[, .(code = as.character(SNOMED_Concept_ID), desc = Description, code_type = "SNOMED", count = as.numeric(Usage))]
n_patients <- 62288202
gp_snomed[, per_100k_patients := round(count / (n_patients/100000))]
data.table::fwrite(gp_snomed, "/Users/xx20081/Library/CloudStorage/OneDrive-UniversityofBristol/phenotyping/NHS_digital_code_counts/snomed_counts.tsv", sep = "\t")

# combine and save
nhs_counts <- rbind(nhs_icd10, nhs_opcs4, gp_snomed, fill = TRUE)
usethis::use_data(nhs_counts, overwrite = TRUE)

