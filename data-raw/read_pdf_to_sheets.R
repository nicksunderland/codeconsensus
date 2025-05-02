library(tabulapdf)
library(data.table)
library(googlesheets4)
library(googledrive)

drive_auth()

DaRe2THINK <- list(
  atrial_fibrillation            = list(pages = 3:4),
  stroke                         = list(pages = 10:12),
  #tia = list(pages = 13),
  #arterial_thromboembolism = list(pages = 14:16),
  myocardial_infarction          = list(pages = 17:19),
  penetrating_ulcer_of_aorta     = list(pages = 21),
  atherosclerosis_pvd            = list(pages = 20),
  diabetes_mellitus              = list(pages = 22:34),
  systemic_arterial_hypertension = list(pages = 51:53)
)

pheno_gdoc  <- gs4_find("chadsva_phenotyping_codeconsensus")
gdoc_sheets <- sheet_names(pheno_gdoc)
pheno_gdoc2  <- gs4_find("chadsva_phenotyping$")
gdoc_sheets2 <- sheet_names(pheno_gdoc2)

for (i in seq_along(DaRe2THINK)) {
  try({
    pdf_table <- lapply(extract_tables("/Users/xx20081/Downloads/ztac046_supplementary_data/D2Tcoding_Appendix1_codelists_23May2022.pdf",
                                       pages = DaRe2THINK[[i]][["pages"]], col_names=FALSE),
                       function(x) {
                         desc <- x[[1]]
                         #desc <- desc[grepl("^[A-Z]", desc) & !is.na(desc)]
                         code <- x[[3]]
                         #code <- code[!is.na(code)]
                         d <- data.table(description = as.character(desc), code = as.character(code))

                         counter <- 0
                         within_lines <- FALSE
                         start <- FALSE
                         for (ii in seq_len(nrow(d))) {

                           if (d[ii, !is.na(description) & !is.na(code)]) {
                             start <- TRUE
                             counter <- counter + 1
                             d[ii, group := counter]
                           } else if (d[ii, is.na(code)] & within_lines==FALSE & start) {
                             counter <- counter + 1
                             within_lines <- TRUE
                             d[ii, group := counter]
                           } else if (d[ii, !is.na(description)] & start) {
                             d[ii, group := counter]
                             within_lines <- FALSE
                           } else if (within_lines == TRUE & start) {
                             d[ii, group := counter]
                           }
                         }
                         d <- d[, .(description = paste0(na.omit(description), collapse=" "),
                                    source = "SNOMED",
                                    code = paste0(na.omit(code), collapse=" ")), by="group"]
                        d <- d[!is.na(group)]
                        d[, group := NULL]
                       }) |> rbindlist()

    sheet_name <- grep(names(DaRe2THINK)[i], gdoc_sheets, value = TRUE)
    sheet_dat <- pheno_gdoc |> read_sheet(sheet_name) |> as.data.table()
    sheet_dat[, `:=`(code = as.character(code), description = as.character(description))]
    sheet_dat <- (merge(sheet_dat, pdf_table[, .(code, description)], by = "code", all = TRUE)
                 [, concept := concept[!is.na(concept)][1]]
                 [is.na(source), source := "SNOMED"]
                 [, `:=`(CODEconsensus = !is.na(description.x), DaRe2THINK = !is.na(description.y))]
                 [, description := fcoalesce(description.x, description.y)]
                 [, `:=`(description.x = NULL, description.y = NULL)])

    if (sheet_name %in% gdoc_sheets2) pheno_gdoc2 |> sheet_delete(sheet = sheet_name)
    pheno_gdoc2 |>
      sheet_add(sheet = sheet_name) |>
      sheet_write(data = sheet_dat, sheet = sheet_name)
  })
}




