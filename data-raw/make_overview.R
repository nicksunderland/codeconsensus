library(devtools)
load_all()

library(data.table)
library(yaml)
library(openxlsx)

# concept directory
concept_dir <- system.file("concepts", package = "codeconsensus")


# project files
proj_files        <- list.files(concept_dir, pattern = "\\.ya?ml$", recursive = FALSE, full.names = TRUE)
names(proj_files) <- sub("\\.ya?ml$", "", basename(proj_files))


# concept files
concept_files <- lapply(proj_files, function(f) {
  yml       <- read_yaml(f)
  con_files <- file.path(dirname(f), yml$id, paste0(yml$concepts, ".yaml"))
  cons      <- lapply(con_files, function(cf) {
    cyml <- read_yaml(cf)
    d    <- as.data.table(c(list(project = yml$id), cyml[c("id", "name")]))
    for (term in cyml$terminology) d[, (term) := "yes"]
    d
  }) |> rbindlist(fill = TRUE)
}) |> rbindlist(fill = TRUE)


# write to excel
write.xlsx(concept_files, file.path(concept_dir, "concept_summary.xlsx"))

