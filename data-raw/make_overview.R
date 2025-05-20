library(data.table)
library(yaml)
library(readxl)


concept_dir <- system.file("concepts", package = )

# Find all YAML files in inst/ recursively
yaml_files <- list.files(inst_dir, pattern = "\\.ya?ml$", recursive = TRUE, full.names = TRUE)

# Initialize results list
results <- list()

# Function to extract ontology info (customize pattern as needed)
extract_ontologies <- function(parsed_yaml) {
  # Try to find ontology-related fields; adapt this logic to your structure
  keys <- names(unlist(parsed_yaml))
  ontology_keys <- keys[str_detect(keys, regex("ontology|electronic.counting", ignore_case = TRUE))]
  unique(unlist(parsed_yaml[ontology_keys]))
}

# Process each YAML file
for (file in yaml_files) {
  parsed <- tryCatch(yaml::read_yaml(file), error = function(e) NULL)

  if (!is.null(parsed)) {
    name <- parsed$name %||% basename(file)  # Use `name` field or fallback to file name
    ontologies <- extract_ontologies(parsed)
    if (is.null(ontologies) || length(ontologies) == 0) {
      ontologies <- NA_character_
    }

    results[[length(results) + 1]] <- list(
      file = file,
      name = name,
      ontologies = paste(ontologies, collapse = "; ")
    )
  }
}

# Convert to data.frame
df <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors = FALSE))

# Write to Excel
output_file <- "ontology_summary.xlsx"
openxlsx::write.xlsx(df, output_file)

cat("Ontology summary written to", output_file, "\n")
