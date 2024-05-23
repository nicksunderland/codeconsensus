#' concept UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyTree shinyTree dfToTree
#' @import data.table
#' @import Rdiagnosislist
mod_concept_ui <- function(id, title, definition, pmid, domain, terminology, concept_term, valueset_definition, regexes){
  ns <- NS(id)

  # the concept panel
  tabPanel(title = title,
           # the information box
           div(
             style = "background-color: #f7f7f7; border: 1px solid #ddd; padding: 10px; margin-bottom: 20px;",
             h3(paste(id, "definition:")),
             p(definition),
             h5("Domain:",              span(domain,                          style = "font-weight: normal;")),
             h5("Terminology:",         span(terminology,                     style = "font-weight: normal;")),
             h5("Concept term:",        span(concept_term,                    style = "font-weight: normal;")),
             h5("ValueSet definition:", span(valueset_definition,             style = "font-weight: normal;")),
             h5("Search expressions:",  span(paste0(regexes, collapse = "|"), style = "font-weight: normal;"))
           ),
           textAreaInput(ns("user_comments"), label = "Comments", width = "100%"),
           # the codes table
           fluidRow(column(2, actionButton(ns("save"), "Save")),
                    column(10, textOutput(ns("message_box")))),
           shinyTree::shinyTree(ns("tree"), theme="proton", wholerow = FALSE, search = F, unique = FALSE, checkbox = TRUE),
           hr(),
           "Currently Selected:",
           verbatimTextOutput(ns("sel_names"))
           # DT::dataTableOutput(ns("code_table")))
  )

}

#' concept Server Functions
#'
#' @noRd
mod_concept_server <- function(id, regexes, user){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # reactive values
    selec_list <- reactiveVal(NULL)

    # update reactive value when tree changes
    observeEvent(input$tree, {
      output$message_box <- renderText("")
      selec_list(shinyTree::get_selected(input$tree))
    })

    observeEvent(input$user_comments, {
      saved_data <- votes()
      comments   <- saved_data[CONCEPTID == "COMMENTS", SELECTED]
      output$user_comments <- renderText(comments)
      output$message_box <- renderText("")
    })

    # save button
    observeEvent(input$save, {

      # get the selected in a list (named with CONCEPT_STR)
      selected <- selec_list()

      # get all the (unique) codes and see which are selected
      selec_dat <- hierarch_codes()[, .(CONCEPTID, SELECTED = any(CONCEPT_STR %in% selected)), by = "CONCEPTID"]

      # the voting db_table
      db_vote <- paste0(toupper(gsub("[^A-Za-z0-9_]+", "_", id)), "_VOTE")

      # delete previous entries
      query_db(paste0("DELETE FROM ", db_vote, " WHERE USERNAME = '", user, "'"), type = "send")
      check <- query_db(paste0("SELECT COUNT(*) AS user_count FROM ",  db_vote, " WHERE USERNAME = '", user, "'"), type = "get")
      if (check$USER_COUNT != 0) {
        output$message_box <- renderText("Error removing previous data")
      }

      # send updated row
      sql <- paste0("INSERT INTO ", db_vote, " ( USERNAME, COMMENTS, ", paste0("CID_", selec_dat$CONCEPTID, collapse = ", "), ")",
                    "VALUES ( '", user,  "', '", input$user_comments, "', ", paste0("'", selec_dat$SELECTED, "'", collapse = ", "), " )")
      safe_pattern <- "^[a-zA-Z0-9_;\\s\\,\\.\\*\\=\\<\\>\\'\\%\\(\\)]+$"
      if (!grepl(safe_pattern, sql, perl = TRUE)) {
        output$message_box <- renderText("Invalid characters in comments box - use only [A-Z0-9.,%<>]")
        return()
      }
      query_db(sql, type = "send")

      # check
      check <- query_db(paste0("SELECT COUNT(*) AS user_count FROM ",  db_vote, " WHERE USERNAME = '", user, "'"), type = "get")
      if (check$USER_COUNT > 0) {
        output$message_box <- renderText("Successfully saved")
      } else {
        output$message_box <- renderText("Error saving data")
      }

    })

    # revert button
    observeEvent(input$revert, {
      # Your action when the button is pressed
      output$message <- renderText("Save button pressed!")

    })

    # reactive hierarch_codes table
    hierarch_codes <- reactive({

      # get the database code tables
      db_tables <- query_db("SELECT table_name FROM all_tables WHERE owner = 'SHINY'")

      # db_name for this concept and the db_name_votes
      db_name <- toupper(gsub("[^A-Za-z0-9_]+", "_", id))

      # if doesn't exist CREATE TABLE
      if (!db_name %in% db_tables$TABLE_NAME) {

        db_create <- query_db(paste("CREATE TABLE",
                                    db_name,
                                    "(",
                                    "conceptId VARCHAR2(4000),",
                                    "thisrowid INT,",
                                    "parentrowid INT,",
                                    "childrowid VARCHAR2(4000),",
                                    "icd10_code VARCHAR2(4000),",
                                    "opcs4_code VARCHAR2(4000),",
                                    "ctv3_simple VARCHAR2(4000),",
                                    "concept_str VARCHAR2(4000)",
                                    ")"), type = "send")

        # load the SNOMED library
        require(Rdiagnosislist)
        SNOMED <- Rdiagnosislist::sampleSNOMED()

        # get the terms to search for for this concept
        regex    <- paste0(regexes, collapse = "|")

        # generate the concept
        concept  <- Rdiagnosislist::SNOMEDconcept(x = regex, exact_match = FALSE, active_only = FALSE, SNOMED = SNOMED)

        # get the mapping to other coding systems
        mappings <- c("icd10" = "icd10_code", "opcs4" = "opcs4_code", "ctv3simple" = "ctv3_simple")
        maps     <- Rdiagnosislist::getMaps(concept, to = names(mappings)) |> data.table::as.data.table()
        maps[, conceptId := as.character(conceptId)]
        maps[, unname(mappings) := lapply(.SD, function(x) sapply(x, paste, collapse = ",")), .SDcols = unname(mappings)]
        maps[, unname(mappings) := lapply(.SD, function(x) replace(x, x == "", NA_character_)), .SDcols = unname(mappings)]

        # convert to hierarchy table
        hierarch_codes <- Rdiagnosislist::showCodelistHierarchy(concept) |> data.table::as.data.table()
        hierarch_codes[, conceptId := as.character(conceptId)]
        hierarch_codes[, conceptId_lab := ifelse(.N > 1, paste0(conceptId, "*"), conceptId), by = "conceptId"] # is duplicated somewhere

        # join the other coding systems
        hierarch_codes[maps, unname(mappings) := mget(unname(mappings)), on = "conceptId"]
        hierarch_codes[, concept_str := apply(.SD, 1, function(row) paste0(row[!is.na(row)], collapse = " | ")), .SDcols = c("conceptId_lab", "term", unname(mappings))]
        hierarch_codes[, conceptId_lab := NULL]

        # rename for database
        db_cols <- c("conceptId", "rowid", "parentrowid", "childrowid", "icd10_code", "opcs4_code", "ctv3_simple", "concept_str")
        hierarch_codes[, names(hierarch_codes)[!names(hierarch_codes) %in% db_cols] := NULL]
        data.table::setnames(hierarch_codes, names(hierarch_codes), toupper(names(hierarch_codes)))
        data.table::setnames(hierarch_codes, "ROWID", "THISROWID") # ROWID is a reserved name

        # sort out storage of multiple integers
        hierarch_codes[, CHILDROWID := lapply(CHILDROWID, function(x) ifelse(length(x) == 0, NA_character_, paste0(x, collapse=",")))]

        # write to database
        db_write <- query_db(type = "write", name = db_name, value = hierarch_codes, overwrite = TRUE)

      }

      # read the hierarchical codes table
      hierarch_codes <- query_db(paste("SELECT * FROM", db_name), type = "get")
      hierarch_codes[, THISROWID   := as.integer(THISROWID)]
      hierarch_codes[, PARENTROWID := as.integer(PARENTROWID)]
      hierarch_codes[, CHILDROWID  := lapply(CHILDROWID, function(x) {
        if (is.na(x)) {
          integer(0L)
        } else {
          as.integer(unlist(strsplit(x, ",")))
        }
      })]

      return(hierarch_codes)
    })

    # get the
    votes <- reactive({

      # get the database code tables
      db_tables <- query_db("SELECT table_name FROM all_tables WHERE owner = 'SHINY'")

      # db_name for this concept and the db_name_votes
      db_vote <- paste0(toupper(gsub("[^A-Za-z0-9_]+", "_", id)), "_VOTE")

      # get the codes
      hc <- hierarch_codes()

      # if the voting table doesnt exist, create it
      if (!db_vote %in% db_tables$TABLE_NAME) {

        db_create <- query_db(paste("CREATE TABLE",
                                    db_vote,
                                    "(",
                                    "USERNAME VARCHAR2(50),",
                                    "COMMENTS CLOB,",
                                    paste0("CID_", unique(hc$CONCEPTID), " BOOLEAN", collapse = ", "),
                                    ")"), type = "send")

      }

      # read the voting table for this user and join (get default test if no entry yet)
      votes <- query_db(paste0("SELECT * FROM ", db_vote, " WHERE USERNAME = '", user, "'"), type = "get")
      if (nrow(votes) == 0) {
        votes <- query_db(paste0("SELECT * FROM ", db_vote, " WHERE USERNAME = 'test'"), type = "get")
      }

      votes <- data.table::melt.data.table(votes, id.vars = "USERNAME", variable.name =  "CONCEPTID", value.name = "SELECTED")
      votes[, CONCEPTID := sub("^CID_", "", CONCEPTID)]

      return(votes)
    })


    # selection tree
    output$tree <- shinyTree::renderTree({

      # get codes
      hc   <- hierarch_codes()
      tree <- build_tree(hc)

      # get selections for this user
      sel <- votes()

      # join
      hc[sel, SELECTED := as.logical(i.SELECTED), on = "CONCEPTID"]

      # apply
      tree <- modify_selected(tree, hc)

      return(tree)
    })




  })
}


# function
build_tree <- function(hierarch_codes, row_id = NA) {

  # start case
  if (is.na(row_id)) {
    children  <- hierarch_codes[is.na(hierarch_codes$PARENTROWID), THISROWID] # children of the top of the tree are the start enteries
    hierarchy <- list()

    # Loop through each top-level child and build the hierarchy
    for (child_row_id in children) {
      child_hierarchy <- build_tree(hierarch_codes, child_row_id)
      hierarchy       <- c(hierarchy, child_hierarchy)
    }

    return(hierarchy)

  } else {


    # Get the children of the current row_id
    children    <- hierarch_codes[hierarch_codes$THISROWID == row_id, CHILDROWID][[1]]
    concept_str <- hierarch_codes[THISROWID == row_id, CONCEPT_STR]

    # If there are no children, return the current row as a list
    if (length(children) == 0) {
      return(setNames(list(c("")), concept_str))
    }

    # otherwise, recursively build the list of children
    children_list <- lapply(children, function(child_row_id) {
      build_tree(hierarch_codes, child_row_id)
    })
    children_list <- do.call(c, children_list)

    # Return the current row as a list with nested children
    return(setNames(list(children_list), concept_str))
  }
}



# recursively set selected or not
modify_selected <- function(tree, hierarch_codes) {
  for (name in names(tree)) {
    if (is.list(tree[[name]])) {
      tree[[name]] <- modify_selected(tree[[name]], hierarch_codes) # recursively process
      selected_value <- any(hierarch_codes[CONCEPT_STR == name, SELECTED])
      attr(tree[[name]], "stselected") <- selected_value
      attr(tree[[name]], "stopened") <- TRUE
    } else {
      selected_value <- any(hierarch_codes[CONCEPT_STR == name, SELECTED])
      attr(tree[[name]], "stselected") <- selected_value
      attr(tree[[name]], "stopened") <- TRUE
    }
  }
  return(tree)
}


## To be copied in the UI
# mod_concept_ui("concept_1")

## To be copied in the server
# mod_concept_server("concept_1")
