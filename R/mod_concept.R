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
             h5("Search expressions:\n",span(paste0(regexes, collapse = " | "), style = "font-weight: normal;"))
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

#' @title concept Server Functions
#' @param id string, id of this module
#' @param regexes string, regex pattern
#' @param user string, the current user
#' @param db_table_names a reactive that lives in the main server function (returns a character table names vector from the database)
#'
#' @noRd
mod_concept_server <- function(id, regexes, user, db_table_names){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # -----------------------------
    # reactive values / expressions
    # -----------------------------
    db_attributes  <- reactive({ data.table::data.table(name    = names(tree_attributes(tree(), "name")),
                                                        db_name = unlist(tree_attributes(tree(), "db_name"))) })

    # the tree of options
    tree <- reactive({
      tree_path <- system.file("concepts", paste0(id, ".RDS"), package = "hfphenotyping")
      tree <- readRDS(tree_path)
      return(tree)
    })

    # the selected options from the database
    db_selected <- reactive({

      # db_name for this concept and the db_name_votes
      db_vote <- toupper(paste0(id, "_VOTE"))

      # if the voting table doesnt exist, create it
      if (!db_vote %in% db_table_names()) {

        db_cols <- tree_attributes(tree(), "db_name")
        db_create <- query_db(paste("CREATE TABLE",
                                    db_vote,
                                    "(",
                                    "USERNAME VARCHAR2(50),",
                                    "COMMENTS CLOB,",
                                    paste0(db_attributes()$db_name, " BOOLEAN", collapse = ", "),
                                    ")"), type = "send")

      }

      # read the voting table for this user and join (get default test if no entry yet)
      votes <- query_db(paste0("SELECT * FROM ", db_vote, " WHERE USERNAME = '", user, "'"), type = "get")

      if (nrow(votes) == 0) {
        votes <- query_db(paste0("SELECT * FROM ", db_vote, " WHERE USERNAME = 'test'"), type = "get")
      }

      votes <- data.table::melt.data.table(votes, id.vars = "USERNAME", variable.name =  "db_name", value.name = "selected", variable.factor = FALSE)
      votes[db_attributes(), name := i.name, on = "db_name"]

      return(votes)
    })

    # save button
    observeEvent(input$save, {

      # the voting db_table
      db_vote <- toupper(paste0(id, "_VOTE"))

      # delete previous entries
      query_db(paste0("DELETE FROM ", db_vote, " WHERE USERNAME = '", user, "'"), type = "send")
      check <- query_db(paste0("SELECT COUNT(*) AS user_count FROM ",  db_vote, " WHERE USERNAME = '", user, "'"), type = "get")
      if (check$USER_COUNT != 0) {
        output$message_box <- renderText("Error removing previous data")
      }

      # get the selected data
      select_dat   <- data.table::data.table(db_name  = tree_attributes(input$tree, "db_name"),
                                             selected = tree_attributes(input$tree, "stselected"))

      # send updated row
      sql <- paste0("INSERT INTO ", db_vote, " ( USERNAME, COMMENTS, ", paste0(select_dat$db_name, collapse = ", "), ")",
                    " VALUES ( '", user,  "', '", input$user_comments, "', ", paste0("'", select_dat$selected, "'", collapse = ", "), " )")
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

    # selection tree
    output$tree <- shinyTree::renderTree({

      # get tree
      tree <- tree()

      # get selections for this user
      sel <- data.table::copy(db_selected())

      # set coments
      comments   <- sel[db_name == "COMMENTS", selected]
      sel <- sel[db_name != "COMMENTS", ]
      sel[, selected := as.logical(as.integer(selected))]
      updateTextAreaInput(session, "user_comments", value = comments)
      updateTextInput(session, "message_box", value = "")

      # apply selection
      tree <- modify_selected(tree, sel)

      return(tree)
    })

  })
}



# recursively set selected or not
modify_selected <- function(tree, selected) {
  for (name_ in names(tree)) {
    if (is.list(tree[[name_]]) && length(tree[[name_]]) > 0) {
      selected_value <- any(selected[name == name_, selected])
      attr(tree[[name_]], "stselected") <- selected_value
      tree[[name_]] <- modify_selected(tree[[name_]], selected) # recursively process
    } else {
      selected_value <- any(selected[name == name_, selected])
      attr(tree[[name_]], "stselected") <- selected_value
    }
  }
  return(tree)
}


## To be copied in the UI
# mod_concept_ui("concept_1")

## To be copied in the server
# mod_concept_server("concept_1")
