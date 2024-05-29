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
             h3(paste(title, "definition:")),
             p(definition),
             h5("Domain:",              span(domain,                          style = "font-weight: normal;")),
             h5("Terminology:",         span(terminology,                     style = "font-weight: normal;")),
             h5("Concept term:",        span(concept_term,                    style = "font-weight: normal;")),
             h5("Search expressions:\n",span(paste0(regexes, collapse = " | "), style = "font-weight: normal;"))
           ),
           textAreaInput(ns("user_comments"), label = "Comments", width = "100%"),
           selectInput(ns("plot_type"), label = "Plot selection", choice = c("off", "agreement", "counts", "percentage", "concurrency (counts)", "concurrency (%)")),
           conditionalPanel(condition = paste0("input.plot_type != 'off'"), plotOutput(ns("plot")), ns = ns),
           fluidRow(column(12, tags$label("Save selection"))),
           fluidRow(column(2, actionButton(ns("save"), "Save")),
                    column(10, textOutput(ns("message_box")))),
           shinycssloaders::withSpinner(
             shinyTree::shinyTree(ns("tree"), theme="proton", wholerow = FALSE, search = F, unique = FALSE, checkbox = TRUE, three_state = FALSE),
             type = 8
           )
  )

}

#' @title concept Server Functions
#' @param id string, id of this module
#' @param regexes string, regex pattern
#' @param username string, the current user
#' @param concept_name string, the concept name specified in teh yaml config
#' @param selected_summary a reactive
#' @importFrom glue glue
#' @noRd
mod_concept_server <- function(id, concept_name, regexes, username, selected_summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # -----------------------------
    # reactive values / expressions
    # -----------------------------
    message <- reactiveVal("")

    concept_id <- reactive({
      sql <- glue::glue("SELECT CONCEPT_ID FROM CONCEPTS WHERE CONCEPT_NAME = '{concept_name}'")
      res <- query_db(sql, type = "get")
      return(res$CONCEPT_ID)
    })

    code_ids <- reactive({
      tree_codes <- paste0("'", unlist(tree_attributes(tree(), 'code')), "'", collapse = ", ")
      sql <- glue::glue("SELECT CODE_ID, CODE FROM CODES WHERE CODE IN ({tree_codes})")
      res <- query_db(sql, type = "get")
      return(res)
    })

    selected <- reactive({
      # check if this user has previously selected data, if not saved yet use default
      user <- username
      sql <- glue::glue("SELECT COUNT(*) AS USER_COUNT FROM SELECTED WHERE USERNAME = '{user}' AND CONCEPT_ID = {concept_id()}")
      check <- query_db(sql, type = "get")
      if (check$USER_COUNT == 0) {
        user <- "default"
      }

      # get the codes and select status
      code_ids <- paste0(code_ids()$CODE_ID, collapse = ", ")
      sql <- glue::glue("SELECT CODE_ID, SELECTED FROM SELECTED WHERE USERNAME = '{user}' AND CONCEPT_ID = {concept_id()} AND CODE_ID IN ({code_ids})")
      res <- query_db(sql, type = "get")
      res[code_ids(), CODE := i.CODE, on = "CODE_ID"]
      res[, CODE_ID := NULL]

      # add average selection here
      res[other_users_avg(), AVG_SELECTED := i.AVG_SELECTED, on = "CODE"]

      return(res)
    })

    tree <- reactive({
      print("loading base tree")
      tree_path <- system.file("concepts", paste0(id, ".RDS"), package = "hfphenotyping")
      tree <- readRDS(tree_path)
      return(tree)
    })

    # observeEvent(input$tree, {
    #   print("tree modified")
    #
    # })

    # comments
    comments <- reactive({
      sql <- glue::glue("SELECT COMMENTS FROM COMMENTS WHERE USERNAME = '{username}' AND CONCEPT_ID = {concept_id()}")
      res <- query_db(sql, type = "get")
      return(res$COMMENTS)
    })

    # other users' selection
    other_users_avg <- reactive({
      sql <- glue::glue("
        SELECT
          SELECTED.CONCEPT_ID,
          SELECTED.CODE_ID,
          CODES.CODE,
          CODES.CODE_DESC,
          CONCEPTS.CONCEPT_NAME,
          AVG(SELECTED.SELECTED) AS AVG_SELECTED,
          COUNT(DISTINCT SELECTED.USERNAME) AS NUM_RATERS
        FROM
          SELECTED
        INNER JOIN
          CODES ON SELECTED.CODE_ID = CODES.CODE_ID
        INNER JOIN
          CONCEPTS ON SELECTED.CONCEPT_ID = CONCEPTS.CONCEPT_ID
        WHERE
          SELECTED.USERNAME != '{username}' AND SELECTED.CONCEPT_ID = '{concept_id()}'
        GROUP BY
          SELECTED.CONCEPT_ID,
          SELECTED.CODE_ID,
          CONCEPTS.CONCEPT_NAME,
          CODES.CODE,
          CODES.CODE_DESC")
      res <- query_db(sql, type = "get")
      return(res)
    })

    # agreement plot
    agreement_plot <- reactive({

      req(input$tree)

      # process the data for the current selection
      avg <- data.table::copy(other_users_avg())
      current <- data.table::data.table(CODE     = unlist(tree_attributes(input$tree, "code")),
                                        SELECTED = unlist(tree_attributes(input$tree, "stselected")))
      avg[current, SELECTED := i.SELECTED, on = "CODE"]
      avg[, AGREE := AVG_SELECTED > 0.5 & SELECTED | AVG_SELECTED <= 0.5 & !SELECTED]
      avg[, Y_LABEL := paste0(CODE, " | ", ifelse(nchar(CODE_DESC > 20), paste0(substr(CODE_DESC, 1, 15), "..."), CODE_DESC))]
      avg[, Y_LABEL := factor(Y_LABEL, levels = Y_LABEL)]

      # plot
      ggplot2::ggplot(avg, ggplot2::aes(y = Y_LABEL, x = AVG_SELECTED, fill = AGREE)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "darkgreen")) +
        ggplot2::labs(title    = "Agreement",
                      subtitle = "(green: >50% agreement with your code selection; red: <=50% agreement)",
                      x        = "% of other raters choosing this code",
                      y        = "Code")
    })



    # -----------------------------
    # CONTROLS
    # -----------------------------
    # save button
    observeEvent(input$save, {

      # get the current code select status
      current <- data.table::data.table(USERNAME   = username,
                                        CODE       = unlist(tree_attributes(input$tree, 'code')),
                                        SELECTED   = as.integer(unlist(tree_attributes(input$tree, 'stselected'))),
                                        CONCEPT_ID = concept_id())
      current[code_ids(), CODE_ID := i.CODE_ID, on = "CODE"]
      current[, CODE := NULL]

      # remove the old rows from the database
      code_ids <- paste0(current$CODE_ID, collapse = ", ")
      sql <- glue::glue("DELETE FROM SELECTED WHERE USERNAME = '{username}' AND CONCEPT_ID = {concept_id()} AND CODE_ID IN ({code_ids})")
      query_db(query_str = sql, type = "send")

      # update with the new rows
      sql <- glue::glue("INSERT INTO SELECTED ({paste(names(current), collapse = ', ')}) VALUES ({paste0(rep('?', ncol(current)), collapse = ', ')})")
      res <- query_db(query_str = sql, type = "update", value = as.list(current))

      # remove the old comments
      sql <- glue::glue("DELETE FROM COMMENTS WHERE USERNAME = '{username}' AND CONCEPT_ID = {concept_id()}")
      query_db(query_str = sql, type = "send")

      # update with new comments
      vals <- list(USERNAME = username, CONCEPT_ID = concept_id(), COMMENTS = input$user_comments)
      sql <- glue::glue("INSERT INTO COMMENTS ({paste(names(vals), collapse = ', ')}) VALUES ({paste0(rep('?', length(vals)), collapse = ', ')})")
      res1 <- query_db(query_str = sql, type = "update", value = vals)

      # report
      if (res & res1) {
        message("Save successful")
      } else {
        message("Error saving...")
      }
    })


    # -----------------------------
    # OUTPUTS
    # -----------------------------
    # selection tree
    output$tree <- shinyTree::renderTree({
      message("")
      print("tree render")

      # render comments too
      updateTextAreaInput(session = session, "user_comments", value = comments())

      # get tree
      tree <- modify_selected(tree(), selected())

      return(tree)
    })

    # message box
    output$message_box <- renderText({ message() })

    # plot
    output$plot <- renderPlot({

      if (input$plot_type == "agreement") {
        browser()
        return(agreement_plot())
      }

    })

  })
}



# recursively set selected or not
modify_selected <- function(tree, selected) {
  for (name in names(tree)) {
    code   <- attr(tree[[name]], "code")
    status <- any(as.logical(selected[CODE == code, SELECTED]))
    icon   <- cut(selected[CODE == code, AVG_SELECTED], breaks = c(0, 0.2, 0.4, 0.6, 0.8, Inf), labels = c("fa fa-battery-empty", "fa fa-battery-quarter", "fa fa-battery-half", "fa fa-battery-three-quarters", "fa fa-battery-full"))
    if (is.null(icon) || length(icon)==0 || is.na(icon)) icon <- "fa fa-battery-empty"
    attr(tree[[name]], "sticon") <- as.character(icon)
    attr(tree[[name]], "stselected") <- status
    attr(tree[[name]], "stopened") <- FALSE
    if (is.list(tree[[name]]) && length(tree[[name]]) > 0) {
      tree[[name]] <- modify_selected(tree[[name]], selected) # recursively process
    }
  }
  return(tree)
}


## To be copied in the UI
# mod_concept_ui("concept_1")

## To be copied in the server
# mod_concept_server("concept_1")


