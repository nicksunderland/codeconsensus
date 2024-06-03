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
             h5("Search expressions:\n",span(paste0("(", regexes, ")", collapse = " | "), style = "font-weight: normal;"))
           ),
           # user comments section
           textAreaInput(ns("user_comments"), label = "Comments", width = "100%"),
           # selection boxes
           fluidRow(column(4, selectInput(ns("plot_type"), label = "Plot selection", choices = c("off", "counts"))),
                    column(4, selectInput(ns("code_display"), label = "Code display", choices = c("default", "nhs_counts", "ukbb_counts"))),
                    column(4, downloadButton(ns("download"), label = "Download codes"), style = "margin-top: 25px;")),
           # conditional plot panel
           conditionalPanel(condition = paste0("input.plot_type != 'off'"), plotOutput(ns("plot")), ns = ns),
           # save and output messages
           fluidRow(column(12, tags$label("Save selection"))),
           fluidRow(column(2, actionButton(ns("save"), "Save/Refresh")),
                    column(8, textOutput(ns("message_box")))),
           # the tree
           shinycssloaders::withSpinner(
             shinyTree::shinyTree(ns("tree"), theme="proton", wholerow = FALSE, search = FALSE, unique = FALSE, checkbox = TRUE, three_state = TRUE, tie_selection = TRUE),
             type = 8
           )
  )

}

#' @title concept Server Functions
#' @param id string, id of this module
#' @param user reactiveValues, the current user
#' @param include string, vector of ids for the inclusion concepts
#' @param exclude string, vector of ids for the exclusion concepts
#' @importFrom glue glue
#' @noRd
mod_concept_server <- function(id, include, exclude, user, derived){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # -----------------------------
    # reactive values / expressions
    # -----------------------------
    message      <- reactiveVal("")
    concept      <- reactiveVal(id)
    subconcepts  <- reactiveVal(c(include, exclude))
    include_ids  <- reactiveVal(include)
    exclude_ids  <- reactiveVal(exclude)
    derived      <- reactiveVal(derived)
    tree         <- reactiveVal(NULL)

    # the tree data
    load_base_tree <- reactive({
      print("loading base tree")

      # the base tree
      tree <- TreeElement("Tree", "Tree", "", stdisabled = TRUE, sticon = "fa fa-folder", children = list())

      # the inclusion concepts tree
      include <- TreeElement("Include", "INCLUDE", "concepts", sticon = "fa fa-folder", stdisabled = TRUE)

      # populate inclusion tree
      for (id in include_ids()) {
        tree_path <- system.file("concepts", paste0(id, ".RDS"), package = "hfphenotyping")
        t <- TreeElement(sub("_", " ", id), "CONCEPT", "concept", stdisabled = TRUE, sticon = "fa fa-folder", children = readRDS(tree_path))
        include[[paste0(attr(t, "code"), " | ", attr(t, "description"))]] <- t
      }
      tree[[paste0(attr(include, "code"), " | ", attr(include, "description"))]] <- include

      # populate exclusion tree
      if (length(exclude_ids()) > 0) {

        exclude <- TreeElement("Exclude", "EXCLUDE", "concepts", sticon = "fa fa-folder", stdisabled = TRUE)

        for (id in exclude_ids()) {
          tree_path <- system.file("concepts", paste0(id, ".RDS"), package = "hfphenotyping")
          t <- TreeElement(sub("_", " ", id), "CONCEPT", "concept", stdisabled = TRUE, sticon = "fa fa-folder", children = readRDS(tree_path))
          exclude[[paste0(attr(t, "code"), " | ", attr(t, "description"))]] <- t
        }

        tree[[paste0(attr(exclude, "code"), " | ", attr(exclude, "description"))]] <- exclude

      }

      # return tree
      return(tree)
    })

    concept_id <- reactive({
      sql <- glue::glue("SELECT CONCEPT_ID FROM CONCEPTS WHERE CONCEPT = '{id}'")
      res <- query_db(sql, type = "get")
      return(res$CONCEPT_ID)
    })

    # the subconcept ids for this module
    subconcept_ids <- reactive({
      subconcepts <- paste0("'", subconcepts(), "'", collapse = ", ")
      sql <- glue::glue("SELECT CONCEPT_ID FROM CONCEPTS WHERE CONCEPT IN ({subconcepts})")
      res <- query_db(sql, type = "get")
      return(res$CONCEPT_ID)
    })

    # the code ids for this module
    code_ids <- reactive({
      print("code_ids <- reactive")
      tree_codes <- paste0("'", unlist(tree_attributes(load_base_tree(), 'code')), "'", collapse = ", ")
      sql <- glue::glue("SELECT CODE_ID, CODE FROM CODES WHERE CODE IN ({tree_codes})")
      res <- query_db(sql, type = "get")
      return(res)
    })

    # the saved selected codes for this module and user
    selected <- reactive({
      print("selected <- reactive")

      input$save # save will trigger a refresh of selected
      input$code_display # changing code view will trigger a refresh of selected

      # check if logged in, or just viewing (username==NULL) - display average of raters if just viewing
      tree_codes <- paste0("'", unlist(tree_attributes(load_base_tree(), 'code')), "'", collapse = ", ")
      if (!is.null(user[["username"]]) && user[["is_rater"]]) {

        # user logged in and is a rater
        subconcept_ids <- paste0("'", subconcept_ids(), "'", collapse = ", ")
        sql <- glue::glue("SELECT
                              CODES.CODE,
                              CODES.CODE_TYPE,
                              SELECTED.SELECTED,
                              NULL AS DISABLED
                           FROM
                              SELECTED
                           INNER JOIN
                              CODES ON SELECTED.CODE_ID = CODES.CODE_ID
                           WHERE
                              SELECTED.USERNAME = '{user[['username']]}' AND SELECTED.CONCEPT_ID IN ({subconcept_ids}) AND CODES.CODE IN ({tree_codes})")

      } else {

        # user is not a rater, present rater results
        subconcept_ids <- paste0("'", subconcept_ids(), "'", collapse = ", ")
        sql <- glue::glue("SELECT
                              CODES.CODE,
                              CODES.CODE_TYPE,
                              CASE WHEN AVG(SELECTED.SELECTED) > 0.5 THEN 1 ELSE 0 END AS SELECTED,
                              1 AS DISABLED
                           FROM
                              SELECTED
                           INNER JOIN
                              CODES ON SELECTED.CODE_ID = CODES.CODE_ID
                           WHERE
                              SELECTED.CONCEPT_ID IN ({subconcept_ids}) AND CODES.CODE IN ({tree_codes})
                           GROUP BY
                              SELECTED.CONCEPT_ID, CODES.CODE, CODES.CODE_TYPE")

      }

      # get the codes and select status
      res <- query_db(sql, type = "get")

      return(res)
    })

    # the comments for with concept for this user
    comments <- reactive({
      print("comments <- reactive")

      input$save # save will trigger a refresh of selected

      # if logged in, enable comments
      if (!is.null(user[["username"]])) {

        sql <- glue::glue("SELECT COMMENTS FROM COMMENTS WHERE USERNAME = '{user[['username']]}' AND CONCEPT_ID = '{concept_id()}'")
        res <- query_db(sql, type = "get")
        str <- res$COMMENTS

      } else {

        str <- ""

      }

      return(str)
    })

    # code counts
    counts <- reactive({

      # validation
      req(tree(), input$code_display)
      validate(need(input$code_display %in% c("nhs_counts", "ukbb_counts"), "Please choose a data source from `Code display`"))

      # get the internal package data (see /data folder)
      tree_codes <- data.table::data.table(code      = unlist(tree_attributes(tree(), "code")),
                                           code_type = unlist(tree_attributes(tree(), "code_type")))

      if (input$code_display == "nhs_counts") {

        code_counts <- tree_codes[nhs_counts, count := i.count, on = "code"]

      } else if (input$code_display == "ukbb_counts") {

        code_counts <- tree_codes[ukbb_counts, count := i.count, on = "code"]

      }
      code_counts <- code_counts[order(-count), head(.SD, 20), by = code_type]
      code_counts[, code := factor(code, levels = code[order(code_type, count)])]
      code_counts <- code_counts[!is.na(count), ]
      return(code_counts)
    })

    # save function
    save <- function(tree, concept_id, comments, derived = FALSE, username = NULL, is_rater = NULL) {
      print("save function")

      # can only save things if logged in
      if (is.null(username)) {

        showNotification("Save error, you are not logged in...", type = "error", duration = 30)

      } else {

        # remove the old comments
        sql <- glue::glue("DELETE FROM COMMENTS WHERE USERNAME = '{username}' AND CONCEPT_ID = '{concept_id}'")
        query_db(query_str = sql, type = "send")

        # update with new comments
        vals <- list(USERNAME = username, CONCEPT_ID = concept_id, COMMENTS = comments)
        sql  <- glue::glue("INSERT INTO COMMENTS ({paste(names(vals), collapse = ', ')}) VALUES ({paste0(rep('?', length(vals)), collapse = ', ')})")
        res  <- query_db(query_str = sql, type = "update", value = vals)
        res1 <- TRUE

        # if a rater, save comments and codes
        if (is_rater & !derived) {

          # get the current code select status
          current <- data.table::data.table(USERNAME   = username,
                                            CODE       = unlist(tree_attributes(tree, 'code')),
                                            DISABLED   = unlist(tree_attributes(tree, 'stdisabled')),
                                            SELECTED   = as.integer(unlist(tree_attributes(tree, 'stselected'))),
                                            CONCEPT_ID = concept_id)
          current <- current[DISABLED == FALSE, ]
          current[, DISABLED := NULL]
          current[code_ids(), CODE_ID := i.CODE_ID, on = "CODE"]
          current[, CODE := NULL]

          # remove the old rows from the database
          code_ids <- paste0(current$CODE_ID, collapse = ", ")
          sql <- glue::glue("DELETE FROM SELECTED WHERE USERNAME = '{username}' AND CONCEPT_ID = {concept_id} AND CODE_ID IN ({code_ids})")
          query_db(query_str = sql, type = "send")

          # update with the new rows
          sql  <- glue::glue("INSERT INTO SELECTED ({paste(names(current), collapse = ', ')}) VALUES ({paste0(rep('?', ncol(current)), collapse = ', ')})")
          res1 <- query_db(query_str = sql, type = "update", value = as.list(current))

        }

        # report
        if (res & res1) {
          showNotification("Save successful", type = "message", duration = 10)
        } else {
          showNotification("Save error, problem sending data to database, please report bug", type = "error", duration = 30)
        }

      }

    }


    # -----------------------------
    # CONTROLS
    # -----------------------------
    # save button
    session$userData$observer_store[[id]] <- observeEvent(input$save, {
      print("save button")
      save(tree       = input$tree,
           concept_id = concept_id(),
           comments   = input$user_comments,
           username   = user[["username"]],
           is_rater   = user[["is_rater"]],
           derived    = derived())
    })


    # download the codes
    output$download <- downloadHandler(
      filename = function() {
        # paste0(input$dataset, ".tsv")
      },
      content = function(file) {
        # vroom::vroom_write(data(), file)
      }
    )


    # -----------------------------
    # OUTPUTS
    # -----------------------------
    # selection tree
    output$tree <- shinyTree::renderTree({
      print("tree render")
      req(input$code_display)

      # render comments too
      updateTextAreaInput(session = session, "user_comments", value = comments())

      # load if needed
      if (is.null(tree())) {
        tree(load_base_tree())
      }

      tree <- modify_tree(tree(), selected(), label_option = input$code_display, disable_tree = is.null(user[["is_rater"]]) || !user[["is_rater"]] || derived())

      return(tree)
    })

    # message box
    output$message_box <- renderText({
      print(paste0("message box render `", message(), "`"))
      message()
    })

    # plot
    output$plot <- renderPlot({

      validate(need(nrow(counts()) > 0, "No data available."))

      if (input$plot_type == "counts") {
        ggplot2::ggplot(counts(), ggplot2::aes(x = count, y = code, fill = code_type)) +
          ggplot2::geom_col() +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position = "none") +
          ggplot2::labs(title = paste0("Code counts: ", sub("_", " ", input$code_display)),
                        x = "Count",
                        y = "Code") +
          ggplot2::facet_wrap(~code_type, nrow = 1, scales = "free")
      }

    })

  })
}


# recursively set selected or not
modify_tree <- function(tree, selected, label_option = "default", disable_tree = FALSE) {

  option <- match.arg(label_option, choices = c("default", "nhs_counts", "ukbb_counts"))

  for (name in names(tree)) {

    # current element
    element <- tree[[name]]
    tree[[name]] <- NULL

    # get details for this code
    code      <- attr(element, "code")
    code_type <- attr(element, "code_type")
    desc      <- attr(element, "description")
    lab_opt   <- if(label_option != "default" && !is.null(attr(element, label_option))) paste0("n=", attr(element, label_option)) else NULL
    status    <- any(as.logical(selected[CODE == code & CODE_TYPE == code_type, SELECTED]))
    disabled  <- any(as.logical(selected[CODE == code & CODE_TYPE == code_type, DISABLED]))

    # default is no additional information
    new_name <- paste0(c(code, lab_opt, desc), collapse = " | ")

    # update attributes
    attr(element, "stselected") <- status
    if (disable_tree) {
      attr(element, "stdisabled") <- TRUE
    }

    # recursively processs
    if (is.list(element) && length(element) > 0) {
      tree[[new_name]] <- modify_tree(element, selected, label_option, disable_tree) # recursively process
    } else {
      tree[[new_name]] <- element
    }

  }

  return(tree)
}

#       # copy all attributes across, except names as that is what we're changing
#       for (attr_name in names(attributes(new_element))) {
#         if (attr_name != "names") {
#           attr(modified_tree[[new_name]], attr_name) <- attr(new_element, attr_name)
#         }
#         if (attr_name == "stselected") {
#           attr(modified_tree[[new_name]], "stselected") <- status
#         } else if (attr_name == "stdisabled" && disabled) {
#           attr(modified_tree[[new_name]], "stdisabled") <- disabled
#         }



