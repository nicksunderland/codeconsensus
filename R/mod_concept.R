utils::globalVariables(c('disabled', 'nhs_counts', 'count', 'i.per_100k_episodes', 'i.per_100k_patients',
                         'ukbb_counts', 'i.count', 'biovu_counts', 'DISABLED', 'CODE_ID', 'i.CODE_ID',
                         'N', 'CONCEPT', 'max_n', 'i.max_n', 'code_type', 'old', 'i.SELECTED',
                         'SELECTED'), package = "hfphenotyping")

#' concept UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import jsTreeR
#' @import data.table
mod_concept_ui <- function(id, config){
  ns <- NS(id)

  # the concept panel
  tabPanel(title = config$name,
           # the information box
           div(
             style = "background-color: #f7f7f7; border: 1px solid #ddd; padding: 10px; margin-bottom: 20px;",
             h3(config$name),
             h5("Definition: ",        span(config$definition, style = "font-weight: normal;")),
             h5("Reference:",          span(a(href = config$reference, sub("(^https?://[^/]+).*", "\\1", config$reference)), style = "font-weight: normal;")),
             h5("Terminologies:",      span(paste0(unlist(config$terminology), collapse = ", "),        style = "font-weight: normal;")),
             h5("Search expressions:", span(paste0("(", unlist(config$regexes), ")", collapse = " | "), style = "font-weight: normal;")),
             hr(),
             h5("Adjudicator notes:",  span(config$notes, style = "font-weight: normal;"))
           ),
           # user comments section & preferred terms input and agree boxes
           fluidRow(
             column(8, textAreaInput(ns("user_comments"), label = "Comments", height = "150px", width = "100%")),
             uiOutput(ns("preferred_terms_ui"))
           ),
           # conditional plot panel
           conditionalPanel(condition = paste0("input.plot_type != 'off'"), plotOutput(ns("plot")), ns = ns),
           # the tree and controls
           fluidRow(column(12, uiOutput(ns("consesus_status")))),
           hr(),
           fluidRow(
             column(9, shinycssloaders::withSpinner(jsTreeR::jstreeOutput(ns("tree")), type = 8)),
             column(3,
                    div(
                      style = "background-color: #f7f7f7; border: 1px solid #ddd; padding: 8px; margin-bottom: 8px;",
                      selectInput(ns("code_display"), label = "Count display", choices = c("default", "nhs_count", "ukbb_count", "biovu_count", "nhs_count_per100k_episodes", "gp_count_per100k_patients", "ukbb_count_per100k_episodes")),
                      selectInput(ns("plot_type"), label = "Plot selection", choices = c("off", "counts")),
                      numericInput(ns("count_filter"), "Count filter", min = 0, max = 1000, value = 0),
                      checkboxInput(ns("cascade"), "Cascade", value = FALSE),
                      checkboxInput(ns("expand"), "Expand", value = FALSE),
                      fluidRow(
                        column(6, actionButton(ns("save"), "Save")),
                        column(6, downloadButton(ns("download"), "Download"))
                      )
                    )
             )
           )

  )

}

#' @title concept Server Functions
#' @param id string, id of this module
#' @param user reactiveValues, the current user
#' @param include string, vector of ids for the inclusion concepts
#' @param exclude string, vector of ids for the exclusion concepts
#' @importFrom glue glue
#' @importFrom shinyjs disable
#' @noRd
mod_concept_server <- function(id, config, user, project_id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    # -----------------------------
    # reactive values / expressions
    # -----------------------------
    config       <- reactiveValues(!!!config)
    message      <- reactiveVal(NULL)
    comments     <- reactiveVal(NULL)
    js_tree      <- reactiveVal(NULL)
    selected     <- reactiveVal(NULL)
    preferred_terms <- reactiveValues()
    for (term in names(config$perferred_term)) {
      t <- config$perferred_term[[term]]
      preferred_terms[[term]] <- list(code = t$code, agree = FALSE)
    }


    # -----------------------------
    # Dynamic UI elements - remove this
    # -----------------------------
    output$preferred_terms_ui <- renderUI({
      req(config$domain != "Derived")

      els <- list()
      for (term in names(preferred_terms)) {
        els <- c(els, list(fluidRow(column(8, div(HTML(paste0("<strong>Preferred ", term, ": </strong> ", preferred_terms[[term]]$code))), style = "margin-top: 15px;"),# style="font-size:80%;"),
                                    column(4, checkboxInput(ns(paste0(term, "_agree")), "Agree?")))))#, style = "margin-top: 15px;"))))
        shinyjs::disable(ns(term))
      }

      column(4, !!!els)
    })

    # display whether there is consensus data for this tree
    output$consesus_status <- renderUI({

      subconcept_ids <- paste0("'", subconcept_ids(), "'", collapse = ", ")
      sql <- glue('SELECT s."CONCEPT_ID",
                       CASE WHEN COUNT(ss."USERNAME") > 0 THEN 1 ELSE 0 END AS "CONSENSUS_EXISTS"
                   FROM (
                          SELECT DISTINCT "CONCEPT_ID"
                          FROM "SELECTED"
                          WHERE "CONCEPT_ID" IN ({subconcept_ids})
                        ) s
                  LEFT JOIN "SELECTED" ss ON s."CONCEPT_ID" = ss."CONCEPT_ID" AND ss."USERNAME" = \'consensus\'
                  GROUP BY s."CONCEPT_ID"')
      res <- query_db(sql, type = "get")
      consensus <- nrow(res) > 0 && all(as.logical(res$CONSENSUS_EXISTS))

      if (consensus) {
        el <- div(id = "reached",
                  style = "margin-top: 10px;",
                  icon("check-circle", class = "fa-1x", style = "color: green;"),
                  span("Consensus reached", style = "color: green;"))
      } else {
        el <- div(id = "pending",
                  style = "margin-top: 10px;",
                  icon("times-circle", class = "fa-1x", style = "color: red;"),
                  span("Consensus review pending", style = "color: red;"))
      }

      return(el)
    })


    # -----------------------------
    # Reactive expressions
    # -----------------------------
    # the concept id for this module (could be a derived module)
    concept_id <- reactive({
      sql <- glue::glue('SELECT "CONCEPT_ID" FROM "CONCEPTS" WHERE "CONCEPT" = \'{id}\'')
      res <- query_db(sql, type = "get")
      return(res$CONCEPT_ID)
    })

    # the subconcept ids for this module (just the concept_id if a concept, or a character vector of concept ids if a derived)
    subconcept_ids <- reactive({
      subconcepts <- paste0("'", c(config$include, config$exclude), "'", collapse = ", ")
      sql <- glue::glue('SELECT "CONCEPT_ID" FROM "CONCEPTS" WHERE "CONCEPT" IN ({ subconcepts })')
      res <- query_db(sql, type = "get")
      return(res$CONCEPT_ID)
    })

    # the code ids for this module's codes
    code_ids <- reactive({
      req(js_tree())
      print("code_ids <- reactive")
      tree_codes <- paste0("'", unlist(tree_attributes(js_tree(), 'code')), "'", collapse = ", ")
      sql <- glue::glue('SELECT "CODE_ID", "CODE_TYPE", "CODE" FROM "CODES" WHERE "CODE" IN ({tree_codes})')
      res <- query_db(sql, type = "get")
      return(res)
    })


    # ------------------------
    # Load tree
    # ------------------------
    load_js_tree <- reactive({
      print("loading base tree")

      # the inclusion concepts tree
      include <- TreeNode(text     = "Include",
                          type     = "root",
                          data     = list(code      = "Include",
                                          code_type = NULL,
                                          desc      = NULL),
                          checked  = FALSE,
                          selected = FALSE,
                          opened   = TRUE,
                          disabled = TRUE,
                          children = list())

      # populate inclusion tree
      for (id in config$include) {
        print(id)
        tree_path <- system.file("concepts", project_id, paste0(id, ".RDS"), package = "hfphenotyping")
        include$children <- c(include$children, list(readRDS(tree_path)))
      }

      # populate exclusion tree
      exclude <- NULL
      if (length(config$exclude) > 0) {

        # the exclusion concepts tree
        exclude <- TreeNode(text     = "Exclude",
                            type     = "root",
                            data     = list(code      = "Exclude",
                                            code_type = NULL,
                                            desc      = NULL),
                            checked  = FALSE,
                            selected = FALSE,
                            opened   = TRUE,
                            disabled = TRUE,
                            children = list())

        for (id in config$exclude) {
          tree_path <- system.file("concepts", project_id, paste0(id, ".RDS"), package = "hfphenotyping")
          exclude$children <- c(exclude$children, list(readRDS(tree_path)))
        }
      }

      # set tree
      tree <- if (!is.null(exclude)) {
        list(include, exclude)
      } else {
        list(include)
      }

      # to jsTree
      tree <- jsTreeR::jstree(tree, types = NodeTypes, theme = "proton", checkboxes = TRUE, checkWithText = TRUE,
                              search = TRUE,
                              coreOptions = list(expand_selected_onload = FALSE))

      # return
      js_tree(tree)
    })

    # the saved selected codes for this module and user
    load_selected <- reactive({
      print("selected <- reactive")

      # check if logged in, or just viewing (username==NULL) - display average of raters if just viewing
      tree_codes        <- paste0("'", unlist(tree_attributes(js_tree(), 'code')), "'", collapse = ", ")
      subconcept_ids    <- paste0("'", subconcept_ids(), "'", collapse = ", ")
      user_is_rater     <- !is.null(user[["username"]]) && user[["is_rater"]]
      user_is_concensus <- !is.null(user[["username"]]) && user[["username"]] == "consensus"
      disable_flag      <- if(user_is_concensus) "0" else "1"

      # user logged in and is a rater
      if (user_is_rater & !user_is_concensus) {

        sql <- glue::glue('SELECT
                              "CODES"."CODE",
                              "CODES"."CODE_TYPE",
                              "CONCEPTS"."CONCEPT",
                              "SELECTED"."SELECTED",
                              NULL AS "DISABLED",
                              NULL AS "OPENED"
                           FROM
                              "SELECTED"
                           INNER JOIN
                              "CODES" ON "SELECTED"."CODE_ID" = "CODES"."CODE_ID"
                           INNER JOIN
                              "CONCEPTS" ON "SELECTED"."CONCEPT_ID" = "CONCEPTS"."CONCEPT_ID"
                           WHERE
                              "SELECTED"."USERNAME" = \'{user[["username"]]}\'
                              AND "SELECTED"."SELECTED" = 1
                              AND "SELECTED"."CONCEPT_ID" IN ({subconcept_ids})
                              AND "CODES"."CODE" IN ({tree_codes})')

      # user not a rater, or in the consensus login (tree enabled), or not logged in (tree disable) - show the consensus results
      } else {

        sql <- glue::glue('
                          WITH "AVG_SELECTED" AS (
                              SELECT
                                  "CONCEPT_ID",
                                  "CODE_ID",
                                  AVG("SELECTED") AS "AVG_SELECTED"
                              FROM
                                  "SELECTED"
                              WHERE
                                  "USERNAME" != \'consensus\' AND
                                  "CONCEPT_ID" IN ({subconcept_ids}) AND
                                  "CODE_ID" IN (SELECT "CODE_ID" FROM "CODES" WHERE "CODE" IN ({tree_codes}))
                              GROUP BY
                                  "SELECTED"."CONCEPT_ID", "SELECTED"."CODE_ID"
                          ),
                          "CONSENSUS_SELECTED" AS (
                              SELECT
                                  "CONCEPT_ID",
                                  "CODE_ID",
                                  "SELECTED" AS "CONSENSUS_SELECTED"
                              FROM
                                  "SELECTED"
                              WHERE
                                  "USERNAME" = \'consensus\' AND
                                  "CONCEPT_ID" IN ({subconcept_ids}) AND
                                  "CODE_ID" IN (SELECT "CODE_ID" FROM "CODES" WHERE "CODE" IN ({tree_codes}))
                          )
                          SELECT
                              "CODES"."CODE",
                              "CODES"."CODE_TYPE",
                              "CONCEPTS"."CONCEPT",
                              "AVG_SELECTED"."AVG_SELECTED",
                              0 AS "DISABLED",
                              NULL AS "OPENED",
                              COALESCE("CONSENSUS_SELECTED"."CONSENSUS_SELECTED", CASE WHEN "AVG_SELECTED"."AVG_SELECTED" >= 0.5 THEN 1 ELSE 0 END) AS "SELECTED"
                          FROM
                              "AVG_SELECTED"
                          LEFT JOIN
                              "CONSENSUS_SELECTED" ON "AVG_SELECTED"."CONCEPT_ID" = "CONSENSUS_SELECTED"."CONCEPT_ID" AND "AVG_SELECTED"."CODE_ID" = "CONSENSUS_SELECTED"."CODE_ID"
                          INNER JOIN
                              "CODES" ON "AVG_SELECTED"."CODE_ID" = "CODES"."CODE_ID"
                          INNER JOIN
                              "CONCEPTS" ON "AVG_SELECTED"."CONCEPT_ID" = "CONCEPTS"."CONCEPT_ID"
                          ')
      }

      # get the codes and select status
      res <- query_db(sql, type = "get")

      # set selected
      selected(res)
    })

    # the comments for with concept for this user
    load_comments <- reactive({
      print("comments <- reactive")

      comment_str <- ""

      # if logged in, enable comments
      if (!is.null(user[["username"]])) {

        sql <- glue::glue('SELECT * FROM "COMMENTS" WHERE "USERNAME" = \'{user[["username"]]}\' AND "CONCEPT_ID" = \'{concept_id()}\'')
        res <- query_db(sql, type = "get")

        # extract the data
        if(nrow(res) > 0 && !all(is.na(res$COMMENTS))) {

          comment_str <- res$COMMENTS

          for (term in names(preferred_terms)) {
            preferred_terms[[term]]$code <- res[[term]]
            if (!all(is.na(res[[term]])) && res[[term]] == config$perferred_term[[term]]$code) {
              preferred_terms[[term]]$agree <- TRUE
            } else {
              preferred_terms[[term]]$agree <- FALSE
            }
          }

        }

      }

      comments(comment_str)
    })

    # code counts (stored data in internal data objects `nhs_counts`, `ukbb_counts`, ... etc)
    counts <- reactive({

      # validation
      req(js_tree(), input$code_display)
      validate(need(input$code_display %in% c("nhs_count", "ukbb_count", "biovu_count", "nhs_count_per100k_episodes", "gp_count_per100k_patients", "ukbb_count_per100k_episodes"), "Please choose a data source from `Code display`"))

      # get the internal package data (see /data folder)
      tree_codes <- data.table::data.table(code      = unlist(tree_attributes(js_tree(), "code")),
                                           code_type = unlist(tree_attributes(js_tree(), "code_type")),
                                           disabled  = unlist(tree_attributes(js_tree(), "disabled")))
      tree_codes <- tree_codes[disabled == FALSE, ]

      if (input$code_display == "nhs_count_per100k_episodes") {

        code_counts <- tree_codes[nhs_counts, count := i.per_100k_episodes, on = c("code", "code_type")]

      } else if (input$code_display == "gp_count_per100k_patients") {

        code_counts <- tree_codes[nhs_counts, count := i.per_100k_patients, on = c("code", "code_type")]

      } else if (input$code_display == "ukbb_count_per100k_episodes") {

        code_counts <- tree_codes[ukbb_counts, count := i.per_100k_episodes, on = c("code", "code_type")]

      } else if (input$code_display == "ukbb_count") {

        code_counts <- tree_codes[ukbb_counts, count := i.count, on = c("code", "code_type")]

      } else if (input$code_display == "nhs_count") {

        code_counts <- tree_codes[nhs_counts, count := i.count, on = c("code", "code_type")]

      } else if (input$code_display == "biovu_count") {

        code_counts <- tree_codes[biovu_counts, count := i.count, on = c("code", "code_type")]

      }

      code_counts <- code_counts[!is.na(count), ]
      code_counts <- code_counts[order(-count), utils::head(.SD, 20), by = code_type]
      code_counts[, code := factor(code, levels = unique(code[order(code_type, count)]))]
      return(code_counts)
    })


    # -----------------------------
    # CONTROLS / OBSERVERS
    # -----------------------------
    # save button
    session$userData$observer_store[[paste0("save_", id)]] <- observeEvent(input$save, {
      print("save button")

      # need a tree to start with
      req(input$tree)

      # cant save if not logged in
      if (is.null(user[["username"]])) {
        showNotification("Save error, you are not logged in...", type = "error", duration = 30)
        return(NULL)
      }

      # ------------------------
      # save comments & preferred terms
      # ------------------------
      # remove the old comments
      sql <- glue::glue('DELETE FROM "COMMENTS" WHERE "USERNAME" = \'{user[["username"]]}\' AND "CONCEPT_ID" = \'{concept_id()}\'')
      query_db(query_str = sql, type = "send")

      # update with new comments
      vals <- list(USERNAME   = user[['username']],
                   CONCEPT_ID = concept_id(),
                   COMMENTS   = input$user_comments)

      # add preferred terms
      for (term in names(preferred_terms)) {
        vals[[term]] <- input[[term]]
      }

      # insert
      cols         <- paste0('"', names(vals), '"', collapse = ",")
      placeholders <- paste0("$", seq_along(vals), collapse = ",")
      sql  <- glue::glue('INSERT INTO "COMMENTS" ({cols}) VALUES ({placeholders})')
      res  <- query_db(query_str = sql, type = "execute", value = vals)

      # report
      if (res) {
        comments(input$user_comments)
        showNotification("Saved comments", type = "message", duration = 10)
      } else {
        showNotification("Error saving comments, problem sending data to database", type = "error", duration = 30)
      }

      # ------------------------
      # save selected
      # ------------------------
      # can only save selected if user is a rater and not a derived concept (as should just update the base concept(s))
      if (user[["is_rater"]] && !config$domain == "Derived") {

        # get the current code select status
        current <- data.table::data.table(USERNAME   = user[["username"]],
                                          CODE       = unlist(tree_attributes(input$tree_full, 'code')),
                                          CODE_TYPE  = unlist(tree_attributes(input$tree_full, 'code_type')),
                                          DISABLED   = unlist(tree_attributes(input$tree_full, 'disabled')),
                                          SELECTED   = as.integer(unlist(tree_attributes(input$tree_full, 'selected'))),
                                          OPENED     = as.integer(unlist(tree_attributes(input$tree_full, 'opened'))),
                                          CONCEPT    = id,
                                          CONCEPT_ID = concept_id())

        # clean up
        save_dat <- current[DISABLED == FALSE, ]
        save_dat[code_ids(), CODE_ID := i.CODE_ID, on = c("CODE", "CODE_TYPE")]

        if (any(is.na(save_dat$CODE_ID))) {
          # find this bug....
          browser()
        }

        # remove the old rows from the database that does match the current selection
        rows_modified <- 0
        old_selected <- data.table::copy(selected())
        save_dat[old_selected, old := i.SELECTED, on = "CODE"][is.na(old), old := 0]
        remove_dat <- save_dat[old == 1 & SELECTED == 0, ]
        if (nrow(remove_dat) > 0) {
          code_id_str <- paste0(remove_dat$CODE_ID, collapse = ", ")
          sql <- glue::glue('DELETE FROM "SELECTED" WHERE "USERNAME" = \'{user[["username"]]}\' AND "CONCEPT_ID" = {concept_id()} AND "CODE_ID" IN ({code_id_str})')
          rows_modified <- query_db(query_str = sql, type = "execute")
        }

        # update with the new selected rows
        save_dat <- save_dat[SELECTED == 1 & old == 0, ]
        if (nrow(save_dat) > 0) {
          code_id_str  <- paste0(save_dat$CODE_ID, collapse = ", ")
          col_names    <- c("USERNAME", "CODE_ID", "SELECTED", "CONCEPT_ID")
          cols         <- paste0('"', col_names, '"', collapse = ",")
          placeholders <- paste0("$", seq_along(col_names), collapse = ",")
          sql          <- glue::glue('INSERT INTO "SELECTED" ({cols}) VALUES ({placeholders})')
          n_rows       <- query_db(query_str = sql, type = "execute", value = as.list(save_dat[, .SD, .SDcols = col_names]))
          rows_modified <- rows_modified + n_rows
        }

        # report and set local reactiveVals
        showNotification(paste0("Saved/modified ", rows_modified, " selection(s)"),  type = "message", duration = 10)
        selected(current)

      }

    })

    # download button
    output$download <- downloadHandler(
      filename = function() {
        paste0("codes_", id, "_", Sys.Date(), ".tsv")
      },
      content = function(file) {

        if (length(input$tree_selected_paths)==0) {
          showNotification("No codes selected...", type = "error", duration = 30)
          return(NULL)
        }

        out <- data.table::data.table(PHENO     = id,
                                      CONCEPT   = lapply(input$tree_selected_paths, function(x) sub("^(?:Ex|In)clude/(.*?)/.*", "\\1", x$path)),
                                      CODE      = lapply(input$tree_selected,       function(x) x$data[["code"]]),
                                      CODE_TYPE = lapply(input$tree_selected,       function(x) x$data[["code_type"]]),
                                      DESC      = lapply(input$tree_selected,       function(x) x$data[["desc"]]),
                                      INCLUDE   = lapply(input$tree_selected_paths, function(x) grepl("^Include", x$path)),
                                      EXCLUDE   = lapply(input$tree_selected_paths, function(x) grepl("^Exclude", x$path)))



        data.table::fwrite(out, file, sep="\t", row.names=FALSE)
      }
    )

    # count filter slider
    session$userData$observer_store[[paste0("count_filter_", id)]] <- observeEvent(input$count_filter, {
      session$sendCustomMessage("hideNodes", list(id = ns("tree"), count_filter = input$count_filter, code_display = input$code_display))
    })

    # expand selection option
    session$userData$observer_store[[paste0("expand_", id)]] <- observeEvent(input$expand, {
      session$sendCustomMessage("expandNodes", list(id = ns("tree"), expand = input$expand))
    })

    # cascade selection option
    session$userData$observer_store[[paste0("cascade_", id)]] <- observeEvent(input$cascade, {
      session$sendCustomMessage("cascadeNodes", list(id = ns("tree"), cascade = input$cascade))
    })

    # code display select box
    session$userData$observer_store[[paste0("code_display_", id)]] <- observeEvent(input$code_display, {
      req(counts())
      #print(max(counts()[, list(count)], na.rm = TRUE))
      updateNumericInput(session, "count_filter", value = 0, max = max(counts()[, list(count)], na.rm = TRUE))
      #session$sendCustomMessage("updateFilter", list(id = ns("count_filter"),  max = max(counts()[, list(count)], na.rm = TRUE)))
    })


    # -----------------------------
    # OUTPUTS
    # -----------------------------
    # selection tree
    output$tree <- jsTreeR::renderJstree({

      # load if needed
      if (is.null(js_tree())) load_js_tree()
      if (is.null(selected())) load_selected()
      if (is.null(comments())) load_comments()

      # render comments
      updateTextAreaInput(session = session, "user_comments", value = comments())

      # render the preferred terms
      for (term in names(preferred_terms)) {
        updateTextInput(session = session, term, value = preferred_terms[[term]]$code)
        updateCheckboxInput(session = session, paste0(term, "_agree"), value = preferred_terms[[term]]$agree)
      }

      # modify tree with previous selection and label options
      print("tree render")
      tree <- modify_tree(tree           = js_tree(),
                          selected       = selected(),
                          label_option   = input$code_display,
                          show_agreement = !is.null(user[["username"]]) && user[["username"]] == "consensus",
                          disable_tree   = is.null(user[["is_rater"]]) || !user[["is_rater"]] || config$domain == "Derived")

      tree <- htmlwidgets::onRender(tree, glue::glue("function(el, x) {{
                                                        var tree = $.jstree.reference(el.id);
                                                        tree.settings.checkbox.three_state = {if (input$cascade) 'true' else 'false'};
                                                        tree.settings.checkbox.cascade = '{if (input$cascade) 'undetermined+up+down' else 'undetermined'}';
                                                      }}"))

      updateCheckboxInput(session = session, "expand", value = FALSE)

      # return
      return(tree)
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
                        # x = input$code_display,
                        y = "Code") +
          ggplot2::facet_wrap(~code_type, nrow = 1, scales = "free")
      }

    })

  })
}


# optimised version
modify_tree <- function(tree, selected, label_option = "default", show_agreement = FALSE, disable_tree = FALSE) {

  # if setting the initial select status
  if (!is.null(selected)) {
    # Pre-compute status and disabled information for fast lookup
    selected_status   <- stats::setNames(as.logical(selected$SELECTED), paste(selected$CODE, selected$CODE_TYPE, selected$CONCEPT))
    selected_disabled <- stats::setNames(as.logical(selected$DISABLED), paste(selected$CODE, selected$CODE_TYPE, selected$CONCEPT))
    selected_opened   <- stats::setNames(as.logical(selected$OPENED), paste(selected$CODE, selected$CODE_TYPE, selected$CONCEPT))

    # add average selected data if present (if not a rater)
    if ("AVG_SELECTED" %in% names(selected) && show_agreement) {
      agreement <- setNames((1 - abs(selected$SELECTED - selected$AVG_SELECTED)), paste(selected$CODE, selected$CODE_TYPE, selected$CONCEPT))
    }
  }

  # Recursive function to modify the tree in place
  modify_tree_recursive <- function(sub_tree) {
    for (i in seq_along(sub_tree)) {

      lab_opt   <- if (label_option != "default" && !is.null(sub_tree[[i]]$data[[label_option]])) paste0("value=", sub_tree[[i]]$data[[label_option]]) else NULL
      new_name  <- paste0(c(sub_tree[[i]]$data$code, lab_opt, sub_tree[[i]]$data$desc), collapse = " | ")

      # Only set attributes if they change
      if (sub_tree[[i]]$text != new_name) {
        sub_tree[[i]]$text <- new_name
      }

      key       <- paste(sub_tree[[i]]$data$code, sub_tree[[i]]$data$code_type, sub_tree[[i]]$data$concept_id)
      status    <- unname(selected_status[key])
      disabled  <- unname(selected_disabled[key])
      opened    <- unname(selected_opened[key])

      # add average selected data if present (if not a rater)
      if ("AVG_SELECTED" %in% names(selected) && show_agreement) {
        agree_value <- unname(agreement[key])
        sub_tree[[i]]$type <- if (!is.na(agree_value) && agree_value < 1.0) {
          "code_red"
        } else {
          "code"
        }
      } else {
        if (grepl("code", sub_tree[[i]]$type) &&  sub_tree[[i]]$type != "code") {
          sub_tree[[i]]$type <- "code"
        }
      }

      if ((!is.null(sub_tree[[i]]$state$selected) && !is.null(status) && !is.na(status)) && sub_tree[[i]]$state$selected != status) {
        sub_tree[[i]]$state$selected <- status
      }

      if ((!is.null(sub_tree[[i]]$state$opened) && !is.null(opened) && !is.na(opened)) && sub_tree[[i]]$state$opened != opened) {
        sub_tree[[i]]$state$opened <- opened
      }

      if (disable_tree) {
        if (is.null(sub_tree[[i]]$state$disabled) || sub_tree[[i]]$state$disabled != TRUE) {
          sub_tree[[i]]$state$disabled <- TRUE
        }
      } else {
        if (!is.null(sub_tree[[i]]$state$disabled) && !is.na(disabled) && sub_tree[[i]]$state$disabled != disabled) {
          sub_tree[[i]]$state$disabled <- disabled
        }
      }

      if (length(sub_tree[[i]]$children) > 0) {
        # Recursively process the subtree
        sub_tree[[i]]$children <- modify_tree_recursive(sub_tree[[i]]$children)
      }

    }

    return(sub_tree)
  }

  tree$x$data <- modify_tree_recursive(tree$x$data)
  return(tree)
}


