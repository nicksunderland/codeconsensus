#' concept UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyTree shinyTree
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
             h5("Search expressions:\n",span(paste0("(", unlist(regexes), ")", collapse = " | "), style = "font-weight: normal;"))
           ),
           # user comments section
           textAreaInput(ns("user_comments"), label = "Comments", width = "100%"),
           # selection boxes
           fluidRow(column(3, selectInput(ns("plot_type"), label = "Plot selection", choices = c("off", "counts"))),
                    column(3, selectInput(ns("code_display"), label = "Code display", choices = c("default", "nhs_count", "ukbb_count"))),
                    column(4, sliderInput(ns("count_slider"), "Count filter", min = 0, max = 10000, value = 0, step = 100)),
                    column(2, downloadButton(ns("download"), label = "Download codes"), style = "margin-top: 25px;")),
           # conditional plot panel
           conditionalPanel(condition = paste0("input.plot_type != 'off'"), plotOutput(ns("plot")), ns = ns),
           # save and output messages
           fluidRow(column(12, tags$label("Save selection"))),
           fluidRow(column(2, actionButton(ns("save"), "Save/Refresh")),
                    column(3, checkboxInput(ns("cascade"), "Cascade", value = FALSE)),
                    column(3, checkboxInput(ns("expand"), "Expand all", value = FALSE)),
                    column(6, textOutput(ns("message_box")))),
           # the tree
           shinycssloaders::withSpinner(
             jsTreeR::jstreeOutput(ns("tree")),
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
    concept      <- reactiveVal(id)
    subconcepts  <- reactiveVal(c(include, exclude))
    include_ids  <- reactiveVal(include)
    exclude_ids  <- reactiveVal(exclude)
    derived      <- reactiveVal(derived)
    message      <- reactiveVal(NULL)
    comments     <- reactiveVal(NULL)
    js_tree      <- reactiveVal(NULL)
    selected     <- reactiveVal(NULL)

    # the concept id for this module (could be a derived module)
    concept_id <- reactive({
      sql <- glue::glue("SELECT CONCEPT_ID FROM CONCEPTS WHERE CONCEPT = '{id}'")
      res <- query_db(sql, type = "get")
      return(res$CONCEPT_ID)
    })

    # the subconcept ids for this module (just the concept_id if a concept, or a character vector of concept ids if a derived)
    subconcept_ids <- reactive({
      subconcepts <- paste0("'", subconcepts(), "'", collapse = ", ")
      sql <- glue::glue("SELECT CONCEPT_ID FROM CONCEPTS WHERE CONCEPT IN ({subconcepts})")
      res <- query_db(sql, type = "get")
      return(res$CONCEPT_ID)
    })

    # the code ids for this module's codes
    code_ids <- reactive({
      print("code_ids <- reactive")
      tree_codes <- paste0("'", unlist(tree_attributes(js_tree(), 'code')), "'", collapse = ", ")
      sql <- glue::glue("SELECT CODE_ID, CODE FROM CODES WHERE CODE IN ({tree_codes})")
      res <- query_db(sql, type = "get")
      return(res)
    })

    # the tree data
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
      for (id in include_ids()) {
        tree_path <- system.file("concepts", paste0(id, ".RDS"), package = "hfphenotyping")
        include$children <- c(include$children, readRDS(tree_path))
      }

      # populate exclusion tree
      exclude <- NULL
      if (length(exclude_ids()) > 0) {

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

        for (id in exclude_ids()) {
          tree_path <- system.file("concepts", paste0(id, ".RDS"), package = "hfphenotyping")
          exclude$children <- c(exclude$children, readRDS(tree_path))
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
                              coreOptions = list(expand_selected_onload = FALSE))

      # return
      js_tree(tree)
    })

    # the saved selected codes for this module and user
    load_selected <- reactive({
      print("selected <- reactive")

      # check if logged in, or just viewing (username==NULL) - display average of raters if just viewing
      tree_codes     <- paste0("'", unlist(tree_attributes(js_tree(), 'code')), "'", collapse = ", ")
      subconcept_ids <- paste0("'", subconcept_ids(), "'", collapse = ", ")
      user_is_rater  <- !is.null(user[["username"]]) && user[["is_rater"]]

      if (user_is_rater) {

        # user logged in and is a rater
        sql <- glue::glue("SELECT
                              CODES.CODE,
                              CODES.CODE_TYPE,
                              CONCEPTS.CONCEPT,
                              SELECTED.SELECTED,
                              NULL AS DISABLED
                           FROM
                              SELECTED
                           INNER JOIN
                              CODES ON SELECTED.CODE_ID = CODES.CODE_ID
                           INNER JOIN
                              CONCEPTS ON SELECTED.CONCEPT_ID = CONCEPTS.CONCEPT_ID
                           WHERE
                              SELECTED.USERNAME = '{user[['username']]}'
                              AND SELECTED.SELECTED = 1
                              AND SELECTED.CONCEPT_ID IN ({subconcept_ids})
                              AND CODES.CODE IN ({tree_codes})")

      } else {

        # user is not a rater, present rater results
        sql <- glue::glue("SELECT
                              CODES.CODE,
                              CODES.CODE_TYPE,
                              CONCEPTS.CONCEPT,
                              CASE WHEN AVG(SELECTED.SELECTED) > 0.5 THEN 1 ELSE 0 END AS SELECTED,
                              1 AS DISABLED
                           FROM
                              SELECTED
                           INNER JOIN
                              CODES ON SELECTED.CODE_ID = CODES.CODE_ID
                           INNER JOIN
                              CONCEPTS ON SELECTED.CONCEPT_ID = CONCEPTS.CONCEPT_ID
                           WHERE
                              SELECTED.CONCEPT_ID IN ({subconcept_ids})
                              AND CODES.CODE IN ({tree_codes})
                           GROUP BY
                              SELECTED.CONCEPT_ID, CONCEPTS.CONCEPT, CODES.CODE, CODES.CODE_TYPE")
      }

      # get the codes and select status
      res <- query_db(sql, type = "get")

      # set selected
      selected(res)
    })

    # the comments for with concept for this user
    load_comments <- reactive({
      print("comments <- reactive")

      str <- ""

      # if logged in, enable comments
      if (!is.null(user[["username"]])) {

        sql <- glue::glue("SELECT COMMENTS FROM COMMENTS WHERE USERNAME = '{user[['username']]}' AND CONCEPT_ID = '{concept_id()}'")
        res <- query_db(sql, type = "get")

        if(nrow(res) > 0 && !all(is.na(res$COMMENTS))) {
          str <- res$COMMENTS
        }

      }

      comments(str)
    })

    # code counts (stored data in internal data objects `nhs_counts`, `ukbb_counts`, ... etc)
    counts <- reactive({

      # validation
      req(js_tree(), input$code_display)
      validate(need(input$code_display %in% c("nhs_count", "ukbb_count"), "Please choose a data source from `Code display`"))

      # get the internal package data (see /data folder)
      tree_codes <- data.table::data.table(code      = unlist(tree_attributes(js_tree(), "code")),
                                           code_type = unlist(tree_attributes(js_tree(), "code_type")))

      if (input$code_display == "nhs_count") {

        code_counts <- tree_codes[nhs_counts, count := i.count, on = "code"]

      } else if (input$code_display == "ukbb_count") {

        code_counts <- tree_codes[ukbb_counts, count := i.count, on = "code"]

      }
      code_counts <- code_counts[!is.na(count), ]
      code_counts <- code_counts[order(-count), head(.SD, 20), by = code_type]
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
      # save comments
      # ------------------------
      if (!is.null(input$user_comments) && input$user_comments != "") {

        # remove the old comments
        sql <- glue::glue("DELETE FROM COMMENTS WHERE USERNAME = '{user[['username']]}' AND CONCEPT_ID = '{concept_id()}'")
        query_db(query_str = sql, type = "send")

        # update with new comments
        vals <- list(USERNAME = user[['username']], CONCEPT_ID = concept_id(), COMMENTS = input$user_comments)
        sql  <- glue::glue("INSERT INTO COMMENTS ({paste(names(vals), collapse = ', ')}) VALUES ({paste0(rep('?', length(vals)), collapse = ', ')})")
        res  <- query_db(query_str = sql, type = "update", value = vals)

        # report
        if (res) {
          comments(input$user_comments)
          showNotification("Saved comments", type = "message", duration = 10)
        } else {
          showNotification("Error saving comments, problem sending data to database", type = "error", duration = 30)
        }

      }

      # ------------------------
      # save selected
      # ------------------------
      # can only save selected if user is a rater and not a derived concept (as should just update the base concept(s))
      if (user[["is_rater"]] && !derived()) {

        # get the current code select status
        current <- data.table::data.table(USERNAME   = user[["username"]],
                                          CODE       = unlist(tree_attributes(input$tree_full, 'code')),
                                          CODE_TYPE  = unlist(tree_attributes(input$tree_full, 'code_type')),
                                          DISABLED   = unlist(tree_attributes(input$tree_full, 'disabled')),
                                          SELECTED   = as.integer(unlist(tree_attributes(input$tree_full, 'selected'))),
                                          CONCEPT_ID = concept_id())

        # clean up
        current <- current[DISABLED == FALSE, ]
        current[code_ids(), CODE_ID := i.CODE_ID, on = "CODE"]

        # remove the old rows from the database
        code_id_str <- paste0(current$CODE_ID, collapse = ", ")
        if (any(is.na(current$CODE_ID))) {
          # find this bug....
          browser()
        }
        sql <- glue::glue("DELETE FROM SELECTED WHERE USERNAME = '{user[['username']]}' AND CONCEPT_ID = {concept_id()} AND CODE_ID IN ({code_id_str})")
        query_db(query_str = sql, type = "send")

        # update with the new rows
        cols <- c("USERNAME", "CODE_ID", "SELECTED", "CONCEPT_ID")
        sql <- glue::glue("INSERT INTO SELECTED ({paste(cols, collapse = ', ')}) VALUES ({paste0(rep('?', length(cols)), collapse = ', ')})")
        res <- query_db(query_str = sql, type = "update", value = as.list(current[, .SD, .SDcols = cols]))

        # report and set local reactiveVals
        if (res) {
          showNotification("Saved selection", type = "message", duration = 10)
          selected(current)
        } else {
          showNotification("Error saving selection, problem sending data to database", type = "error", duration = 30)
        }

      }

    })

    # download button
    output$download <- downloadHandler(
      filename = function() {
        paste0("codes_", id, "_", Sys.Date(), ".tsv")
      },
      content = function(file) {
        out <- data.table::data.table(PHENO     = id,
                                      CONCEPT   = sapply(input$tree_selected_paths, function(x) sub("^(?:Ex|In)clude/(.*?)/.*", "\\1", x$path)),
                                      CODE      = unlist(tree_attributes(input$tree_selected, "code")),
                                      CODE_TYPE = unlist(tree_attributes(input$tree_selected, "code_type")),
                                      DESC      = unlist(tree_attributes(input$tree_selected, "desc")),
                                      INCLUDE   = sapply(input$tree_selected_paths, function(x) grepl("^Include", x$path)),
                                      EXCLUDE   = sapply(input$tree_selected_paths, function(x) grepl("^Exclude", x$path)))

        data.table::fwrite(out, file, sep="\t", row.names=FALSE)
      }
    )

    # count filter slider
    session$userData$observer_store[[paste0("count_slider_", id)]] <- observeEvent(input$count_slider, {
      session$sendCustomMessage(ns("hideNodes"), input$count_slider[1])
    })

    # expand selection option
    session$userData$observer_store[[paste0("expand_", id)]] <- observeEvent(input$expand, {
      session$sendCustomMessage(ns("expandNodes"), input$expand)
    })

    # cascade selection option
    session$userData$observer_store[[paste0("cascade_", id)]] <- observeEvent(input$cascade, {
      session$sendCustomMessage(ns("cascadeNodes"), input$cascade)
    })

    # code display select box
    session$userData$observer_store[[paste0("code_display_", id)]] <- observeEvent(input$code_display, {
      req(counts())
      print(max(counts()[, .(count)], na.rm = TRUE))
      updateSliderInput(session, "count_slider", max = max(counts()[, .(count)], na.rm = TRUE))
    })


    # custom JS functions: CustomMessageHandler connected to sendCustomMessage event above
    onrender <- reactive({
      print("onrender <- reactive")
      glue::glue("
        function(el, x) {{

          // turn off cascading as will auto select parent codes which may not always be relevant
          var tree = $.jstree.reference(el.id);
          tree.settings.checkbox.three_state = false; // prevent cascading
          tree.settings.checkbox.cascade = 'undetermined'; // show parent nodes with children selected with a square
          //console.log(tree);

          Shiny.addCustomMessageHandler('{ns('cascadeNodes')}', function(cascade) {{
            var tree = $.jstree.reference(el.id);
            if (tree) {{
              if (cascade) {{
                tree.settings.checkbox.three_state = true;
                tree.settings.checkbox.cascade = 'undetermined+up+down';
              }} else {{
                tree.settings.checkbox.three_state = false;
                tree.settings.checkbox.cascade = 'undetermined';
              }}
            }}
          }});

          Shiny.addCustomMessageHandler('{ns('expandNodes')}', function(expand) {{
            var tree = $.jstree.reference(el.id);
            if (tree) {{
              if (expand) {{
                tree.open_all();
              }} else {{
                tree.close_all();
              }}
            }}
          }});

          // the slot to receive a `session$sendCustomMessage(ns('hideNodes') ...`
          Shiny.addCustomMessageHandler('{ns('hideNodes')}', function(count_slider) {{
            var tree = $.jstree.reference(el.id);
            var json = tree.get_json(null, {{flat: true}});
            for(var i = 0; i < json.length; i++) {{
              var id = json[i].id;
              var count = parseFloat(json[i].data.{input$code_display});
              var count_slider = parseFloat(count_slider);
              //console.log(count);
              //console.log(count_slider);
              //console.log(count !== null && !isNaN(count) && count < count_slider);
              if(count !== null && !isNaN(count) && count < count_slider) {{
                tree.hide_node(id);
              }} else {{
                tree.show_node(id);
              }}
           }}
         }});

       }}")
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

      # modify tree with previous selection and label options
      print("tree render")
      tree <- modify_tree(tree         = js_tree(),
                          selected     = selected(),
                          label_option = input$code_display,
                          disable_tree = is.null(user[["is_rater"]]) || !user[["is_rater"]] || derived())

      # apply js on render rules
      tree <- htmlwidgets::onRender(tree, onrender())

      # return
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


# optimised version
modify_tree <- function(tree, selected, label_option = "default", disable_tree = FALSE) {
  option <- match.arg(label_option, choices = c("default", "nhs_count", "ukbb_count"))

  # if setting the initial select status
  if (!is.null(selected)) {
    # Pre-compute status and disabled information for fast lookup
    selected_status   <- setNames(as.logical(selected$SELECTED), paste(selected$CODE, selected$CODE_TYPE))
    selected_disabled <- setNames(as.logical(selected$DISABLED), paste(selected$CODE, selected$CODE_TYPE))
  }

  # Recursive function to modify the tree in place
  modify_tree_recursive <- function(sub_tree) {
    for (i in seq_along(sub_tree)) {

      lab_opt   <- if (label_option != "default" && !is.null(sub_tree[[i]]$data[[label_option]])) paste0("n=", sub_tree[[i]]$data[[label_option]]) else NULL
      new_name  <- paste0(c(sub_tree[[i]]$data$code, lab_opt, sub_tree[[i]]$data$desc), collapse = " | ")

      # Only set attributes if they change
      if (sub_tree[[i]]$text != new_name) {
        sub_tree[[i]]$text <- new_name
      }

      # if setting the initial select status
      if (!is.null(selected)) {
        key       <- paste(sub_tree[[i]]$data$code, sub_tree[[i]]$data$code_type)
        status    <- unname(selected_status[key])
        disabled  <- unname(selected_disabled[key])

        if ((!is.null(sub_tree[[i]]$state$selected) && !is.null(status) && !is.na(status)) && sub_tree[[i]]$state$selected != status) {
          sub_tree[[i]]$state$selected <- status
        }
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


