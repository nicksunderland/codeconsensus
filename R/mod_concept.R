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
    concept      <- reactiveVal(id)
    subconcepts  <- reactiveVal(c(include, exclude))
    include_ids  <- reactiveVal(include)
    exclude_ids  <- reactiveVal(exclude)
    derived      <- reactiveVal(derived)
    message      <- reactiveVal(NULL)
    comments     <- reactiveVal(NULL)
    selected     <- reactiveVal(NULL)
    tree         <- reactiveVal(NULL)

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
      tree_codes <- paste0("'", unlist(tree_attributes(load_tree(), 'code')), "'", collapse = ", ")
      sql <- glue::glue("SELECT CODE_ID, CODE FROM CODES WHERE CODE IN ({tree_codes})")
      res <- query_db(sql, type = "get")
      return(res)
    })

    # the tree data
    load_tree <- reactive({
      print("loading base tree")

      # the base tree
      tree <- TreeElement("Tree", "Tree", "", stdisabled = TRUE, sticon = "fa fa-folder", children = list())

      # the inclusion concepts tree
      include <- TreeElement("Include", "INCLUDE", "concepts", sticon = "fa fa-folder", stdisabled = TRUE)

      # populate inclusion tree
      for (id in include_ids()) {
        tree_path <- system.file("concepts", paste0(id, ".RDS"), package = "hfphenotyping")
        t <- TreeElement(gsub("_", " ", id), "CONCEPT", "concept", stdisabled = TRUE, stopened = FALSE, sticon = "fa fa-folder", children = readRDS(tree_path))
        include[[paste0(attr(t, "code"), " | ", attr(t, "description"))]] <- t
      }
      tree[[paste0(attr(include, "code"), " | ", attr(include, "description"))]] <- include

      # populate exclusion tree
      if (length(exclude_ids()) > 0) {

        exclude <- TreeElement("Exclude", "EXCLUDE", "concepts", sticon = "fa fa-folder", stdisabled = TRUE)

        for (id in exclude_ids()) {
          tree_path <- system.file("concepts", paste0(id, ".RDS"), package = "hfphenotyping")
          t <- TreeElement(gsub("_", " ", id), "CONCEPT", "concept", stdisabled = TRUE, stopened = FALSE, sticon = "fa fa-folder", children = readRDS(tree_path))
          exclude[[paste0(attr(t, "code"), " | ", attr(t, "description"))]] <- t
        }

        tree[[paste0(attr(exclude, "code"), " | ", attr(exclude, "description"))]] <- exclude

      }

      # set tree
      tree(tree)

      # return
      return(tree)
    })

    # the saved selected codes for this module and user
    load_selected <- reactive({
      print("selected <- reactive")

      # check if logged in, or just viewing (username==NULL) - display average of raters if just viewing
      tree_codes     <- paste0("'", unlist(tree_attributes(load_tree(), 'code')), "'", collapse = ", ")
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

      # set comments
      comments(str)
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

    # save comments function
    save_comments <- function(concept_id, username, comments) {
      print("save_comments function")

      # remove the old comments
      sql <- glue::glue("DELETE FROM COMMENTS WHERE USERNAME = '{username}' AND CONCEPT_ID = '{concept_id}'")
      query_db(query_str = sql, type = "send")

      # update with new comments
      vals <- list(USERNAME = username, CONCEPT_ID = concept_id, COMMENTS = comments)
      sql  <- glue::glue("INSERT INTO COMMENTS ({paste(names(vals), collapse = ', ')}) VALUES ({paste0(rep('?', length(vals)), collapse = ', ')})")
      res  <- query_db(query_str = sql, type = "update", value = vals)

      # return
      return(res)
    }

    # save function
    save_selected <- function(tree, username, concept_id, code_ids) {
      print("save_selected function")

      # get the current code select status
      current <- data.table::data.table(USERNAME   = username,
                                        CODE       = unlist(tree_attributes(tree, 'code')),
                                        CODE_TYPE  = unlist(tree_attributes(tree, 'code_type')),
                                        DISABLED   = unlist(tree_attributes(tree, 'stdisabled')),
                                        SELECTED   = as.integer(unlist(tree_attributes(tree, 'stselected'))),
                                        CONCEPT_ID = concept_id)

      # can occur if nothing selected
      if (nrow(current) == 0) return(list(res = FALSE, dat = current))

      # clean up
      current <- current[DISABLED == FALSE, ]
      current[code_ids, CODE_ID := i.CODE_ID, on = "CODE"]

      # remove the old rows from the database
      code_id_str <- paste0(current$CODE_ID, collapse = ", ")
      sql <- glue::glue("DELETE FROM SELECTED WHERE USERNAME = '{username}' AND CONCEPT_ID = {concept_id} AND CODE_ID IN ({code_id_str})")
      query_db(query_str = sql, type = "send")

      # update with the new rows
      cols <- c("USERNAME", "CODE_ID", "SELECTED", "CONCEPT_ID")
      sql <- glue::glue("INSERT INTO SELECTED ({paste(cols, collapse = ', ')}) VALUES ({paste0(rep('?', length(cols)), collapse = ', ')})")
      res <- query_db(query_str = sql, type = "update", value = as.list(current[, .SD, .SDcols = cols]))

      # return list of save result and current select data
      return(list(res = res,
                  dat = current[, .(CODE, CODE_TYPE, SELECTED, DISABLED)]))
    }


    # -----------------------------
    # CONTROLS / OBSERVERS
    # -----------------------------
    # save button
    session$userData$observer_store[[paste0("save_", id)]] <- observeEvent(input$save, {
      print("save button")

      # cant save if not logged in
      if (is.null(user[["username"]])) {
        showNotification("Save error, you are not logged in...", type = "error", duration = 30)
        return(NULL)
      }

      # only save if data different
      if (!is.null(comments())) {

        res <- save_comments(concept_id = concept_id(), username = user[["username"]], comments = input$user_comments)

        # report and reset local
        if (res) {
          showNotification("Saved comments", type = "message", duration = 10)
          comments(input$user_comments)
        } else {
          showNotification("Error saving comments, problem sending data to database", type = "error", duration = 30)
        }
      }

      # can only save selected if a rater, not a derived concept, and only save if data different
      if (!is.null(tree()) && user[["is_rater"]] && !derived()) {

        res <- save_selected(tree = input$tree, username = user[["username"]], concept_id = concept_id(), code_ids = code_ids())

        # report and set local
        if (res[["res"]]) {
          showNotification("Saved selection", type = "message", duration = 10)
          tree(input$tree)
          selected(res[["dat"]])
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
        out <- selected()
        out <- out[SELECTED == 1, ]
        data.table::fwrite(out[, .(CONCEPT, CODE, CODE_TYPE)], file, sep="\t", row.names=FALSE)
      }
    )


    # -----------------------------
    # OUTPUTS
    # -----------------------------
    # selection tree
    output$tree <- shinyTree::renderTree({
      print("tree render")
      req(input$code_display)

      # load if needed
      if (is.null(tree())) load_tree()
      if (is.null(selected())) load_selected()
      if (is.null(comments())) load_comments()

      # render comments
      updateTextAreaInput(session = session, "user_comments", value = comments())

      # build tree
      tree <- modify_tree(tree         = tree(),
                          selected     = selected(),
                          label_option = input$code_display, disable_tree = is.null(user[["is_rater"]]) || !user[["is_rater"]] || derived())

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
  option <- match.arg(label_option, choices = c("default", "nhs_counts", "ukbb_counts"))

  # Pre-compute status and disabled information for fast lookup
  selected_status <- setNames(as.logical(selected$SELECTED), paste(selected$CODE, selected$CODE_TYPE))
  selected_disabled <- setNames(as.logical(selected$DISABLED), paste(selected$CODE, selected$CODE_TYPE))

  # Recursive function to modify the tree in place
  modify_tree_recursive <- function(sub_tree) {
    for (name in names(sub_tree)) {
      element <- sub_tree[[name]]

      code      <- attr(element, "code")
      code_type <- attr(element, "code_type")
      desc      <- attr(element, "description")
      lab_opt   <- if (label_option != "default" && !is.null(attr(element, label_option))) paste0("n=", attr(element, label_option)) else NULL
      key       <- paste(code, code_type)
      status    <- selected_status[key]
      disabled  <- selected_disabled[key]
      new_name  <- paste0(c(code, lab_opt, desc), collapse = " | ")

      # Only set attributes if they change
      if ((!is.null(attr(element, "stselected")) && !is.null(status) && !is.na(status)) && attr(element, "stselected") != status) {
        attr(element, "stselected") <- status
      }

      if (disable_tree) {
        if (is.null(attr(element, "stdisabled")) || attr(element, "stdisabled") != TRUE) {
          attr(element, "stdisabled") <- TRUE
        }
      } else {
        if (!is.null(attr(element, "stdisabled")) && !is.na(disabled) && attr(element, "stdisabled") != disabled) {
          attr(element, "stdisabled") <- disabled
        }
      }

      if (is.list(element) && length(element) > 0) {
        # Recursively process the subtree
        sub_tree[[new_name]] <- modify_tree_recursive(element)
      } else {
        sub_tree[[new_name]] <- element
      }

      # Remove the old name entry
      if (new_name != name) {
        sub_tree[[name]] <- NULL
      }
    }

    return(sub_tree)
  }

  return(modify_tree_recursive(tree))
}


# slow version
# # recursively set selected or not
# modify_tree <- function(tree, selected, label_option = "default", disable_tree = FALSE) {
#
#   option <- match.arg(label_option, choices = c("default", "nhs_counts", "ukbb_counts"))
#
#   for (name in names(tree)) {
#
#     # current element
#     element <- tree[[name]]
#     tree[[name]] <- NULL
#
#     # get details for this code
#     code      <- attr(element, "code")
#     code_type <- attr(element, "code_type")
#     desc      <- attr(element, "description")
#     lab_opt   <- if(label_option != "default" && !is.null(attr(element, label_option))) paste0("n=", attr(element, label_option)) else NULL
#     status    <- any(as.logical(selected[CODE == code & CODE_TYPE == code_type, SELECTED]))
#     disabled  <- any(as.logical(selected[CODE == code & CODE_TYPE == code_type, DISABLED]))
#
#     # default is no additional information
#     new_name <- paste0(c(code, lab_opt, desc), collapse = " | ")
#
#     # update attributes
#     attr(element, "stselected") <- status
#     if (disable_tree) {
#       attr(element, "stdisabled") <- TRUE
#     }
#
#     # recursively processs
#     if (is.list(element) && length(element) > 0) {
#       tree[[new_name]] <- modify_tree(element, selected, label_option, disable_tree) # recursively process
#     } else {
#       tree[[new_name]] <- element
#     }
#
#   }
#
#   return(tree)
# }
