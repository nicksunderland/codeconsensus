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
           # user comments section
           textAreaInput(ns("user_comments"), label = "Comments", width = "100%"),
           # selection boxes
           fluidRow(column(4, selectInput(ns("plot_type"), label = "Plot selection", choices = c("off", "agreement", "counts", "percentage", "concurrency (counts)", "concurrency (%)"))),
                    column(4, selectInput(ns("code_display"), label = "Code display", choices = c("nhs_counts", "default"))),
                    column(4, downloadButton(ns("download"), label = "Download codes"), style = "margin-top: 25px;")),
           # conditional plot panel
           conditionalPanel(condition = paste0("input.plot_type != 'off'"), plotOutput(ns("plot")), ns = ns),
           # save and output messages
           fluidRow(column(12, tags$label("Save selection"))),
           fluidRow(column(2, actionButton(ns("save"), "Save")),
                    column(8, textOutput(ns("message_box")))),
           # the tree
           shinycssloaders::withSpinner(
             shinyTree::shinyTree(ns("tree"), theme="proton", wholerow = FALSE, search = FALSE, unique = FALSE, checkbox = TRUE, three_state = FALSE, tie_selection = TRUE),
             type = 8
           )
  )

}

#' @title concept Server Functions
#' @param id string, id of this module
#' @param regexes string, regex pattern
#' @param username string, the current user
#' @param concept_name string, the concept name specified in teh yaml config
#' @importFrom glue glue
#' @noRd
mod_concept_server <- function(id, concept_name, regexes, username){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # -----------------------------
    # reactive values / expressions
    # -----------------------------
    message <- reactiveVal("")
    tree <- reactiveVal(NULL)

    # the tree data
    load_base_tree <- reactive({
      print("loading base tree")
      tree_path <- system.file("concepts", paste0(id, ".RDS"), package = "hfphenotyping")
      tree <- readRDS(tree_path)
      return(tree)
    })

    # the concept id for this module
    concept_id <- reactive({
      sql <- glue::glue("SELECT CONCEPT_ID FROM CONCEPTS WHERE CONCEPT_NAME = '{concept_name}'")
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

      # get the codes and select status
      code_ids <- paste0(code_ids()$CODE_ID, collapse = ", ")
      sql <- glue::glue("SELECT CODE_ID, SELECTED FROM SELECTED WHERE USERNAME = '{username}' AND CONCEPT_ID = {concept_id()} AND CODE_ID IN ({code_ids})")
      res <- query_db(sql, type = "get")
      res[code_ids(), CODE := i.CODE, on = "CODE_ID"]
      res[, CODE_ID := NULL]

      return(res)
    })

    # the comments for with concept for this user
    comments <- reactive({
      print("comments <- reactive")
      input$save # save will trigger a refresh of selected
      sql <- glue::glue("SELECT COMMENTS FROM COMMENTS WHERE USERNAME = '{username}' AND CONCEPT_ID = {concept_id()}")
      res <- query_db(sql, type = "get")
      return(res$COMMENTS)
    })

    # save function
    save <- function(tree, comments, concept_id, username) {
      print("save function")

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
      sql <- glue::glue("INSERT INTO SELECTED ({paste(names(current), collapse = ', ')}) VALUES ({paste0(rep('?', ncol(current)), collapse = ', ')})")
      res <- query_db(query_str = sql, type = "update", value = as.list(current))

      # remove the old comments
      sql <- glue::glue("DELETE FROM COMMENTS WHERE USERNAME = '{username}' AND CONCEPT_ID = {concept_id}")
      query_db(query_str = sql, type = "send")

      # update with new comments
      vals <- list(USERNAME = username, CONCEPT_ID = concept_id, COMMENTS = comments)
      sql <- glue::glue("INSERT INTO COMMENTS ({paste(names(vals), collapse = ', ')}) VALUES ({paste0(rep('?', length(vals)), collapse = ', ')})")
      res1 <- query_db(query_str = sql, type = "update", value = vals)

      # report
      if (res & res1) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }


    # -----------------------------
    # CONTROLS
    # -----------------------------
    # save button
    observeEvent(input$save, {
      print("save button")

      # run save
      res <- save(input$tree, input$user_comments, concept_id(), username)

      # report
      if (res) {
        message("Save successful")
      } else {
        message("Error saving...")
      }

    })

    # changing display of the codes, save tree first
    observeEvent(input$code_display, {
      req(input$tree)
      print("code_display select")

      # run save
      res <- save(input$tree, input$user_comments, concept_id(), username)

      # report
      if (res) {
        message("Save successful")
      } else {
        message("Error saving...")
      }
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

      # render comments too
      updateTextAreaInput(session = session, "user_comments", value = comments())

      # load if needed
      if (is.null(tree())) {
        tree(load_base_tree())
      }

      # get tree
      tree <- modify_selected(tree(), selected())
      tree <- rename_tree(tree, option = input$code_display)

      return(tree)
    })

    # message box
    output$message_box <- renderText({
      print(paste0("message box render `", message(), "`"))
      message()
    })

    # plot
    # output$plot <- renderPlot({
    #
    #   if (input$plot_type == "agreement") {
    #     return(NULL)
    #   }
    #
    # })

  })
}

# rename elements
rename_tree <- function(tree, option = "default") {

  option <- match.arg(option, choices = c("default", "nhs_counts"))

  renamed_tree <- list()
  for (name in names(tree)) {

    # get the element to process
    new_element <- tree[[name]]

    # default is no additional information
    if (option == "default") {
      new_name <- paste0(attr(tree[[name]], "code"), " | ", attr(tree[[name]], "description"))

    # else use the option to extract the attribute with the same name (see raw-data folder for how the trees are constructed)
    } else {
      count_str <- ""
      if (!is.null(attr(tree[[name]], option))) {
        count_str <- paste0("n=", attr(tree[[name]], option), " | ")
      }
      new_name <- paste0(attr(tree[[name]], "code"), " | ", count_str, attr(tree[[name]], "description"))
    }

    # recursive
    if (length(new_element) > 0 && is.list(new_element)) {

      renamed_tree[[new_name]] <- rename_tree(new_element, option)

      # copy all attributes across, except names as that is what we're changing
      for (attr_name in names(attributes(new_element))) {
        if (attr_name != "names") {
          attr(renamed_tree[[new_name]], attr_name) <- attr(new_element, attr_name)
        }
      }

    # exit case for recursion
    } else {

      renamed_tree[[new_name]] <- new_element

    }
  }

  return(renamed_tree)
}


# recursively set selected or not
modify_selected <- function(tree, selected) {
  for (name in names(tree)) {
    code   <- attr(tree[[name]], "code")
    status <- any(as.logical(selected[CODE == code, SELECTED]))
    attr(tree[[name]], "stselected") <- status
    if (is.null(attr(tree[[name]], "stopened"))) {
      attr(tree[[name]], "stopened") <- FALSE
    }
    if (is.list(tree[[name]]) && length(tree[[name]]) > 0) {
      tree[[name]] <- modify_selected(tree[[name]], selected) # recursively process
    }
  }
  return(tree)
}



