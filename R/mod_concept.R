#' concept UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_concept_ui <- function(id, title, definition, pmid, domain, terminology, concept_term, valueset_definition, regexes){
  ns <- NS(id)

  # the concept panel
  tabPanel(title = title,
           # the information box
           div(
             style = "background-color: #f7f7f7; border: 1px solid #ddd; padding: 10px; margin-bottom: 20px;",
             h3("Definition:"),
             p(definition),
             h5("PMID:", span(pmid, style = "font-weight: normal;")),
             h5("Domain:", span(domain, style = "font-weight: normal;")),
             h5("Terminology:", span(terminology, style = "font-weight: normal;")),
             h5("Concept term:", span(concept_term, style = "font-weight: normal;")),
             h5("ValueSet definition:", span(valueset_definition, style = "font-weight: normal;")),
             h5("Search expressions:", span(regexes, style = "font-weight: normal;"))#,
             # textOutput(ns("testing"))
           ),
           # the codes table
           DT::dataTableOutput(ns("code_table")))
}

#' concept Server Functions
#'
#' @noRd
mod_concept_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # test database connection
    # output$testing <- renderText({
    #   con <- dbConnect(drv=dbDriver("Oracle"), user="username", pass="password", dbname='(description= (retry_count=20)(retry_delay=3)(address=(protocol=tcps)(port=1522)(host=adb.uk-london-1.oraclecloud.com))(connect_data=(service_name=gdbb1bd7f3d63a8_heartfailurephenotyping_high.adb.oraclecloud.com))(security=(ssl_server_dn_match=yes)))')
    #   dbGetQuery(con, "select * from global_name")$GLOBAL_NAME
    # })


    # Generate sample data
    data <- data.frame(
      Type = LETTERS[1:5],
      Code = 1:5,
      Desc = c("Foo", "Bar", "Baz", "Foo", "Bar"),
      Checked = rep(FALSE, 5)
    )

    output$code_table <- DT::renderDataTable({
      render_checkbox_table(data)
    })

    render_checkbox_table <- function(data) {
      DT::datatable(
        data,
        options = list(
          columnDefs = list(
            list(
              targets = c(4),
              render = JS(
                "function(data, type, row, meta) {",
                "  if (type === 'display') {",
                "    return '<input type=\"checkbox\" class=\"dt-checkbox\">';",
                "  }",
                "  return data;",
                "}"
              )
            )
          ),
          initComplete = JS(
            "function(settings, json) {",
            "  var table = settings.oInstance.api();",
            "  table.on('click', 'input[type=\"checkbox\"]', function() {",
            "    var data = table.row($(this).closest('tr')).data();",
            "    var rowIndex = $(this).closest('tr').index();",
            "    Shiny.setInputValue('checkbox', {rowIndex: rowIndex, checked: this.checked});",
            "  });",
            "}"
          )
        )
      )
    }

  })
}

## To be copied in the UI
# mod_concept_ui("concept_1")

## To be copied in the server
# mod_concept_server("concept_1")
