#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import DT
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Generate sample data
  data <- data.frame(
    Type = LETTERS[1:5],
    Code = 1:5,
    Desc = c("Foo", "Bar", "Baz", "Foo", "Bar"),
    Checked = rep(FALSE, 5)
  )

  # Render the tables with checkboxes for each tab
  output$checkbox_table_1 <- DT::renderDataTable({
    render_checkbox_table(data)
  })

  output$checkbox_table_2 <- DT::renderDataTable({
    render_checkbox_table(data)
  })

  output$checkbox_table_3 <- DT::renderDataTable({
    render_checkbox_table(data)
  })

  output$plot <- renderPlot({
    hist(1:100)
  })

  # Function to render checkbox table
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

}
