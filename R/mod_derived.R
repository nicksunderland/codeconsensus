#' derived UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_derived_ui <- function(id){
  ns <- NS(id)

  tabPanel(title = "Derived",
           # the information box
           div(
             style = "background-color: #f7f7f7; border: 1px solid #ddd; padding: 10px; margin-bottom: 20px;",
             h3("Introduction:"),
             p("...I am a derived phenotype from various base concepts")
           ),
           hr(),
           # composition
           p("some image displaying the derivation of the phenotype")
  )

}

#' derived Server Functions
#'
#' @noRd
mod_derived_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_derived_ui("derived_1")

## To be copied in the server
# mod_derived_server("derived_1")
