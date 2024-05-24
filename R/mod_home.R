#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)

  # the home panel
  tabPanel(title = "Home",
                   # the information box
                   div(
                     style = "background-color: #f7f7f7; border: 1px solid #ddd; padding: 10px; margin-bottom: 20px;",
                     h3("Introduction:"),
                     p("...some intro"),
                     h5("Voting:"),
                     p("...how to vote")
                   ),
                   hr(),
                   # graphs
                   fluidRow(column(6, plotOutput(ns("plot"))),
                            column(6, plotOutput(ns("plot2"))))
  )
}

#' home Server Functions
#' @import ggplot2
#' @noRd
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot <- renderPlot({

      ggplot2::ggplot(data    = data.frame(x = runif(1000)),
                      mapping = ggplot2::aes(x = x)) +
        ggplot2::geom_histogram() +
        ggplot2::labs(title = "some summary of voting")

    })


    output$plot2 <- renderPlot({

      d <- expand.grid(x = as.factor(LETTERS[1:15]), y = as.factor(LETTERS[1:15]))

      d$value <- runif(nrow(d))

      ggplot2::ggplot(d,
                      mapping = ggplot2::aes(x = x, y = y, fill = value)) +
        ggplot2::geom_tile() +
        ggplot2::labs(title = "some concurrency plot")


    })

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
