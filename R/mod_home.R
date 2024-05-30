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
                     p("The exercise aims to gather expert consesus on which codes define key base phenotypes."),
                     h5("Voting:"),
                     p("Read the description of the concept and make any comments in the comments box.
                        Then select the codes that define this concept.
                        Leave codes that do not define the concept blank.
                        Regularly save progress by clicking the save button.
                        Returning to the home page, you can view the agreement in terms of Cohen's kappa
                        with other raters. The number of other rates contributing to the statistic are
                        presented at the end of the bars on the plot below. A missing bar indicates that you
                        have not yet saved any codes against this phenotype/concept.
                       ")
                   ),
                   hr(),
                   actionButton(ns("refresh"), "Refresh"),
                   # graphs
                   plotOutput(ns("cohens_kappa_plot"))
  )
}

#' home Server Functions
#' @import ggplot2
#' @noRd
mod_home_server <- function(id, username, selected_summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    refresh <- reactiveVal(NULL)

    observeEvent(input$refresh, refresh(TRUE))

    # Cohen's kappa
    cohens_kappa_plot <- reactive({

      summary <- data.table::copy(selected_summary())
      summary <- data.table::dcast(summary, CONCEPT_NAME ~ USER_CATEGORY, value.var = c("AVG_SELECTED", "NUM_RATERS"))
      summary[, Po := pmin(AVG_SELECTED_USER, AVG_SELECTED_OTHER_USERS) + (1 - pmax(AVG_SELECTED_USER, AVG_SELECTED_OTHER_USERS))]
      summary[, Pe := (1 - pmax(AVG_SELECTED_USER, AVG_SELECTED_OTHER_USERS)) * pmin(AVG_SELECTED_USER, AVG_SELECTED_OTHER_USERS) + pmax(AVG_SELECTED_USER, AVG_SELECTED_OTHER_USERS) * (1 - pmin(AVG_SELECTED_USER, AVG_SELECTED_OTHER_USERS))]
      summary[, kappa := (Po - Pe) / (1 - Pe)]

      # plotting
      breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
      labels <- c("0 - Poor", "0.2 - Fair", "0.4 - Moderate", "0.6 - Substantial", "0.8 - Almost perfect", "1.0 - Perfect")
      ggplot2::ggplot(summary, ggplot2::aes(y = as.factor(CONCEPT_NAME),
                                            x = kappa,
                                            fill = kappa)) +
        ggplot2::geom_col() +
        ggplot2::geom_text(ggplot2::aes(label = paste0("*", NUM_RATERS_OTHER_USERS)), hjust = -0.2) +
        ggplot2::scale_fill_gradient2(low = scales::muted("red"),
                                      mid = "white",
                                      high = scales::muted("blue"), limits = c(0,1), midpoint = 0.5) +
        ggplot2::labs(title = paste0("Agreement for user: ", username),
                      subtitle = "*number of other raters",
                      x     = "Agreement (Cohen's kappa)",
                      y     = "Concept") +
        ggplot2::scale_x_continuous(breaks = breaks, labels = labels, limits = c(0,1)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")

    })

    output$cohens_kappa_plot <- renderPlot({

      cohens_kappa_plot()

    })


    return(refresh)
  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
