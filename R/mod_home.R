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
                   shinycssloaders::withSpinner(
                     plotOutput(ns("kappa_plot")),
                     type = 8
                   )
  )
}

#' home Server Functions
#' @import ggplot2
#' @noRd
mod_home_server <- function(id, concept_names){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # create a reactive to calculate the Fleiss' kappa
    kappa <- reactive({

      input$refresh # will trigger a repull of data

      names <- paste0("'", concept_names, "'", collapse = ", ")
      sql <- glue::glue("SELECT
                            SELECTED.USERNAME,
                            SELECTED.SELECTED,
                            CODES.CODE,
                            CONCEPTS.CONCEPT_NAME
                         FROM
                            SELECTED
                         INNER JOIN
                            CODES ON SELECTED.CODE_ID = CODES.CODE_ID
                         INNER JOIN
                            CONCEPTS ON SELECTED.CONCEPT_ID = CONCEPTS.CONCEPT_ID
                         WHERE
                            CONCEPTS.CONCEPT_NAME IN ({names})")

      res <- query_db(sql, type = "get")

      res <- data.table::dcast(res, CODE + CONCEPT_NAME ~ USERNAME, value.var = "SELECTED", fill = NA)

      calc_fleiss_kappa <- function(ratings_matrix) {
        N <- nrow(ratings_matrix)
        k <- ncol(ratings_matrix)

        p <- rowSums(ratings_matrix, na.rm = TRUE) / k
        category_counts <- colSums(t(ratings_matrix), na.rm = TRUE)
        P <- category_counts / (N * k)

        P_o <- mean(p^2 + (1 - p)^2)
        P_e <- sum(P^2)

        kappa <- (P_o - P_e) / (1 - P_e)
        return(kappa)
      }

      kappas <- res[, .(kappa = calc_fleiss_kappa(as.matrix(.SD[, -c("CODE"), with = FALSE]))), by = "CONCEPT_NAME"]

      return(kappas)
    })



    output$kappa_plot <- renderPlot({

      breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
      labels <- c("0 - Poor", "0.2 - Fair", "0.4 - Moderate", "0.6 - Substantial", "0.8 - Almost perfect", "1.0 - Perfect")
      ggplot2::ggplot(kappa(), ggplot2::aes(y = as.factor(CONCEPT_NAME),
                                            x = kappa,
                                            fill = kappa)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_gradient2(low = scales::muted("red"),
                                      mid = "white",
                                      high = scales::muted("blue"), limits = c(0,1), midpoint = 0.5) +
        ggplot2::labs(title = "Inter-rater agreement",
                      x     = "Agreement (Fleiss' kappa)",
                      y     = "Concept") +
        ggplot2::scale_x_continuous(breaks = breaks, labels = labels, limits = c(0,1)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")

    })

  })
}

