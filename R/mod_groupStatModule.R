#' @title Shiny Module for data transformation
#' @description groupStatModule UI Function
#'
#'
#' @param id id of module
#' @export
#' @import shiny
#'
#'
#' @importFrom reactable reactableOutput
#' @importFrom shinyjs hidden useShinyjs
mod_groupStatModule_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    useShinyjs(),
    column( # Result Area
      width = 9,
      reactableOutput(
        outputId = ns("myTable"),
        width = "100%"
      )
    ),
    column( # options
      width = 3,
      selectizeInput(
        inputId = ns("groups"),
        label = "",
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      selectInput(
        inputId = ns("func"),
        label = "summary",
        choices = c("mean", "median", "sd", "iqr" = "IQR", "mad", "min", "max"),
        width = "100%"
      ), # not quantile, first, last, nth, n, n_distint
      numericInput(
        inputId = ns("round"),
        label = "Round",
        value = 2,
        min = 0,
        max = 5,
        step = 1
      ),
      actionButton( # Main Action
        inputId = ns("build"),
        label = "build",
        style = "font-weight: bold; width: 100%; background: #004B4D; border-radius: 0; color: white; border: 0;"
      ),
      shinyjs::hidden(
        div(
          id = ns("hidden"),
          tags$button(
            tagList(fontawesome::fa("download"), "Download as CSV"),
            class = "btn btn-default shiny-download-link",
            style = "margin-top: 1em; width: 100%; font-weight: bold; width: 100%; background: #004B4D; border-radius: 0; color: white; border: 0;",
            onclick = paste0("Reactable.downloadDataCSV('", id, "-myTable', 'downloads.csv')")
          )
        )
      )
    )
  )
}

#' @title Shiny Module for data transformation
#' @description groupStatModule Server Functions
#' @export
#' @import shiny
#' @param id id of module
#' @param inputData "reactive" data
#' @return transformed data
#' @importFrom dplyr group_by summarise across
#' @importFrom reactable reactable renderReactable
#' @importFrom shinyjs show
mod_groupStatModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(
        inputId = "groups",
        label = "group by (Factor / Character)",
        choices = union(names(Filter(is.factor, data)), names(Filter(is.character, data)))
      )
    })

    observeEvent(input$build, {
      req(input$build)
      data <- inputData()
      v <- data %>%
        group_by(across(input$groups)) %>%
        summarise(across(names(Filter(is.numeric, data)), match.fun(input$func), na.rm = TRUE))

      for (i in 2:ncol(v)) {
        v[, i] <- round(v[, i], digits = input$round)
      }

      output$myTable <- renderReactable(reactable(v))

      shinyjs::show(id = "hidden")
    })
  })
}
