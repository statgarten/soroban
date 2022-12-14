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
mod_groupStatModule_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
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
      actionButton( # Main Action
        inputId = ns("build"),
        label = "build",
        style = "font-weight: bold; width: 100%; background: #004B4D; border-radius: 0; color: white; border: 0;"
      )
    )
  )

  ## NOT USED DOWNLOAD BUTTON

  # tags$button(
  #   tagList(fontawesome::fa("download"), "Download as CSV"),
  #   class = "btn btn-default shiny-download-link",
  #   onclick = "Reactable.downloadDataCSV('groupStatModule_1-myTable', 'downloads.csv')"
  # )
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
        summarise(across(names(Filter(is.numeric, data)), match.fun(input$func), na.rm = TRUE)) %>%
        reactable()

      output$myTable <- renderReactable(v)
    })
  })
}
