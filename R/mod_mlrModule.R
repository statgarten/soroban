#' @title Shiny Module for data transformation
#' @description mlrModule UI Function
#'
#'
#' @param id id of module
#' @export
#' @import shiny
#'
mod_mlrModule_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column( # Result Area
      width = 9,
      column(
        width = 6,
        plotOutput(ns("plot"), width = "100%")
      ),
      column(
        width = 6,
        verbatimTextOutput(ns("text"))
      )
    ),
    column( # Options
      width = 3,
      selectInput(
        ns("x"),
        "x",
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      selectInput(
        ns("y"),
        "y",
        choices = NULL,
        width = "100%"
      ),
      actionButton( # Main Action
        ns("reg"),
        "reg",
        style = "font-weight: bold;background: #3EC70B;color: white; width: 100%"
      )
    )
  )
}

#' @title Shiny Module for data transformation
#' @description mlrModule Server Functions
#' @export
#' @import shiny
#' @param id id of module
#' @param inputData "reactive" data
#'
#' @noRd
#' @importFrom caret varImp
mod_mlrModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)

    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(inputId = "x", label = "x", choices = colnames(data))
      updateSelectizeInput(inputId = "y", label = "y", choices = colnames(data))
    })

    observeEvent(input$reg, {
      data <- inputData()
      x <- paste0(input$x, collapse = " + ") # 1
      f <- as.formula(paste0(input$y, " ~ ", x)) # 2
      model <- step(lm(f, data), trace = 0)

      output$text <- renderPrint({
        summary(model)
      })

      vI <- varImp(model)
      vI$Features <- rownames(vI)
      colnames(vI)[1] <- "Importance"
      rownames(vI) <- NULL
      output$plot <- renderPlot({
        ggplot(
          data = vI,
          aes(y = reorder(Features, Importance), x = Importance, fill = Importance)
        ) +
          geom_bar(stat = "identity") +
          theme(legend.position = "none") +
          xlab(NULL) +
          ylab(NULL) +
          ggtitle("Feature Importance")
      })
    })
  })
}
