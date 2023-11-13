#' @title Shiny Module for k means clustering
#' @description kmsModule UI Function
#'
#'
#' @param id id of module
#' @export
#' @import shiny
#'
#' @importFrom plotly plotlyOutput
mod_kmsModule_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column( # Result Area
      width = 8,
      plotlyOutput(ns("plot"), width = "100%")
    ),
    column( # Options
      width = 4,
      style = "border-left: dotted 1px black",
      h4("Description"),
      h5("❓ Below variable will show character, not duplicated. and used to Label each point of cluster"),
      h5("⚠️ If no variable exists, try add index (row number) to data"),
      hr(),
      sliderInput(
        ns("k"),
        "k",
        min = 2,
        max = 10,
        value = 4,
        step = 1,
        width = "100%"
      ),
      checkboxInput(
        ns("scale"),
        "scale",
        value = TRUE,
        width = "100%"
      ),
      selectInput(
        inputId = ns("labels"),
        label = "",
        choices = NULL,
        width = "100%"
      ),
      actionButton( # Main Action
        ns("cluster"),
        "cluster",
        style = "font-weight: bold; width: 100%; background: #004B4D; border-radius: 0; color: white; border: 0;"
      )
    )
  )
}

#' @title Shiny Module for k means clustering
#' @description kmsModule Server Functions
#' @export
#' @import shiny
#' @param id id of module
#' @param inputData "reactive" data
#'
#'
#' @importFrom factoextra fviz_cluster
#' @importFrom plotly ggplotly
mod_kmsModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)

    observeEvent(inputData(), {
      data <- inputData()

      charNames <- names(Filter(is.character, data))
      uniqueNames <- find_unique_column(data)
      values <- intersect(charNames, uniqueNames)

      updateSelectizeInput(
        inputId = "labels",
        label = "Labels-Opt (Character)",
        choices = c("NULL", values)
      )
    })

    observeEvent(input$cluster, {
      data <- inputData()

      if (input$labels != "NULL") { # keep label variable
        labels <- data[[input$labels]]
      }

      data <- Filter(is.numeric, data) # select numeric only

      if (input$scale) {
        data <- scale(data)
      }

      km.res <- kmeans(data, centers = input$k)

      if (input$labels != "NULL") {
        rownames(data) <- labels
      }

      output$plot <- renderPlotly({
        ggplotly(
          fviz_cluster(
            km.res,
            data = data,
            ggtheme = theme_minimal()
          ) +
            theme(legend.position = "none")
        )
      })
    })
  })
}

## Not export
find_unique_column <- function(data) {
  unique_columns <- character(0)

  for (col in colnames(data)) {
    if (length(unique(data[[col]])) == nrow(data)) {
      unique_columns <- c(unique_columns, col)
    }
  }

  return(unique_columns)
}
