#' @title Shiny Module for data transformation
#' @description pcaModule UI Function
#'
#' @param id id of module
#' @export
#' @import shiny
#' @importFrom shinyjs hidden
mod_pcaModule_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column( # Result Area
      width = 8,
      style = "border-right: dotted 1px black",
      h4("PCA Plot"),
      shinyjs::hidden(
        uiOutput(outputId = ns("biplotSlot"))
      )
    ),
    column( # Options
      width = 4,
      style = "border-left: dotted 1px black",
      h4("Description"),
      h5("❓ PCA is a statistical technique for reducing the dimensionality of a dataset"),
      h5("⚠️ PCA columns requires 2 columns or more"),
      hr(),
      h4("Options"),
      selectInput(
        inputId = ns("columns"),
        label = "",
        choices = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      checkboxInput(
        inputId = ns("scale"),
        "variable normalize",
        value = TRUE,
        width = "100%"
      ),
      selectInput(
        inputId = ns("group"),
        label = "",
        choices = NULL,
        width = "100%"
      ),
      selectInput(
        inputId = ns("labels"),
        label = "",
        choices = NULL,
        width = "100%"
      ),
      sliderInput(
        inputId = ns("slotSize"),
        label = "height of plot",
        min = 400, max = 1000, step = 50, value = 400,
        ticks = FALSE,
        width = "100%"
      ),
      actionButton( # Main Action
        inputId = ns("pca"),
        label = "draw",
        style = "font-weight: bold; width: 100%; background: #004B4D; border-radius: 0; color: white; border: 0;"
      )
    )
  )
}

#' @title Shiny Module for data transformation
#' @description pcaModule Server Functions
#' @export
#' @import shiny
#' @param id id of module
#' @param inputData "reactive" data
#'
#' @importFrom showtext showtext_auto
#' @import ggbiplot
#' @import magrittr
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom dplyr select
#' @importFrom shinyjs show
mod_pcaModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    showtext_auto()
    req(inputData)

    observeEvent(input$slotSize, { # change slot Size
      req(input$slotSize)
      output$biplotSlot <- renderUI({
        plotlyOutput(outputId = ns("biplot"), height = paste0(input$slotSize, "px"))
      })
    })

    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(
        inputId = "columns",
        label = "PCA columns (Numeric)",
        choices = names(Filter(is.numeric, data))
      )

      updateSelectizeInput(
        inputId = "labels",
        label = "Point Labels (Character)",
        choices = c("NULL", names(Filter(is.character, data)))
      )

      updateSelectInput(
        inputId = "group",
        label = "Group columns (Factor)",
        choices = names(Filter(is.factor, data))
      )
    })

    observeEvent(input$pca, { # draw plot
      req(input$pca)

      data <- inputData()

      groups <- data[[input$group]]
      labels <- data[[input$labels]]

      shinyjs::show(id = "biplotSlot")

      output$biplot <- renderPlotly({
        data <- data %>%
          select(input$columns) %>%
          replace(is.na(.), 0)

        pp <- prcomp(data, scale. = input$scale)

        if (input$labels == "NULL") {
          labels <- NULL
        }

        ggbiplot(
          pp,
          obs.scale = 1,
          var.scale = 1,
          labels = labels,
          ellipse = TRUE,
          circle = TRUE,
          groups = groups
        ) %>%
          ggplotly()
      })
    })
  })
}
