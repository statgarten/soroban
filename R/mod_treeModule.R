#' @title Shiny Module for data transformation
#' @description treeModule UI Function
#'
#' @param id id of module
#' @export
#' @import shiny
#'
mod_treeModule_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column( # Result Area
      width = 9,
      plotOutput(ns("plot"))
    ),
    column( # Options
      width = 3,
      selectInput(ns("x"), "x", choices = NULL, multiple = TRUE, width = "100%"),
      selectInput(ns("y"), "y", choices = NULL, width = "100%"),
      selectInput(ns("nodePlotX"), "nodePlotX", choices = NULL, width = "100%"),
      selectInput(ns("nodePlotColor"), "nodePlotColor", choices = NULL, width = "100%"),
      selectInput(ns("nodePlotShape"), "nodePlotShape", choices = NULL, width = "100%"),
      sliderInput(ns("nodePlotAlpha"), "nodePlotAlpha", min = 0, max = 1, value = 0.5, step = 0.1, width = "100%"),
      h5("options"),
      actionButton(ns("openNode"), "node", width = "100%"),
      actionButton(ns("openEdge"), "edge", width = "100%"),
      actionButton(ns("openTerminal"), "terminal", width = "100%"),
      actionButton( # Main action
        ns("tree"),
        "tree",
        style = "font-weight: bold;background: #3EC70B;color: white; width: 100%"
      )
    )
  )
}

#' @title Shiny Module for data transformation
#' @description treeModule Server Functions
#' @export
#' @import shiny
#' @param id id of module
#' @param inputData "reactive" data
#' @return transformed data
#'
#' @import ggparty
#' @import ggplot2
#' @importFrom colourpicker colourInput
#' @importFrom partykit lmtree
#'
mod_treeModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)

    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(
        inputId = "x",
        label = "x",
        choices = colnames(data)
      )

      updateSelectizeInput(
        inputId = "y",
        label = "y",
        choices = colnames(data)
      )

      updateSelectizeInput(
        inputId = "nodePlotX",
        label = "nodePlotX",
        choices = colnames(data)
      )

      updateSelectizeInput(
        inputId = "nodePlotColor",
        label = "nodePlotShape",
        choices = colnames(data)
      )

      updateSelectizeInput(
        inputId = "nodePlotShape",
        label = "nodePlotShape",
        choices = colnames(data)
      )
    })

    observeEvent(input$openNode, {
      showModal(
        modalDialog(
          title = "node options",
          tagList(
            sliderInput("nodeNameSize", "nodeNameSize", min = 0, max = 20, value = 10, step = 1, width = "100%"),
            sliderInput("nodePvalSize", "nodePvalSize", min = 0, max = 20, value = 10, step = 1, width = "100%")
          ),
          easyClose = TRUE,
          footer = NULL
        )
      )
    })

    observeEvent(input$openEdge, {
      showModal(
        modalDialog(
          title = "edge options",
          tagList(
            sliderInput("edgeSize", "edgeSize", min = 0.1, max = 3, value = 1, step = 0.1, width = "100%"),
            sliderInput("edgeLabelSize", "edgeLabelSize", min = 0.1, max = 10, value = 1, step = 0.5, width = "100%"),
            colourInput("edgeLabelColor", "edgeLabelColor", value = "grey")
          ),
          easyClose = TRUE,
          footer = NULL
        )
      )
    })

    observeEvent(input$openTerminal, {
      showModal(
        modalDialog(
          title = "regression line options",
          tagList(
            sliderInput("predictSize", "predictSize", min = 0, max = 5, value = 1, step = 0.1, width = "100%"),
            colourInput("predictColor", "predictColor", value = "grey"),
            sliderInput("nodeNSize", "nodeNSize", min = 0, max = 20, value = 4, step = 1, width = "100%"),
            sliderInput("terminalSpace", "tree/plot Ratio", min = 0, max = 1, value = 0.5, step = 0.1, width = "100%")
          ),
          easyClose = TRUE,
          footer = NULL
        )
      )
    })

    observeEvent(input$tree, {
      req(input$tree)

      x <- paste0(input$x, collapse = " + ") # 1
      f <- as.formula(paste0(input$y, " ~ ", x)) # 2, 3

      if (input$y %in% names(Filter(is.numeric, inputData()))) {
        tr_tree <- lmtree(f, data = inputData())
        ggtree <- ggparty(
          tr_tree,
          terminal_space = ifelse(is.null(input$terminalSpace), 0.5, input$terminalSpace), # 4
          add_vars = list(p.value = "$node$info$p.value")
        ) +
          geom_edge(size = ifelse(is.null(input$edgeSize), 1, input$edgeSize)) + # 5
          geom_edge_label(
            colour = ifelse(is.null(input$edgeLabelColor), "grey", input$edgeLabelColor), # 11
            size = ifelse(is.null(input$edgeLabelSize), 1, input$edgeLabelSize) # 6
          ) +
          geom_node_plot(
            gglist = list(
              geom_point(
                aes_string(
                  x = input$nodePlotX, # 7
                  y = input$y, # 2
                  col = input$nodePlotColor, # 8
                  shape = input$nodePlotShape # 9
                ),
                alpha = input$nodePlotAlpha # 10
              ),
              theme_bw(base_size = 20) # input$baseSize) # 12; Fixed as 20
            ),
            scales = "fixed",
            id = "terminal",
            shared_axis_labels = T,
            shared_legend = T,
            legend_separator = T,
            predict = input$nodePlotX, # 7
            predict_gpar = list( # 10
              col = ifelse(is.null(input$predictColor), "grey", input$predictColor),
              size = ifelse(is.null(input$predictSize), 1, input$predictSize)
            )
          )
      } # numeric
      else {
        tr_tree <- ctree(f, data = inputData())

        ggtree <- ggparty(
          tr_tree,
          terminal_space = ifelse(is.null(input$terminalSpace), 0.5, input$terminalSpace), # 4
          add_vars = list(p.value = "$node$info$p.value")
        ) +
          geom_edge(size = ifelse(is.null(input$edgeSize), 1, input$edgeSize)) + # 5
          geom_edge_label(
            colour = ifelse(is.null(input$edgeLabelColor), "grey", input$edgeLabelColor), # 11
            size = ifelse(is.null(input$edgeLabelSize), 1, input$edgeLabelSize) # 6
          ) +
          geom_node_plot(
            gglist = list(
              geom_bar(
                aes_string(x = "''", fill = input$y),
                position = position_fill()
              ),
              xlab(input$y)
            ),
            scales = "fixed",
            id = "terminal",
            shared_axis_labels = T,
            shared_legend = T,
            legend_separator = T
          )
      } # categoric

      output$plot <- renderPlot({
        ggtree +
          geom_node_label(aes(col = splitvar),
            line_list = list(
              aes(label = splitvar),
              aes(label = paste("p =", formatC(p.value, format = "e", digits = 2)))
            ),
            line_gpar = list(
              list(size = input$nodeNameSize),
              list(size = input$nodePvalSize)
            ),
            ids = "inner"
          ) +
          geom_node_label(
            aes(
              label = paste0("N = ", nodesize)
            ),
            fontface = "bold",
            ids = "terminal",
            size = ifelse(is.null(input$nodeNSize), 4, input$nodeNSize),
            nudge_y = 0.01
          ) +
          theme(legend.position = "none")
      })
    })
  })
}