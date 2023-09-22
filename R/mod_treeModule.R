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
      width = 8,
      style = "border-right: dotted 1px black",
      h4("Regression Tree"),
      plotOutput(ns("plot"))
    ),
    column( # Options
      width = 4,
      style = "border-left: dotted 1px black",
      h4("Description"),
      h5("Regression trees are decision trees in which the target variables can take continuous values instead of class labels in leaves."),
      h5("Regression trees use modified split selection criteria and stopping criteria."),
      hr(),
      h4("Options"),
      h5("Decision Tree"),
      fluidRow(
        column(
          width = 6,
          selectInput(ns("x"), "Predictor", choices = NULL, multiple = TRUE, width = "100%")
        ),
        column(
          width = 6,
          selectInput(ns("y"), "Target", choices = NULL, width = "100%")
        )
      ),
      fluidRow(
        column(
          width = 4,
          actionButton(ns("openNode"), "Node", width = "100%", class = "myButton")
        ),
        column(
          width = 4,
          actionButton(ns("openEdge"), "Edge", width = "100%", class = "myButton")
        ),
        column(
          width = 4,
          actionButton(ns("openTerminal"), "Terminal", width = "100%", class = "myButton")
        )
      ),
      hr(),
      h5("Linear Regression"),
      h5("Note: Variable must included in Decision Tree - Predictor"),
      selectInput(ns("nodePlotX"), "X (Numeric)", choices = NULL, width = "100%"),
      selectInput(ns("nodePlotColor"), "Color (Factor)", choices = NULL, width = "100%"),
      selectInput(ns("nodePlotShape"), "Shape (Factor)", choices = NULL, width = "100%"),
      actionButton( # Main action
        ns("tree"),
        label = "tree",
        class = "myButton",
        width = "100%"
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
#' @importFrom partykit lmtree ctree
#'
mod_treeModule_server <- function(id, inputData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req(inputData)

    observeEvent(inputData(), {
      data <- inputData()
      updateSelectizeInput(
        inputId = "x",
        label = "Predictor (Numeric / Factor)",
        choices = union(names(Filter(is.numeric, data)), names(Filter(is.factor, data)))
      )

      updateSelectizeInput(
        inputId = "y",
        label = "Target",
        choices = colnames(data)
      )

      updateSelectizeInput(
        inputId = "nodePlotX",
        label = "X (Numeric)",
        choices = names(Filter(is.numeric, data))
      )

      updateSelectizeInput(
        inputId = "nodePlotColor",
        label = "Color (Factor)",
        choices = names(Filter(is.factor, data))
      )

      updateSelectizeInput(
        inputId = "nodePlotShape",
        label = "Shape (Factor)",
        choices = names(Filter(is.factor, data))
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
            sliderInput("terminalSpace", "tree/plot Ratio", min = 0, max = 1, value = 0.5, step = 0.1, width = "100%"),
            sliderInput("nodePlotAlpha", "Transparency", min = 0, max = 1, value = 0.5, step = 0.1, width = "100%")
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
                alpha = ifelse(is.null(input$nodePlotAlpha), 0.5, input$nodePlotAlpha) # 10
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
