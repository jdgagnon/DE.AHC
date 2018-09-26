# Copyright 2018 John Gagnon
# This program is distributed under the terms of the GNU General Public License

# UI ----------------------------------------------------------------------

. <- "Stop NOTE"

ui <-
  fluidPage(
    title = "DE vs AGO2-HITS-CLIP",
    theme = shinythemes::shinytheme("cosmo"),
    tabPanel(
        h2("Heatmap", style = "margin-top: 30px; margin-bottom: 30px"),
        fluidPage(
          ##### Plot sidebarPanel ####
          sidebarPanel(
            #### Sheet selection ####
            h4("Build plot"),
            selectInput(
                "filter",
                "Filter on:",
                choices = c("PValue",
                            "FDR"),
                selected = "PValue"
            ),
            numericInput(
                "cuttoff",
                "Cuttoff:",
                value = 0.1
            ),
            selectInput(
                "pathway",
                "Pathway",
                choices = NULL
              ),
            checkboxInput(
              "seedMatch",
              "Restrict to 3'UTR seed-match",
              value = FALSE
              ),
            selectInput(
              "gene",
              "Gene(s)",
              multiple = TRUE,
              choices = "all"
            )),
          mainPanel(
            #### Plot ####
            plotOutput("myPlot", width = "600px", height = "1000px")
            )
    )))

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  `%>%` <- magrittr::`%>%`

  readData_example <- function(path = NULL) {
    if (is.null(path)) {
      dir(system.file("app/www", package = "DE.AGO2.HITS.CLIP"))
    } else {
      system.file("app/www",
                  path,
                  package = "DE.AGO2.HITS.CLIP",
                  mustWork = TRUE)
    }
  }

  df_file <- readData_example("expression.csv")
  df_nr <- readr::read_csv(df_file, col_names = TRUE)
  df1 <- reactiveVal(df_nr)
  df2 <- reactiveVal(NULL)
  y.text <- reactiveVal(ggplot2::element_blank())

  pathways_file <- readData_example("pathways.csv")
  pathways <- readr::read_csv(pathways_file, col_names = TRUE)
  pathway_names <- colnames(pathways)[which(colnames(pathways) != "Gene")]
  updateSelectInput(session,
                    "pathway",
                    choices = c("none", pathway_names))
  allGenes <- df_nr$Gene
  updateSelectInput(session,
                    "gene",
                    choices = c("all", allGenes),
                    selected = "all")


  observeEvent({
    input$pathway
    input$seedMatch
    input$gene}, {
      req(input$pathway,
          pathway_names,
          input$gene)

      if (input$gene != "all") {
        df <- dplyr::filter(df1(), Gene %in% c(input$gene))
        y.text(ggplot2::element_text(size = 16, colour = "black"))
      } else if (input$pathway != "none") {
        genes <- pathways[[input$pathway]]
        df <- dplyr::filter(df1(), Gene %in% genes)
        y.text(ggplot2::element_text(size = 16, colour = "black"))
      } else {
        df <- df1()
        y.text(ggplot2::element_blank())
      }

      if (input$seedMatch) {
        df <- dplyr::filter(df, Location == "3UTR")
      }

      df2(df)
  })

  heatPlot <- reactive({
    filter <- input$filter
    cuttoff <- input$cuttoff
    DE.AGO2.HITS.CLIP::heatmaper(data = df2(),
                               filter = filter,
                               cuttoff = cuttoff,
                               y.text = y.text()
                               )
  })

  output$myPlot <- renderPlot({
    heatPlot()
  })

  #### Stop app on close ####
  session$onSessionEnded(stopApp)
}

shiny::shinyApp(ui, server)
