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
            fileInput("deFile",
                      "DE file:"),
            fileInput("ahcFile",
                      "AGO2 HITS-CLIP file:"),
            #### Sheet selection ####
            h4("Build plot"),
            selectInput(
              "miRNA",
              "Select miRNA:",
              choices = NULL
            ),
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
                value = 0.05
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
              choices = "all",
              selected = "all"
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
      dir(system.file("app/www", package = "DE.AHC"))
    } else {
      system.file("app/www",
                  path,
                  package = "DE.AHC",
                  mustWork = TRUE)
    }
  }

  pathways_file <- readData_example("pathways.csv")
  pathways <- readr::read_csv(pathways_file, col_names = TRUE)
  pathway_names <- colnames(pathways)[which(colnames(pathways) != "Gene")]
  updateSelectInput(session,
                    "pathway",
                    choices = c("none", pathway_names))

  y.text <- reactiveVal(ggplot2::element_blank())
  dfDE <- reactiveVal(NULL)
  dfAHC <- reactiveVal(NULL)
  df1 <- reactiveVal(NULL)
  df2 <- reactiveVal(NULL)
  miRNA_list_file <- readData_example("miRNAs")
  miRNA_list <- dget(miRNA_list_file)
  updateSelectInput(session,
                    "miRNA",
                    choices = miRNA_list,
                    selected = "mmu-miR-16-5p")
  de_file <- readData_example("expression.csv")
  de_df <- readr::read_csv(de_file, col_names = TRUE)
  allGenes <- de_df$Gene
  updateSelectInput(session,
                    "gene",
                    choices = c("all", allGenes),
                    selected = "all")
  dfDE(de_df)


  observeEvent({
    input$miRNA}, {
      req(input$miRNA)
      if (input$miRNA != "User defined") {
        miR_file <- paste0(input$miRNA, "_ahc.csv")
        print(miR_file)
        ahc_file <- readData_example(miR_file)
        ahc_df <- readr::read_csv(ahc_file, col_names = TRUE)
        dfAHC(ahc_df)
      }
  }, priority = 10)

  observeEvent({
    input$miRNA}, {
      req(dfDE(),
          dfAHC())
      de <- dfDE()
      ahc <- dfAHC()
      df1(dplyr::full_join(ahc, de, by = "Gene"))
  }, priority = 1)




  observeEvent({
    input$deFile
    input$ahcFile}, {
      req(input$deFile,
          input$ahcFile)
      de <- readr::read_csv(input$deFile$datapath, col_names = TRUE)
      ahc <- readr::read_csv(input$ahcFile$datapath, col_names = TRUE)
      df1(dplyr::full_join(ahc, de, by = "Gene"))
      updateSelectInput(session,
                        "miRNA",
                        choices = c("User defined", miRNA_list),
                        selected = "User defined")
  })


  observeEvent({
    input$pathway
    input$seedMatch
    input$gene
    input$miRNA
    df1()}, {
      req(input$pathway,
          pathway_names,
          input$gene,
          input$miRNA,
          df1())

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
    req(input$pathway,
        pathway_names,
        input$gene,
        input$miRNA,
        df2())
    filter <- input$filter
    cuttoff <- input$cuttoff
    DE.AHC::heatmaper(data = df2(),
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
