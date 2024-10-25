GeneInfo_intro <- function() {
  tags$div(fluidRow(# box(width = 12, solidHeader = TRUE, status = "primary",
    tags$p(
      tags$h4(
        style = "font-size:20px;",
        style = "line-height:1.5",
        "IMPC Gene summary information for all the one-to-one mouse
                     orthologues of Mendelian disease associated genes.
                     It includes information on mouse lines that have entered
                     the IMPC phenotyping pipeline,associated significant abnormal
                     phenotypes, and whether a PhenoDigm score could be computed.",
        align = "left",
        id = "page-text"
      )
    )
    #)
    ), 
    fluidRow(tags$i(
      tags$h5(
        "For multiple search terms, use '|' between the words, e.g. syndrome|demise",
        align = "right"
      )
  )))
}

mod_gene_info_ui  <- function(id) {
  ns <- NS(id)
  
  title <-  "Gene based information for human disease associated genes"
  intro <- GeneInfo_intro
  
  tagList(
    fluidRow(
      tags$h3(
        tags$strong(title),
        style = "font-size:25px;",
        align = "center",
        id = "page-subheading"
      )
    ),
    tags$br(),
    intro(),
    tags$br(),
    fluidRow(reactableOutput(ns(
      "gene_summary_table"
    ))),
    fluidRow(downloadButton(ns("save_button"), "Download Gene Info"))
  )
  
}

mod_gene_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("read_data.R")
    
    table <- create_gene_summary_table()
    
    output$gene_summary_table <- renderReactable({
      reactable(
        table,
        rownames = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        striped = TRUE,
        theme = reactableTheme(backgroundColor = "#f7fbff")
      )
    })
    
    output$save_button <- downloadHandler(
      filename = "impc_gene_summary.csv",
      content = function(file) {
        write.csv(table, file, row.names = FALSE)
      }
    )
    
  })
}
