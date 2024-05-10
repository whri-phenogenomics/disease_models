
PhenoDigm_intro <- function() {
  tags$div(
    fluidRow(
      #box(width = 12, solidHeader = TRUE, status = "primary",
          #tags$p(tags$h4(tags$strong(("Description")), 
          #               style = "font-size:20px;",
          #               align = "left",
           #              id = "page-text")),
          tags$p(tags$h4(style = "font-size:20px;",
                     style = "line-height:1.5",
      "This table contains information on IMPC mouse knockout orthologs to 
      human disease associated genes for which overlapping phenotypes were 
      identified based on automated identification through the PhenoDigm algorithm. 
      The information displayed here includes the IMPC mouse line description, 
      human orthologue, associated OMIM/Orphanet disorder, Phenodigm percentage score, 
      human (HPO) and mouse(MP) phenotypes contributing to the score.",
        align = "left", 
      id = "page-text")
      )
     # )
    ),
    fluidRow(
      tags$i(tags$h5(
        "For multiple search terms, use '|' between the words, e.g. syndrome|demise",
        align = "right")
      )
    )
  )
}

mod_phenodigm_ui <- function(id){
  ns <- NS(id)
  
  title <-  "IMPC models of Mendelian disease"
  intro <- PhenoDigm_intro
  
  tagList(
    fluidRow(
      tags$h3(tags$strong(title), 
              style = "font-size:25px;",
              align = "center", 
              id = "page-subheading")
    ),
    tags$br(),
    intro(),
    tags$br(),
    fluidRow(DT::dataTableOutput(ns("phenodigm_table"))),
    fluidRow(downloadButton(ns("save_button"),"Download PhenoDigm scores"))
    )
  
}






mod_phenodigm_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    source("read_data.R")

    table <- create_phenodigm_table()
        
    output$phenodigm_table <- DT::renderDataTable({
      DT::datatable(table, rownames = TRUE, filter = "top",
        options = list(scrollX = TRUE, scrollY = TRUE, autoWidth = TRUE,
          search = list(regex = TRUE, caseInsensitive = TRUE), pageLength = 10
        )
      )
    })
    
    output$save_button <- downloadHandler(
      filename = "impc_phenodigm.csv",
      content = function(file) {
        write.csv(table, file, row.names = FALSE)
      }
    )

  })
}

