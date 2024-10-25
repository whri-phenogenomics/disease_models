PhenoDigmOther_intro <- function() {
  tags$div(fluidRow(#box(width = 12, solidHeader = TRUE, status = "primary",
    #tags$p(tags$h4(tags$strong(("Description")),
    #               style = "font-size:20px;",
    #               align = "left",
    #              id = "page-text")),
    tags$p(
      tags$h4(
        style = "font-size:20px;",
        style = "line-height:1.5",
        "This table contains information on IMPC mouse knockout orthologs
                     to human genes without a known disease association according
                     to OMIM/Orphanet. The PhenoDigm algorithm computes the similarity
                     between the IMPC mouse phenotypes and the human phenotypes of
                     known disorders. Only mouse model - disease pairs with a PhenoDigm score > 40
                     are displayed here. The information shown in the table below includes the
                     IMPC mouse line description, human orthologue, OMIM/Orphanet disorder,
                     Phenodigm percentage score, and both the human (HPO) and mouse (MP) phenotypes
                     contributing to the score.",
        align = "left",
        id = "page-text"
      )
    )
    # )
    ), fluidRow(tags$i(
      tags$h5(
        "For multiple search terms, use '|' between the words, e.g. syndrome|demise",
        align = "right"
      )
  )))
}

mod_phenodigm_other_ui <- function(id) {
  ns <- NS(id)
  
  title <-  "IMPC mouse knockouts of non-disease genes and their similarity with
  known Mendelian disorders"
  intro <- PhenoDigmOther_intro
  
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
      "phenodigm_other_table"
    ))),
    fluidRow(downloadButton(
      ns("save_button"), "Download PhenoDigm scores"
    ))
  )
  
}

mod_phenodigm_other_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("read_data.R")
    
    table <- create_phenodigm_other_table()
    
    output$phenodigm_other_table <- renderReactable({
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
      filename = "impc_phenodigm_other.csv",
      content = function(file) {
        write.csv(table, file, row.names = FALSE)
      }
    )
    
  })
}
