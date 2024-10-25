mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    tags$h2(
      tags$strong("IMPC Disease Models Portal"),
      style = "font-size:40px;",
      align = "center",
      id = ns("page-title")
    )
  ),
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      tags$p(
        tags$h4(
          tags$strong(("About")),
          style = "font-size:25px;",
          align = "left",
          id = "page-text"
        )
      ),
      tags$p(
        tags$h4(
          style = "font-size:20px;",
          style = "line-height:1.5",
          "The Disease Models Portal contains information on the International Mouse
      Phenotyping Consortium (IMPC) mouse knockouts orthologues of human disease
      associated genes and the phenotypic similarity between them computed by the
      PhenoDigm algorithm.",
          align = "left",
          id = "page-text"
        )
      ),
      tags$p(
        tags$h4(
          style = "font-size:20px;",
          style = "line-height:1.5",
          "The mouse knockout data including mouse line information
                     and the associated abnormal phenotypes assessed through the
                     phenotyping pipeline can be accessed through the",
          tags$a(href = "https://www.mousephenotype.org/", "IMPC portal"),
          ".The human phenotype annotations
                     corrsponding to ",
          tags$a(href = "https://www.omim.org/", "OMIM"),
          "and",
          tags$a(href = "https://www.orpha.net/", "Orphanet"),
          "disorders used in the pipeline were obtained from the",
          tags$a(href = "https://hpo.jax.org", "Human Phenotype Ontology (HPO)"),
          "resource.",
          align = "left",
          id = "page-text"
        )
      ),
      tags$p(
        tags$h4(
          style = "font-size:20px;",
          style = "line-height:1.5",
          "At present, based on IMPC Data Release 20.1, this portal
                     contains information for 1,311 mouse gene knockouts with a
                     human orhtologue associated to disease with some sort of
                     phenotypic overlap.",
          align = "left",
          id = "page-text"
        )
      ),
      tags$p(
        tags$h4(
          style = "font-size:20px;",
          style = "line-height:1.5",
          "For more information on Methods and Datasets visit the
                     'Publication' tab.",
          align = "left",
          id = "page-text"
        )
      ),
      tags$br(),
    )
  ),
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      fluidRow(column(8, tags$p(
        tags$h4(
          tags$strong(("Gene summary")),
          style = "font-size:25px;",
          align = "left",
          id = "page-text"
        )
      ), tags$p(
        tags$h4(
          style = "font-size:20px;",
          "Gene based search to display a summary of the information
    contained on this portal.
    Type the gene symbol in the Search Gene box.
    For more detailed information and access to the full dataset available,
                   go to the 'Disease models' tab.",
          align = "left",
          id = "page-text"
        )
      )), column(
        4, box(
          title = "Search Gene",
          status = "warning",
          textInput(
            inputId = ns("gene_search"),
            label = NULL,
            placeholder = "SPATA16",
            width = 200
          )
        )
      ), ),
      tags$br(),
      fluidRow(
        valueBoxOutput(ns("Gene_in_IMPC_Box"), width = 4),
        valueBoxOutput(ns("Gene_in_IMPC_Disease_Box"), width = 4),
        valueBoxOutput(ns("Phenotypes_Available_Box"), width = 4),
        valueBoxOutput(ns("PhenoDigm_Score_Box"), width = 4),
        valueBoxOutput(ns("Mouse_Phenotypes_Box"), width = 4),
        valueBoxOutput(ns("Human_Phenotypes_Box"), width = 4)
        
      )
    )
  ),
  fluidRow(tags$p(
    tags$h4(
      "Last updated 07.05.2024",
      style = "font-size:20px;",
      align = "left",
      id = "page-text"
    )
  )))
  
}



mod_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("read_data.R")
    
    gene_info <- load_gene_summary() %>%
      mutate(disease_gene = ifelse(disorder_id != "-", "yes", "no")) %>%
      select(
        gene_symbol,
        disease_gene,
        IMPC_pipeline,
        IMPC_phenotypes,
        PhenoDigm_match,
        max_score,
        disorder_name
      ) %>%
      rename(disorder_name_gene = disorder_name)
    
    model_info <- load_phenodigm()
    
    
    model_info_summary = model_info %>%
      select(gene_symbol,
             disorder_name,
             score,
             query_phenotype,
             match_phenotype) %>%
      distinct() %>%
      group_by(gene_symbol) %>%
      summarise(
        disorder_name = paste0(unique(disorder_name), collapse = "|"),
        score = paste0(unique(score), collapse = "|"),
        query_phenotype = paste0(unique(query_phenotype), collapse = "|"),
        match_phenotype = paste0(unique(match_phenotype), collapse = "|")
      )
    
    gene_info_model = gene_info %>%
      left_join(model_info_summary, by = c("gene_symbol" = "gene_symbol"))
    
    gene_selected <- reactive({
      gene_info_model %>%
        filter(gene_symbol %in% c(toupper(as.character(
          input$gene_search
        ))))
      
    })
    
    
    
    ## box for gene summary: gene in IMPC box
    output$Gene_in_IMPC_Box <- renderValueBox({
      valueBox(
        value = tags$p("Gene in IMPC", style = "font-size: 50%;"),
        #ifelse(dim(gene_selected())[1] == 0, "Gene entered IMPC pipeline",
        #       "Gene has not entered IMPC pipeline"),
        ifelse(
          gene_selected()$IMPC_pipeline == "yes",
          "Gene entered IMPC pipeline",
          "Gene has not entered IMPC pipeline"
        ),
        
        color = "orange"
      )
    })
    
    ## box for gene summary: human disease association
    output$Gene_in_IMPC_Disease_Box <- renderValueBox({
      valueBox(
        value = tags$p("Gene with human disease association", style = "font-size: 50%;"),
        # ifelse(dim(gene_selected())[1] == 0, "-",
        #        gene_selected()$disorder_name),
        ifelse(
          gene_selected()$disease_gene == "yes",
          gene_selected()$disorder_name_gene,
          "-"
        ),
        color = "orange"
      )
    })
    
    
    ## box for gene summary: abnormal phenotypes available
    output$Phenotypes_Available_Box <- renderValueBox({
      valueBox(
        value = tags$p("Associated abnormal phenotypes", style = "font-size: 50%;"),
        # ifelse(dim(gene_selected())[1] ==0, "No","Yes"),
        ifelse(
          gene_selected()$IMPC_phenotypes == "yes",
          "Mouse with associated abnormal phenotypes",
          "-"
        ),
        color = "orange"
      )
    })
    
    ## box for gene summary: PhenoDigm score
    output$PhenoDigm_Score_Box <- renderValueBox({
      valueBox(
        value = tags$p("Maximum PhenoDigm score", style = "font-size: 50%;"),
        # ifelse(dim(gene_selected())[1] ==0, "-",
        #        gene_selected()$score),
        ifelse(
          !is.na(gene_selected()$max_score),
          gene_selected()$max_score,
          "-"
        ),
        color = "blue"
      )
    })
    
    ## box for gene summary: mouse phenotypes
    output$Mouse_Phenotypes_Box <- renderValueBox({
      valueBox(
        value = tags$p("Matching mouse phenotypes", style = "font-size: 50%;"),
        #ifelse(dim(gene_selected())[1] ==0, "-",
        #      gene_selected()$match_phenotype),
        ifelse(
          !is.na(gene_selected()$match_phenotype),
          gene_selected()$match_phenotype,
          "-"
        ),
        
        
        color = "blue"
      )
    })
    
    
    ## box for gene summary: human phenotypes
    output$Human_Phenotypes_Box <- renderValueBox({
      valueBox(
        value = tags$p("Matching disorder phenotypes", style = "font-size: 50%;"),
        #ifelse(dim(gene_selected())[1] ==0, "-",
        #       gene_selected()$query_phenotype),
        ifelse(
          !is.na(gene_selected()$query_phenotype),
          gene_selected()$query_phenotype,
          "-"
        ),
        color = "blue"
      )
    })
    
    
  })
}
