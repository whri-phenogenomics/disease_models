library(shiny)
library(shinydashboard)
library(fst)
library(reactable)
library(plotly)
library(dplyr)
library(stringr)

source("mod_home.R")
source("mod_phenodigm.R")
source("mod_gene_info.R")
source("mod_phenodigm_other.R")
source("mod_content.R")



tab_defs <- data.frame(
  text = c(
    "Home",
    "PhenoDigm Scores",
    "Gene Summary",
    "PhenoDigm Other",
    "Publication"
  ),
  name = c(
    "HomePage",
    "PhenoDigm",
    "GeneInfo",
    "PhenoDigmOther",
    "Review"
  ),
  tab_icon = c("home", "table", "dna", "table", "newspaper")
)

tab_builder <- function(text, name, tab_icon) {
  menuItem(text,
           tabName = name,
           icon = icon(tab_icon, lib = 'font-awesome'))
}

all_tabs = list()
for (tab in 1:nrow(tab_defs)) {
  all_tabs[[tab]] <-  do.call(tab_builder, tab_defs[tab, ])
}

sidebar <- dashboardSidebar(sidebarMenu(id = 'tabs', .list = all_tabs))

app_ui <- function(request) {
  tagList(dashboardPage(
    dashboardHeader(title = "Disease Models"),
    sidebar,
    dashboardBody(
      includeCSS("www/style.css"),
      tabItems(
        tabItem("HomePage", mod_home_ui("HomePage")),
        tabItem("PhenoDigm", mod_phenodigm_ui("PhenoDigm")),
        tabItem("GeneInfo", mod_gene_info_ui("GeneInfo")),
        tabItem(
          "PhenoDigmOther",
          mod_phenodigm_other_ui("PhenoDigmOther")
        ),
        tabItem("Review", mod_content_ui("Review"))
        
      )
    )
  ))
  
}

app_server <- function(input, output, session) {
  observe({
    tab <- input$tabs
    
    if (tab == "HomePage") {
      server <- mod_home_server
    } else if (tab == "PhenoDigm") {
      server <- mod_phenodigm_server
    } else if (tab == "GeneInfo") {
      server <- mod_gene_info_server
    } else if (tab == "PhenoDigmOther") {
      server <- mod_phenodigm_other_server
    } else if (tab == "Review") {
      server <- mod_content_server
    }
    
    server(tab)
    
  })
  
}

shinyApp(app_ui, app_server)
