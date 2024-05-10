mod_content_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
     # box(width = 12, solidHeader = TRUE, status = "primary",
          tags$p(tags$h4(tags$strong(("Disease Models manuscript")), 
                         style = "font-size:25px;",
                         align = "center",
                         id = "page-text")),
          tags$p(tags$h4(style = "font-size:20px;", 
                         style = "line-height:1.5",
                         align = "left", 
                         id = "page-text",
                         "A manuscript entitled 'Computational identification 
                         of disease models through cross-species phenotype 
                         comparison' that describes the methods and data 
                         presented in the app has been submitted.")),
          tags$p(tags$h4(style = "font-size:20px;", 
                         style = "line-height:1.5",
                         align = "left", 
                         id = "page-text",
                         "For more information visit the", 
                         tags$a(href = "https://www.mousephenotype.org/", "IMPC portal"), 
                         )),
          tags$br()
   #       )
      ),
    fluidRow( 
      box(width = 12,
                 tags$div(
                   style = "text-align: center;",
                   tags$img(src = "Fig1abapp.png", 
                            width = 1500,
                            height = 900)
                 )
           #  ),
      #box(width = 6,
      #    tags$div(
      #      style = "text-align: center;",
      #      tags$img(src = "Figure2.jpeg", 
       #              width = 800,
        #             height = 500)
         # )
      )   
      
      )
    
  )
}

mod_content_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}
