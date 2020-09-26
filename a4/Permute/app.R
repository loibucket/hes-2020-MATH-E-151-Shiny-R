#PermutePartial
#Change the name to Permutation when you finish the homework
library(shiny)
library(shinydashboard)
library(shinyWidgets)

header <- dashboardHeader(title = "Per Mutations")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(
    column(
      width = 4,
      box(
          width = NULL, height = 220,
          h3("Input"),
          textInput("atext","a","(12)"),
          textInput("btext","b","(13)")
      ),
      box(
          width = NULL, height = 150,
          h3("Products"),
          h4(uiOutput("prodab")),
          h4(uiOutput("prodba"))
      ),
      box(
        width = NULL, height = 150,
        h3("Inverses"),
        h4(uiOutput("inva")),
        h4(uiOutput("invb"))
      ),
      box(
        width = NULL, height = 150,
        h3("Conjugates"),
        h4(uiOutput("conja")),
        h4(uiOutput("conjb"))
      ),

    ),
    column(
        width = 4,
        box(
            width = NULL, height = 350,
            h3("Powers of a"),
            uiOutput("powersa"),
            tags$head(tags$style("#powersa{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        ),
        box(
          width = NULL, height = 350,
          h3("Powers of ab"),
          uiOutput("powersab"),
          tags$head(tags$style("#powersab{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
        ),
    ),
    column(
      width = 4,
      box(
        width = NULL, height = 350,
        h3("Powers of b"),
        uiOutput("powersb"),
        tags$head(tags$style("#powersb{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
      ),
      box(
        width = NULL, height = 350,
        h3("Powers of ba"),
        uiOutput("powersba"),
        tags$head(tags$style("#powersba{color:red; font-size:20px;
            font-style:italic;
            overflow-y:scroll; max-height:250px;}"))
      ),
    ),
    column(
        width = 4,
        actionBttn("btncalc","Calculate",
            color = "primary", size = "lg") #an awesome button from shinyWidgets
    )
  )    
)

ui <- dashboardPage(header, sidebar, body)


source("permutecalc.R")    


server <- function(input, output) {
  
  output$prodab <- renderUI("ab = (123)")
  output$prodba <- renderUI("ba = (132)")
  
  output$powersa <- renderUI(HTML(paste("(12)","I",sep = "<br/>")))
  output$powersb <- renderUI(HTML(paste("(13)","I",sep = "<br/>")))
  
  output$powersab <- renderUI(HTML(" "))
  output$powersba <- renderUI(HTML(" "))
  
  output$invab <- renderUI(HTML(" "))
  output$invba <- renderUI(HTML(" "))
  
  output$conjab <- renderUI(HTML(" "))
  output$conjba <- renderUI(HTML(" "))
  
  observeEvent(input$btncalc, {
    
    ab <- Perm.multiply(input$atext,input$btext)
    output$prodab <- renderUI(paste("ab =  ",ab))
    
    ba <- Perm.multiply(input$btext,input$atext)
    output$prodba <- renderUI(paste("ba =  ",ba))
    
    output$powersa <- renderUI(HTML(Perm.powerString(input$atext)))
    output$powersb <- renderUI(HTML(Perm.powerString(input$btext)))
    
    output$powersab <- renderUI(HTML(Perm.powerString(ab)))
    output$powersba <- renderUI(HTML(Perm.powerString(ba)))
    
    output$inva <- renderUI(HTML(Perm.inverse(input$atext)))
    output$invb <- renderUI(HTML(Perm.inverse(input$btext)))
    
    output$conja <- renderUI(HTML(Perm.conjugate(input$atext,input$btext)))
    output$conjb <- renderUI(HTML(Perm.conjugate(input$btext,input$atext)))
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
