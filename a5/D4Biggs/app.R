#Starter file for any Shiny dashboard app
#This should replace the default app.r that displays Old Faithful data
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
#The user interface
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Lucida Console", Monaco, monospace;
      font-weight: bold;
      font-size: 34px;
      background-color: black;
    }
  ')
))
header <- dashboardHeader(title = "D4 According to Biggs and Loi",titleWidth=800)
sidebar <- dashboardSidebar(
  width = 100,
  actionButton("btninit", "Initialize"),
  actionButton("btni","Apply i"),
  actionButton("btnr","Apply r"),
  actionButton("btns","Apply s"),
  actionButton("btnt","Apply t"),
  actionButton("btnw","Apply w"),
  actionButton("btnx","Apply x"),
  actionButton("btny","Apply y"),
  actionButton("btnz","Apply z")
)
body <- dashboardBody(stylesheet,
  fluidRow(
    column(
      width = 12,
      plotOutput("configs", height = 200)
    )
  ),
  fluidRow(
    column(
      width = 4,
      plotOutput("square", height = 300)
    ),
    column(
      width = 8,
      dataTableOutput("multable")
#      tableOutput("multable")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin="yellow")

#Functions that implement the mathematics
source("d4calc.R")



#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  #Variables that are shared among server functions
  D4DF <- D4.makeDataFrame()
  config <- "ABCD"
  #Initialization
  output$configs <- renderPlot(D4.showConfigs(D4DF))
  output$square <- renderPlot(D4.showSquare(config))
  tbl <-outer(D4DF$name,D4DF$name,vD4.multiply,DF=D4DF)
  colnames(tbl) <- D4DF$name
  rownames(tbl) <- D4DF$name 
  #Use options to suppress the fancy controls
  output$multable <- renderDataTable(tbl, options = list(dom = "t"))
#  output$multable <- renderTable(tbl, rownames = TRUE)
    #Functions that respond to events in the input
  observeEvent(input$btninit,{
    config <<- "ABCD"
    output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btni,{
      config <<- D4.apply("i",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnr,{
      config <<- D4.apply("r",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btns,{
      config <<- D4.apply("s",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnt,{
    config <<- D4.apply("t",config)
    output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnw,{
    config <<- D4.apply("w",config)
    output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnx,{
      config <<- D4.apply("x",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btny,{
      config <<- D4.apply("y",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
  observeEvent(input$btnz,{
      config <<- D4.apply("z",config)
      output$square <- renderPlot(D4.showSquare(config))
  })
}

#Run the app
shinyApp(ui = ui, server = server)