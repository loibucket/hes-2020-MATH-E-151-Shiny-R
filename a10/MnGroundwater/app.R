#MnGroundwater
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(resampledata)
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))

#Here are the columns that are really factors
facs <- c(1,2,10)

#The user interface
header <- dashboardHeader(title = "Minnesota Groundwater Data - Loi",
                          titleWidth = 700)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=2,
           selectInput("factor1", "Factor 1", colnames(MnGroundwater)[facs]),
           selectInput("factor2", "Factor 2", colnames(MnGroundwater)[facs]),
           selectInput("num1", "Numeric 1", colnames(MnGroundwater)[-facs]),
           selectInput("num2", "Numeric 2", colnames(MnGroundwater)[-facs]),
           actionBttn("btnHist", "Histogram for Numeric 1"),
           actionBttn("btnBar", "Barplots for Factor 1"),
           actionBttn("btnBox", "Boxplot for Numeric 1 against Factor 1"),
           actionBttn("btnTable", "Table for Factor 2 against Factor 1"),
           actionBttn("btnScat", "Scatter plot for Numeric 2 against Numeric 1"),
           actionBttn("btnRegress", "Regression line for Numeric 2 against Numeric 1")
    ),
    column(width=10,
    fluidRow(stylesheet,
       plotOutput("plot",height="700px"),
       tableOutput("tbl2")),
       tableOutput("tbl")),
    )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")

#Additional functions are OK here, but no variables
MnGroundwater

server <- function(session, input, output) {
  output$tbl <- renderTable(MnGroundwater)
  #histogram
  observeEvent(input$btnHist, {
    output$plot <- renderPlot({
      x    <- MnGroundwater[,input$num1]
      par(mgp=c(6,1,0),mar=c(11,6,2,2))
      hist(x, breaks = 25, col = "#75AADB", border = "white",
           xlab = input$num1,
           main = "", warn.unused = FALSE, las=2)
    })
  })
  #barplot
  observeEvent(input$btnBar,{
    output$plot <- renderPlot({
      x <- MnGroundwater[,input$factor1]
      par(mar=c(10,5,2,2))
      barplot(table(x),las=2)
    })
  })
  #boxplot
  observeEvent(input$btnBox,{
    output$plot <- renderPlot({
      formula <- as.formula(paste(input$num1,"~",input$factor1))
      par(mgp=c(8,1,0),mar=c(10,6,2,2))
      boxplot(formula, MnGroundwater,las=2)
    })
  })
  #table
  observeEvent(input$btnTable,{
    x <- MnGroundwater[,input$factor1]
    y <- MnGroundwater[,input$factor2]
    tbl<- as.data.frame.matrix(table(x,y))
    output$tbl2 <- renderTable(tbl,rownames= TRUE)
  })
  #scatter
  observeEvent(input$btnScat,{
    x <- MnGroundwater[,input$num1]
    y <- MnGroundwater[,input$num2]
    output$plot <- renderPlot({plot(x,y, xlab = input$num1, ylab = input$num2)})
  })
  #regression
  observeEvent(input$btnRegress,{
    x <- MnGroundwater[,input$num1]
    y <- MnGroundwater[,input$num2]
    #formula <- as.formula(paste0(input$num2,"~",input$num1))
    formula <- as.formula(paste0("y~x"))
    MnGroundwater.lm <- lm(formula, MnGroundwater)
    output$plot <- renderPlot({plot(x,y, xlab = input$num1, ylab = input$num2)
      abline(MnGroundwater.lm, col="red")})
  })
}

#Run the app
shinyApp(ui = ui, server = server)