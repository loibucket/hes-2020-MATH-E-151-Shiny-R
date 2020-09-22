#Arithmetic
#August 30, 2020
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
    h2{
      font-family: "Gabriola", cursive;
      color:purple;
      font-size: 150%
    }
  ')
))
#The user interface
#The user interface
header <- dashboardHeader(title = "Loi Arithmetic")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    fluidRow(stylesheet,
        column(width = 6,
            sliderInput("left", "Left operand",-7, 7, 1 ),
            sliderInput("right", "Right operand",-7, 7, 1 ),
            radioButtons(
                "operand",
                "Type",
                choices = c("sum"="sum","product"="product")
            ),
            textOutput("choice"),
            h2(uiOutput("results")),
            h2(uiOutput("formula")),
            )
        )
    )
ui <- dashboardPage(header, sidebar, body)

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
#This works without any explicit reactive function
    output$choice <- renderText(paste("You chose", input$operand))
    output$results <- renderUI(
        if (input$operand=="sum"){
            paste0("Sum is ",input$left+input$right)
        } else {
            paste0("Product is ",input$left*input$right)
        }
    )
    output$formula <- renderUI(
      if (input$operand=="sum"){
        jaxI(paste0('x=',input$left,',y=',input$right,',x+y=',input$left+input$right))
      } else {
        jaxI(paste0('x=',input$left,',y=',input$right,',xy=',input$left*input$right))
      }
    )
#The next two lines fail
# sum <- input$left+input$right     #not allowed
# output$results <- renderUI(paste0("Sum is ",sum))    
#  Use a reactive expression instead and it works   
#   sum <- reactive({input$left+input$right})  
#   output$results <- renderUI(paste0("Sum is ",sum()))
# Defining an ordinary function also works
#  sum <- function() input$left+input$right #Shiny must know this is reactive and reevaluate it
#  output$results <- renderUI(paste0("Sum is ",sum()))
}

#Run the app
shinyApp(ui = ui, server = server)