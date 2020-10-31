#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
Sys.setenv(RGL_USE_NULL = TRUE) #suppress popup 3D display
library("rgl") #for 3D plotting
library(shinyjs)
source("jaxmat.R") #for displaying mathematics
stylesheet <- tags$head(
  tags$style(
    HTML(
      '
* {
font-family: "Courier New", Courier, monospace;
}

.main-header .logo {
font-family: Arial, Helvetica, sans-serif;
font-weight: bold;
font-size: 24px;
}

h1, h2, h3, h4, h5, h6{
margin-left: 20px;
font-family: "Courier New", Courier, monospace;
font-weight: bold;
}

h1{
color: blue;
}

h2, h3{
color: blue;
}

h6{
color: gray;
font-style: italic;
}

table{
margin-left:20px;
font-size:20px
}

th,td {
padding-left:10px;
padding-right:10px;
}

tr:nth-child(even) {background-color: lightgray;}

th {
background-color: green;
color: white;
}

'
    )
  )
)

#the matrices
m0120 = matrix(c(0, 1, 2, 0), nrow = 2, ncol = 2, byrow = TRUE)
m1121 = matrix(c(1, 1, 2, 1), nrow = 2, ncol = 2, byrow = TRUE)
m2002 = matrix(c(2, 0, 0, 2), nrow = 2, ncol = 2, byrow = TRUE)
m2122 = matrix(c(2, 1, 2, 2), nrow = 2, ncol = 2, byrow = TRUE)
m0210 = matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
m2212 = matrix(c(2, 2, 1, 2), nrow = 2, ncol = 2, byrow = TRUE)
m1001 = matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
m1211 = matrix(c(1, 2, 1, 1), nrow = 2, ncol = 2, byrow = TRUE)
m0000 = matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)

j0120 = jax.matrix(matrix(c(0, 1, 2, 0), nrow = 2, ncol = 2, byrow = TRUE))
j1121 = jax.matrix(matrix(c(1, 1, 2, 1), nrow = 2, ncol = 2, byrow = TRUE))
j2002 = jax.matrix(matrix(c(2, 0, 0, 2), nrow = 2, ncol = 2, byrow = TRUE))
j2122 = jax.matrix(matrix(c(2, 1, 2, 2), nrow = 2, ncol = 2, byrow = TRUE))
j0210 = jax.matrix(matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE))
j2212 = jax.matrix(matrix(c(2, 2, 1, 2), nrow = 2, ncol = 2, byrow = TRUE))
j1001 = jax.matrix(matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE))
j1211 = jax.matrix(matrix(c(1, 2, 1, 1), nrow = 2, ncol = 2, byrow = TRUE))
j0000 = jax.matrix(matrix(c(0, 0, 0, 0), nrow = 2, ncol = 2, byrow = TRUE))

add = jaxD(paste0("+"))
subtract = jaxD(paste0("-"))
multiply = jaxD(paste0("`times"))
divide = jaxD(paste0("`div"))

#The user interface
header <- dashboardHeader(title = "Loi Conformal Matrix Arithmetic", titleWidth = 600)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(fluidRow(
  useShinyjs(),
  stylesheet,
  
  fluidRow(h1("press any button!")),
    
  
  column(
    width = 3,
    fluidRow(
    actionButton("a0120", h3(j0120)),
    actionButton("a1121", h3(j1121)),
    actionButton("a2002", h3(j2002)),
    actionButton("a2122", h3(j2122)),
    actionButton("a0210", h3(j0210)),
    actionButton("a2212", h3(j2212)),
    actionButton("a1001", h3(j1001)),
    actionButton("a1211", h3(j1211)),
    actionButton("a0000", h3(j0000))
    ),
    fluidRow(
    h1("A"),
    h1(uiOutput("A"))
    )
  ),

  column(
    width = 3,
    actionButton("add", h1(add)),
    actionButton("subtract", h1(subtract)),
    actionButton("multiply", h1(multiply)),
    actionButton("divide", h1(divide)),
     fluidRow(
    h1("Operation"),
    h1(uiOutput("Operation"))
    ) 
  ),
  
  column(
    width = 3,
    fluidRow(
      actionButton("b0120", h3(j0120)),
      actionButton("b1121", h3(j1121)),
      actionButton("b2002", h3(j2002)),
      actionButton("b2122", h3(j2122)),
      actionButton("b0210", h3(j0210)),
      actionButton("b2212", h3(j2212)),
      actionButton("b1001", h3(j1001)),
      actionButton("b1211", h3(j1211)),
      actionButton("b0000", h3(j0000))
    ),
    fluidRow(
      h1("B"),
      h1(uiOutput("B"))
    )
  ),
  
  column(
    width = 3,
    fluidRow(
      h1("="),
      h1(uiOutput("Res"))
    )
  )
))

ui <-
  dashboardPage(header, sidebar, body, skin = "green") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")
#Any images, audio, or stylesheet must go into a subfolder named www
#Additional functions are OK here, but no variables

server <- function(session, input, output) {
  
  output$A <- renderUI(j0120)
  output$B <- renderUI(j0120)
  output$Operation <- renderUI(add)
  output$Res <- renderUI(jax.matrix((m0120+m0120)%%3))
  
  a = m0120
  b = m0120
  op = "add"
  
  update = function(){
    if(op=="add"){
      output$Res <- renderUI(jax.matrix((a+b)%%3))
    }
    
    else if(op=="subtract"){
      output$Res <- renderUI(jax.matrix((a-b)%%3))
    }
    
    else if(op=="multiply"){
      output$Res <- renderUI(jax.matrix((a%*%b)%%3))
    }
    
    else if(op=="divide"){
      if (all(b == m0000)){      output$Res <- renderUI("no solution!")
      } else {      
        output$Res <- renderUI(jax.matrix((a%*%solve(b))%%3))
      }
    }
  }
  
  ###a
  observeEvent(input$a0120, {
    a <<- m0120
    output$A <- renderUI(j0120)
    update()
  })
  observeEvent(input$a1121, {
    a <<- m1121
    output$A <- renderUI(j1121)
    update()
  })
  observeEvent(input$a2002, {
    a <<- m2002
    output$A <- renderUI(j2002)
    update()
  })
  observeEvent(input$a2122, {
    a <<- m2122
    output$A <- renderUI(j2122)
    update()
  })
  observeEvent(input$a0210, {
    a <<- m0210
    output$A <- renderUI(j0120)
    update()
  })
  observeEvent(input$a2212, {
    a <<- m2212
    output$A <- renderUI(j2212)
    update()
  })
  observeEvent(input$a1001, {
    a <<- m1001
    output$A <- renderUI(j1001)
    update()
  })
  observeEvent(input$a1211, {
    a <<- m1211
    output$A <- renderUI(j1211)
    update()
  })
  observeEvent(input$a0000, {
    a <<- m0000
    output$A <- renderUI(j0000)
    update()
  })
  
  
  ###b
  observeEvent(input$b0120, {
    b <<- m0120
    output$B <- renderUI(j0120)
    update()
  })
  observeEvent(input$b1121, {
    b <<- m1121
    output$B <- renderUI(j1121)
    update()
  })
  observeEvent(input$b2002, {
    b <<- m2002
    output$B <- renderUI(j2002)
    update()
  })
  observeEvent(input$b2122, {
    b <<- m2122
    output$B <- renderUI(j2122)
    update()
  })
  observeEvent(input$b0210, {
    b <<- m0210
    output$B <- renderUI(j0120)
    update()
  })
  observeEvent(input$b2212, {
    b <<- m2212
    output$B <- renderUI(j2212)
    update()
  })
  observeEvent(input$b1001, {
    b <<- m1001
    output$B <- renderUI(j1001)
    update()
  })
  observeEvent(input$b1211, {
    b <<- m1211
    output$B <- renderUI(j1211)
    update()
  })
  observeEvent(input$b0000, {
    b <<- m0000
    output$B <- renderUI(j0000)
    update()
  })
  
  
  ###ops
  observeEvent(input$add, {
    op <<- "add"
    output$Operation <- renderUI(add)
    update()
  })
  observeEvent(input$subtract, {
    op <<- "subtract"
    output$Operation <- renderUI(subtract)
    update()
  })
  observeEvent(input$multiply, {
    op <<- "multiply"
    output$Operation <- renderUI(multiply)
    update()
  })
  observeEvent(input$divide, {
    op <<- "divide"
    output$Operation <- renderUI((divide))
    update()
  })

}

#Run the app
shinyApp(ui = ui, server = server)

  
       
