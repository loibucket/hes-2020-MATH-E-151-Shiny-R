#SL2F4
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")
#The user interface
header <- dashboardHeader(
  title = HTML("From Matrix to Permutation - SL(2,F<sub>4</sub>)"),
  titleWidth = 500
)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(
    column(
      width = 3,
      title = "Create a Matrix",
      radioButtons("trace", "Choose the trace",
        choiceNames = c("Trace 0","Trace 1","Trace x","Trace x+1"),
        choiceValues = c("0","1","x","x+1")
      ),
      actionButton("generate","Create the Matrix"),
      uiOutput("matrixA"),
      actionButton("applysub","Apply to Subspaces"),
      actionButton("permute","Construct the Permutation"),
      actionButton("powers","Calculate Powers")
    ),
    column(
      width = 3,
      h3("The Five Subspaces"),
      h4("Subspace 1"),
      h5(jax.vecList(rbind(c(0,1,"x","x+1"),c(0,0,0,0)))),
      h4("Subspace 2"),
      h5(jax.vecList(rbind(c(0,0,0,0),c(0,1,"x","x+1")))),
      h4("Subspace 3"),
      h5(jax.vecList(rbind(c(0,1,"x","x+1"),c(0,1,"x","x+1")))),
      h4("Subspace 4"),
      h5(jax.vecList(rbind(c(0,1,"x","x+1"),c(0,"x","x+1",1)))),
      h4("Subspace 5"),
      h5(jax.vecList(rbind(c(0,1,"x","x+1"),c(0,"x+1",1,"x"))))
    ),
    column(
      width = 3,
      h3("Action on Subspaces"),
      h4("Action on Subspace 1"),
      uiOutput("sub1"),
      h4("Action on Subspace 2"),
      uiOutput("sub2"),
      h4("Action on Subspace 3"),
      uiOutput("sub3"),
      h4("Action on Subspace 4"),
      uiOutput("sub4"),
      h4("Action on Subspace 5"),
      uiOutput("sub5"),
      h3("Cycle Representation"),
      uiOutput("perm")
         
    ),
    column(
      width = 3,
      h3("The Powers of A"),
      uiOutput("power1"),
      uiOutput("power2"),
      uiOutput("power3"),
      uiOutput("power4"),
      uiOutput("power5")
    )
  )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("F4calc.R")
source("permutecalc.R")





#Functions that read the input and modify the ouptpu and input
server <- function(session, input, output) {
  #Variables that are shared among server functions
  A <- matrix(c("0","x","x+1","1"))
    
  #Functions that respond to events in the input
  observeEvent(input$generate,{
     A <<- F4CreateMatrix(input$trace)
     output$matrixA <- renderUI({jax.matrix(A, name = "A")})
     output$sub1 <- renderUI("")
     output$sub2 <- renderUI("")
     output$sub3 <- renderUI("")
     output$sub4 <- renderUI("")
     output$sub5 <- renderUI("")
     output$power1 <- renderUI("")
     output$power2 <- renderUI("")
     output$power3 <- renderUI("")
     output$power4 <- renderUI("")
     output$power5 <- renderUI("")
     output$perm <- renderUI("")
  })
  observeEvent(input$applysub,{
      v1 <- c("1","0")
      x1 <- ActOnVector(A, v1)
      output$sub1 <- renderUI({jax.mTimesV("A",v1,x1)})
      v2 <- c("0","1")
      x2 <- ActOnVector(A, v2)
      output$sub2 <- renderUI({jax.mTimesV("A",v2,x2)})
      v3 <- c("1","1")
      x3 <- ActOnVector(A, v3)
      output$sub3 <- renderUI({jax.mTimesV("A",v3,x3)})
      v4 <- c("1","x")
      x4 <- ActOnVector(A, v4)
      output$sub4 <- renderUI({jax.mTimesV("A",v4,x4)})
      v5 <- c("1","x+1")
      x5 <- ActOnVector(A, v5)
      output$sub5 <- renderUI({jax.mTimesV("A",v5,x5)})
  })
  observeEvent(input$permute,{
      fval <- sapply(1:5,Transform,A=A)
      output$perm <-renderUI(h3(cycle.convert(fval)))
    
  })
  observeEvent(input$powers,{
    output$power1 <- renderUI({jax.matrix(A, name = "A")})
    B <- F4MatProd(A,A)
    output$power2 <- renderUI({jax.matrix(B, name = "A^2")})
    B2 <- F4MatProd(A,B)
    output$power3 <- renderUI({jax.matrix(B2, name = "A^3")})
    B3 <- F4MatProd(A,B2)
    output$power4 <- renderUI({jax.matrix(B3, name = "A^4")})
    B4 <- F4MatProd(A,B3)
    output$power5 <- renderUI({jax.matrix(B4, name = "A^5")})
    B5 <- F4MatProd(A,B4)
  })
}

#Run the app
shinyApp(ui = ui, server = server)