#GL2Z3
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")
#The user interface
header <- dashboardHeader(
  title = HTML("GL<sub>2</sub>Z<sub>3Loi</sub>"),
  titleWidth = 600
)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(
    column(
      width = 3,
      title = "Create a Matrix",
      radioButtons("det", "Choose the determinant",
                   choiceNames = c("Determinant 1", "Determinant -1"),
                   choiceValues = c(1,-1)
      ),
      radioButtons("trace", "Choose the trace",
        choiceNames = c("Trace 0","Trace 1","Trace -1"),
        choiceValues = c(0,1,-1)
      ),
      actionButton("generate","Create the Matrix"),
      uiOutput("matrixA"),
      actionButton("applysub","Apply to Subspaces"),
      actionButton("permute","Construct the Permutation"),
      actionButton("powers","Calculate Powers"),
      actionButton("multm1","Multiply A by -1"),
      actionButton("mult2","Multiply A by 2")
    ),
    column(
      width = 3,
      h3("The Four Subspaces"),
      h4("Subspace 1"),
      h5(jax.vecList(rbind(c(0,1,-1),c(0,0,0)))),
      h4("Subspace 2"),
      h5(jax.vecList(rbind(c(0,0,0),c(0,1,-1)))),
      h4("Subspace 3"),
      h5(jax.vecList(rbind(c(0,1,-1),c(0,1,-1)))),
      h4("Subspace 4"),
      h5(jax.vecList(rbind(c(0,1,-1),c(0,-1,1)))),
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
      uiOutput("power5"),
      uiOutput("power6"),
      uiOutput("power10"),
      uiOutput("power12"),
      uiOutput("power20"),
      uiOutput("power24")
    )
  )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("Z3calc.R")
source("permutecalc.R")


#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  #Variables that are shared among server functions
  A <- matrix(nrow = 2, ncol = 2)
  
  #Functions that respond to events in the input
  #According to the documentation, a global withMathJax() will not work here
  observeEvent(input$generate,{
     A <<- Z3CreateMatrix(as.numeric(input$det),as.numeric(input$trace))
     output$matrixA <- renderUI({jax.matrix(A, name = "A")})
     output$sub1 <- renderUI("")
     output$sub2 <- renderUI("")
     output$sub3 <- renderUI("")
     output$sub4 <- renderUI("")
     output$power1 <- renderUI("")
     output$power2 <- renderUI("")
     output$power3 <- renderUI("")
     output$power4 <- renderUI("")
     output$power5 <- renderUI("")
     output$power6 <- renderUI("")
     output$perm <- renderUI("")
  })
  observeEvent(input$applysub,{
      v1 <- c(1,0)
      x <- ActOnVector(A, v1)
      output$sub1 <- renderUI({jax.mTimesV("A",v1,x)})
      v2 <- c(0,1)
      x2 <- ActOnVector(A, v2)
      output$sub2 <- renderUI({jax.mTimesV("A",v2,x2)})
      v3 <- c(1,1)
      x3 <- ActOnVector(A, v3)
      output$sub3 <- renderUI({jax.mTimesV("A",v3,x3)})
      v4 <- c(1,-1)
      x4 <- ActOnVector(A, v4)
      output$sub4 <- renderUI({jax.mTimesV("A",v4,x4)})
  })
  observeEvent(input$permute,{
    suppressWarnings(fval <- sapply(1:4,Transform,A=A))
    output$perm <-renderUI(h3(Perm.cycle.convert(fval)))
  })
#It may be possible to put the MathJax stuff into a function
  observeEvent(input$powers,{
      output$power1 <- renderUI({jax.matrix(A, name = "A")})
      B <- Z3MatProd(A,A)
      output$power2 <- renderUI({jax.matrix(B, name = "A^2")})
      B2 <- Z3MatProd(A,B)
      output$power3 <- renderUI({jax.matrix(B2, name = "A^3")})
      B3 <- Z3MatProd(A,B2)
      output$power4 <- renderUI({jax.matrix(B3, name = "A^4")})
      B4 <- Z3MatProd(A,B3)
      output$power5 <- renderUI({jax.matrix(B4, name = "A^5")})
      B5 <- Z3MatProd(A,B4)
      output$power6 <- renderUI({jax.matrix(B5, name = "A^6")})

      B10 <- Z3MatProd(B4,B4)
      output$power10 <- renderUI({jax.matrix(B10, name = "A^{10}")})
      B12 <- Z3MatProd(B5,B5)
      output$power12 <- renderUI({jax.matrix(B12, name = "A^{12}")})
      B20 <- Z3MatProd(B10,B10)
      output$power20 <- renderUI({jax.matrix(B20, name = "A^{20}")})
      B24 <- Z3MatProd(B12,B12)
      output$power24 <- renderUI({jax.matrix(B24, name = "A^{24}")})
  })

  #Multiply by -1
  observeEvent(input$multm1,{
    A <<- matrix(vZ3Prod(A,-1),nrow = 2)
    output$matrixA <- renderUI({jax.matrix(A, name = "A")})
  })

  #Multiply by 2
observeEvent(input$mult2,{
  A <<- matrix(vZ3Prod(A,2),nrow = 2)
  output$matrixA <- renderUI({jax.matrix(A, name = "A")})
  })
}
  

#Run the app
shinyApp(ui = ui, server = server)