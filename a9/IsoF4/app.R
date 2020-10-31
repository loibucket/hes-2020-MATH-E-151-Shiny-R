#SL2F4
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")
#The user interface
header <- dashboardHeader(
  title = HTML("IsoF<sub>4_Loi</sub>"),
  titleWidth = 500
)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(
      column(
      width = 2,
      title = "Create a Matrix",
      radioButtons("trace", "Choose the trace",
        choiceNames = c("Trace 0","Trace 1","Trace x","Trace x+1"),
        choiceValues = c("0","1","x","x+1"))
      
      ),column(width=2,
      actionButton("generateA",jaxI(paste0("Create`:Matrix`:A"))),
      uiOutput("matrixA"),      
      actionButton("generateB",jaxI(paste0("Create`:Matrix`:B"))),
      uiOutput("matrixB")
      
      ),column(width = 2,
      actionButton("permuteA",jaxI(paste0("Construct`:Permutation`:p_{A}"))),
      uiOutput("permA"),
      actionButton("permuteB",jaxI(paste0("Construct`:Permutation`:p_{B}"))),
      uiOutput("permB"),
      actionButton("calculatePAPB",jaxI(paste0("Calculate`:p_{1}=p_{A}p_{B}"))),
      uiOutput("calcPAPB"),  

      ),column(width = 2,
      actionButton("calculateAB",jaxI(paste0("Calculate`:AB"))),
      uiOutput("calcAB"), 
      actionButton("permuteAB",jaxI(paste0("Construct`:Permutation`:p_{2}=p_{AB}"))),
      uiOutput("permAB"),  
       
      ),column(width = 2,       
      actionButton("equal",jaxI(paste0("Does`:p_{1}=p_{2}`:?"))),
      uiOutput("equ"),    
    ),
  )
)
ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
source("F4calc.R")
source("permutecalc.R")

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  #Variables that are shared among server functions
  #static solution
  A <- matrix(c("0","1","1","0"),nrow=2,ncol=2)
  B <- matrix(c("1","0","1","1"),nrow=2,ncol=2)
  AB <- matrix(c("1","1","1","0"),nrow=2,ncol=2)
  pA <- "(12)(45)"
  pB <- "(13)(45)"
  pApB <- "(132)"
  pAB <- "(132)"
  
  #Functions that respond to events in the input
  ##A
  observeEvent(input$generateA,{
     A <<- F4CreateMatrix(input$trace)
     output$matrixA <- renderUI({jax.matrix(A, name = "A")})
  })
  
  ##B
  observeEvent(input$generateB,{
    B <<- F4CreateMatrix(input$trace)
    #
    output$matrixB <- renderUI({jax.matrix(B, name = "B")})
  })
  

  ##pA
  observeEvent(input$permuteA,{
    suppressWarnings(fval <- sapply(1:5,Transform,A=A))
    pA <<- cycle.convert(fval)
    #
    output$permA <-renderUI(h3(jaxI(paste0("p_{A}=",pA))))
    output$matrixA <- renderUI({jax.matrix(A, name = "A")})    
  })
  
  ##pB
  observeEvent(input$permuteB,{
    suppressWarnings(fval <- sapply(1:5,Transform,A=B))
    pB <<- cycle.convert(fval)
    #
    output$permB <-renderUI(h3(jaxI(paste0("p_{B}=",pB))))
    output$matrixB <- renderUI({jax.matrix(B, name = "B")})
  })

  ##p1 = pApB
  observeEvent(input$calculatePAPB,{
    suppressWarnings(fval <- sapply(1:5,Transform,A=A))
    pA <<- cycle.convert(fval)
    #
    suppressWarnings(fval <- sapply(1:5,Transform,A=B))
    pB <<- cycle.convert(fval)
    #
    pApB <<- multiply(pA,pB)
    #
    output$calcPAPB <-renderUI(h3(jaxI(paste0("p_{1}=",pApB))))
    output$permB <-renderUI(h3(jaxI(paste0("p_{B}=",pB))))
    output$permA <-renderUI(h3(jaxI(paste0("p_{A}=",pA))))
    output$matrixB <- renderUI({jax.matrix(B, name = "B")})
    output$matrixA <- renderUI({jax.matrix(A, name = "A")})       
  })    
  
  
  ##AB
  observeEvent(input$calculateAB,{
    AB <<- F4MatProd(A,B)
    #
    output$calcAB <- renderUI({jax.matrix(AB, name = "AB")})
    output$matrixB <- renderUI({jax.matrix(B, name = "B")})
    output$matrixA <- renderUI({jax.matrix(A, name = "A")})
  })
  
  
  ##p2 = pAB
  observeEvent(input$permuteAB,{
    AB <<- F4MatProd(A,B)
    #
    suppressWarnings(fval <- sapply(1:5,Transform,A=AB))
    pAB <<- cycle.convert(fval)
    #
    output$permAB <-renderUI(h3(jaxI(paste0("p_{2}=",pAB))))
    output$calcAB <- renderUI({jax.matrix(AB, name = "AB")})
    output$matrixB <- renderUI({jax.matrix(B, name = "B")})
    output$matrixA <- renderUI({jax.matrix(A, name = "A")})          
  })
  
  ##p1=p2?
  observeEvent(input$equal,{
    if(pAB==pApB){output$equ <-renderUI(h3("YES!"))}
    else {output$equ <-renderUI(h3("NO!"))}
    output$permAB <-renderUI(h3(jaxI(paste0("p_{2}=",pAB))))
    output$calcPAPB <-renderUI(h3(jaxI(paste0("p_{1}=",pApB))))
    output$permB <-renderUI(h3(jaxI(paste0("p_{B}=",pB))))
    output$permA <-renderUI(h3(jaxI(paste0("p_{A}=",pA))))
    output$calcAB <- renderUI({jax.matrix(AB, name = "AB")})
    output$matrixB <- renderUI({jax.matrix(B, name = "B")})
    output$matrixA <- renderUI({jax.matrix(A, name = "A")})          
  })
}

#Run the app
shinyApp(ui = ui, server = server)