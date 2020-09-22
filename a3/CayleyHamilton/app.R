#Symmetric
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(pracma)   #for rref()
library(expm)
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Cayley-Hamilton Matrix",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
      h3("Make a matrix"),
      actionBttn("btnmake","Make a new matrix"),
      uiOutput("matA"),
      uiOutput("matA2"),
      uiOutput("matA3"),
      #actionBttn("btnpoly","Find the Polynomial"),
      #uiOutput("poly"),
      #actionBttn("btnroot","Find the Eigenvalues"),
      #uiOutput("roots"),
      actionBttn("btnvect","Find Eigenvectors"),
      uiOutput("vect"),
      actionBttn("btnprop","Check Properties")
    ),
    column(width = 8,
      h3("Flexible Space for Computation"),
      uiOutput("ctrl1"),
      uiOutput("flexmsg1"),
      #uiOutput("flexmsg2"),
      uiOutput("ctrl2"),
      uiOutput("ctrl3"),
      uiOutput("ctrl4"),
      uiOutput("ctrl5"),
      uiOutput("ctrl6")
    )
  
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")
#Any images or stylesheet must go into a www subfolder

#Additional functions are OK here, but no variables


server <- function(session, input, output) {
  #Variables that are shared among server functions (use <<-)
  M <- matrix(nrow = 3, ncol = 3)   #simulated matrix of data
  A <- matrix(ncol = 3, nrow = 3)   #symmetric matrix to diagonalize
  w <- numeric(4)     #vector to start Axler's method
  S <- matrix(nrow = 3, ncol = 4)  #powers of A acting on w
  rts <- numeric(4)   #the eigenvalues (smallest first)
  D <- matrix(ncol = 3, nrow = 3)  #diagonal matrix of eigenvalues
  v1 <- numeric(4)
  v2 <- numeric(4)
  v3 <- numeric(4)
  v4 <- numeric(4)
  P <- matrix(ncol = 3, nrow = 3)  #matrix of eigenvectors
  matI <- diag(1, nrow = 3)  #identity matrix
  B <- matrix(ncol = 3, nrow = 3)  #projection matrix for eigenvector
  pPrint <- ""   #printable version of polynomial
  pEval <-""     #R code version of polynomial
  pCoeff <- numeric(4)  #coefficients in the polynomial
  
  a<-0
  b<-0
  c<-0
  d<-0
  e<-0
  f<-0
  g<-0
  h<-0
  i<-0
  
  p0<-0
  p1<-1
  p2<-2
  p3<-3
  
  eigenvalues <- c()

#Useful functions
#Make a vector that could be a standard basis vector
  makew <- function() {
    n <- sample(0:4, 1)
    return( switch(n, c(1,0,0),
                  c(0,1,0),
                  c(0,0,1),
                  round(rnorm(3),2),
                  round(rnorm(3),2),
                  round(rnorm(3),2))
    )
  }
    
#Clear out the entire right column  
  clearFlex <- function() {
    output$ctrl1 <- renderUI("")
    output$ctrl2 <- renderUI("")
    output$ctrl3 <- renderUI("")
    output$ctrl4 <- renderUI("")
    output$ctrl5 <- renderUI("")
    output$ctrl6 <- renderUI("")
    output$flexmsg1 <- renderUI("")
  }
  
#Functions that respond to events in the input
#Step 1
  observeEvent(input$btnmake,{
    clearFlex()
    #Create the necessary buttons and an explanatory message.
    output$ctrl1 <- renderUI(actionBttn("btnrect","Random Matrix"))
    output$ctrl2 <- renderUI(actionBttn("btnthree","Random Matrix, 3 eigenvalues"))
  })
  
#Make a random matrix M whose columns sum to zero
  observeEvent(input$btnrect, {
    eigenvalues <<- c()
    eigenvalues <<- makeMatrix()
    matrixOutput()
  })
  
#Make a random matrix M whose columns sum to zero
  observeEvent(input$btnthree, {
    eigenvalues <<- c()
    while(length(eigenvalues)!=3){
      eigenvalues <<- makeMatrix()
    }
    matrixOutput()
  })
  
matrixOutput <- function(){
  output$flexmsg1 <- renderUI(jaxI(paste0("p(t)= ",
                                          p3,"t^3","+",p2,"t^2","+",p1,"t","+",p0)))
  output$ctrl2 <- renderText(c("λ= ", eigenvalues))
  output$ctrl3 <- renderUI(jax.matrix(M,name = "M"))
  
  A <<- M
  output$matA <- renderUI(jax.matrix(A, name = "A"))
  output$matA2 <- renderUI(jaxI(paste0("p(t)= ",p3,"t^3","+",p2,"t^2","+",p1,"t","+",p0)))
  output$matA3 <- renderText(c("λ= ", eigenvalues))
  
  pCoeff <<- c(p0,p1,p2,1)
  rts <<- eigenvalues
}
  
#make random 3x3 matrix  
makeMatrix <- function(){
  
  eigenvalues <<- c()
  M <<- matrix(sample(0:4,9,replace=T), nrow = 3, ncol = 3)
  a <<- M[1,1]
  b <<- M[1,2]
  c <<- M[1,3]
  d <<- M[2,1]
  e <<- M[2,2]
  f <<- M[2,3]
  g <<- M[3,1]
  h <<- M[3,2]
  i <<- M[3,3]
  
  #poly equation
  #t^3 -it^2 -et^2 +eit -fht -at^2 +ait +eat -eai +afh -dbt +dib +fbg +cdh -cgt +ecg
  
  #t^3 
  #1
  p3 <<- -1
  
  #t^2
  #-i-e-a
  p2 <<- -(-i-e-a)
  
  #t
  #ei -fh +ai +ea -db -cg
  p1 <<- -(e*i -f*h +a*i +e*a -d*b -c*g)
  
  #1
  p0 <<- e*a*i -a*f*h +b*f*g -b*d*i +c*d*h -e*c*g
  
  for(t in c(0,1,2,3,4)){
    pt <- p3*t^3 +p2*t^2 +p1*t +p0
    if (pt == 0){
      eigenvalues <- append(eigenvalues,t)
    }
  }
  
  eigenvalues <<- sort(eigenvalues)
  return(eigenvalues)
}
 
#Step 4 - find eigenvectors
  #Create the necessary buttons
  observeEvent(input$btnvect, {
    clearFlex()
    if (length(eigenvalues) < 3){
      output$ctrl1 <- renderUI(h4("need 3 eigenvalues to proceed"))
      return()
    }
    output$ctrl1 <- renderUI(actionBttn("btnproj",
                        "Make Projection Matrix"))
    output$ctrl3 <- renderUI(actionBttn("btnunit",
                        "Unit Eigenvector for Largest Eigenvalue"))
    output$ctrl5 <- renderUI(actionBttn("btnall",
                        "Make All Eigenvectors"))
  })
  
#Make B, whose image is eigenvector 1
  observeEvent(input$btnproj, {
    B <<- (A-rts[1]*matI)%*%(A-rts[2]*matI)
    output$ctrl2 <- renderUI(jax.matrix(round(B,2)))
  })
  
#Create and test a unit eigenvector
  observeEvent(input$btnunit,{
    if(sum(B[,1]*B[,1])==0){
      v1 <<- B[,1]
    } else {v1 <<- B[,1]/sqrt(sum(B[,1]*B[,1]))}
    output$ctrl4 <- renderUI(withTags(
      table(
        tr(
          td(jaxI(paste0("\\lambda_1 = ",rts[length(rts)]," | "))),
          td(jax.vector(round(v1,3),name = "v_1")),
          td(jax.vector(round(A%*%v1,3),name = "Av_1")),
          td(jax.vector(round(rts[3]*v1,3),name = "\\lambda_1v_1"))
        )
      )
    ))
  })
  
#Make a matrix with all the eigenvectors
  observeEvent(input$btnall, {
    x <<-(A-rts[1]*matI)%*%(A-rts[3]*matI)%*%c(1,0,0)
    if (sum(x)==0){
      v2 <<- x
    } else {v2 <<- x/sqrt(sum(x^2))}
    x <<-(A-rts[2]*matI)%*%(A-rts[3]*matI)%*%c(1,0,0)
    if (sum(x)==0){
      v3 <<- x
    } else {v3 <<- x/sqrt(sum(x^2))}
    output$ctrl5 <- renderUI(withTags(
      table(
        tr(
          td(jaxI(paste0("\\lambda_2 = ",rts[length(rts)-1]," | "))),
          td(jax.vector(round(v2,3),name = "v_2")),
          td(jax.vector(round(A%*%v2,3),name = "Av_2")),
          td(jax.vector(round(rts[2]*v2,3),name = "\\lambda_2v_2"))
        ),
        tr(
          td(jaxI(paste0("\\lambda_3 = ",rts[length(rts)-2]," | "))),
          td(jax.vector(round(v3,3),name = "v_3")),
          td(jax.vector(round(A%*%v3,3),name = "Av_3")),
          td(jax.vector(round(rts[1]*v3,3),name = "\\lambda_3v_3"))
        )
      )
      
    ))
    P <<- cbind(v1,v2,v3)
    D <<- diag(c(rts[3],rts[2],rts[1]))
    output$vect <<- renderUI(jax.matrix(round(P,3), "P"))
  })
  
#Step 5 - Demonstrate the advertised properties
#Create the necessary buttons
  observeEvent(input$btnprop, {
    clearFlex()
    output$ctrl1 <- renderUI(actionBttn("btncayley","Cayley-Hamilton"))
  })
  
  observeEvent(input$btncayley, {
    output$ctrl1 <- renderUI(jaxI("Cayley-Hamilton"))
    output$ctrl2 <- renderUI(jax.matrix(A,"A"))
    output$ctrl3 <- renderUI(jaxI(paste0(p3,"A^3","+",p2,"A^2","+",p1,"A","+",p0,"=")))
    output$ctrl4 <- renderUI(jax.matrix(p3*A%^%3+p2*A%^%2+p1*A+p0*diag(3)))
    output$ctrl5 <- renderUI(
      "a zero matrix is expected for caley-hamilton to be valid")
    })
}
#Run the app
shinyApp(ui = ui, server = server)