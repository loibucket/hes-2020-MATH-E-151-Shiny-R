#ThreeQTest
#September 20, 2020
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
    #inv{
      color: white;
      background-color: blue
    }
    #err{
      color:red;
      font-family: fantasy;
      font-weight:800;
      font-size: 120%
    }
    #now{
      color:purple;
      font-size: 200%;
      font-family: cursive
    }
    .modal-title{
      font-size: 150%;
      color:magenta;
    }
    h2{
      color: blue;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Loi's 3 Question Test",
                          titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
      actionBttn("testbtn","Take a test")
      
    ),
    column(width = 4,
      uiOutput("M"),
      uiOutput("Minv")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "red") #other colors available


server <- function(session, input, output) {
  #Variables that are shared among server functions (use <<-)
  m <- NULL
  q <- 1
  try <- 1
  q1c <- "NO"
  q2c <- "NO"
  q3c <- "NO"

  #Functions that respond to events in the input
  
  observeEvent(input$testbtn, {
    try <<- 1
    qone()
  })
  
  ###1
  qone <- function() {
    showModal(modalDialog(id = "qone",
                          h3("Question 1"),
                          h2("How many seconds has passed since January 1, 1970?"),
                          h3(uiOutput("err")),
                          footer = tagList(
                            actionButton("q1yes","more than 1 600 000 000"),
                            actionButton("q1x","between 1 000 000 000 and 1 600 000 000"),
                            actionButton("q1y","less than 1 000 000 000"),
                            actionButton("q1z","none of the above"),
                          )
    ))
  }
  
  observeEvent(input$q1yes, {
    q1c <<- "YES"
    output$err <- renderUI("")
    showModal(modalDialog(title = "Yes",id = "yes",
                          h2("Yes, more than 1 600 000 000"),
                          footer = tagList(
                            actionButton("qtwo","Question 2"),
                          )
    ))
  })
  
  observeEvent(input$q1x, {q1no()})
  observeEvent(input$q1y, {q1no()})
  observeEvent(input$q1z, {q1no()})
  
  q1no <- function() {
    if (try < 2 ){
      output$err <- renderUI("No -- Try Again!")
      try <<- try+1
    }
    else {
      output$err <- renderUI("")
      showModal(modalDialog(title = "No",id = "no", h3("No, Incorrect"), footer = tagList(actionButton("qtwo","Question 2"))))
    }
  }
  
  ###2
  observeEvent(input$qtwo, {
    try <<- 1
    qtwo()
  })
  
  qtwo <- function() {
    showModal(modalDialog(id = "qtwo",
                          h3("Question 2"),
                          h2("What color is this sentence?"),
                          h3(uiOutput("err")),
                          footer = tagList(
                            actionButton("q2x","Cyan"),
                            actionButton("q2y","Green"),
                            actionButton("q2z","Gray"),
                            actionButton("q2yes","Blue"),
                          )
    ))
  }
  
  observeEvent(input$q2yes, {
    q2c <<- "YES"
    output$err <- renderUI("")
    showModal(modalDialog(title = "Yes",id = "yes",
                          h2("Yes, blue"),
                          footer = tagList(
                            actionButton("qthree","Question 3"),
                          )
    ))
  })
  
  observeEvent(input$q2x, {q2no()})
  observeEvent(input$q2y, {q2no()})
  observeEvent(input$q2z, {q2no()})
  
  q2no <- function() {
    if (try < 2 ){
      output$err <- renderUI("No -- Try Again!")
      try <<- try+1
    }
    else {
      output$err <- renderUI("")
      showModal(modalDialog(title = "No",id = "no", h3("No, Incorrect"), footer = tagList(actionButton("qthree","Question 3"))))
    }
  }
  
  
  ###3
  observeEvent(input$qthree, {
    try <<- 1
    qthree()
  })
  
  qthree <- function() {
    showModal(modalDialog(id = "qthree",
                          h3("Question 3"),
                          h2("How many ounces in a cup?"),
                          h3(uiOutput("err")),
                          footer = tagList(
                            actionButton("q3x","6"),
                            actionButton("q3y","7"),
                            actionButton("q3yes","8"),
                            actionButton("q3z","9")
                          )
    ))
  }
  
  observeEvent(input$q3yes, {
    q3c <<- "YES"
    output$err <- renderUI("")
    showModal(modalDialog(title = "Yes",id = "yes",
                          h2("Yes, 8oz in 1 cup"),
                          footer = tagList(
                            actionButton("done","Done"),
                          )
    ))
  })
  
  observeEvent(input$q3x, {q3no()})
  observeEvent(input$q3y, {q3no()})
  observeEvent(input$q3z, {q3no()})
  
  q3no <- function() {
    if (try < 2 ){
      output$err <- renderUI("No -- Try Again!")
      try <<- try+1
    }
    else {
      output$err <- renderUI("")
      showModal(modalDialog(title = "No",id = "no", h3("No, Incorrect"), footer = tagList(actionButton("done","Done"))))
    }
  }
  
  observeEvent(input$done, {
    done()
  })
  
  ###Done
  done <- function() {
    showModal(modalDialog(id = "done",
                          h3("Results"),
                          h2("1"),
                          q1c,
                          h2("2"),
                          q2c,
                          h2("3"),
                          q3c,
                          footer = tagList(
                            "Reload Page to Try Again"
                          )
    ))
  }  

}

#Run the app
shinyApp(ui = ui, server = server)