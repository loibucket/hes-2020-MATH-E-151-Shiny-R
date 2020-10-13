#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
source("jaxmat.R")   #for displaying mathematics
source("permutecalc.R")   
source("spherical.R")
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
#The user interface
header <- dashboardHeader(title = "Loi's R Shiny Math Challenge!", titleWidth=600)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(fluidRow(
  useShinyjs(),
  stylesheet,
  
  column(
    width = 12, 
    h2("Score 5 pts to win a flight!"),
  ),
  
  column(
    width = 4, 
    actionBttn("modalArithBtn","Arthmetic Challenge!"),
    sliderInput("aSlider", "set highest a",1, 50, 10 ),
    sliderInput("bSlider", "set highest b",1, 50, 10 ),
  ),
  
  column(
    width = 4, 
    actionBttn("modalMatrixBtn","Matrix Challenge!"),
    sliderInput("matSlider", "set highest number",1, 50, 10 ),
  ),
  
  column(
    width = 4, 
    actionBttn("modalCyclicBtn","Cyclic Challenge!"),
    sliderInput("cyclicASlider", "set left digits",2, 9, 2 ),
    sliderInput("cyclicBSlider", "set right digits",2, 9, 2 ),
  ),
  
  column(width=4,
         h3(uiOutput("ctrlA")),
         uiOutput("ctrlB"),
         checkboxInput("threeD","Use 3D Display", value = FALSE),
         selectInput("selstart","Starting City",choices = "Boston"),
         uiOutput("geogA"),
         uiOutput("vecA"),
         selectInput("seldest","Destination City",choices = "London"), 
         uiOutput("geogB"),
         uiOutput("vecB"),
         
         uiOutput("msgdist"),
         uiOutput("dirvec"),
         uiOutput("heading"),
         uiOutput("pole"),
  ),
  column(width=8,
         uiOutput("plot")  #will be replaced by 3D or Mercator display
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
  #Variables that are shared among server functions (use <<-)
  ans = 0
  score = 0
  cnt = 0
  mode = "o"
  scoreLimit = 5
  
  #Functions that respond to events in the input
  ##submit action
  observeEvent(input$submitBtn, {
    shinyjs::hide("submitBtn")
    showAnswer()
  })

  ##show answer
  showAnswer <- function(){
    if(is.na(input$answerInput) || input$answerInput != ans){
      output$msgOutput <- renderUI(HTML(paste("<b style='color:red;'>",ans,"incorrect!","</b>")))
    } else {
      output$msgOutput <- renderUI(HTML(paste("<b style='color:green;'>",ans,"correct!","</b>")))
      score <<- score+1
    }
    cnt <<- cnt+1
    f=""
    if(score >= scoreLimit){
      f="Flight Awarded!"
    }
    output$scoreOutput = renderUI(paste(f,"score:",score," out of ",cnt))
    shinyjs::show("nextBtn")
  }
  
  #next action
  observeEvent(input$nextBtn, {
    output$msgOutput <- renderText("")
    removeModal()
    if(mode=="arith"){showModalArith()}
    else if(mode=="matrix"){showModalMatrix()}
    else if(mode=="cyclic"){showModalCyclic()}
    else(removeModal())
  })
  
  #close action
  observeEvent(input$closeBtn, {
    output$msgOutput <- renderText("")
    removeModal()
    if (score >= scoreLimit){
      output$ctrlA <- renderUI("Flight Awarded!")
      output$ctrlB <- renderUI(actionBttn("btnadd","Add Flight!"))
    }
  })
  
  ##arithmetic action
  observeEvent(input$modalArithBtn, {
    score <<- 0
    cnt <<- 0
    mode <<- "arith"
    clearText()
    showModalArith()
  })
  
  ##modal arithmetic
  showModalArith <- function() {
    #numbers
    a = sample.int(input$aSlider,1)
    b = sample.int(input$aSlider,1)
    #operations
    oo = sample(1:4, 1)
    op = switch(oo,"+","-","`times","`div")
    ans <<- switch(oo,a+b,a-b,a*b,floor(a/b))
    #modal
    showModal(modalDialog(id = "modalArith",
                          h3("Arithmetic!"),
                          h3(jaxI(paste(a,op,b,"= ?"))),
                          h3(numericInput("answerInput", "Answer:", ""), actionButton("submitBtn","Submit")),
                          h3(uiOutput("msgOutput")),
                          footer = tagList(
                            h3(uiOutput("scoreOutput")),
                            actionButton("nextBtn","Next"),
                            actionButton("closeBtn","Exit")
                          ),
    ))
    #messages
    if(op=="`div"){
      output$msgOutput <- renderUI(HTML(paste("<i style='color:black;'>","omit the remainder!","</b>")))
    }
    #button display
    shinyjs::show("submitBtn")
    shinyjs::hide("nextBtn")
  }
    
  ##matrix action
  observeEvent(input$modalMatrixBtn, {
    score <<- 0
    cnt <<- 0
    mode <<- "matrix"    
    clearText()
    showModalMatrix()
  })
    
  ##modal matrix
  showModalMatrix <- function() { 
    #numbers
    w = sample.int(input$matSlider,1)
    x = sample.int(input$matSlider,1)
    y = sample.int(input$matSlider,1)
    z = sample.int(input$matSlider,1)
    #matrices
    M = matrix(c(w,x,y,z),nrow=2,ncol=2,byrow=TRUE)
    A = matrix(c(w,x),nrow=2,ncol=1,byrow=TRUE)
    B = matrix(c(y,z),nrow=2,ncol=1,byrow=TRUE)
    C = matrix(c(w,x),nrow=1,ncol=2,byrow=TRUE)
    D = matrix(c(y,z),nrow=2,ncol=1,byrow=TRUE)
    #operations
    oo = sample(1:4, 1)
    msg = switch(oo,"Find the determinant!","Find the sum!","Find the dot product!","Find the product!")
    ans <<- floor(switch(oo,det(M),sum(M),sum(A*B),C%*%D))
    #expressions
    rep = switch(oo,
                 jax.matrix(M),
                 jax.matrix(M),
                 jaxD(paste0(tex.matrix(A),"`cdot",tex.matrix(B))),
                 jaxD(paste0(tex.matrix(C),"`times",tex.matrix(D))),
    )
    #modal         
    showModal(modalDialog(id = "modalMatrix",
                      h3("Matrices!"),
                      h3(rep),
                      h3(numericInput("answerInput", "Answer:", ""), actionButton("submitBtn","Submit")),
                      h3(uiOutput("msgOutput")),
                      footer = tagList(
                        h3(uiOutput("scoreOutput")),
                        actionButton("nextBtn","Next"),
                        actionButton("closeBtn","Exit")
                      )
    ))
    #messages
    output$msgOutput <- renderUI(HTML(paste("<i style='color:black;'>",msg,"</b>")))
    #button display
    shinyjs::show("submitBtn")
    shinyjs::hide("nextBtn")
  }
  
  ##cyclic action
  observeEvent(input$modalCyclicBtn, {
    score <<- 0
    cnt <<- 0
    mode <<- "cyclic"    
    clearText()
    showModalCyclic()
  })
  
  ##cyclic matrix
  showModalCyclic <- function() { 
    
    nums = min(input$cyclicASlider + input$cyclicBSlider,9)
    groupA = sample.int(nums,input$cyclicASlider)
    groupB = sample.int(nums,input$cyclicBSlider)
    
    A = paste0(groupA,collapse="")
    B = paste0(groupB,collapse="")
    A = paste0("(",A,")")
    B = paste0("(",B,")")
    ans <<- Perm.multiply(A,B)
    
    rep = jaxD(paste0(A,"`times",B))
    
    showModal(modalDialog(id = "modalCylic",
                          h3("Cyclics!"),
                          h3(rep),
                          h3(textInput("answerInput", "Answer:", ""), actionButton("submitBtn","Submit")),
                          h3(uiOutput("msgOutput")),
                          footer = tagList(
                            h3(uiOutput("scoreOutput")),
                            actionButton("nextBtn","Next"),
                            actionButton("closeBtn","Exit")
                          )
    ))
    #button display
    shinyjs::show("submitBtn")
    shinyjs::hide("nextBtn")
  }
  
  ##Airmiles
  
  clearText <- function(){
    output$scoreOutput = renderUI("")
    output$msgdist = renderUI("")
    output$dirvec = renderUI("")
    output$heading = renderUI("")
    output$pole = renderUI("")
  }
  
  vA <- NULL
  vB <- NULL
  use3d <- FALSE
  #make empty reactive DF
  rvals <- reactiveValues(cityDF = NULL) 
  #make static DF, when observed is copied into reactive DF
  cityDF <- sph.makeCityDF("cities.csv") 
  updateSelectInput(session, "selstart", choices = cityDF$Name, selected = "Boston")
  updateSelectInput(session, "seldest", choices = cityDF$Name, selected = "London")
  #New data frame to keep track of all the routes
  rteDF <- data.frame(Start = character(100),Dest = character(100), stringsAsFactors=FALSE)
  nroute <- 0;
  
  redrawMap <- function(){
    if (use3d){
      output$plot <- renderUI({rglwidgetOutput("globe",
                                               width = "100%", height = "700px")}) 
      output$globe <- renderRglwidget({             
        sph.blankPlot3D()
        rgl.viewpoint(theta = 0, phi = -90, zoom = 0.5)
        sph.showCities3D(rvals$cityDF)
        if (nroute == 0)
          return(rglwidget(width = "700px", height = "700px"))
        for (i in (1:nroute)){
          sph.plotRoute3D(rvals$cityDF,rteDF[i,1], rteDF[i,2],nstop = 5)
        }
        return(rglwidget())
      })
    }
    else{
      output$plot <- renderUI({plotOutput("mercator",
                                          width = "100%", height = "700px")})
      output$mercator <- renderPlot({
        sph.blankPlot()
        sph.showCities(rvals$cityDF)
        if (nroute == 0)
          return()
        for (i in (1:nroute)){
          sph.plotRoute2D(rvals$cityDF,rteDF[i,1], rteDF[i,2],nstop = 5)
        }
      })
    }
  }
  
  #Functions that respond to events in the input
  #update reactive DF with static DF
  observe({
    rvals$cityDF <- sph.makeCityDF("cities.csv")
  })
  
  observeEvent(input$threeD,{
    use3d <<- input$threeD
    redrawMap()
  })
  
  observeEvent(input$selstart,{
    ll <- sph.latlong(rvals$cityDF,input$selstart)
    output$geogA <- renderUI(paste("Latitude",round(ll[1],2),
                                   "Longitude",round(ll[2],2)))
    vA <<- sph.makeXYZ(ll)
    output$vecA <- renderUI(jax.vector(sph.makeXYZ(ll),name = "v_A"))
  })
  
  observeEvent(input$seldest,{
    ll <- sph.latlong(rvals$cityDF,input$seldest)
    output$geogB <- renderUI(paste("Latitude",round(ll[1],2),
                                   "Longitude",round(ll[2],2)))
    vB <<- sph.makeXYZ(ll)
    output$vecB <- renderUI(jax.vector(sph.makeXYZ(ll),name = "v_B"))
  })
  
  observeEvent(input$btnadd,{
    if (input$selstart == input$seldest)
      return()
    nroute <<- nroute+1
    rteDF[nroute,1] <<- input$selstart
    rteDF[nroute,2] <<- input$seldest
    output$msgdist <- renderUI(paste("Distance",
                                     round(sph.distance(vA,vB,unit = "kilometers"),1),"kilometers"))
    vAB <- sph.directionVector(vA,vB)
    output$dirvec <- renderUI(jax.vector(vAB, name = "v_{AB}"))
    angle <- round(sph.compass(vA,vAB), digits = 2)
    output$heading <- renderUI(paste("Takeoff heading", angle,"north of east"))
    pole <- vA%x%vB
    if (pole[1] <0 )
      pole <- -pole
    output$pole <- renderUI(p("Pole of great circle",
                              jax.vector(round(pole,3), name= "N_{AB}")))
    redrawMap()
    output$ctrlA <- renderUI("")
    output$ctrlB <- renderUI("")
  })
}

#Run the app
shinyApp(ui = ui, server = server)
