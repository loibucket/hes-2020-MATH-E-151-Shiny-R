#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
Sys.setenv(RGL_USE_NULL = TRUE)  #suppress popup 3D display
library("rgl")      #for 3D plotting
library(shinyjs)
source("jaxmat.R")   #for displaying mathematics
source("permutecalc.R")   
source("spherical.R")
source("d4calc.R")
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

.modal-lg {
width: 900px;
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
  
  column(
    width = 12, 
    h2(""),
  ),
  
  column(
    width = 4, 
    actionBttn("modalD4Btn","D4 Challenge!"),
    sliderInput("d4Slider", "set max turns",2, 10, 3 ),
  ),
  
  column(
    width = 4, 
    actionBttn("modalHamBtn","Hamiltonian Challenge!"),
    sliderInput("hamSlider", "set max vertices",1, 50, 10 ),
  ),
  
  column(
    width = 4, 
    actionBttn("modalCyclicBtn","Euler Challenge!"),
    sliderInput("cyclicASlider", "set left digits",2, 9, 2 ),
    sliderInput("cyclicBSlider", "set right digits",2, 9, 2 ),
  ),
  
  column(width=4,
         h3(uiOutput("ctrlA")),
         uiOutput("ctrlB"),
         checkboxInput("threeD","Use 3D Display", value = TRUE),
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
  
  
  
  ##Hamiltonian action
  hamVisited = 0
  observeEvent(input$modalHamBtn, {
    score <<- 0
    cnt <<- 0
    mode <<- "ham"   
    clearText()
    showModalHam()
  })
  
  dfHam = data.frame(matrix(ncol = 2, nrow = 0))  
  showModalHam <- function() {
    
    #modal
    showModal(modalDialog(id = "modalArith",
                          h3("Hamiltonian Walk!"),
                          plotOutput("walk",click = "plot_click",width=500,height=500),
                          h3(numericInput("answerInput", "Visited:", ""), actionButton("submitBtn","Submit")),
                          h3(uiOutput("msgOutput")),
                          footer = tagList(
                            h3(uiOutput("scoreOutput")),
                            actionButton("nextBtn","Next"),
                            actionButton("closeBtn","Exit")
                          ),
    ))
    
    #make points
    g = list()
    for(i in 1:input$hamSlider){
      g[[i]] = sample(1:5,2,replace=TRUE)
    }
    
    dfHam <<- data.frame(matrix(ncol = 2, nrow = 0))  
    dfHam <<- rbind(dfHam,sample(g,1)[[1]])
    
    #sample points
    for(i in 1:input$hamSlider){
      s = sample(g,1)[[1]]
      if(s[1]==tail(dfHam)[1,1] && s[2]==tail(dfHam,1)[1,2]){
        #skip
      } else {
        dfHam <<- rbind(dfHam,s)
      }
    }
    dfHam <<- cbind(dfHam,replicate(length(dfHam[,1]),"blue"))
    colnames(dfHam) <<- c("x","y","bg")
    
    #create lines
    dg=data.frame(matrix(ncol = 4, nrow = 0))
    for(i in 1:(length(dfHam$x)+1)){
      a = i%%length(dfHam$x)+1
      b = (i+1)%%length(dfHam$x)+1
      
      xa = dfHam$x[a]
      ya = dfHam$y[a]
      
      xb = dfHam$x[b]
      yb = dfHam$y[b]
      
      if (xa == xb && ya == yb){
        #same point, skip
      }else if (xa == xb){
        dg = rbind(dg,c(xa,min(ya,yb),xb,max(ya,yb)))
      } else if (dfHam$x[a] < dfHam$x[b]){
        dg = rbind(dg,c(xa,ya,xb,yb))
      } else {
        dg = rbind(dg,c(xb,yb,xa,ya))
      }
    }
    
    output$walk <- renderPlot({
        drawPoints(dfHam)
        drawLines(dg)
      })
    
    ans<<-nrow(unique(dfHam))
    updateTextInput(session,"answerInput",value="0")
    disable("answerInput")
    
    #button display
    shinyjs::hide("submitBtn")
    shinyjs::show("nextBtn")
  }
  
  udf=data.frame()
  
  #draw points
  drawPoints = function(df){
    #draw points
    udf<<-unique(df)
    row.names(udf) <- 1:nrow(udf)
    plot(udf$y~udf$x,xlim=c(0,5),ylim=c(0,5),asp=1,pch = 21,cex = 5,bg=udf$bg,xlab="x",ylab="y")
  }
  
  udg=data.frame()
  
  #draw lines
  drawLines = function(dg){
    udg <<- unique(dg)
    colnames(udg) = c("a","b","c","d")
    for(i in 1:(length(udg$a))){
      segments(udg$a[i],udg$b[i],udg$c[i],udg$d[i])
    }
  }

  observeEvent(input$plot_click,{
    x=as.integer(round(input$plot_click$x,0))
    y=as.integer(round(input$plot_click$y,0))
    output$msgOutput = renderText(paste(x,y))
    
    #check if visit able
    w=which(udf$x==x&udf$y==y)
    if (udf$bg[w] != "yellow"){
      if(sum(udf$bg=="red")!=0){
        output$msgOutput = renderText(paste(x,y,"not nearby node!"))
        return()
      }
    }
    
    ##clear yellow
    for(i in 1:length(udf$bg)){
      if(udf$bg[i]=="yellow"){
        udf$bg[i]<<-"blue"
      }
    }
    
    #mark visted
    udf$bg[w]<<-"red"   
    
    #get neighbor verts
    rows = as.integer(rownames(dfHam[(dfHam$x==x&dfHam$y==y),]))

    len = length(dfHam$bg)
    for (r in rows){
      
      #mark previous row
      pre=(r-1)%%len
      if(pre<=0){pre=pre+len}
      preCond = udf$x==dfHam$x[pre] & udf$y==dfHam$y[pre]
      w = which(preCond,TRUE)
      if(udf$bg[w]!="red"){
        udf$bg[w]<<-"yellow"
      }

      #mark next row
      post=(r+1)%%len
      if(post<=0){post=post+len}
      postCond = udf$x==dfHam$x[post] & udf$y==dfHam$y[post]
      w = which(postCond,TRUE)
      if(udf$bg[w]!="red"){
        udf$bg[w]<<-"yellow"
      }
    }
  
    output$walk <- renderPlot({
      drawPoints(udf)
      drawLines(udg)
    })
    
    v=sum(udf$bg=="red")
    updateTextInput(session,"answerInput",value=v)
    if(v==ans){
      delay(200,showAnswer())
    }
  })
  
  
  ##D4 action
  d4Turn = 1
  config = "ABCD"
  
  observeEvent(input$modalD4Btn, {
    score <<- 0
    cnt <<- 0
    mode <<- "d4"    
    clearText()
    showModalD4()
  })
  
  showModalD4 <- function() { 
    d4Turn <<- 1
    showModal(modalDialog(id = "modalD4",
                          size="l",
                          h1("D4!"),
                          plotOutput("configs", height=200),
                          fluidRow(
                            column(6,h1("Match This!")),
                            column(6,h1("To This!"))
                          ),
                          fluidRow(
                            column(6,plotOutput("square", height=200)),
                            column(6,plotOutput("squaretwo", height=200))
                          ),
                          actionButton("btnr","Apply r"),
                          actionButton("btns","Apply s"),
                          actionButton("btnt","Apply t"),
                          actionButton("btnw","Apply w"),
                          actionButton("btnx","Apply x"),
                          actionButton("btny","Apply y"),
                          actionButton("btnz","Apply z"),
                          h3(textOutput("turnId")),
                          h3(textInput("answerInput", "", ""), actionButton("submitBtn","Submit")),
                          h3(uiOutput("msgOutput")),
                          footer = tagList(
                            h3(uiOutput("scoreOutput")),
                            actionButton("nextBtn","Next"),
                            actionButton("closeBtn","Exit")
                          )
    ))
    
    D4DF <- D4.makeDataFrame()
    output$configs <- renderPlot(D4.showConfigs(D4DF))
    
    updateTextInput(session,"answerInput",value="")
    disable("answerInput")
    
    a = sample(c("i","r","s","t","w","x","y","z"),2)
    
    config <<- D4.apply(a[1],"ABCD")
    output$square <- renderPlot(D4.showSquare(config))
    
    ans <<- D4.apply(a[2],"ABCD")
    output$squaretwo <- renderPlot(D4.showSquare(ans))

    #button display
    shinyjs::show("submitBtn")
    shinyjs::hide("nextBtn")
    
    output$turnId <- renderText(paste("Turn", d4Turn))
  }
  
  #do not put observeEvent inside other functions, it will stack if that func is called multiple times
  observeEvent(input$btnr,{
    config <<- D4.apply("r",config)
    updateD4(config)
  })
  observeEvent(input$btns,{
    config <<- D4.apply("s",config)
    updateD4(config)
  })
  observeEvent(input$btnt,{
    config <<- D4.apply("t",config)
    updateD4(config)
  })
  observeEvent(input$btnw,{
    config <<- D4.apply("w",config)
    updateD4(config)
  })
  observeEvent(input$btnx,{
    config <<- D4.apply("x",config)
    updateD4(config)
  })
  observeEvent(input$btny,{
    config <<- D4.apply("y",config)
    updateD4(config)
  })
  observeEvent(input$btnz,{
    config <<- D4.apply("z",config)
    updateD4(config)
  })
  
  updateD4 <- function(config){
    output$square <- renderPlot(D4.showSquare(config))
    updateTextInput(session,"answerInput",value=config)
    d4Turn <<- d4Turn+1
    if (d4Turn>input$d4Slider){
      #shinyjs::hide("submitBtn")
      shinyjs::hide("btnr")
      shinyjs::hide("btns")
      shinyjs::hide("btnt")
      shinyjs::hide("btnw")
      shinyjs::hide("btnx")
      shinyjs::hide("btny")
      shinyjs::hide("btnz")
      #showAnswer()
      output$turnId <- renderText(paste("Out of Turns!"))
    } else {
      output$turnId <- renderText(paste("Turn",d4Turn))
    }
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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
      output$msgOutput <- renderUI(HTML(paste("<b style='color:red;'>","incorrect!",ans,"</b>")))
    } else {
      output$msgOutput <- renderUI(HTML(paste("<b style='color:green;'>","correct!","</b>")))
      score <<- score+1
    }
    cnt <<- cnt+1
    f=""
    shinyjs::show("nextBtn")
    if(score >= scoreLimit){
      f="Flight Awarded!"
      shinyjs::hide("nextBtn")
    }
    output$scoreOutput = renderUI(paste(f,"score:",score," out of ",cnt))
  }
  
  #next action
  observeEvent(input$nextBtn, {
    output$msgOutput <- renderText("")
    removeModal()
    if(mode=="arith"){showModalArith()}
    else if(mode=="matrix"){showModalMatrix()}
    else if(mode=="cyclic"){showModalCyclic()}
    else if(mode=="d4"){showModalD4()}
    else if(mode=="ham"){showModalHam()}
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
