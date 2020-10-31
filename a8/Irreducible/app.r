#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
Sys.setenv(RGL_USE_NULL = TRUE) #suppress popup 3D display
library("rgl") #for 3D plotting
library(shinyjs)
source("fieldcalc.R")
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

#replacements
x2_2 = rev(c(0,-2%%5))
x2_3 = rev(c(0,-3%%5))

x2_x1 = rev(c(-1%%5,-1%%5))
x2_x2 = rev(c(-1%%5,-2%%5))

x2_2x3 = rev(c(-2%%5,-3%%5))
x2_2x4 = rev(c(-2%%5,-4%%5))

x2_3x3 = rev(c(-3%%5,-3%%5))
x2_3x4 = rev(c(-3%%5,-4%%5))

x2_4x1 = rev(c(-4%%5,-1%%5))
x2_4x2 = rev(c(-4%%5,-2%%5))

#replacements in list form
plist = list()

plist[[1]] = x2_2
plist[[2]] = x2_3
plist[[3]] = x2_x1
plist[[4]] = x2_x2
plist[[5]] = x2_2x3
plist[[6]] = x2_2x4
plist[[7]] = x2_3x3
plist[[8]] = x2_3x4
plist[[9]] = x2_4x1
plist[[10]] = x2_4x2

#formatted output
p1 = jaxD("x^2+2")
p2 = jaxD("x^2+3")

p3 = jaxD("x^2+x+1")
p4 = jaxD("x^2+x+2")

p5 = jaxD("x^2+2x+3")
p6 = jaxD("x^2+2x+4")

p7 = jaxD("x^2+3x+3")
p8 = jaxD("x^2+3x+4")

p9 = jaxD("x^2+4x+1")
p10 = jaxD("x^2+4x+2")

#The user interface
header <- dashboardHeader(title = "Loi Test For Generator, p=5, n=2", titleWidth = 800)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(fluidRow(
  
  useShinyjs(),
  
  stylesheet,
  
  fluidRow(h1("Pick a polynomial!")),
    
  column(
    width = 3,
    fluidRow(
    actionButton("p1", h3(p1)),
    actionButton("p2", h3(p2)),
    actionButton("p3", h3(p3)),
    actionButton("p4", h3(p4)),
    actionButton("p5", h3(p5)),
    actionButton("p6", h3(p6)),
    actionButton("p7", h3(p7)),
    actionButton("p8", h3(p8)),
    actionButton("p9", h3(p9)),
    actionButton("p10", h3(p10))
    ),
    fluidRow(
      h1("polynomial")
    ),
    fluidRow(
      h1(uiOutput("A"))
    ),
    fluidRow(
      h1("replacement")
    ),
    fluidRow(
      h1(uiOutput("R"))
    ),
    fluidRow(
      h1(uiOutput("Isgen"))
    )
  ),

  column(
    width = 3,
    fluidRow(
      h1("Power Table")
    ),
    fluidRow(
      h1("=>"),
      h3(uiOutput("Res"))
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
  
  update = function(){
    output$R <- renderUI(jaxD(paste0("x^2=",a[1],"+",a[2],"x")))
    pt = powerTable(5,2,c(0,1),a)
    for(k in 3:(length(pt)-1)){
      output$Isgen <- renderUI(jaxD(paste0("x^{24}=1,x`:is`:a`:generator!")))
      if (pt[[k]][1] == 1 && pt[[k]][2] == 0){
        output$Isgen <- renderUI(jaxD(paste0("x^{",k-2,"}=1,x`:is`:not`:a`:generator!")))
        break
      } 
    }
    gens = list()
    for(i in 2:length(pt)){
      gens[[i]] = jaxD(paste0("x^{",i-2,"}=",pt[[i]][1],"+",pt[[i]][2],"x"))
    }
    output$Res <- renderUI(gens)
  }
  
  a = plist[[1]]
  output$A <- renderUI(p1)
  update()
  
  ###a
  observeEvent(input$p1, {
    a <<- plist[[1]]
    output$A <- renderUI(p1)
    update()
  })
  observeEvent(input$p2, {
    a <<- plist[[2]]
    output$A <- renderUI(p2)
    update()
  })
  observeEvent(input$p3, {
    a <<- plist[[3]]
    output$A <- renderUI(p3)
    update()
  })
  observeEvent(input$p4, {
    a <<- plist[[4]]
    output$A <- renderUI(p4)
    update()
  })
  observeEvent(input$p5, {
    a <<- plist[[5]]
    output$A <- renderUI(p5)
    update()
  })
  observeEvent(input$p6, {
    a <<- plist[[6]]
    output$A <- renderUI(p6)
    update()
  })
  observeEvent(input$p7, {
    a <<- plist[[7]]
    output$A <- renderUI(p7)
    update()
  })
  observeEvent(input$p8, {
    a <<- plist[[8]]
    output$A <- renderUI(p8)
    update()
  })
  observeEvent(input$p9, {
    a <<- plist[[9]]
    output$A <- renderUI(p9)
    update()
  })
  observeEvent(input$p10, {
    a <<- plist[[10]]
    output$A <- renderUI(p10)
    update()
  })
  
  
}

#Run the app
shinyApp(ui = ui, server = server)

  
       
