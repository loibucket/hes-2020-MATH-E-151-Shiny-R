#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
#source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(tags$style(HTML('
      .main-header .logo {
      font-family: Arial, Helvetica, sans-serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#The user interface
header <- dashboardHeader(title = "Petersen's graph + Hamiltonian and Euler Walks",
                          titleWidth = 800)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width=4,
      tableOutput("vertex"),
      tableOutput("edge")
    ),
    column(width = 6,
      h2("Click on the Vertices to Create a Hamiltonian Walk!",actionBttn("btnResetV", "Reset")),
      h2("Click on the Edges to Create an Euler Walk!",actionBttn("btnResetE", "Reset")),
      h2("Click on the Vertices Multiple Times to Change its Color!"),
      plotOutput("plot", click = "plot_click",  height = 700),
      h2(textOutput("plottext")),
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "red") #other 

makeVertexDF <- function() {
  DF <- data.frame(V = character(10), x = numeric (10), y = numeric(10), bg = character(10))
  DF$V <- c("RGP","RYB","YGP","RGB","YBP","YGB","BGP","RBP","RYP","RYG")
  DF$x[1:5] <- 2*sin((0:4)*2*pi/5)
  DF$y[1:5] <- 2*cos((0:4)*2*pi/5)
  DF$x[6:10] <- sin((0:4)*2*pi/5)
  DF$y[6:10] <- cos((0:4)*2*pi/5)
  DF$bg <- rep("white",10)
  return(DF)
}

makeEdgeDF <- function() {
  DF <- data.frame(V1 = character(20), V2 = character(20), color = character(20))
  DF$V1 <- c("RGP","RYB","YGP","RGB","YBP","RGP","RYB","YGP","RGB","YBP","RYP","RYG","YGB","BGP","RBP","RGP","YBP","RGB","YGP","RYB")
  DF$V2 <- c("RYB","YGP","RGB","YBP","RGP","YGB","BGP","RBP","RYP","RYG","YGB","BGP","RBP","RYP","RYG","RYG","RYP","RBP","BGP","YGB")
  DF$color <- c("red","orange","green","blue","purple","green","blue","purple","red","orange","orange","green","blue","purple","red","black","black","black","black","black")
  DF$lty <- rep(1,20)
  print(DF)
  return(DF)
}

distance <- function (x1,y1,x2,y2){
  d = ((x1-x2)^2 + (y1-y2)^2)^0.5
  return (d)
}

server <- function(session, input, output) {
  
  plotVertices <- function(DF) {
    par (mar = c(0,0,0,0))
    plot(DF$x,DF$y, xlim = c(-2.5,2.5), ylim = c(-2.5,2.5), asp = 1, pch = 21, cex = 10, bg=DF$bg)
    text(DF$x,DF$y,DF$V, cex = 1.5)
  }
  
  plotEdges <- function(vDF,eDF) {
    for (i in 1:20){
      v1 <- eDF[i,1]
      v2 <- eDF[i,2]
      color <- eDF[i,3]
      x1 <- vDF[which.max(vDF$V == v1),2]
      y1 <- vDF[which.max(vDF$V == v1),3]
      x2 <- vDF[which.max(vDF$V == v2),2]
      y2 <- vDF[which.max(vDF$V == v2),3]
      segments(x1,y1,x2,y2,col = color, lwd = 2, lty=eDF[i,"lty"])
    }
  }
  
  PeteDF <- makeVertexDF()
  edgeDF <- makeEdgeDF()
  output$vertex <- renderTable(PeteDF)
  output$edge <- renderTable(edgeDF)
  output$plot <- renderPlot({
    plotVertices(PeteDF)
    plotEdges(PeteDF,edgeDF)
  })
  
  print(edgeDF)
  
  observeEvent(input$btnResetV,{
    PeteDF$bg <<- rep("white",10)
    output$plot <- renderPlot({
      plotVertices(PeteDF)
      plotEdges(PeteDF,edgeDF)
    })
  })
  
  observeEvent(input$btnResetE,{
    edgeDF$lty <<- rep(1,20)
    output$plot <- renderPlot({
      plotVertices(PeteDF)
      plotEdges(PeteDF,edgeDF)
    })
  })
  
  # check for edge and vertex clicks
  observeEvent(input$plot_click,{
    x = round(input$plot_click$x,digits=2)
    y = round(input$plot_click$y,digits=2)
    
    vclick = 0 #1 if vertex is clicked
    # check if vertex is clicked
    for (row in 1:nrow(PeteDF)){
      dist = distance(x,y,PeteDF[row,"x"],PeteDF[row,"y"])
      
      if (dist <= 0.22){
        # if click on vertex, color the vertex
        output$plottext <- renderText(paste("You clicked on vertex", PeteDF[row,"V"], "!"))
        if (PeteDF[row,"bg"]=="gray"){
          PeteDF[row,"bg"]<<-"beige"
        } else if(PeteDF[row,"bg"]=="beige") {
          PeteDF[row,"bg"]<<-"pink"
        } else {
          PeteDF[row,"bg"]<<-"gray"
        }
        
        srow <<- row
        output$plot <- renderPlot({
          plotVertices(PeteDF)
          plotEdges(PeteDF,edgeDF)
        })
        vclick = 1
        break #end for loop
      }
    }
    
    # if vertex was not clicked, check if edge was clicked
    if (!vclick) {
      for (row in 1:nrow(edgeDF)){
      
        a = which(PeteDF$V == edgeDF[row,"V1"]) 
        b = which(PeteDF$V == edgeDF[row,"V2"]) 
        
        xa = PeteDF[a,"x"]
        ya = PeteDF[a,"y"]
        
        xb = PeteDF[b,"x"]
        yb = PeteDF[b,"y"]
        
        e = distance(x,y,xa,ya)
        f = distance(x,y,xb,yb)
        ef = distance(xa,ya,xb,yb)
        
        epsil = .005
        if ( (1-epsil)*ef<(e+f) && (e+f)<(1+epsil)*ef ){
          output$plottext <- renderText(paste("You clicked on edge",PeteDF[a,"V"],",",PeteDF[b,"V"],"!"))
          edgeDF[row,"lty"] <<- 2
          print(edgeDF)
          output$plot <- renderPlot({
            plotVertices(PeteDF)
            plotEdges(PeteDF,edgeDF)
          })
          break
          
        } else {
          # if edge was not clicked, show coordinates of where clicked
          output$plottext <- renderText(paste("You clicked on",x,",",y,"!"))
        } #endif
        
      } #endfor
    } #endif
    
  }) #endobserve
  
}

#Run the app
shinyApp(ui = ui, server = server)