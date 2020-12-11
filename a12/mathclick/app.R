#ImgClick
#This is an R version of an example from W3Schools.com
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
source("jaxmat.R")   #for displaying mathematics

#The user interface
header <- dashboardHeader(title = "Math is Cool!")
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  useShinyjs(),  #enable onclick 
  #tags$iframe(name = "picture", width = "200", height = "200"),
  fluidRow(
    # triangle
    column(width = 4,
      #img (id = "planet", src="planets.gif", alt="Planets", usemap="#planetmap", width="145", height="126"),
      img (id = "planet", src="1200px-Rtriangle.svg.png", alt="Planets", usemap="#planetmap", width="400", height="300"),
      tags$map(name="planetmap",
        tags$area(id = "sun", target = "picture", shape="circle" ,coords="180,130,40" ,alt="Sun"),
        tags$area(id = "merc", target = "picture",  shape="circle", coords="200,270,40", alt="Mercury"),
        tags$area(id = "venus", target = "picture",  shape="circle", coords="360,150,40", alt="Venus")
      ),
      h3(textOutput("astro"))
    ),
    # curve
    column(width = 4,
      plotOutput("plot1", click = "plot_click", 
        dblclick = dblclickOpts(id = "plot_dblclick", delay = 200),
        hover = hoverOpts(id = "plot_hover", delay = 500),
        brush = brushOpts(id = "plot_brush"),
        width = 500, height = 500),
      h3(textOutput("plottext")),
      ),
    # text questions
    column(width = 4,
      h3(textOutput("btext")),
      h2(id="closure",jaxI("Closure")),
      h2(id="assoc",jaxI("Associativity")),
      h2(id="commu",jaxI("Commutativity")),
      h2(id="identity",jaxI("Identity")),
      h1(textOutput("ctext")),
    ),
  )
)

ui <- dashboardPage(header, sidebar, body)

#Functions that implement the mathematics
#This file must go into the same directory as app.R
#source(".R")

#Variables that are shared among server functions

#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  
    # triangle
    output$astro <- renderText("Click on the center of the hypotenuse!")
    onclick("sun",{output$astro <- renderText("You clicked on the hypotenuse c!")})
    onclick("merc",{output$astro <- renderText("You clicked leg b!")})
    onclick("venus",{output$astro <- renderText("You clicked leg a!")})
    onclick("planet",{output$astro <- renderText("You clicked on the diagram")})
    
    # curve
    output$plottext <- renderText("Click near the curve!")
    output$plot1 <- renderPlot({
      x = seq(-10, 10, 0.05)
      y = sqrt(x^3 + 7)
      z = -sqrt(x^3 + 7)
      plot(x,y,ylim=c(-10,10),type="l")
      lines(x,z)
    })
    observeEvent(input$plot_click,{
                   x = round(input$plot_click$x,digits=1)
                   y = round(input$plot_click$y,digits=1)
                   gy = y
                   gx = sign(y^2 - 7)*abs(y^2 - 7)^(1/3)
                   if (abs(gx-x)<0.5){
                     output$plottext <- renderText(paste("You clicked on",x,",",y,"a point near the curve!"))
                   } else {
                     output$plottext <- renderText(paste("You clicked on",x,",",y,"!"))
                   }
                  })
    
    # text question
    output$btext <- renderText("Which is not a requirement for a group?")
    onclick("closure", output$ctext <-renderText("Required!"))
    onclick("assoc", output$ctext <-renderText("Required!"))
    onclick("commu", output$ctext <-renderText("Not Required!"))
    onclick("identity", output$ctext <-renderText("Required!"))
}

#Run the app
shinyApp(ui = ui, server = server)