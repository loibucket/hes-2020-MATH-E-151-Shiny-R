#AirMiles3D
library(shiny)
library(shinydashboard)
library(shinyWidgets)
Sys.setenv(RGL_USE_NULL = TRUE)  #suppress popup 3D display
library("rgl")      #for 3D plotting
source("jaxmat.R")
#It is more conventional to put these style settings in a separate file in www
#but this is a better location for experimenting.
stylesheet <- tags$head(tags$style(HTML('
  
    * {
    font-family: Arial, Helvetica, sans-serif
      }

    .main-header .logo {
      font-family: "Courier New", Courier, monospace;
      font-weight: bold;
      font-size: 30px;
      }
  ')
))
#The user interface
header <- dashboardHeader(title = "Loi Circle Routes", titleWidth = 500)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
    fluidRow(stylesheet,
             column(width=4,
                    checkboxInput("threeD","Use 3D Display", value = FALSE),
                    selectInput("selstart","Starting City",choices = "Boston"),
                    uiOutput("geogA"),
                    uiOutput("vecA"),
                    selectInput("seldest","Destination City",choices = "London"), 
                    uiOutput("geogB"),
                    uiOutput("vecB"),
                    actionBttn("btnadd","Add Route"),

                    uiOutput("cityadd"),
                    uiOutput("msgdist"),
                    uiOutput("dirvec"),
                    uiOutput("heading"),
                    uiOutput("pole"),
                    
                    h2("Add city"),
                    textInput("addcity", "City", value = "", width = NULL, placeholder = NULL),
                    numericInput("latitude", "Latitude:", 0, min = -90, max = 90),
                    numericInput("longitude", "Longitude:", 0, min = -180, max = 180),
                    actionBttn("btncity","Add City"),
                    
             ),
             column(width=8,
                    uiOutput("plot")  #will be replaced by 3D or Mercator display
             )
    )
)
ui <- dashboardPage(header, sidebar, body, skin = "green")

#Functions that implement the mathematics
#This file must go into the same directory as app.R
source("spherical.R")


#Functions that read the input and modify the output and input
server <- function(session, input, output) {
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
  rteDF <- data.frame(Start = character(100),Dest = character(100),
                        stringsAsFactors=FALSE)
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
  
  #add city to DF
  observeEvent(input$btncity,{
    if (sum(input$addcity %in% rvals$cityDF$Name)>0){
      return()
    }
    rvals$cityDF[nrow(rvals$cityDF) + 1,] <- list(input$addcity,input$latitude,input$longitude)
    #update to use reactive DF
    updateSelectInput(session, "selstart", choices = rvals$cityDF$Name, selected = "Boston")
    updateSelectInput(session, "seldest", choices = rvals$cityDF$Name, selected = "London")
  })
  
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
  })


}

#Run the app
shinyApp(ui = ui, server = server)