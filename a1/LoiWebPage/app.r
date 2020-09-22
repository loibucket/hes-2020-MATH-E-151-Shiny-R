#Replace this line with the folder name of the app
library(shiny)
library(shinydashboard)
library(shinyWidgets)
source("jaxmat.R")   #for displaying mathematics
stylesheet <- tags$head(
  tags$style(
    HTML(
      '

* {
font-family: courier;
}

.main-header .logo {
font-family: "Courier New", Courier, monospace;
font-weight: bold;
font-size: 24px;
}

h1, h2, h3, h4, h5, h6{
margin-left: 20px;
font-family: "Georgia", Times, "Times New Roman", serif;
font-weight: bold;
}

h1{
color: blue;
}

h2, h3{
color: green;
}

h6{
color: gray;
font-family: Arial, Helvetica, sans-serif;
font-style: italic;
}

table{
margin-left:20px;
}

table{
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
header <- dashboardHeader(title = "Loi's Shiny Page", titleWidth=600)


sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(fluidRow(
  stylesheet,
  
  column(
    width = 4,
    tabName = "Education",
    h1("My Education"),
    h2("Bachelors Degrees"),
    h4("University of Connecticut"),
    
    withTags(ul(
      li("Mechanical Engineering"),
      li("Applied Mathematics"),
    )),
    
    h2("Masters Degrees"),
    h4("Rensselaer Polytechnic Institute"),
    withTags(ul(li(
      "Mechanical Engineering"
    ),
    li("MBA"), )),
    
    h3("Other"),
    h5("Certificates, courses, etc."),
    
    withTags(ul(
      li("Graduate Certifiate in Data Science"),
      li("freeCodeCamp Certificate in Front End Dev"),
    )),
    
    h6("created with Shiny"),
  ),
  
  
  column(
    width = 4,
    h1("My Hobbies"),
    
    p(jaxD("badminton + swimming = sports")),
    p(jaxD(tex.frac("computers","coding"))),
    p(
      jax.fTimesP("f",c("badminton", "computers"), c("badminton+swiming",tex.frac("computers","coding")))),
    
    withTags(table(
      caption("Hobbies"),
      tr(th("Indoor"), th("Outdoor")),
      tr(td("Videos"), td("Drones")),
      tr(td("Reading"), td("Badminton")),
      tr(td("Computers"), td("Sports")),
      tr(td("Coding"), td("Swimming")),
      tr(td(" "), td(" "))
    )),
    tags$br(),
    withTags(table(
      tr(th("Links")),
      tr(td(
        a(href = "https://github.com/loibucket", "Github")
      )),
      tr(td(
        a(href = "https://www.linkedin.com/in/loicheng/", "LinkedIn")
      )),
      tr(td(
        a(href = "https://loiapps.herokuapp.com/", "Heroku")
      )),
      tr(td(
        a(href = "https://www.youtube.com/channel/UCZY46Q2pIjQhE26B7nJ3x2Q/", "YouTube")
      )),
    )),
  ),
  column(
    width = 4, 
    h3("Big Data Processing"),
    h3(
      tags$iframe(
        width = "320",
        height = "180",
        src = "https://www.youtube.com/embed/f0dU5-QMWaQ",
        frameborder = "0",
        allow = "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
        allowfullscreen = NA
      )
    ),
    h3("Fixing My Drone"),
    h3(img(src = "IMG_8078.jpg", width = "50%")),
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
  
  #Initialization
  
  #Functions that respond to events in the input
}

#Run the app
shinyApp(ui = ui, server = server)