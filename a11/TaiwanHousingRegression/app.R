#MLRegression
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(resampledata)   #datasets to accompany Chihara and Hesterberg
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Georgia", Times, "Times New Roman", serif;
      font-weight: bold;
      font-size: 24px;
    }
  ')
))
#Load the dataset
CER <- read.csv("Taiwan_Real_Estate_Pricing.csv")
dset <- subset(CER,TRUE,select=c(sold_date,house_age,nearest_MRT_m,convenience_stores,latitude, longitude, price_per_m2))



#The user interface
header <- dashboardHeader(title = "Linear Regression - Taiwan Housing Prices - Loi C",
                          titleWidth = 800)
sidebar <- dashboardSidebar(disable = TRUE
  
  
)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width = 6,
           tableOutput("tbl")  #display the data set
    ),
    column(width = 3,
       h3("Single Linear Regression"),
       selectInput("select","Predictor",choices = c("house_age","nearest_MRT_m", "convenience_stores", "latitude","longitude")),
       plotOutput("scatter"),
       h3(uiOutput("eqn")),
       h3(uiOutput("resid")),
       h3(uiOutput("corr"))
       
          
    ),
    column(width = 3,
       h3("Multiple Linear Regression"),
       checkboxInput("house_age","house_age"),
       checkboxInput("nearest_MRT_m","nearest_MRT_m"),
       checkboxInput("convenience_stores","convenience_stores"),
       checkboxInput("latitude","latitude"),
       checkboxInput("longitude","longitude"),
       h3(uiOutput("resid2")),
       h3(uiOutput("coeff1")),
       h3(uiOutput("coeff2")),
       h3(uiOutput("coeff3")),
       h3(uiOutput("coeff4")),
       h3(uiOutput("coeff5")),
       h3(uiOutput("coeff6")),

    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available


#Additional functions are OK here, but no variables


server <- function(session, input, output) {
  predictors <- c( FALSE,FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  output$tbl <- renderTable(dset)
  observeEvent(input$select,{
    predictor <- input$select
    #Find the regression line by projection
    A <- cbind(rep(1,nrow(dset)),dset[,predictor])
    B <- t(A)%*%A
    P <- A%*%solve(B)%*%t(A)
    y.hat <- P%*%dset$price_per_m2     #predicted values, on the regression line
    output$scatter <- renderPlot({plot(dset[,predictor],dset$price_per_m2,pch = ".",cex = 3)  #scatter plot of the data
    points(dset[,predictor],y.hat,type = "b")
    })
    
    coeff <- solve(B)%*%t(A)%*%dset$price_per_m2;coeff
    output$eqn <- renderUI(HTML(paste("The equation of the regression line is",br(), 
                                 "y = ",round(coeff[2],3),"x + ", round(coeff[1],3))))
                                 
    #Here is the length of the shortest vector to the 2D subspace
    lenResid <- sqrt(sum((dset$price_per_m2-y.hat)^2))
    output$resid <- renderUI(HTML(paste("The length of the vector of residuals is",br(),round(lenResid,3))))
    corr <- cor(dset$price_per_m2,dset[,predictor])   
    output$corr <- renderUI(paste("The correlation between predictor and price_per_m2 is",round(corr,3)))
    
  })
  
  doMultiple <- function(){
    vpred <- which(predictors == TRUE)
    if (length(vpred) == 0) return()
    A <- as.matrix(cbind(rep(1,nrow(dset)),dset[,vpred]))
    B <- t(A)%*%A
    P <- A%*%solve(B)%*%t(A)
    y.hat <- P%*%dset$price_per_m2     #predicted values, on the regression line
    lenResid <- sqrt(sum((dset$price_per_m2-y.hat)^2))
    output$resid2 <- renderUI(HTML(paste("The length of the vector of residuals is",br(),round(lenResid,3))))
    coeff <- solve(B)%*%t(A)%*%dset$price_per_m2
    output$coeff1 <- renderUI(paste("Constant =", round(coeff[1],3)))
    if (2 %in% vpred)
      output$coeff2 <- renderUI(paste("Coefficient for house_age =", round(coeff[1+which.max(vpred==2)],3)))
    else output$coeff2 <- NULL 
    if (3 %in% vpred)
      output$coeff3 <- renderUI(paste("Coefficient for nearest_MRT_m =", round(coeff[1+which.max(vpred==3)],3)))
    else output$coeff3 <- NULL
    if (4 %in% vpred)
      output$coeff4 <- renderUI(paste("Coefficient for convenience_stores =", round(coeff[1+which.max(vpred==4)],3)))
    else output$coeff4 <- NULL
    if (5 %in% vpred)
      output$coeff5 <- renderUI(paste("Coefficient for latitude =", round(coeff[1+which.max(vpred==5)],3)))
    else output$coeff5 <- NULL
    if (6 %in% vpred)
      output$coeff6 <- renderUI(paste("Coefficient for longitude =", round(coeff[1+which.max(vpred==6)],3)))
    else output$coeff6 <- NULL
  }
  observeEvent(input$house_age,{
    predictors[2] <<- input$house_age
    doMultiple()
  })
  observeEvent(input$nearest_MRT_m,{
    predictors[3] <<- input$nearest_MRT_m
    doMultiple()
  })
  observeEvent(input$convenience_stores,{
    predictors[4] <<- input$convenience_stores
    doMultiple()
  })
  observeEvent(input$latitude,{
    predictors[5] <<- input$latitude
    doMultiple()
  })
  observeEvent(input$longitude,{
    predictors[6] <<- input$longitude
    doMultiple()
  })
}

#Run the app
shinyApp(ui = ui, server = server)