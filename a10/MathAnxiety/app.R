#Name of the app goes here: MathAnxiety
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
dset <- get("MathAnxiety")
#Clean up the dataset if necessary

ageString = "Age"
amaString = "AMAS: Abbreviated Math Anxiety Scale"
rcmaString = "RCMAS: Revised Children's Manifest Anxiety Scale"
aritString = "Arithmetic Performance"

#The user interface
header <- dashboardHeader(title = "Permutation testing - MathAnxiety - Loi",
                          titleWidth = 800)
sidebar <- dashboardSidebar(width = 200,
  radioButtons("rnumbers", "Choices", choiceValues = c("Age","AMAS","RCMAS","Arith"), choiceNames = c(ageString,amaString,rcmaString,aritString)),
  actionBttn("btnnew","New permuted dataset"),
  radioButtons("rfactors", "Factors", choices = c("Gender","Grade"))
)
body <- dashboardBody(
  fluidRow(stylesheet,
    column(width = 3,
           h3("The actual dataset"),
           tableOutput("tbl")  #display the data set
    ),
    column(width = 3,
           h3("A permuted dataset"),
           tableOutput("scramtbl")
          
    ),
    column(width = 3,
           h3("Analysis of actual dataset"),
           plotOutput("trueplot"),
           uiOutput("truediff"),
           uiOutput("trueratio"),
           uiOutput("truemed"),
           h3("Analysis of permuted dataset"),
           plotOutput("scrambleplot"),
           uiOutput("scramdiff"),
           uiOutput("scramratio"),
           uiOutput("scrammed")
    ),
    column(width = 3,
      h3("Permutation test"),
      sliderInput("nsample","Number of permuted samples",1000,20000,5000),
      radioButtons("btnstat","Statistic",
                   choiceNames = c("Difference of means","Ratio of means","Difference of medians"),
                   choiceValues = c("diff","ratio","mdiff")),
      actionBttn("btntest","Conduct the test"),
      plotOutput("hist"),
      uiOutput("pval")
    )
  )
)
ui <- dashboardPage(header, sidebar, body, skin = "green") #other colors available


#Additional functions are OK here, but no variables

###

###

#result <- doPermtest(dset,"Beer",10,"mdiff");result


server <- function(session, input, output) {
  
  ###
  doPlot <- function(dset, consume) {
    formula <- as.formula(paste(consume,"~",input$rfactors))
    ##formula <- as.formula(paste(consume,"~","Gender"))
    boxplot(formula, dset)
  }
  
  scramble <- function(dset,colselect){
    dset[[colselect]] <- sample(dset[[colselect]])
    #dset$Gender <- sample(dset$Gender)
    return(dset)
  }
  
  doPermtest <- function(dset,consume,nPerm,stat){
    result <- numeric(nPerm)
    for (i in 1:nPerm){
      permset <- scramble(dset,input$rfactors)
      factone <- which(permset[[input$rfactors]] == levels(permset[[input$rfactors]])[1])
      result[i] <- switch(stat,
                          diff = mean(permset[factone,consume])-mean(permset[-factone,consume]),
                          ratio = mean(permset[,consume][factone])/mean(permset[,consume][-factone]),
                          mdiff = median(permset[,consume][factone])-median(permset[,consume][-factone])
      )
    }
    return (result)
  }
  ###
  
  consume <- "Age"
  output$tbl <- renderTable(dset)
  permset <- scramble(dset,"Gender")
  output$scramtbl <- renderTable(permset)
  
  analyze <- function(dset,consume,realdata,factone){
    if (realdata){
      output$trueplot <- renderPlot({doPlot(dset,consume)})
      output$truediff <- renderUI(h4(paste("Difference in means =",
                                           round(mean(dset[factone,consume])-mean(dset[-factone,consume]),digits =2))))
      output$trueratio <- renderUI(h4(paste("Ratio of means =",
                                    round(mean(dset[factone,consume])/mean(dset[-factone,consume]),digits =2))))
      output$truemed <- renderUI(h4(paste("Difference in medians =",
                                    round(median(dset[factone,consume])-median(dset[-factone,consume]),digits=2))))
    }
    else {
      output$scrambleplot <- renderPlot({doPlot(dset,consume)})
      output$scramdiff <- renderUI(h4(paste("Difference in means =",
                                            round(mean(dset[factone,consume])-mean(dset[-factone,consume]),digits=2))))
      output$scramratio <- renderUI(h4(paste("Ratio of means =",
                                          round(mean(dset[factone,consume])/mean(dset[-factone,consume]),digits =2))))
      output$scrammed <- renderUI(h4(paste("Difference in medians =",
                                        round(median(dset[factone,consume])-median(dset[-factone,consume]),digits =2))))
    }
  }
  
  factone <- which(dset$Gender == "Boy") 
  analyze(dset,consume,TRUE,factone)
  analyze(permset,consume,FALSE,factone)
  
  observeEvent(input$rnumbers,{
    consume <<- input$rnumbers
    factone <- which(dset[[input$rfactors]] == levels(dset[[input$rfactors]])[1])
    analyze(dset,consume,TRUE,factone)
    analyze(permset,consume,FALSE,factone)
  })
  
  observeEvent(input$btnnew,{
    permset <<- scramble(dset,input$rfactors)
    output$scramtbl <- renderTable(permset)
    factone <- which(permset[[input$rfactors]] == levels(permset[[input$rfactors]])[1])
    analyze(permset,consume,FALSE,factone)
  })
  
  observeEvent(input$btntest,{
    result <- doPermtest(dset,consume,input$nsample,input$btnstat)
    factone <- which(dset[[input$rfactors]] == levels(dset[[input$rfactors]])[1])
    vline <- switch(input$btnstat,
                    diff = mean(dset[factone,consume])-mean(dset[-factone,consume]),
                    ratio = mean(dset[factone,consume])/mean(dset[-factone,consume]),
                    mdiff = median(dset[factone,consume])-median(dset[-factone,consume])
    )
    
    output$hist <- renderPlot({
      hist(result)
      abline(v = vline, col = "blue")
    })
    pvalue <- round((sum(result >= vline )+1)/(input$nsample+1),digits=10)
    output$pval <- renderUI(h3(paste("Pvalue =", pvalue)))
  })
}

#Run the app
shinyApp(ui = ui, server = server)