#GroupD6
source("buttonrows.R")
library(shiny)
library(shinydashboard)
library(shinyWidgets)
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Lucida Console", Monaco, monospace;
      font-weight: bold;
      font-size: 34px;
      background-color: black;
    }
  ')
))
ui <- dashboardPage(
    skin="yellow",
    dashboardHeader(title = "Group D6 by Loi", titleWidth=600),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      stylesheet,
        fluidRow(
            column(width=3,
                   box(
                       width = NULL,
                       height = 400,
                       h4("Elements of the group"),
                       h5("The identity"),
                       controlRow1(
                           "ctrlI"
                       ),  #agb
                       h5("Order 6 elements"),
                       controlRow2(
                           c("ctrl123456", "ctrl165432")
                       ),  #agb
                       h5("Order 3 elements"),
                       controlRow2(
                           c("ctrl135.246", "ctrl153.264")
                       ),  #agb
                       h5("Order 2 elements"),
                       controlRow7(
                         c("ctrl13.46","ctrl26.35","ctrl15.24","ctrl14.25.36","ctrl14.23.56","ctrl12.36.45","ctrl16.25.34")
                       )  #agb
                       
                   ),#box
                   box(
                       width = NULL,
                       height = 100,
                       title = "Subgroups",
                       buttonRow4(
                           inputIds = c("btnC3","btnC6","btnS3","btnV4"),
                           labels = c("Show C3","Show C6","Show S3","Show V4"),
                           btnStyle = "padding:4px;font-size:100%"
                       )  #agb
                   ),#box
                   # box(
                   #     width = NULL,
                   #     height = 100,
                   #     title = "Cosets",
                   #     buttonRow2(
                   #         inputIds = c("btnLC", "btnRC"),
                   #         labels = list("Left Cosets", "Right Cosets"),
                   #         btnStyle = "padding:4px;font-size:100%"
                   #     )  #agb
                   # ),
                   box(
                     width = NULL,
                     height = 120,
                     title = "Clear",
                     buttonRow1(
                       inputIds = c("btnclear"),
                       labels = list("Clear"),
                       btnStyle = "padding:4px;font-size:120%"
                     ),  
                     h4(uiOutput("genmsg"))
                   )#box
            ),  #col
            column(
                width = 9,
                box(
                    width = NULL,
                    h4("Inputs and Products"),
                    height = 350,
                    htmlOutput("results"),
                    tags$head(tags$style("#results{color:red; font-size:20px;
                    font-style:italic; overflow-y:scroll;
                    max-height: 300px; background: ghostwhite;}")),
                ),
                box(width = NULL, height = 60,actionBttn("reset", "Clear Inputs and Products") ),

                box(width = NULL,
                    height = 800,
                    h4("Multiplication Table"),
                    tableOutput("multable")
                )
            )
        )  #fluid
    ), #body
)

source("permutecalc.R")
source("d6calc.R")
#Computes a product as specified by "a" and "b" in vector v
evaluate <- function(v,a,b) {
  result <- "I"
  for (i in 1:length(v)){
    result <- Perm.multiply(result,ifelse(v[i]=="a",a,b))
  }
  return (result)
}
#evaluate(c("a","b"),"(123)","(12)")

#Everything that follows involves something in the UI
server <- function(input, output, session) {
  #Global variables accessible to server()
  N <- 12
  neutral <- "gray90"
  D6DF <- makeD6data(neutral)
  #Elements in the chosen subgroup
  subgroup <- numeric(0)
  #Color for subgroup buttons
  subcolor <- "yellow"
  #Output to display in the text box
  result.list <- ""
  #Result of all multiplications so far
  product <- "I"
  
  #Variables for cosets and conjugate subgroups
  conjugating <- FALSE
  generating <- 0
  a <-"I"
  gena <- "I"
  genb <- "I"

  color.list <- c("pink", "lightblue","lightgreen")   #colors for cosets

    displayButton = function(i) {
        renderUI({actionButton(D6DF[i,1],D6DF[i,2],
                   style=paste("padding:4px;font-size:100%;background:",D6DF[i,3]))}) 
    }
    #show all the buttons
    showButtons <- function() {
      output$ctrlI <- displayButton(1)
      output$ctrl123456<- displayButton(2)                                     
      output$ctrl135.246<- displayButton(3)
      output$ctrl14.25.36<- displayButton(4)
      output$ctrl153.264<- displayButton(5)
      output$ctrl165432<- displayButton(6)
      output$ctrl13.46<- displayButton(7)
      output$ctrl26.35<- displayButton(8)
      output$ctrl15.24<- displayButton(9)
      output$ctrl14.23.56<- displayButton(10)
      output$ctrl12.36.45<- displayButton(11)
      output$ctrl16.25.34<- displayButton(12)
    }
    
    showButtons()
    #Display the multiplication table
    tbl <- outer(D6DF[,2],D6DF[,2],Vectorize(Perm.multiply,c("a","b")))
    colnames(tbl) <- D6DF[,2]
    rownames(tbl) <- D6DF[,2] 
    output$multable <- renderTable(tbl,rownames = TRUE)
    #Multiplies by a specified permutation and displays all calculations so far
    compute.and.show <- function(perm){
        if (conjugating) {
          a <<- perm
          output$conjmsg <- renderUI(paste0("Conjugating by element ",perm,collapse=""))
          conjugating <<- FALSE
          return()
        }
        if (generating==1) {
          gena <<- perm
          output$genmsg <- renderUI(paste0("Generating with element ",gena,collapse=""))
         return()
        }
        if (generating==2) {
          genb <<- perm
          output$genmsg <- 
            renderUI(paste0("Generating with elements ",gena," and ", genb,collapse=""))
          return()
        }
        product <<- Perm.multiply(perm,product)
        line.out <- paste(perm,product,sep = "&emsp;")
        result.list <<- paste(result.list, line.out, sep = "<br/>")
        output$results<-renderUI(HTML(result.list))
    }
    #Marks all elements in a subgroup with a color
    mark.subgroup <- function() {
        for (i in 1:N){
            D6DF[i,3] <<- ifelse(i %in% subgroup,subcolor,neutral)
        }
    }
    #Event handlers for all the element buttons 
    observeEvent(input$btnI,{compute.and.show("I")})
    observeEvent(input$btn123456,{compute.and.show("(123456)")})
    observeEvent(input$btn135.246,{compute.and.show("(135)(246)")})
    observeEvent(input$btn14.25.36,{compute.and.show("(14)(25)(36)")})
    observeEvent(input$btn153.264,{compute.and.show("(153)(264)")})
    observeEvent(input$btn165432,{compute.and.show("(165432)")})
    observeEvent(input$btn13.46,{compute.and.show("(13)(46)")})
    observeEvent(input$btn26.35,{compute.and.show("(26)(35)")})
    observeEvent(input$btn15.24,{compute.and.show("(15)(24)")})
    observeEvent(input$btn14.23.56,{compute.and.show("(14)(23)(56)")})
    observeEvent(input$btn12.36.45,{compute.and.show("(12)(36)(45)")})
    observeEvent(input$btn16.25.34,{compute.and.show("(16)(25)(34)")})

    #The reset button clears the output and reinitializes the product
    observeEvent(input$reset,{
        result.list <<- ""
        product <<- "I"
        output$results<-renderUI(HTML(result.list))
    })
    #Event handlers for the subgroup buttons
    observeEvent(input$btnC3,{
      subgroup <<- c(1,3,5)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$btnC6,{
      subgroup <<- c(1,2,3,4,5,6)
      mark.subgroup()
      showButtons()
    })
    # S3
    # "(12)(36)(45)"  "(14)(23)(56)"  "(16)(25)(34)"  "(135)(246)"  "(153)(264)"  "I"
    #     11               10              12             3              5           1
    observeEvent(input$btnS3,{
      subgroup <<- c(1,3,5,10,11,12)
      mark.subgroup()
      showButtons()
    })
    # V4
    # "(14)(25)(36)"  "(16)(25)(34)"  "(13)(46)"  "I"
    #      4               12            7         1
    observeEvent(input$btnV4,{
      subgroup <<- c(1,4,7,12)
      mark.subgroup()
      showButtons()
    })
    observeEvent(input$btnclear,{
      subgroup <<- c(0)
      mark.subgroup()
      showButtons()
    })
    #Event handler for left cosets
    observeEvent(input$btnLC,{
        mark.subgroup()
        idx = 1   #index into the color list -- one for each coset
        #Keep creating cosets as long as there are elements that are still gray
        while(length(which(D6DF$color == neutral) >0)){
            #Find the first unassigned group element
            in.coset <- which(D6DF$color == neutral)[1]
            #Generate its left coset and put a new color on the buttons
            for (j in 1:N) {
              if(j %in% subgroup) {
                element <- Perm.multiply(D6DF[in.coset,2],D6DF[j,2])
                k <- which(D6DF[,2] == element)[1]
                D6DF[k,3] <<- color.list[idx]
              }
            }
            idx <- idx + 1
        }
        showButtons()
    })
    #Right cosets work the same way
    observeEvent(input$btnRC,{
        mark.subgroup()
        idx = 1   #index into the color list -- one for each coset
        #Keep creating cosets as long as there are elements that are still gray
        while(length(which(D6DF$color == neutral) > 0)){
            #Find the first unassigned group element
            in.coset <- which(D6DF$color == neutral)[1]
            #Generate its left coset and put a new color on the buttons
            for (j in 1:N) {
              if(j %in% subgroup) {
                  element <- Perm.multiply(D6DF[j,2],D6DF[in.coset,2])
                  k <- which(D6DF[,2] == element)[1]
                  D6DF[k,3] <<- color.list[idx]
                }
            }
            idx <- idx + 1
        }
        showButtons()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
