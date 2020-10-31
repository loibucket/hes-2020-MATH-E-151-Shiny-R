#Modular Multiplication
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(gmp)
library(numbers)
library(dplyr)
#css
stylesheet <- tags$head(tags$style(HTML('
    .main-header .logo {
      font-family: "Lucida Console", Monaco, monospace;
      font-weight: bold;
      font-size: 34px;
      background-color: black;
    }
  ')
))
#html
ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Loi's Modular Multiplication", titleWidth = 1000),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      stylesheet,
        fluidRow(
            column(
              width = 2,
                sliderInput("slider", "q", 1, 52, 10),
                box(width = NULL,
                    h4("Order of Elements"),
                    plotOutput("plot", height = "300px")
                ),
                tableOutput("counts"),
            ),
            column(
              width = 10,
                box(width = NULL,
                    height = 900,
                    h4("Multiplication Table"),
                    tableOutput("multable")
                ),

            )
        ) #fluid
    ), #body
)
#js
server <- function(input, output, session) {

  #prime numbers
  primes = 1:52
  primes = primes[isprime(1:52) != 0]
  #non prime numbers
  nonPrimes = 1:52
  nonPrimes = nonPrimes[isprime(1:52) == 0]

  #slider input
  observeEvent(input$slider, {
    q = input$slider
    #move slider from prime to nonprime
    if (q %in% primes) {
      mod = 0
      if (q + 1 %in% nonPrimes) { q = q + 1 }
      else if (q - 1 %in% nonPrimes) { q = q - 1 }
      updateSliderInput(session, "slider", min = 1, max = 52, value = q)
    }
    coprimes = coPrime(1:q - 1, q)
    #Display the multiplication table
    x <- coprimes
    y <- coprimes
    #tbl <- outer(x, y, "*") #same as %o%
    tbl <- x %o% y %% q
    colnames(tbl) <- x
    rownames(tbl) <- y
    output$multable <- renderTable(tbl, rownames = TRUE, digits = 0)
    #plot
    cnt = ordering(coprimes, q)
    h = cnt[['count']]
    names(h) = cnt[['order']]
    output$plot <- renderPlot(barplot(h))
    output$counts <- renderTable(cnt, rownames = FALSE, digits = 0)
  })

  #Find the co-primes in the given vector
  coPrime <- function(vec, q) {
    coprimes = numeric()
    for (i in vec) {
      if (GCD(i, q) == 1) {
        coprimes = c(coprimes, i)
      }
    }
    return(coprimes)
  }

  #orders
  ordering <- function(coprimes, q) {
    orders = double()
    for (c in coprimes) {
      orders = c(orders, level(c, q))
    }
    ord_df = data.frame(orders)
    cnt = count(ord_df, orders)
    names(cnt) = c("order", "count")
    return(cnt)
  }

  #levels
  level <- function(num, q) {
    lvl = 1
    lvlOneRemainder = num %% q
    remainder = num %% q
    while (remainder != 1) {
      lvl = lvl + 1
      remainder = (lvlOneRemainder * remainder) %% q
    }
    return(lvl)
  }
}

# Run the application
shinyApp(ui = ui, server = server)
