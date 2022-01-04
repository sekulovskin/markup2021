
library(shiny)


ui <- fluidPage(
  #add main title
  titlePanel("Nikola's first Shiny app"),
 
  tabsetPanel(  #add tabs
    
  
    #Normal
    tabPanel("Normal distribution",
    fluidRow(
    column(width = 3,
           sliderInput("n1", "Select value for the sample size (N)", min = 10, max = 10000, 
                       value = 50, step = 10)
    )),
  fluidRow(
    column(width = 3, 
           sliderInput("M", "Select value for the Mean (M)", min = 0, max = 1000, 
                       value = 0, step = 1)
    )),
  fluidRow(
    column(width = 3, 
           sliderInput("SD", "Select value for the Standard Deviation (SD)", min = 0, max = 100, 
                       value = 1, step = 1)
  )),
  
         plotOutput("hist1")
    ),
  
  #Exponential
  tabPanel("Exponential distribution",
           fluidRow(
             column(width = 3,
                    sliderInput("n2", "Select value for the sample size (N)", min = 10, max = 10000, 
                                value = 50, step = 10)
             )),
           fluidRow(
             column(width = 3, 
                    sliderInput("r", "Select value for the rate parameter", min = 1, max = 10000, 
                                value = 1, step = 1)
             )),
           plotOutput("hist2")
  ),
  
  # Poisson
  tabPanel("Poisson distribution",
           fluidRow(
             column(width = 3,
                    sliderInput("n3", "Select value for the sample size (N)", min = 10, max = 10000, 
                                value = 50, step = 10)
             )),
           fluidRow(
             column(width = 3, 
                    sliderInput("lambda", "Select value for the parameter (lambda)", min = 1, max = 10000, 
                                value = 4, step = 1)
             )),
           plotOutput("hist3")
  ),
  
  # Chi-square
  tabPanel("Chi-square distribution",
           fluidRow(
             column(width = 3,
                    sliderInput("n4", "Select value for the sample size (N)", min = 10, max = 10000, 
                                value = 50, step = 10)
             )),
           fluidRow(
             column(width = 3, 
                    sliderInput("df1", "Select value for the degrees of freedom", min = 1, max = 1000, 
                                value = 4, step = 5)
             )),
           fluidRow(
             column(width = 3, 
                    sliderInput("ncp", "Select value for the non-centrality parameter", min = 1, max = 4, 
                                value = 1, step = 1)
             )),
           plotOutput("hist4")
  ),
  
  # Binomial
  tabPanel("Binomial distribution",
           fluidRow(
             column(width = 3,
                    sliderInput("n5", "Select value for the number of observations", min = 5, max = 10000, 
                                value = 50, step = 5)
             )),
           fluidRow(
             column(width = 3, 
                    sliderInput("size", "Select the number of trials for each observation", min = 1, max = 100, 
                                value = 10, step = 2)
             )),
           fluidRow(
             column(width = 3, 
                    sliderInput("prob", "Select the success probability", min = 0, max = 1, 
                                value = 0.5, step = 0.001)
             )),
           plotOutput("hist5")
  ),
))


server <- function(input, output) {
  output$hist1 <- renderPlot({
    hist(rnorm(n = input$n1, mean = input$M, sd = input$SD), main = "Normal data", xlab = "", 
         col = "grey", border = "white")
  })
  output$hist2 <- renderPlot({
    hist(rexp(n = input$n2, rate = input$r), main = "Exponential data", xlab = "", 
         col = "grey", border = "white")
  })
  output$hist3 <- renderPlot({
    hist(rpois(n = input$n3, lambda = input$lambda), main = "Poisson data", xlab = "",
         col = "grey", border = "white")
  })
  output$hist4 <- renderPlot({
    hist(rchisq(n = input$n4, df = input$df1, ncp = input$ncp), main = "Chi-square data", xlab = "",
         col = "grey", border = "white")
  })
  output$hist5 <- renderPlot({
    hist(rbinom(n = input$n5, size = input$size, prob = input$prob), main = "Binomial data", xlab = "",
         col = "grey", border = "white")
  })
}

shinyApp(ui, server)
