library(shiny)
library(shinythemes)
ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Central limit theorem - The Exponential Distribution"),
  
  fluidRow(
    column(4, wellPanel(
      sliderInput("n", "Sample size", min = 0, max = 100,
                  value = 50, step = 1),
      sliderInput("a", HTML("&lambda;"), min = 0, max = 10,
                  value = 2, step = 1),
      withMathJax(),
      helpText(' Probability density function $$ \\Huge \\lambda e^{-\\lambda x} $$')
    )),
    mainPanel(
      plotOutput("image")
    )
  )
)


smean <- function(n,a)
{ library(stats)
  x <- rexp(n,rate=a) 
  y <- ((1/n)*sum(x) - 1/a)/sqrt((1/a^2)/n)
  return(y)
}

rep_sample <- function(n,a,i)
{
  x <- replicate(i,smean(n,a))
  return(x)
  
}


server <- function(input, output) {
  z <- seq(from = -5,to = 5, by = 0.01)
  z <- z[-1]
  
  X <- reactive({
    x <- as.data.frame(rep_sample(input$n, input$a,1000))
    names(x) <- "X"
    x
  })
  
  library(ggplot2)
  # image1 creates a new PNG file each time Radius changes
  output$image <- renderPlot({
    p <- ggplot(X(),aes(x = X)) + 
      geom_histogram(aes(y = ..density..),binwidth = 0.3, color = "gray36", fill = "gray33") +
      geom_line(aes(x = z,y = dnorm(z), colour="N(0,1)"), lwd = 0.75) + 
      scale_colour_manual(" ",values = c("royalblue1")) +
      xlab(" ") + ylab("probability") +
   theme(text = element_text(size=15,  color = 'black' ) )  
  print(p)
  })
  
}


shinyApp(ui, server)






