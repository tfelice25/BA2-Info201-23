

library(shiny)
library(tidyverse)

bike_data <- read_delim("thalia's work/SeoulBikeData3.CSV")


ui <- fluidPage(
  
  titlePanel("BA2 Info Final App"),
  tabsetPanel(type = "tabs",
              tabPanel("Overview Page"),
              tabPanel("Question 1"),
              tabPanel("Question 2"),
              tabPanel("Question 3"),
              tabPanel("QUestion 4"),
              
              
              
              
  )
)

server <- function(input, output) {
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
