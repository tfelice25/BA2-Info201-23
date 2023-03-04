

library(shiny)
library(tidyverse)

bike_data <- read_delim("thalia's work/SeoulBikeData3.CSV")


ui <- fluidPage(
  
  titlePanel("BA2 Info Final App"),
  
  


)

server <- function(input, output) {
  
  



}

# Run the application 
shinyApp(ui = ui, server = server)
