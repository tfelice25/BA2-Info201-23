

library(shiny)
library(tidyverse)

bike_data <- read_delim("thalia's work/SeoulBikeData3.CSV")


ui <- fluidPage(
  
  titlePanel("BA2 Info Final App"),
  tabsetPanel(type = "tabs",
              tabPanel("Overview Page"),
              tabPanel("Question 1"),
              tabPanel("Question 2"),
              tabPanel("Question 3",
                       sidebarLayout(
                         sidebarPanel(),
                         mainPanel(plotOutput("hour_plot")),
                       )),
              tabPanel("Question 4"),
              tabPanel("Conclusion")
              
              
              
              
  )
)

server <- function(input, output) {
  
  output$hour_plot <- renderPlot({
    bike_data %>% 
      select(hour,num_bikes_rented,seasons) %>% 
      group_by(hour) %>% 
      summarize(avg_bikes=mean(num_bikes_rented)) %>% 
      ggplot(aes(hour,avg_bikes))+geom_point()+geom_smooth()
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
