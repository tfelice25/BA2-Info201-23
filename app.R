

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
                         sidebarPanel(checkboxGroupInput("Seasons", label="Seasons", choices = list(
                           "Year Round"="Year Round", "Spring"="Spring", "Summer"="Summer",
                           "Autumn"="Autumn", "Winter"="Winter"), selected = "Year Round")
                           ),
                         mainPanel(plotOutput("hour_plot")),
                       )),
              tabPanel("Question 4"),
              tabPanel("Conclusion")
              
              
              
              
  )
)
# comment to see if app is being shared and tracked correctly 

server <- function(input, output) {
  
  output$hour_plot <- renderPlot({
    
    year_round_data <- bike_data %>% 
      select(hour,num_bikes_rented,seasons) %>% 
      group_by(hour) %>% 
      summarize(avg_bikes=mean(num_bikes_rented))
    
    seasons_data <- bike_data %>% 
      select(hour,num_bikes_rented,seasons) %>% 
      filter(seasons%in%input$Seasons) %>% 
      group_by(hour,seasons) %>% 
      summarize(avg_bikes=mean(num_bikes_rented))
    
    if ("Year Round"%in%input$Seasons==TRUE) {
        ggplot()+geom_point(data=year_round_data,aes(hour,avg_bikes),color="Black")+geom_smooth(data=year_round_data,aes(hour,avg_bikes),se=FALSE,color="Black")+
        geom_point(data=seasons_data,aes(hour,avg_bikes,col=seasons))+geom_smooth(data=seasons_data,aes(hour,avg_bikes,col=seasons),se=FALSE)
      
    }
    else{
      ggplot()+geom_point(data=seasons_data,aes(hour,avg_bikes,col=seasons))+geom_smooth(data=seasons_data,aes(hour,avg_bikes,col=seasons),se=FALSE)
    }
    
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
