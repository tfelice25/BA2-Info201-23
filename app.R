

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
                         mainPanel(plotOutput("hour_plot"),
                                   plotOutput("hour_diff")),
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
  
  output$hour_diff <- renderPlot({
    
    year_round_data <- bike_data %>% 
      select(hour,num_bikes_rented,seasons) %>% 
      group_by(hour) %>% 
      summarize(avg_bikes=mean(num_bikes_rented)) %>% 
      pull(avg_bikes)
    
    
    avg_finder <- function(season) {
      seasons_data <- bike_data %>% 
        select(hour,num_bikes_rented,seasons) %>% 
        filter(seasons%in%season) %>% 
        group_by(hour,seasons) %>% 
        summarize(avg_bikes=mean(num_bikes_rented)) %>% 
        pull(avg_bikes)
      
      
      mean(seasons_data-year_round_data)
    }
    
    seasons_list <- c("Spring","Summer","Autumn","Winter")
    
    diff_list <- c(avg_finder("Spring"),avg_finder("Summer"),avg_finder("Autumn"),avg_finder("Winter"))
    
    diff_df <- data.frame(seasons_list,diff_list)
    

    ggplot(diff_df)+geom_col(aes(seasons_list,diff_list,fill=seasons_list))
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
