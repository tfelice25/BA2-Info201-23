

library(shiny)
library(dplyr)
library(tidyverse)

bike_data <- read_csv("bikes.csv")
num <- unique(bike_data$year)

ui <- fluidPage(
  
  titlePanel("BA2 Info Final App"),
  tabsetPanel(type = "tabs",
              tabPanel("Overview Page"),
              tabPanel("Question 1",
                       sidebarPanel("This graph shows the average number of bike rentals 
                       each month for the year selected.", 
                                    radioButtons("year", "Select Year",
                                                 choices = c(num))),
                       mainPanel(textOutput("text"), textOutput("text2"), plotOutput("plot"), tableOutput("sample"))
                       ),
              tabPanel("Question 2"),
              tabPanel("Question 3"),
              tabPanel("Question 4"),
              tabPanel("Conclusion")
  )
)
# comment to see if app is being shared and tracked correctly 

server <- function(input, output) {
  
  # Create a bar chart of average bike rentals by month
  output$plot <- renderPlot({
   new <- 
    bike_data%>%
      filter(year %in% input$year)
   new%>%
     group_by(month) %>%
     summarize(avg_rentals = mean(num_bikes_rented)) %>%
      ggplot(aes(month, avg_rentals, fill = month)) +
     geom_bar(stat = "identity", position = "dodge")+
      ggtitle(paste("Average Bike Rentals by Month for", input$year))
  })
  
  output$sample <- renderTable({
    newg <- 
      bike_data%>%
      filter(year %in% input$year)
    newg%>%
      group_by(month) %>%
      summarize(avg_rentals = mean(num_bikes_rented)) 
  })
  
  year_data <- reactive({
    bike_data%>%
      filter(year == input$year)%>%
      group_by(month) %>%
      summarize(total_rentals = mean(num_bikes_rented)) %>%
      filter(total_rentals == max(total_rentals))
  })
  
  # Find the month with the most rentals for the input year
  output$text <- renderPrint({
      paste("The month with the most rentals for this year on average is", year_data()[1])
  })
  
  avg_data <- reactive({
    bike_data%>%
      filter(year == input$year)%>%
      group_by(month) %>%
      summarize(total_rentals = mean(num_bikes_rented)) %>%
      filter(total_rentals == min(total_rentals))
  })
  
  # Find the month with the most rentals for the input year
  output$text2 <- renderPrint({
    paste("The month with the least rentals for this year on average is", avg_data()[1])
  })
  
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
