

library(shiny)
library(dplyr)
library(tidyverse)

bike_data <- read_csv("bikes.csv")
bikes2 <- read_delim("bikes2.csv")
num <- unique(bike_data$year)

ui <- fluidPage(
  
  titlePanel("BA2 Info Final App"),
  tabsetPanel(type = "tabs",
      tabPanel("Overview Page",
               imageOutput("seoulbike"),
               h1("Analyzing Bike Rental Data in Seoul, South Korea"),
               h2("Background of the Data"),
               p("The data used in the following interactive visualizations and analysis was provided by the", em("UCI Machine Learning Repository"), ". 
                 It shows data on bike rentals in Seoul, South Korea over a two year period: from December 2017 to November 2018. The dataset includes 14 variables (columns) and 8,760 separate enteries (rows)."),
               h2("Goals"),
               p("We want to analyze what environmental conditions inform demand rates of rentable bikes. Specifically,
                 we imagined our audience would be the managers and designers of bike share companies (such as lime, Bird, MoBike and others),
                 as these individuals would have a special interest in this data. We also imagined our data and our visualizations would be helpful 
                 to organizations trying to design cleaner transportation systems to help prevent climate change. At the end of our analysis, we want to
                 be able to determine", strong("when is the best time, and what are the best environmental conditions, to increase bike stock availability the most?"))
              
               
               
               ),
              tabPanel("Question 1",
                       sidebarPanel("This graph shows the average number of bike rentals 
                       each month for the year selected.", 
                                    radioButtons("year", "Select Year",
                                                 choices = c(num))),
                       mainPanel(textOutput("text"), textOutput("text2"), plotOutput("plot"), tableOutput("sample"))
                       ),
      
      
              tabPanel("Question 2",
                        sidebarLayout(
                           sidebarPanel(
                              sliderInput("month",
                                          "month of the year", 
                                           value = 6,
                                           min = 1,
                                           max = 12),
                              
                              h3("Month Number Key"),
                              p(strong(1), "- January"),
                              p(strong(2), "- February"),
                              p(strong(3), "- March"),
                              p(strong(4), "- April"),
                              p(strong(5), "- May"),
                              p(strong(6), "- June"),
                              p(strong(7), "- July"),
                              p(strong(8), "- August"),
                              p(strong(9), "- September"),
                              p(strong(10), "- October"), 
                              p(strong(11), "- November"),
                              p(strong(12), "- December"), 
                           ),
                        mainPanel(
                          h1("How Does Solar Radiation Affect Bike Rentals on a Given Day?"),
                          plotOutput("plotsolar"),
                          h2("Explanation"),
                          p("While future editions of our app may include a regression line to highlight the 
                            relationship between solar radiation and bike rentals, the current plot shows little to no
                            strong correlation between the two variables."),
                          h2("Data Table With Median, Minimum and Maximum Values"),
                          tableOutput("tablesolar")
                          
                          ))),
                       
                       
              tabPanel("Question 3",
                       sidebarLayout(
                         sidebarPanel(checkboxGroupInput("Seasons", label="Seasons", choices = list(
                           "Year Round"="Year Round", "Spring"="Spring", "Summer"="Summer",
                           "Autumn"="Autumn", "Winter"="Winter"), selected = "Year Round"),
                           h2("Plot 1"),
                           p("For Plot 1 titled \"Average Bike Rentals by Hour\" the idea was to make a plot that could clearly show the average per hour while indicating the continuous aspect of
                             bikes being rented out throughout the continuous course of a day/hour. In doing so, the goal was to be able to see what hours have peak rentals and with the introduction
                             of the different seasonal data, we could compare whether those peaks or dips change based off of the season \n"),
                           p("\n"),
                           h2("Plot 2"),
                           p("Plot number two is not an interactive plot but rather looks to add some visualization and build a little off of the previous plot. We found the difference for each
                             each hour for a season from the year round average in order to see the average deviation each month had. While the prior graph contains this information based off
                             of the distance between the LOESS lines, plot 2 offers an exact value for the average distance and offers a clearer visual to determine deviation. ")
                           
                           ),
                         mainPanel(plotOutput("hour_plot"),
                                   plotOutput("hour_diff"))
                       )),
              

              tabPanel("Question 4: Precipitation Impacts on Bike Rentals",
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput("precipSlider", label = h3("Select Precipitation (mm) Range"), min = 0, 
                                       max = 35, value = c(0, 15)),
                           selectInput("select", label = h3("Select Point Color"), 
                                       choices = list("Black" = 1, "Blue" = 2, "Green" = 3, "Purple" = 4), 
                                       selected = "Red"),
                         ),   
                         mainPanel(plotOutput("precip_plot"),
                                   p("This graphical display shows us that there is a distinct negative correlation between bike rental
                                     rates and precipitation. This makes sense, as those who choose to bike not out of necessity
                                     would likely opt for other modes of transportation if it were raining, which is significantly less
                                     pleasant in the rain. Of course this isn't universally true, as some people likely rely on 
                                     bikes to get to places around the city like their place of work. The average number of bikes rented
                                     for this range of precipitation is:"),
                                   strong(textOutput("avPrecip"))),
                        )),
              
              
              tabPanel("Conclusion", mainPanel(
                p("this is a written conclusion place holder")))
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
  
  # Create table of bike rental average for each month
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
        geom_point(data=seasons_data,aes(hour,avg_bikes,col=seasons))+geom_smooth(data=seasons_data,aes(hour,avg_bikes,col=seasons),se=FALSE)+
        labs(title="Average Bike Rentals by Hour",x="Hour of Day",y="Average Number of Bikes Rented")
      
    }
    else{
      ggplot()+geom_point(data=seasons_data,aes(hour,avg_bikes,col=seasons))+geom_smooth(data=seasons_data,aes(hour,avg_bikes,col=seasons),se=FALSE)+
        labs(title="Average Bike Rentals by Hour",x="Hour of Day",y="Average Number of Bikes Rented")
      
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
    

    ggplot(diff_df)+geom_col(aes(seasons_list,diff_list,fill=seasons_list))+labs(title="Difference from Year Round Average Rental Rate",x="Seasons",y="Average Difference",fill="Seasons")

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
  
  # Find the month with the least rentals for the input year
  output$text2 <- renderPrint({
    paste("The month with the least rentals for this year on average is", avg_data()[1])
  })
  
  bikemonth <- reactive({
    bikes2 %>% 
      filter(month %in% input$month)
  })
  output$plotsolar <- renderPlot({ # plot code goes here
    bikemonth() %>% 
      ggplot(aes(solar_radiation,num_bikes_rented))+
      geom_point()+
      labs(title = "Comparing the Number of Bikes Rented to Solar Radiation", x= "Amount of Solar Radiation in MJ/m2", y="Number of Bikes Rented")
 
  })
  
  #define as reactive for plot
  bikesInput <- reactive({
    bike_data %>% 
      filter(rainfall %in% input$precipSlider)
  })
  
  #make reactive text for plot
  output$avPrecip <- renderText({
    print(mean(bikesInput()$num_bikes_rented))
  })
  
  #access range from slider on plot page
  output$range <- renderPrint({ input$precipSlider })
  
  #access dot color changer for plot
  output$value <- renderPrint({ input$selectColor })
  
  #define colors
  colors <- reactive({ switch(
    input$selectColor,
    "1" = "black",
    "2" = "blue",
    "3" = "green",
    "4" = "purple"
  )
  })
  
  #make plot
  output$precip_plot <- renderPlot({
    bike_data %>% 
      select(rainfall,num_bikes_rented) %>% 
      filter(rainfall>=input$precipSlider[1], rainfall<=input$precipSlider[2]) %>% 
      group_by(rainfall) %>% 
      summarize(avg_bikes=mean(num_bikes_rented)) %>% 
      ggplot(aes(rainfall, avg_bikes))+
      scale_fill_manual(values = colors)+
      geom_point()+
      geom_smooth()+
      labs(x = "Precipitation (mm) per day", y = "Number of Bikes Rented per day",
           title = "Analyzing Impact of Precipitation on Bike Rental Rates in Seoul")
  })
  
  output$tablesolar <- renderTable({
    table <- bikes2 %>% 
      filter(!is.na(solar_radiation), !is.na(num_bikes_rented), month %in% input$month) %>% 
      group_by(month) %>% 
      summarize(median_bikes = median(num_bikes_rented), median_solar = median(solar_radiation), min_bikes = min(num_bikes_rented), max_bikes = max(num_bikes_rented), min_solar = min(solar_radiation), max_solar = max(solar_radiation))
  })
  
  output$seoulbike <- renderImage({
    filename <- normalizePath(file.path("./images/seoulbike.jpeg"))
    list(src = filename,
         alt = paste("Two Women in Seoul Using the Public Bike Share System"))
    }, deleteFile = FALSE)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

