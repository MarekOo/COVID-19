library(tidyr)
library(data.table)
library(ggplot2)
require(scales)
library(ggsci)
library(ggdark)
library(dplyr)
require(maps)
require(viridis)
library(ggpubr)
theme_set(
  theme_void()
)
setwd("~/ShinyApps/sample-apps/covid19_world_map")
#subject = "Deaths"

Cases <- read.csv(paste0("data_aggregated_Cases.csv"),row.names = NULL)
Deaths <- read.csv(paste0("data_aggregated_Deaths.csv"),row.names = NULL)
Recovered <- read.csv(paste0("data_aggregated_Recovered.csv"),row.names = NULL)


#Load the World Map from the maps package
world_map <- map_data("world")



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  #subject <- reactive({
  #  switch(input$subject,
  #         "Cases" = Cases,
  #         "Deaths" = Deaths,
  #         "Recovered" = Recovered)
  #})

  

  
  output$plotA <- renderPlot({
    
    if(input$subject == "Cases"){
      data_aggregated <- Cases
      subject = "Infected Cases"
    }else if(input$subject == "Deaths"){
      data_aggregated <- Deaths
      subject = "Deaths"
    }else{
      data_aggregated <- Recovered
      subject = "Recovered Cases"
    }
    
    
    data_aggregated$region <- as.character(data_aggregated$region)
    data_aggregated$date <- as.character(data_aggregated$date)
    data_aggregated$date <- as.Date(data_aggregated$date, "%Y-%m-%d")
    data_aggregated$X <- NULL
    # sort by date
    #days = sort(unique(data_aggregated$date))
    
    # filter the corona data frame by a specific date, lets say 30th of march
    day = input$day
    data_filtered_by_date <- data_aggregated[which(data_aggregated$date==day),c("region","cases")]
    
    # Now we need to join the two datasets corona and the world map together by the country name
    cases.exp.map <- left_join(data_filtered_by_date, world_map, by = "region")
    
    ggplot(cases.exp.map, aes(long, lat, group = group))+
      geom_polygon(aes(fill = cases ), color = "white")+
      scale_fill_viridis_c(option = "A",direction=-1)+
      dark_theme_gray()+ 
      ggtitle(paste0("COVID-19 ",subject," Date: ",day))
    
  },width = 1024, height = 640)
  
}


