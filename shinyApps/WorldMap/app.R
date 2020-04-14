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
library(shiny)

theme_set(
  theme_void()
)
#setwd("~/ShinyApps/worldmap")

#subject = "Deaths"

Cases <- read.csv(paste0("data_aggregated_Cases.csv"),row.names = NULL)
Deaths <- read.csv(paste0("data_aggregated_Deaths.csv"),row.names = NULL)
Recovered <- read.csv(paste0("data_aggregated_Recovered.csv"),row.names = NULL)

Cases_2 <- Cases
Cases_2$region <- as.character(Cases_2$region)
Cases_2$date <- as.character(Cases_2$date)
Cases_2$date <- as.Date(Cases_2$date, "%Y-%m-%d")
Cases_2$X <- NULL

#Load the World Map from the maps package
world_map <- map_data("world")

#setwd("~/ShinyApps/sample-apps/covid19_world_map")
#Cases <- read.csv(paste0("data_aggregated_Cases.csv"),row.names = NULL)
allDates <- as.Date(Cases$date, "%m.%d.%y")
allCountries <- sort(unique(Cases$region))
days = sort(unique(allDates))
maxDays = length(days)
# Define UI for app that draws a histogram ----

###########################################################################################
###########################################################################################

ui <- fluidPage(
  
  # App title ----
  titlePanel("COVID-19 Outbreak World Map"),
  
  # Sidebar layout with input and output definitions ----
  
  fluidRow(
      column(4,
             sliderInput(inputId = "day",
                         label = "Day of Outbreak",
                         min = min(days),
                         max = max(days),
                         value = max(days)),
             selectInput(inputId = "subject",
                         label = "Choose a dataset:",
                         choices = c("Cases", "Deaths", "Recovered"))
             
            ),
      column(6,
             plotOutput(outputId = "plotA")
      )
    ),
  ####################################################################
  hr(),
  titlePanel("COVID-19 Outbreak World Map"),
  fluidRow(
    column(4,
           selectInput(inputId = "countries",
                       label = "Countries",
                       multiple = TRUE,
                       choices = allCountries),
           dateRangeInput("daterange1", "Date range:",
                          start = min(days),
                          end   = max(days),
                          format = "yy-mm-dd")
           ),
    column(6,
           plotOutput(outputId = "plotB")
    )
  ),
  fluidRow(
    column(4,
           sliderInput(inputId = "day2",
                       label = "Day of Outbreak",
                       min = min(days),
                       max = max(days),
                       value = max(days)),
           selectInput(inputId = "subject",
                       label = "Choose a dataset:",
                       choices = c("Cases", "Deaths", "Recovered"))
           
    ),
    column(6,
           plotOutput(outputId = "plotC")
    )
  )
)
  


###########################################################################################
###########################################################################################
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
    
  })
  
  output$plotB <- renderPlot({
    needle = input$countries
    
    # Subset by Country list
    data_subset = Cases_2[which(Cases_2$region %in% needle),]
    # first impressions
    ggplot(data_subset, aes(x=date,y=cases,color=region)) + 
      geom_line(aes(y=cases),size=1.1,alpha=0.7) + 
      geom_point(aes(y=cases),alpha=0.5) + 
      scale_y_log10(labels = scales::comma) + 
      ggtitle(paste0("COVID-19 infections over Time")) + xlab("Date") + 
      ylab("Number (log)")+
      scale_color_nejm() + 
      dark_theme_gray() +
      theme(axis.text.x = element_text(size = 15)) +
      theme(axis.text.y = element_text(size = 12)) +
      theme(plot.title = element_text(size=20,color="orange"))+
      xlim(input$daterange1[1],input$daterange1[2])
  })
  
  output$plotC <- renderPlot({
    plot(NULL)
  })
  
}


shinyApp(ui = ui, server = server)