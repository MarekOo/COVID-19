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

#Cases <- read.csv(paste0("data_aggregated_Cases.csv"),row.names = NULL)
#Deaths <- read.csv(paste0("data_aggregated_Deaths.csv"),row.names = NULL)
#Recovered <- read.csv(paste0("data_aggregated_Recovered.csv"),row.names = NULL)

#Cases
load("data_aggregated_Cases.RData")
#Deaths
load("data_aggregated_Deaths.RData")
#Recovered
load("data_aggregated_Recovered.RData")


#Load the World Map from the maps package
world_map <- map_data("world")

#setwd("~/ShinyApps/sample-apps/covid19_world_map")

allDates <- Cases$date
allCountries <- sort(unique(Cases$region))
days = sort(unique(allDates))
maxDays = length(days)
start = "2020-01-22"
#this is super weird ... 
end = paste0("20",format(max(allDates),"%Y-%m-%d"))
print(end)
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
                       choices = allCountries,
                       selected = c("USA","Germany","China")),
           dateRangeInput("daterange1", "Date range:",
                          start = start,
                          end   = end,
                          format = "yyyy-mm-dd")
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

server <- function(input, output) {

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
    dateA <- format(as.Date(input$daterange1[1],"%Y-%m-%d"), "%y-%m-%d")
    dateB <- format(as.Date(input$daterange1[2],"%Y-%m-%d"), "%y-%m-%d")
    dateA <- as.Date(dateA)
    dateB <- as.Date(dateB)

    # Subset by Country list
    data_subset = Cases[which(Cases$region %in% needle),]
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
      xlim(dateA,dateB)
  })
  
  output$plotC <- renderPlot({
    plot(NULL)
  })
  
}


shinyApp(ui = ui, server = server)