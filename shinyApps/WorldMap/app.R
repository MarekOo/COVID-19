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

#Cases
load("data_aggregated_Cases.RData")
#Deaths
load("data_aggregated_Deaths.RData")
#Recovered
load("data_aggregated_Recovered.RData")

#Cases
load("data_aggregated_Cases_daily.RData")
#Deaths
load("data_aggregated_Deaths_daily.RData")
#Recovered
load("data_aggregated_Recovered_daily.RData")

#Load the World Map from the maps package
world_map <- map_data("world")

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
  tags$h4("The graph shows the outbreak numbers on a map, having the regions colored by the intensity of the case numbers."),
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
  titlePanel("COVID-19 Outbreak Graph over Time"),
  tags$h4("The graph shows the outbreak timeline. Choose countries and a time range from the menu"),
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
  hr(),
  titlePanel("COVID-19 weekly developlemt"),
  tags$h4("The graph shows the development to assess a turning point, when the country falls below the diagonal line. Choose countries and a time from the menu"),
  fluidRow(
    column(4,
           sliderInput(inputId = "day2",
                       label = "Day of Outbreak",
                       min = min(days),
                       max = max(days),
                       value = max(days)),
           selectInput(inputId = "subject2",
                       label = "Choose a dataset:",
                       choices = c("Cases", "Deaths", "Recovered")),
           selectInput(inputId = "countries2",
                       label = "Countries",
                       multiple = TRUE,
                       choices = allCountries,
                       selected = c("USA","Germany","China"))
           
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
    
    if(input$subject2 == "Cases"){
      data_test <- Cases_daily
      subject = "Infected Cases"
    }else if(input$subject2 == "Deaths"){
      data_test <- Deaths_daily
      subject = "Deaths"
    }else{
      data_test <- Recovered_daily
      subject = "Recovered Cases"
    }
    needle = input$countries2
    data_test = data_test[which(data_test$region %in% needle),]
    
    countries = unique(data_test$region)
    minDate = min(data_test$date)
    data_test$weeklyCases = NA
    data_test$dailyCases = data_test$cases
    data_test$cases = NA
    
    for (i in 1:length(countries)){
      item = countries[i]
      print(item)
      for (j in 1:(length(unique(data_test$date))-7)) {
        print(j)
        startDate = minDate-1+j
        EndDate = startDate+7
        sumWeek = sum(data_test[which(data_test$date<EndDate & data_test$date>=startDate & data_test$region==item),]$dailyCases)
        totalCases = sum(data_test[which(data_test$date<EndDate & data_test$region==item),]$dailyCases)
        data_test[which(data_test$date==EndDate & data_test$region==item),]$weeklyCases = sumWeek
        data_test[which(data_test$date==EndDate & data_test$region==item),]$cases = totalCases
      }
      
    }
    
    data_test <- data_test[data_test$date >= (minDate+7),]
    
    maxDate = minDate+6+j
    maxDate = input$day2
    
    print(maxDate)
    data_plot <- data_test[data_test$date <= maxDate,]
    data_point <- data_test[data_test$date == maxDate,]
    plotC <- ggplot(data_plot, aes(y=weeklyCases,x=cases,colour=region)) + 
      geom_line(size=0.8,alpha=0.3) + 
      geom_abline(intercept = 0, slope = 1, color="white",linetype="dotted")+
      geom_point(data=data_point,aes(y=weeklyCases,x=cases,colour=region),size=3) +
      geom_text(data = data_point, aes(y=weeklyCases,x=cases, label = region), size = 4, vjust = 2.5) +
      scale_x_log10(labels = scales::comma, limits=c(1,1000000),breaks=c(10,100,1000,10000,100000,1000000)) + 
      scale_y_log10(labels = scales::comma, limits=c(1,1000000),breaks=c(10,100,1000,10000,100000,1000000)) + 
      ggtitle(paste0("CVOID-19 ",subject," per Week vs Total Cases on: ",maxDate)) + xlab(paste0("Total COVID-19 ",subject)) + 
      ylab("COVID-19 Cases last 7 Days")+
      scale_color_brewer(palette="Spectral") + 
      dark_theme_gray() +
      theme(axis.text.x = element_text(size = 12,color="grey")) +
      theme(axis.text.y = element_text(size = 12,color="grey")) +
      theme(plot.title = element_text(size=15,color="orange")) +
      theme(legend.position = "none") 
    plot(plotC)
  })
  
}


shinyApp(ui = ui, server = server)