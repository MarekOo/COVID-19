setwd("~/ShinyApps/sample-apps/covid19_world_map")
Cases <- read.csv(paste0("data_aggregated_Cases.csv"),row.names = NULL)
Cases$date <- as.Date(Cases$date, "%Y-%m-%d")
days = sort(unique(Cases$date))
maxDays = length(days)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("COVID-19 Outbreak World Map"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "day",
                  label = "Day of Outbreak",
                  min = min(days),
                  max = max(days),
                  value = max(days)),
      selectInput(inputId = "subject",
                  label = "Choose a dataset:",
                  choices = c("Cases", "Deaths", "Recovered"))
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "plotA")
      
    )
  )
)