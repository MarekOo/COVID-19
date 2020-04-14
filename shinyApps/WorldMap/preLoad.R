# Marek Opuszko - 04/2020 - COVID-19 Outbreak
# Load data from https://raw.githubusercontent.com/CSSEGISandData/COVID-19 an
# save in a usable format

# Data used: https://raw.githubusercontent.com/CSSEGISandData/COVID-19

library(data.table)



subjectList = c("Cases","Deaths","Recovered Cases")
#setwd("~/ShinyApps/sample-apps/covid19_world_map")

for (subject in subjectList) {
  # Load the Dataset 
  if (subject=="Cases") {
    data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  }else if(subject == "Deaths") {
    data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  }else if(subject == "Recovered Cases") {
    data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  }
  
  data$Province.State = as.character(data$Province.State)
  data$Country.Region = as.character(data$Country.Region)
  # don't need lat and long, we will use map data later ...
  data$Lat = NULL
  data$Long = NULL
  #check out the data frame 
  str(data)
  
  # using the melt function to transpose the data frame (needs prior transformation to data table format)
  data_transformed <- melt(as.data.table(data), id=c("Province.State","Country.Region"), fun="Sum")
  # change the variable names to something more "speaking"
  names(data_transformed) <-  c("region","country","date","cases")
  # remove the X invoked by R (no variable names beginning with a number allowed)
  data_transformed$date <- gsub("X", "0", paste(data_transformed$date))
  # transform character dates to actual date format
  data_transformed$date <- as.Date(data_transformed$date, "%m.%d.%Y")
  
  # Now aggregate the covid data frame by region and date -> number of cases per region per date
  data_aggregated <- aggregate(cases ~ country + date, data = data_transformed, sum)
  names(data_aggregated) = c("region","date","cases")
  
  # unfortunately the CSSEGISandData dataset contains some non-standard names for countries. These have to be fixed manually :/
  data_aggregated$region[which(data_aggregated$region=="US")] = "USA"
  data_aggregated$region[which(data_aggregated$region=="Taiwan*")] = "Taiwan"
  
  

  write.csv(data_aggregated,paste0("data_aggregated_",subject,".csv"))
  save(data_aggregated,file = paste0("data_aggregated_",subject,".RData"))
}



