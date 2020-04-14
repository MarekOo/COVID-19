# Marek Opuszko - 04/2020 - COVID-19 Outbreak
# Load data from https://raw.githubusercontent.com/CSSEGISandData/COVID-19 an
# save in a usable format

# Data used: https://raw.githubusercontent.com/CSSEGISandData/COVID-19

library(data.table)



subjectList = c("Cases","Deaths","Recovered")
#setwd("~/shinyApps/WorldMap")

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
  data_daily <- data
  
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
  assign(subject, data_aggregated)
  save(list=c(subject),data_aggregated,file = paste0("data_aggregated_",subject,".RData"))
  
  # Daily Cases
  
  # check how many non date fields you have + 2 !!!
  first_cases_fields = 3 + 1
  
  max = length(data)
  
  # to calculate the daily cases, we simply substract the today vs yesterdays totals
  # sometimes the data is faulty, that's when you get negative cases :/
  for (i in max:first_cases_fields) {
    data_daily[,i] = data_daily[,i] - data_daily[,(i-1)]
    #due to meassure errors we get negative values sometimes ... 
    data_daily[which(data_daily[,i]<0),i]=0
  }
  
  # using the melt function to transpose the data frame (needs prior transformation to data table format)
  data_transformed_daily <- melt(as.data.table(data_daily), id=c("Province.State","Country.Region"), fun="Sum")
  # change the variable names to something more "speaking"
  names(data_transformed_daily) <-  c("region","country","date","cases")
  # remove the X invoked by R (no variable names beginning with a number allowed)
  data_transformed_daily$date <- gsub("X", "0", paste(data_transformed_daily$date))
  # transform character dates to actual date format
  data_transformed_daily$date <- as.Date(data_transformed_daily$date, "%m.%d.%Y")
  
  # Now aggregate the covid data frame by region and date -> number of cases per region per date
  data_aggregated_daily <- aggregate(cases ~ country + date, data = data_transformed_daily, sum)
  names(data_aggregated_daily) = c("region","date","cases")
  
  # unfortunately the CSSEGISandData dataset contains some non-standard names for countries. These have to be fixed manually :/
  data_aggregated_daily$region[which(data_aggregated_daily$region=="US")] = "USA"
  data_aggregated_daily$region[which(data_aggregated_daily$region=="Taiwan*")] = "Taiwan"
  write.csv(data_aggregated,paste0("data_aggregated_",subject,"_daily.csv"))
  assign(paste0(subject,"_daily"), data_aggregated_daily)
  save(list=c(paste0(subject,"_daily")),data_aggregated_daily,file = paste0("data_aggregated_",subject,"_daily.RData"))
}



