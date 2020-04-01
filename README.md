# COVID-19 Analytics of the COVID-19 (Corona) Spread using R
![COVID-19 Cases over time](plots/covid19_cases_top6.png)

## Goal
The overall goal is to model the COVID-19 spread in order to understand the spread and make (simple) predictions about the future trend of the pandemic. In order to easily model the whole thing, the data is logarithmized so that we can then use a simple linear regression. Building on this, we can then make forecasts for the next few days.

Libraries used:
```r
library(tidyr)
library(data.table)
library(ggplot2)
library(scales)
library(ggsci)
library(ggdark)
```

## 1. PREPROCESSING - DATA PREPARATION

Set the working directory and define a subject, Cases, Deaths or Recovered Cases. These 3 Datasets are provided by the CSSEGISandData Data. he dataset needs some adjustments, cleaning unnecesary variables and transpose from wide to long format.
```r
setwd("~/yourdirectoryhere")

subject = "Cases" # or "Deaths" or "Recovered Cases"

# Load the Dataset 
if (subject=="Cases") {
  data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
}else if(subject == "Deaths") {
  data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
}else if(subject == "Recovered Cases") {
  data = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
}

# The dataset needs some adjustments. Clearing data types:
data$Province.State = as.character(data$Province.State)
data$Country.Region = as.character(data$Country.Region)

#We can remove lat and long since we are not using this geo information here
data$Lat <-  NULL
data$Long <- NULL
# check the data frame
str(data)
```
