  # Marek Opuszko - 04/2020 - COVID-19 Outbreak
  # Plot the CORONA COVID-19 Outbreak on a world map and color regions according to the strength of the outbreak
  # Export png images and create a video using ffmpeg

  # Data used: https://raw.githubusercontent.com/CSSEGISandData/COVID-19
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
  
  subject = "Deaths" # or "Deaths" or "Recovered Cases"
  
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
  
  #Load the World Map from the maps package
  world_map <- map_data("world")
  # check it out
  ggplot(world_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill="lightgray", colour = "white")
  
  
  # Now aggregate the covid data frame by region and date -> number of cases per region per date
  data_aggregated <- aggregate(cases ~ country + date, data = data_transformed, sum)
  names(data_aggregated) = c("region","date","cases")
  
  # unfortunately the CSSEGISandData dataset contains some non-standard names for countries. These have to be fixed manually :/
  data_aggregated$region[which(data_aggregated$region=="US")] = "USA"
  data_aggregated$region[which(data_aggregated$region=="Taiwan*")] = "Taiwan"
  
  # sort by date
  days = sort(unique(data_aggregated$date))
  
  #######################################################################################################
  # Plotting corona data on a world map
  #######################################################################################################
  
  # filter the corona data frame by a specific date, lets say 30th of march
  day = days[69]
  data_filtered_by_date <- data_aggregated[which(data_aggregated$date==day),c("region","cases")]
  # Now we need to join the two datasets corona and the world map together by the country name
  cases.exp.map <- left_join(data_filtered_by_date, world_map, by = "region")
  # Plot the map using ggplot polygon, colored by the number of cases using scale_fill_viridis_c colors
  plotWorldMap <- ggplot(cases.exp.map, aes(long, lat, group = group))+
    geom_polygon(aes(fill = cases ), color = "white")+
    scale_fill_viridis_c(option = "A",direction=-1)+
    dark_theme_gray()+ 
    ggtitle(paste0("COVID-19 ",subject," Date: ",day))
  plot(plotWorldMap)
  
  # Only US Staates?
  usa_map <- map_data("state")
  # check it out
  ggplot(usa_map, aes(x = long, y = lat, group = group)) +
    geom_polygon(fill="lightgray", colour = "white")
  
  data_us <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
  exclude_vars <- names(data_us) %in% c("UID", "iso2", "iso3","code3","FIPS","Admin2","Country_Region","Lat","Long_","Combined_Key")
  data_us <- data_us[!exclude_vars]
  data_us$Province_State <- tolower(data_us$Province_State)
  # transpose the table
  data_us_transformed <- melt(as.data.table(data_us), id=c("Province_State"), fun="Sum")
  # change the variable names to something more "speaking"
  names(data_us_transformed) <-  c("region","date","cases")
  # remove the X invoked by R (no variable names beginning with a number allowed)
  data_us_transformed$date <- gsub("X", "0", paste(data_us_transformed$date))
  # transform character dates to actual date format
  data_us_transformed$date <- as.Date(data_us_transformed$date, "%m.%d.%Y")
  
  data_aggregated_us <- aggregate(cases ~ region + date, data = data_us_transformed, sum)
  # pick a date
  us_data_filtered_by_date <- data_aggregated_us[which(data_aggregated_us$date==day),c("region","cases")]
  # filter US corona data
  cases.exp.map_us <- left_join(us_data_filtered_by_date, usa_map, by = "region")
  
  #plot the map 
  plotUSMap <- ggplot(cases.exp.map_us, aes(long, lat, group = group))+
    geom_polygon(aes(fill = cases ), color = "white")+
    scale_fill_viridis_c(option = "D",direction=-1)+
    dark_theme_gray()+ 
    ggtitle(paste0("COVID-19 Infections USA, Date: ",day))
  plot(plotUSMap)

  
  ###########################################################################################################
  ###########################################################################################################
  # Animation
  # produce a png for every time frame
  # use the pngs to create video via ffmpeg or convert
  ###########################################################################################################
  # use this folder for image output -> don't forget to set working directory!
  outputDir = "animation/"
  # Set the max for the y Variable to keep legend in plot stable
  ymaxTotal <- max(data_aggregated$cases)
  
  for (i in 1:length(days)) {
    day = days[i]
    number = str_pad(i, 4, pad = "0")
    print(number)
    data_filtered_by_date <- data_aggregated[which(data_aggregated$date==day),c("region","cases")]
    # Now we need to join the two datasets corona and the world map together by the country name
    cases.exp.map <- left_join(data_filtered_by_date, world_map, by = "region")
    # Plot the map using ggplot polygon, colored by the number of cases using scale_fill_viridis_c colors
    png(file = paste0(outputDir,"covid19_",subject,"_day_",number,".png"), bg = "white",width = 1280,height=768,pointsize = 12)
    plotA <- ggplot(cases.exp.map, aes(long, lat, group = group))+
      geom_polygon(aes(fill = cases ), color = "white")+
      scale_fill_viridis_c(option = "A",direction=-1,limits=c(0,ymaxTotal))+
      dark_theme_gray()+ 
      ggtitle(paste0("COVID-19 daily Infections, Date: ",days[i]))
    plot(plotA)
    dev.off() 
  }
  
  vidDate = max(data_aggregated$date)
  # video with ffmpeg
  system(paste0("ffmpeg -y -framerate 3 -i ",outputDir,"covid19_",subject,"_day_%04d.png -c:v libx264 -r 30 -pix_fmt yuv420p ",outputDir,"covid19_",subject,"_day_",vidDate,".mp4"))
  # gif animation with convert from imagemagick
  system(paste0("convert -delay 10 -loop 0 ",outputDir,"covid19_",subject,"*.png ",outputDir,"covid19_",subject,"_day_",vidDate,".gif"))
