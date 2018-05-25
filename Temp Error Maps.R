library(lubridate)
library(ggplot2)
library(leaflet)
library(dplyr)
library(maps)

complete_df <- read.csv("~/Desktop/DataExpo2018/all_df_completesub.csv")
names(complete_df)[8] <- "forecastValue"

# subset the data into sets of only max and min temps 
maxTemp <- subset(complete_df, weathermeas == "MaxTemp")
minTemp <- subset(complete_df, weathermeas == "MinTemp")

maxTemp$Date <- as.Date(maxTemp$Date)
minTemp$Date <- as.Date(minTemp$Date)

nrow(unique(maxTemp[c("Date", "AirPtCd", "DateofForecast")]))
