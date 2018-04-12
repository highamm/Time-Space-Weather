library(readr)
histWeather <- read_csv("~/Desktop/TimeSpaceExpo/histWeather.csv")

locations <- read_csv("~/Desktop/TimeSpaceExpo/locations.csv")

forecast.df <- read.delim("forecast.dat", sep = "") 
str(data)
str(locations)
str(histWeather)
View(forecast.df)
View(locations)
View(histWeather)

## merge histWeather and locations
merged.df <- merge(locations, histWeather, by = "AirPtCd")

## subsetting data to Eugene only for a single time series
Eug.only <- subset(merged.df, AirPtCd == "KEUG")

## subsetting to a single date so that we have a single spatial process at one
## particular time

July1.only <- subset(merged.df, Date == "2014-07-01")
str(July1.only)
