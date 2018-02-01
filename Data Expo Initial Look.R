library(readr)
histWeather <- read_csv("~/Desktop/Data Expo JSM/DataExpo2018/histWeather.csv")

locations <- read_csv("~/Desktop/Data Expo JSM/DataExpo2018/locations.csv")

forecast.df <- read.delim("forecast.dat", sep = "") 
str(data)
str(locations)
str(histWeather)
View(forecast.df)
View(locations)
View(histWeather)
