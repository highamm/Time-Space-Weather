## File with what we need to reread each time R is restarted

library(readr)

# Erin don't run following lines
forecastdf <- read_csv("~/Desktop/TimeSpaceExpo/forecastdf.csv")
histWeather <- read_csv("~/Desktop/TimeSpaceExpo/histWeather.csv")
locations <- read_csv("~/Desktop/TimeSpaceExpo/locations.csv")

# Erin only 
forecastdf <- read.csv("~/Desktop/DataExpo2018/forecastdf.csv")
histWeather <- read.csv("~/Desktop/DataExpo2018/histWeather.csv")
locations <- read.csv("~/Desktop/DataExpo2018/locations.csv")


## both of us

forecastdfcolnames <- c("Obsnum", "Citynum", "DatePredicted", "Value",
  "Weatherval", "DateofForecast")

colnames(forecastdf) <- forecastdfcolnames
merged.df <- merge(locations, histWeather, by = "AirPtCd")

locations$citynum <- as.numeric(rownames(locations))
foreloc.df <- merge(forecastdf, locations, by.x = "Citynum", by.y = "citynum")

library(tidyr)

## first, get rid of unnecessary columns in histWeather
histWeathersub <- histWeather[ ,c("Date", "Max_TemperatureF",
  "Min_TemperatureF", "PrecipitationIn", "AirPtCd")]

## wide to long
histWeatherlong <- gather(histWeathersub, weathermeas, weatherval, 
  c(Max_TemperatureF, Min_TemperatureF, PrecipitationIn),
  factor_key = TRUE)

library(plyr)

histWeatherlong$weathermeas <- mapvalues(histWeatherlong$weathermeas,
  from = c("Max_TemperatureF", "Min_TemperatureF", "PrecipitationIn"),
  to = c("MaxTemp", "MinTemp", "ProbPrecip"))

foreloc.df$Weatherval <- as.factor(foreloc.df$Weatherval)

# is value the forecasted value and weatherval the observed weather
all.df <- merge(histWeatherlong, foreloc.df,
  by.x = c("Date", "AirPtCd", "weathermeas"),
  by.y = c("DatePredicted", "AirPtCd", "Weatherval"),
  all.x = TRUE)


## convert the character vector of historical weather to numeric
all.df$weatherval <- as.numeric(all.df$weatherval)

all.df_complete <- all.df[complete.cases(all.df), ]

# make sure dates are recorded as Date objects
all.df_complete$Date <- as.Date(all.df_complete$Date)
all.df_complete$DateofForecast <- as.Date(all.df_complete$DateofForecast)

# store values as numeric 
all.df_complete$weatherval <- as.numeric(as.character(all.df_complete$weatherval))
all.df_complete$Value <- as.numeric(as.character(all.df_complete$Value))

# add a column that has the difference in forecast and actual min temp

all.df_complete$forecastDiff <- all.df_complete$weatherval - all.df_complete$Value
all.df_complete$absForecastDiff <- abs(all.df_complete$forecastDiff)

library(lubridate)

all.df_complete$Date <- ymd(all.df_complete$Date)
all.df_complete$DateofForecast <- ymd(all.df_complete$DateofForecast)

# record the distance in time between the forecasted date and the observed date
all.df_complete$LengthForecast <- abs(as.duration(all.df_complete$DateofForecast %--% all.df_complete$Date))
all.df_complete$LengthForecastDays <- seconds_to_period(all.df_complete$LengthForecast)

all.df_complete$LengthForecastDayOnly <- as.numeric(substring(all.df_complete$LengthForecastDays, 1, 1))

# remove any observations where the date of forecast is after observed date
# This data set (all.df_completeSub) has dates where the forecasted date was 
# on or before the observed date
all.df_completeSub <- subset(all.df_complete, all.df_complete$DateofForecast <=
    all.df_complete$Date)
names(all.df_completeSub)[7] <- "forecastValue"


## we should have the same all.df_completeSub after running this script
# Erin
write.csv(all.df_completeSub, "~/Desktop/DataExpo2018/all_df_completesub.csv")
all.df_completeSub <- read.csv("~/Desktop/DataExpo2018/all_df_completesub.csv")

## Matt
write.csv(all.df_completeSub, "all_df_completesub.csv")
all.df_completeSub <- read.csv("all_df_completesub.csv")