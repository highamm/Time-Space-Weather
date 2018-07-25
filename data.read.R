## File with what we need to reread each time R is restarted

library(readr)
library(dplyr)

# Matt only
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

str(histWeather)
head(histWeather)
## first, get rid of unnecessary columns in histWeather
histWeathersub <- histWeather[ ,c("Date", "Max_TemperatureF",
  "Min_TemperatureF", "PrecipitationIn", "AirPtCd")]

## keeping all the variables in histWeathernow
##histWeathersub <- histWeather[ ,c("Date", "Max_TemperatureF",
##  "Min_TemperatureF", "Mean_TemperatureF", "Max_Dew_PointF",
##  "MeanDew_PointF", "Min_DewpointF", "Max_Humidity", "Mean_Humidity",
##  "Min_Humidity", "Mean_Sea_Level_PressureIn", 
##  "Mean_VisibilityMiles", "Max_Wind_SpeedMPH", "Mean_Wind_SpeedMPH",
##  "Max_Gust_SpeedMPH", "PrecipitationIn", "CloudCover",
##  "WindDirDegrees", "AirPtCd")]

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

all.df$Date <- as.Date(all.df$Date)
summary(all.df$Date)

## convert the character vector of historical weather to numeric
all.df$weatherval <- as.numeric(all.df$weatherval)

all.df_complete <- all.df[complete.cases(all.df), ]
summary(all.df_complete$Date)

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
summary(all.df_completeSub$Date)
names(all.df_completeSub)[7] <- "forecastValue"


## we should have the same all.df_completeSub after running this script
# Erin
write.csv(all.df_completeSub, "~/Desktop/DataExpo2018/Data Expo 2018/all_df_completesub.csv")
all_df_completeSub <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/all_df_completesub.csv")
all_df_completeSub <- all.df_completeSub

## Matt
write.csv(all.df_completeSub, "all_df_completesub.csv")
all_df_completeSub <- read.csv("all_df_completesub.csv")


all_df_completeSub$Date <- as.Date(all_df_completeSub$Date)
all_df_completeSub$DateofForecast <- as.Date(all_df_completeSub$DateofForecast)


all_df_completeSub$month <- month(as.POSIXlt(all_df_completeSub$Date))

all_df_completeSub$season <- cut(all_df_completeSub$month, 
  breaks = c(0.5, 2.5, 5.5, 8.5, 11.5, 12.5), 
  labels = c("Winter", "Spring", "Summer", "Fall", "Winter2"), 
  right = FALSE)

all_df_completeSub$season[all_df_completeSub$season == "Winter2"] <- "Winter"
all_df_completeSub$season <- factor(all_df_completeSub$season)


## getting the maxtemponly data set

maxtemponly <- subset(all_df_completeSub, weathermeas == "MaxTemp")
length(maxtemponly$month)
levels(all_df_completeSub$weathermeas)
maxtemponly$month
springtemp <- subset(maxtemponly, month %in% c(3, 4, 5))
summertemp <- subset(maxtemponly, month %in% c(6, 7, 8))
falltemp <- subset(maxtemponly, month %in% c(9, 10, 11))
wintertemp <- subset(maxtemponly, month %in% c(12, 1, 2))

## getting the mintemponly data set
mintemponly <- subset(all_df_completeSub, weathermeas == "MinTemp")
length(mintemponly$month)
levels(all_df_completeSub$weathermeas)
springtempMin <- subset(mintemponly, month %in% c(3, 4, 5))
summertempMin <- subset(mintemponly, month %in% c(6, 7, 8))
falltempMin <- subset(mintemponly, month %in% c(9, 10, 11))
wintertempMin <- subset(mintemponly, month %in% c(12, 1, 2))

##springclean <- springtemp[-which(springtemp$weatherval < 5 & springtemp$forecastValue > 30), ]
springclean <- springtemp
summerclean <- summertemp
fallclean <- falltemp
winterclean <- wintertemp[-which(wintertemp$weatherval > 95), ]
winterclean <- winterclean[winterclean$absForecastDiff < 26, ]

## getting rid of the obvious outliers
springcleanMin <- springtempMin
summercleanMin <- summertempMin[-which(summertempMin$weatherval < 10), ]
summercleanMin <- summercleanMin[-which(summercleanMin$forecastValue < 70 & 
                                    summercleanMin$weatherval > 90), ]
fallcleanMin <- falltempMin[-which(falltempMin$weatherval > 98 | falltempMin$weatherval < -100), ]
wintercleanMin <- wintertempMin[-which(wintertempMin$forecastValue > 40 &
                                   wintertempMin$weatherval < -20), ]

maxtempall <- rbind(springclean, summerclean, fallclean, winterclean)
mintempall <- rbind(springcleanMin, summercleanMin, fallcleanMin, wintercleanMin)


histWeather$Date <- as.Date(histWeather$Date, format = "%Y-%m-%d")
maxtempall$Date <- as.Date(maxtempall$Date, format = "%Y-%m-%d")
mintempall$Date <- as.Date(mintempall$Date, format = "%Y-%m-%d")

maxtempwpreds <- base::merge(histWeather, maxtempall, 
  by.x = c("Date", "AirPtCd"),
  by.y = c("Date", "AirPtCd"))

mintempwpreds <- base::merge(histWeather, mintempall, 
                             by.x = c("Date", "AirPtCd"), 
                             by.y = c("Date", "AirPtCd"))

## test <- dplyr::right_join(histWeather, maxtempall, by = c("Date", "AirPtCd"))

# maxtempalldat <- maxtempwpreds %>% dplyr::group_by(AirPtCd) %>%
#   mutate(adjmeanhum = lag(Mean_Humidity),
#     adjmeanwind = lag(Mean_Wind_SpeedMPH),
#     adjmeandew = lag(MeanDew_PointF),
#     adjmeanpressure = lag(Mean_Sea_Level_PressureIn),
#     adjmeanvis = lag(Mean_VisibilityMiles),
#     adjmaxtemp = lag(Max_TemperatureF))


maxtempalldat <- maxtempwpreds %>% 
  group_by_(.dots = c("AirPtCd", "season")) %>%
  mutate(adjmeanhum = lag(Mean_Humidity),
         adjmeanwind = lag(Mean_Wind_SpeedMPH),
         adjmeandew = lag(MeanDew_PointF),
         adjmeanpressure = lag(Mean_Sea_Level_PressureIn),
         adjmeanvis = lag(Mean_VisibilityMiles),
         adjmaxtemp = lag(Max_TemperatureF))
nrow(maxtempalldat)
summary(maxtempalldat$Date)
names(maxtempalldat)[names(maxtempalldat) == "Value"] <- "forecastValue"


# mintempalldat <- mintempwpreds %>% dplyr::group_by(AirPtCd) %>%
#   mutate(adjmeanhum = lag(Mean_Humidity),
#          adjmeanwind = lag(Mean_Wind_SpeedMPH),
#          adjmeandew = lag(MeanDew_PointF),
#          adjmeanpressure = lag(Mean_Sea_Level_PressureIn),
#          adjmeanvis = lag(Mean_VisibilityMiles),
#          adjmintemp = lag(Min_TemperatureF))

mintempalldat <- mintempwpreds %>% 
  group_by_(.dots = c("AirPtCd", "season")) %>%
  mutate(adjmeanhum = lag(Mean_Humidity),
         adjmeanwind = lag(Mean_Wind_SpeedMPH),
         adjmeandew = lag(MeanDew_PointF),
         adjmeanpressure = lag(Mean_Sea_Level_PressureIn),
         adjmeanvis = lag(Mean_VisibilityMiles),
         adjmintemp = lag(Min_TemperatureF))


nrow(mintempalldat)
summary(mintempalldat$Date)
names(mintempalldat)[names(mintempalldat) == "Value"] <- "forecastValue"


maxtempalldat$Error <- maxtempalldat$weatherval - maxtempalldat$forecastValue
maxtempalldat$SquaredError <- maxtempalldat$Error ^ 2
summary(maxtempalldat$absForecastDiff)

mintempalldat$Error <- mintempalldat$weatherval - mintempalldat$forecastValue
mintempalldat$SquaredError <- mintempalldat$Error ^ 2
summary(mintempalldat$absForecastDiff)

## USE THIS DATA SET FOR MAXTEMP (UNLESS WE DECIDE TO GET RID OF MORE "OUTLIERS")
## USE mintempalldat for minimum

