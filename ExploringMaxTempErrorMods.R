## exploring visualizations and models for maxtemp errors


library(lubridate)
library(dplyr)
library(ggplot2)
library(lme4)

all.df_completeSub <- read.csv("all_df_completesub.csv")
colnames(all.df_completeSub)[8] <- "forecastValue"
histWeather <- read.csv("histWeather.csv")


complete_df <- all.df_completeSub
str(complete_df)

maxtemp <- subset(complete_df, weathermeas == "MaxTemp")

maxtemp$Date <- as.Date(maxtemp$Date)
maxtemp$DateofForecast <- as.Date(maxtemp$DateofForecast)

maxtemp$month <- month(as.POSIXlt(maxtemp$Date))

maxtemp$season <- cut(maxtemp$month, 
  breaks = c(0.5, 2.5, 5.5, 8.5, 11.5, 12.5), 
  labels = c("Winter", "Spring", "Summer", "Fall", "Winter2"), 
  right = FALSE)
maxtemp$season[maxtemp$season == "Winter2"] <- "Winter"
maxtemp$season <- factor(maxtemp$season)

springtemp <- subset(maxtemp, month %in% c(3, 4, 5))
summertemp <- subset(maxtemp, month %in% c(6, 7, 8))
falltemp <- subset(maxtemp, month %in% c(9, 10, 11))
wintertemp <- subset(maxtemp, month %in% c(12, 1, 2))

## get rid of these points. These are almost surely data entry errors.
## The weather value is 1 degree for the first five days of March while the
## forecast is in the 40s.

## getting rid of the obvious outliers
springclean <- springtemp[-which(springtemp$weatherval < 5 & springtemp$forecastValue > 30), ]
summerclean <- summertemp
fallclean <- falltemp
winterclean <- wintertemp[-which(wintertemp$weatherval > 95), ]

#### ISSUE
#winterclean <- winterclean[-which(winterclean$forecastValue < 32 &
   # winterclean$weatherval > 75), ]

winterclean <- winterclean[winterclean$absForecastDiff < 26, ]


# winterclean <- winterclean[winterclean$forecastValue < 31 &
#     winterclean$weatherval < 5), ]

## clean(er) data set
maxtempall <- rbind(springclean, summerclean, fallclean, winterclean)

## use only 1-day forecasts
maxtemponeday <- subset(maxtempall, LengthForecastDayOnly == 1)
str(maxtemponeday)

histWeather <- histWeather
str(histWeather)

histWeather$Date <- as.Date(histWeather$Date, format = "%Y-%m-%d")


maxtempwpreds <- merge(histWeather, maxtemponeday, 
  by.x = c("Date", "AirPtCd"),
  by.y = c("Date", "AirPtCd"))
str(maxtempwpreds)

## first, fit a model to 1 city

# Just for Buffalo 
# temps are a lot less variable in the summer than in the winter 
# spring and fall similar
ggplot(data = subset(maxtempwpreds, city == "Buffalo"),
  aes(x = forecastValue, y = weatherval, group = season)) +
  geom_point() + 
  facet_wrap( ~ season) + 
  geom_smooth() +
  geom_abline(slope = 1, intercept = 0)

str(maxtempwpreds)

buffonly <- subset(maxtempwpreds, city == "Buffalo")

buffonly$adjmeanhum <- lag(buffonly$Mean_Humidity)

## following models ignore time-dependence in weather data

## underprediction of max temps

# model for errors using humidity of previous day 
moderrors <- with(buffonly,
  lm(forecastDiff ~  adjmeanhum))
acf(moderrors$residuals) 
pacf(moderrors$residuals)


# model for errors using same day humidity
moderrors2 <- with(buffonly, 
                   lm(forecastDiff ~ Mean_Humidity))

acf(moderrors2$residuals)


# all mean covariates (except temperature)
moderrors3 <- lm(forecastDiff ~ MeanDew_PointF + Mean_Humidity + 
                        Mean_Sea_Level_PressureIn + Mean_VisibilityMiles + 
                   Mean_Wind_SpeedMPH, 
                 data=maxtempwpreds)

summary(moderrors3)
plot(moderrors3)


moderrors4 <- lm(forecastDiff ~ MeanDew_PointF + Mean_Humidity + 
                   Mean_Sea_Level_PressureIn + Mean_VisibilityMiles + 
                   Mean_Wind_SpeedMPH + as.factor(season) + Mean_Wind_SpeedMPH*as.factor(season), 
                 data=maxtempwpreds)
summary(moderrors4)
plot(moderrors4)


moderrorsrand <- lmer(forecastDiff ~ MeanDew_PointF + Mean_Humidity + 
                        Mean_Sea_Level_PressureIn + Mean_VisibilityMiles + 
                        Mean_Wind_SpeedMPH + as.factor(season) + 
                        Mean_Wind_SpeedMPH*as.factor(season) + (Mean_Wind_SpeedMPH | city), 
                      data=maxtempwpreds)
summary(moderrorsrand)

## there seems to be a little time dependence among the errors in forecast
## and observed with mean humidity in the model for Buffalo.
## 
## I would not expect there to be much time dependence in the errors because
## I do not think that overpredicting the temperature on one day would necessarily
## indicate overprediction or underprediction on the following day.

## include season and humidity
mod <- with(buffonly,
  lm(weatherval ~ forecastValue + adjmeanhum))
acf(mod$residuals) 

ggplot(data = maxtempwpreds, aes(x = Mean_Humidity, y = forecastDiff)) + 
  geom_point()




summary(with(subset(maxtempwpreds, city == "Buffalo"),
  lm(weatherval ~ forecastValue + season + Mean_Wind_SpeedMPH + 
      Mean_Humidity)))
