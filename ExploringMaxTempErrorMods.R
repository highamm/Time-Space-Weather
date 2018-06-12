## exploring visualizations and models for maxtemp errors

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
winterclean <- winterclean[-which(winterclean$forecastValue < 32 &
    winterclean$weatherval > 75), ]
winterclean <- winterclean[-which(winterclean$forecastValue > 31 &
    winterclean$weatherval < 5), ]

## clean(er) data set
maxtempall <- rbind(springclean, summerclean, fallclean, winterclean)

## use only 1-day forecasts
maxtemponeday <- subset(maxtempall, LengthForecastDayOnly == 1)
str(maxtemponeday)

histWeather <- histWeather
str(histWeather)

maxtempwpreds <- merge(histWeather, maxtemponeday, 
  by.x = c("Date", "AirPtCd"),
  by.y = c("Date", "AirPtCd"))
str(maxtempwpreds)

## first, fit a model to 1 city

ggplot(data = subset(test.df, city == "Buffalo"),
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
summary(with(buffonly,
  lm(forecastDiff ~  adjmeanhum)))

## include season and humidity
summary(with(buffonly,
  lm(weatherval ~ forecastValue + adjmeanhum)))


ggplot(data = maxtempwpreds, aes(x = Mean_Humidity, y = forecastDiff)) + 
  geom_point()




summary(with(subset(maxtempwpreds, city == "Buffalo"),
  lm(weatherval ~ forecastValue + season + Mean_Wind_SpeedMPH + 
      Mean_Humidity)))
