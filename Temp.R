## start working on temperature

library(lubridate)
library(dplyr)

complete_df <- all.df_completeSub

summary(all.df_completeSub)
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

ggplot(springtemp, aes(x = forecastValue, y = weatherval)) + geom_point()
ggplot(summertemp, aes(x = forecastValue, y = weatherval)) + geom_point()
ggplot(falltemp, aes(x = forecastValue, y = weatherval)) + geom_point()
ggplot(wintertemp, aes(x = forecastValue, y = weatherval)) + geom_point()

## do a bit of data cleaning

ggplot(winterclean, aes(x = forecastValue, y = weatherval)) + geom_point()


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
ggplot(winterclean, aes(x = forecastValue, y = weatherval)) + geom_point()

## clean(er) data set
maxtempall <- rbind(springclean, summerclean, fallclean, winterclean)
summary(maxtempall$season)
summary(maxtempall)

## one pattern I've noticed: forecast gets much worse for longer days for 
## less temperate cities, which makes sense.
## might think about how to get different cities on the same plot
## without having to change the subset every time we want to look
## at a new city
ggplot(data = subset(maxtempall, city == "Scranton"),
  aes(x = LengthForecastDayOnly, y = absForecastDiff, 
    group = LengthForecastDayOnly)) +
  geom_boxplot() +
  facet_wrap( ~ season)
summary(maxtempall$city)

str(maxtempall)
ggplot(data = subset(maxtempall, city == "Buffalo" & LengthForecastDayOnly == 4),
  aes(x = weatherval, y = forecastValue, group = season)) +
  geom_point() + 
  facet_wrap( ~ season) + 
  geom_smooth() +
  geom_abline(slope = 1, intercept = 0)

## underprediction in winter for Scranton. Also underpredicts winter for Cleveland, Buffalo, etc. Seems to be a regional thing.



## compare temperature predictions across different cities for the 10th and 90th percentile 
## of temperature for each season

quantile(maxtempall$weatherval, c(0.10, 0.90))



ninetyperc <- (maxtempall %>% dplyr::group_by(city, season) %>%
    dplyr::distinct(maxtempall, Date, .keep_all = TRUE) %>% 
    dplyr::mutate(q90 = quantile(weatherval, .750)))


head(ninetyperc)

testdf <- ninetyperc %>% dplyr::filter(LengthForecastDayOnly == 3) %>%
  dplyr::group_by(city, season) %>%
  dplyr::mutate(testval = predict(loess(forecastValue ~ weatherval, family = "gaussian", span = .75, degree = 1), newdata = q90[1]))
str(testdf)
testdf

testdf[ ,c("testval", "q90")]
ggplot(data = testdf, aes(x = q90, y = testval, colour = season)) + geom_point() +
  geom_abline(slope = 1, intercept = 0)

qplot(unique(testdf$testval - testdf$q90))

loess.mod <- loess(forecastValue ~ weatherval, family = "gaussian",
  data = subset(maxtempall, city == "Buffalo" & season == "Winter"),
  span = .75, degree = 1)
predict(loess.mod, newdata = 50)
ninetyperc
