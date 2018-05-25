## start working on temperature

library(lubridate)
library(dplyr)
library(ggplot2)

all.df_completeSub <- read.csv("all_df_completesub.csv")
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


distinct(subset(maxtempall, city == "Buffalo" & season == "Winter"),
  Date) ## about 259 observations per city, season, and date combination

maxtempall <- (maxtempall %>% dplyr::group_by(city, season) %>%
    dplyr::distinct(maxtempall, Date, .keep_all = TRUE) %>% 
    dplyr::mutate(q90 = quantile(weatherval, .90)) %>% 
  dplyr::mutate(q50 = quantile(weatherval, .50)) %>% 
  dplyr::mutate(q10 = quantile(weatherval, .10)))



head(maxtempall)

maxtempall <- maxtempall %>% dplyr::filter(LengthForecastDayOnly == 3) %>%
  dplyr::group_by(city, season) %>%
  dplyr::mutate(q90.pred = predict(loess(forecastValue ~ weatherval, family = "gaussian", span = .75, degree = 1), newdata = q90[1])) %>%
  dplyr::mutate(q50.pred = predict(loess(forecastValue ~ weatherval, family = "gaussian", span = .75, degree = 1), newdata = q50[1])) %>%
  dplyr::mutate(q10.pred = predict(loess(forecastValue ~ weatherval, family = "gaussian", span = .75, degree = 1), newdata = q10[1]))


maxtempall[ ,c("q90", "q90.pred")]

subset(maxtempall, q90 == 86)$q90.pred

ggplot(data = maxtempall, aes(x = q90, y = q90.pred, colour = season)) + geom_point() +
  geom_abline(slope = 1, intercept = 0)

## some values are missing, which is weird.
summary(maxtempall$q10.pred); nrow(maxtempall)


length(unique(maxtempall$q10.pred - maxtempall$q10))
432 / 4; 436 / 4
length(unique(maxtempall$city))
## seems like we are missing a city for some of the quantiles

maxtempunique <- maxtempall %>% distinct(q50.pred, .keep_all = TRUE)
maxtempunique$q90.diff <- maxtempunique$q90.pred - maxtempunique$q90
maxtempunique$q50.diff <- maxtempunique$q50.pred - maxtempunique$q50
maxtempunique$q10.diff <- maxtempunique$q10.pred - maxtempunique$q10

qplot(maxtempunique$q10.diff)

cbind(maxtempunique$q10.diff, maxtempunique$city, maxtempunique$season)
levels(maxtempunique$city)[4]
levels(maxtempunique$season)[2]

## check out why Loess is not fitting for Atlanta in the Spring

subset(maxtempall, city == "Atlanta" & season == "Spring")$q10
subset(maxtempall, city == "Atlanta" & season == "Spring")$q10.pred
## not fitting because loess turns in NAs

loess.mod <- loess(forecastValue ~ weatherval, family = "gaussian",
  data = subset(maxtempall, city == "Atlanta" & season == "Spring"),
  span = 0.75, degree = 1)
loess.mod
predict(loess.mod, newdata = 63); predict(loess.mod, newdata = 67)
## 63 seems to be outside the scope of the model, I suppose. It should not be 
## because it is only the 10th percentile of the actual data....


## ANYWAYS

qplot(maxtempunique$q10.diff)
qplot(maxtempunique$q50.diff)
qplot(maxtempunique$q90.diff)

## we have overprediction for the lower temperatures and underprediction
## for the "average" and higher temperatures, lumping data across cities and seasons

ggplot(data = maxtempunique, aes(x = q10.diff)) + 
  geom_histogram(fill = "white", colour = "black", bins = 14) + 
  facet_wrap( ~season)
## might be interesting to check if my "outliers" match up with Erin's

ggplot(data = maxtempunique, aes(x = q50.diff)) + 
  geom_histogram(fill = "white", colour = "black", bins = 14) + 
  facet_wrap( ~season)

ggplot(data = maxtempunique, aes(x = q90.diff)) + 
  geom_histogram(fill = "white", colour = "black", bins = 14) + 
  facet_wrap( ~season)
