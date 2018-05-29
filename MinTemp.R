## same as temp but now trying to work with mintemp

## start working on temperature

library(lubridate)
library(dplyr)
library(ggplot2)

all.df_completeSub <- read.csv("all_df_completesub.csv")
complete_df <- all.df_completeSub

summary(all.df_completeSub)
mintemp <- subset(complete_df, weathermeas == "MinTemp")

mintemp$Date <- as.Date(mintemp$Date)
mintemp$DateofForecast <- as.Date(mintemp$DateofForecast)

mintemp$month <- month(as.POSIXlt(mintemp$Date))



mintemp$season <- cut(mintemp$month, 
  breaks = c(0.5, 2.5, 5.5, 8.5, 11.5, 12.5), 
  labels = c("Winter", "Spring", "Summer", "Fall", "Winter2"), 
  right = FALSE)
mintemp$season[mintemp$season == "Winter2"] <- "Winter"
mintemp$season <- factor(mintemp$season)

springtemp <- subset(mintemp, month %in% c(3, 4, 5))
summertemp <- subset(mintemp, month %in% c(6, 7, 8))
falltemp <- subset(mintemp, month %in% c(9, 10, 11))
wintertemp <- subset(mintemp, month %in% c(12, 1, 2))

ggplot(springtemp, aes(x = forecastValue, y = weatherval)) + geom_point()
ggplot(summertemp, aes(x = forecastValue, y = weatherval)) + geom_point()
ggplot(falltemp, aes(x = forecastValue, y = weatherval)) + geom_point()
ggplot(wintertemp, aes(x = forecastValue, y = weatherval)) + geom_point()

## do a bit of data cleaning


## get rid of these points. These are almost surely data entry errors.
## The weather value is 1 degree for the first five days of March while the
## forecast is in the 40s.

## getting rid of the obvious outliers
springclean <- springtemp
summerclean <- summertemp[-which(summertemp$weatherval < 10), ]
summerclean <- summerclean[-which(summerclean$forecastValue < 70 & 
    summerclean$weatherval > 90), ]
ggplot(summerclean, aes(x = forecastValue, y = weatherval)) + geom_point()

fallclean <- falltemp[-which(falltemp$weatherval > 98 | falltemp$weatherval < -100), ]
ggplot(fallclean, aes(x = forecastValue, y = weatherval)) + geom_point()

winterclean <- wintertemp[-which(wintertemp$forecastValue > 40 &
    wintertemp$weatherval < -20), ]
ggplot(winterclean, aes(x = forecastValue, y = weatherval)) + geom_point()

## clean(er) data set
mintempall <- rbind(springclean, summerclean, fallclean, winterclean)
summary(mintempall$season)
summary(mintempall)

## one pattern I've noticed: forecast gets much worse for longer days for 
## less temperate cities, which makes sense.
## might think about how to get different cities on the same plot
## without having to change the subset every time we want to look
## at a new city
ggplot(data = subset(mintempall, city == "Scranton"),
  aes(x = LengthForecastDayOnly, y = absForecastDiff, 
    group = LengthForecastDayOnly)) +
  geom_boxplot() +
  facet_wrap( ~ season)
summary(mintempall$city)

ggplot(data = subset(mintempall, city == "Buffalo" & LengthForecastDayOnly == 4),
  aes(x = weatherval, y = forecastValue, group = season)) +
  geom_point() + 
  facet_wrap( ~ season) + 
  geom_smooth() +
  geom_abline(slope = 1, intercept = 0)




## compare temperature predictions across different cities for the 10th and 90th percentile 
## of temperature for each season

quantile(mintempall$weatherval, c(0.10, 0.90))


distinct(subset(mintempall, city == "Buffalo" & season == "Winter"),
  Date) ## about 256 observations per city, season, and date combination

mintempall <- (mintempall %>% dplyr::group_by(city, season) %>%
    dplyr::distinct(mintempall, Date, .keep_all = TRUE) %>% 
    dplyr::mutate(q90 = quantile(weatherval, .90)) %>% 
    dplyr::mutate(q50 = quantile(weatherval, .50)) %>% 
    dplyr::mutate(q10 = quantile(weatherval, .10)))


head(mintempall)

mintempall <- mintempall %>% dplyr::filter(LengthForecastDayOnly == 3) %>%
  dplyr::group_by(city, season) %>%
  dplyr::mutate(q90.pred = predict(loess(forecastValue ~ weatherval, family = "gaussian", span = .75, degree = 1), newdata = q90[1])) %>%
  dplyr::mutate(q50.pred = predict(loess(forecastValue ~ weatherval, family = "gaussian", span = .75, degree = 1), newdata = q50[1])) %>%
  dplyr::mutate(q10.pred = predict(loess(forecastValue ~ weatherval, family = "gaussian", span = .75, degree = 1), newdata = q10[1]))


mintempall[ ,c("q90", "q90.pred")]

ggplot(data = mintempall, aes(x = q90, y = q90.pred, colour = season)) + geom_point() +
  geom_abline(slope = 1, intercept = 0)

## some values are missing, which is weird.
summary(mintempall$q10.pred); nrow(mintempall)


length(unique(mintempall$q10.pred - mintempall$q10))
432 / 4; 436 / 4
length(unique(mintempall$city))
## seems like we are missing a few observations for some of the quantiles

mintempunique <- mintempall %>% distinct(q50.pred, .keep_all = TRUE)
mintempunique$q90.diff <- mintempunique$q90.pred - mintempunique$q90
mintempunique$q50.diff <- mintempunique$q50.pred - mintempunique$q50
mintempunique$q10.diff <- mintempunique$q10.pred - mintempunique$q10

## ANYWAYS

qplot(mintempunique$q10.diff)
qplot(mintempunique$q50.diff)
qplot(mintempunique$q90.diff)

## kind of the opposite pattern for minimum daily temperature, though not quite
## as extreme as the max temperature. Here, we see overprediction for the
## lowest quantile and for the median. There is underprediction for the largest
## quantile

## overprediction is constant across the seasons
ggplot(data = mintempunique, aes(x = q10.diff)) + 
  geom_histogram(fill = "white", colour = "black", bins = 14) + 
  facet_wrap( ~season)

## might be interesting to check if my "outliers" match up with Erin's
ggplot(data = mintempunique, aes(x = q50.diff)) + 
  geom_histogram(fill = "white", colour = "black", bins = 14) + 
  facet_wrap( ~season)

ggplot(data = mintempunique, aes(x = q90.diff)) + 
  geom_histogram(fill = "white", colour = "black", bins = 14) + 
  facet_wrap( ~season)

## the above days are just for forecast lengths of 3. We might consider how to
## incorporate a visualization for different lenghts of forecast days. Like one 
## way might be to facet by length of days and then colour by season?
## Oh, or facet by season and then have boxplots for each of the lengths of days. I
## like that idea better:

mintempall <- rbind(springclean, summerclean, fallclean, winterclean)

mintempall <- (mintempall %>% dplyr::group_by(city, season) %>%
    dplyr::distinct(mintempall, Date, .keep_all = TRUE) %>% 
    dplyr::mutate(q90 = quantile(weatherval, .90)) %>% 
    dplyr::mutate(q50 = quantile(weatherval, .50)) %>% 
    dplyr::mutate(q10 = quantile(weatherval, .10)))

mintempall <- mintempall %>% dplyr::filter(LengthForecastDayOnly %in% c(1, 2, 3, 4, 5)) %>%
  dplyr::group_by(city, season, LengthForecastDayOnly) %>%
  dplyr::mutate(q90.pred = predict(loess(forecastValue ~ weatherval, family = "gaussian", span = .75, degree = 1), newdata = q90[1])) %>%
  dplyr::mutate(q50.pred = predict(loess(forecastValue ~ weatherval, family = "gaussian", span = .75, degree = 1), newdata = q50[1])) %>%
  dplyr::mutate(q10.pred = predict(loess(forecastValue ~ weatherval, family = "gaussian", span = .75, degree = 1), newdata = q10[1]))
head(mintempall)

mintempall[ ,c("q90", "q90.pred")]

ggplot(data = mintempall, aes(x = q90, y = q90.pred, colour = season)) + geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap( ~ LengthForecastDayOnly)

## some values are missing, which is weird.
summary(mintempall$q10.pred); nrow(mintempall)


length(unique(mintempall$q10.pred - mintempall$q10))
432 / 4; 436 / 4
length(unique(mintempall$city))
## seems like we are missing a few observations for some of the quantiles

mintempunique <- mintempall %>% distinct(q50.pred, .keep_all = TRUE)
mintempunique$q90.diff <- mintempunique$q90.pred - mintempunique$q90
mintempunique$q50.diff <- mintempunique$q50.pred - mintempunique$q50
mintempunique$q10.diff <- mintempunique$q10.pred - mintempunique$q10

qplot(mintempunique$q10.diff)
qplot(mintempunique$q50.diff)
qplot(mintempunique$q90.diff)

## kind of the opposite pattern for minimum daily temperature, though not quite
## as extreme as the max temperature. Here, we see overprediction for the
## lowest quantile and for the median. There is underprediction for the largest
## quantile


ggplot(data = mintempunique, aes(x = q90.diff)) + 
  geom_histogram(fill = "white", colour = "black", bins = 14) + 
  facet_wrap( ~season)

ggplot(data = mintempunique, aes(x = as.factor(LengthForecastDayOnly),
  y = q10.diff)) + 
  geom_boxplot() +
  facet_wrap( ~season)

## predictions do seem to get better but are still either under or over predicting,
## depending on the quantile.