## self-contained file for the predictive function


all.df_completeSub <- read.csv("all_df_completesub.csv")
colnames(all.df_completeSub)[8] <- "forecastValue"
histWeather <- read.csv("histWeather.csv")

library(lubridate)
library(dplyr)
library(ggplot2)
library(lme4)

complete_df <- all.df_completeSub

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


springclean <- springtemp[-which(springtemp$weatherval < 5 & springtemp$forecastValue > 30), ]
summerclean <- summertemp
fallclean <- falltemp
winterclean <- wintertemp[-which(wintertemp$weatherval > 95), ]
winterclean <- winterclean[winterclean$absForecastDiff < 26, ]


maxtempall <- rbind(springclean, summerclean, fallclean, winterclean)

## use only 1-day forecasts
maxtemponeday <- subset(maxtempall, LengthForecastDayOnly == 1)

histWeather$Date <- as.Date(histWeather$Date, format = "%Y-%m-%d")

maxtempwpreds <- merge(histWeather, maxtemponeday, 
  by.x = c("Date", "AirPtCd"),
  by.y = c("Date", "AirPtCd"))

maxtemplags <- maxtempwpreds %>% dplyr::group_by(city) %>%
  mutate(adjmeanhum = lag(Mean_Humidity),
    adjmeanwind = lag(Mean_Wind_SpeedMPH),
    adjmeandew = lag(MeanDew_PointF),
    adjmeanpressure = lag(Mean_Sea_Level_PressureIn),
    adjmeanvis = lag(Mean_VisibilityMiles))

maxtemplagscomp <- maxtemplags[complete.cases(maxtemplags$adjmeanhum, 
  maxtemplags$adjmeanwind), ] 

## only use half of the data for model fitting. Save the other half for
## validation

set.seed(070718)
sampindeces <- sample(1:nrow(maxtemplagscomp), size = floor(nrow(maxtemplagscomp) / 2),
  replace = FALSE)

maxtemplagstrain <- maxtemplagscomp[sampindeces, ]
maxtemplagstest <- maxtemplagscomp[-sampindeces, ]
modrand <- lmer(weatherval ~ forecastValue + adjmeanhum
  + adjmeanwind + season + (season | city), 
  data = maxtemplagstrain)
ranef(modrand)

## ERIN!! Hi, and you could also mess around with the model above.
## I picked that one to allow for cities to have different "intercepts"
## for the different seasons and since wind and humidity can easily be found
## on your phone.

## Baltimore is completely missing one of the variables so is
## not included in the model

pred.fun <- function(forecastValue, adjmeanhum, adjmeanwind, season,
  city) {
  todayinfo2 <- data.frame("forecastValue" = forecastValue,
    "season" = season,
    "city" = city,
    "adjmeanhum" = adjmeanhum,
    "adjmeanwind" = adjmeanwind)
  predict(modrand, todayinfo2,
    allow.new.levels = TRUE)
}


maxtemplagstest$newpreds <- with(maxtemplagstest, 
  pred.fun(forecastValue = forecastValue,
    adjmeanhum = adjmeanhum, adjmeanwind = adjmeanwind, 
  season = season, city = city))
maxtemplagstest$oldpreds <- maxtemplagstest$forecastValue
maxtemplagstest$histdata <- maxtemplagstest$weatherval

maxtemplagstest$mod.diff <- maxtemplagstest$newpreds - maxtemplagstest$histdata
maxtemplagstest$orig.diff <- maxtemplagstest$oldpreds - maxtemplagstest$histdata
with(maxtemplagstest,
  tapply(mod.diff, INDEX = list(city, season), FUN = mean))
MSPEfun <- function(x) {
  mean(x^2)
}
with(maxtemplagstest,
  tapply(mod.diff, INDEX = list(city, season), FUN = mean))
with(maxtemplagstest,
  tapply(orig.diff, INDEX = list(city, season), FUN = mean))
with(maxtemplagstest,
  tapply(mod.diff, INDEX = list(city, season), FUN = MSPEfun))
with(maxtemplagstest,
  tapply(orig.diff, INDEX = list(city, season), FUN = MSPEfun))

mean((newpreds - histdata)^2, na.rm = TRUE)
mean((oldpreds - histdata)^2)






modrandnoforecast <- lmer(weatherval ~ adjmeanhum
  + adjmeanwind + month + (month | city), 
  data = maxtemplagstrain)

pred.fun2 <- function(adjmeanhum, adjmeanwind, month,
  city) {
  todayinfo2 <- data.frame("month" = month,
    "city" = city,
    "adjmeanhum" = adjmeanhum,
    "adjmeanwind" = adjmeanwind)
  predict(modrandnoforecast, todayinfo2,
    allow.new.levels = TRUE)
}


maxtemplagstest$newpredsnoforecast <- with(maxtemplagstest, 
  pred.fun2(adjmeanhum = adjmeanhum, adjmeanwind = adjmeanwind, 
    month = month, city = city))
maxtemplagstest$oldpreds <- maxtemplagstest$forecastValue
maxtemplagstest$histdata <- maxtemplagstest$weatherval

maxtemplagstest$mod.diffnoforecast <- maxtemplagstest$newpredsnoforecast - maxtemplagstest$histdata
maxtemplagstest$orig.diff <- maxtemplagstest$oldpreds - maxtemplagstest$histdata

with(maxtemplagstest, mean((newpreds - histdata)^2, na.rm = TRUE))
with(maxtemplagstest, mean((newpredsnoforecast - histdata)^2, na.rm = TRUE))
with(maxtemplagstest, mean((oldpreds - histdata)^2))

with(maxtemplagstest, mean((newpreds - histdata), na.rm = TRUE))
with(maxtemplagstest, mean((newpredsnoforecast - histdata), na.rm = TRUE))
with(maxtemplagstest, mean((oldpreds - histdata)))

## gets rid of the bias but has very large variance


with(maxtemplagstest,
  tapply(mod.diff, INDEX = list(city, season), FUN = mean))
MSPEfun <- function(x) {
  mean(x^2)
}
with(maxtemplagstest,
  tapply(mod.diff, INDEX = list(city, season), FUN = mean))
with(maxtemplagstest,
  tapply(orig.diff, INDEX = list(city, season), FUN = mean))
with(maxtemplagstest,
  tapply(mod.diff, INDEX = list(city, season), FUN = MSPEfun))
with(maxtemplagstest,
  tapply(orig.diff, INDEX = list(city, season), FUN = MSPEfun))












### LOOKING AT 5-DAY Forecast Predictions

maxtempfiveday <- subset(maxtempall, LengthForecastDayOnly == 5)

histWeather$Date <- as.Date(histWeather$Date, format = "%Y-%m-%d")

maxtempwpreds <- merge(histWeather, maxtempfiveday, 
  by.x = c("Date", "AirPtCd"),
  by.y = c("Date", "AirPtCd"))

maxtemplags <- maxtempwpreds %>% dplyr::group_by(city)
str(maxtemplags)

maxtemplagscomp <- maxtemplags

## only use half of the data for model fitting. Save the other half for
## validation

set.seed(070718)
sampindeces <- sample(1:nrow(maxtemplagscomp), size = floor(nrow(maxtemplagscomp) / 2),
  replace = FALSE)

maxtemplagscomp$Day <- day(maxtemplagscomp$Date)
maxtemplagscomp$monthhalf <- maxtemplagscomp$Day
maxtemplagscomp$monthhalf[maxtemplagscomp$Day < 15] <- "FirstHalf"
maxtemplagscomp$monthhalf[maxtemplagscomp$Day >= 15] <- "SecondHalf"
maxtemplagscomp$monthint <- interaction(maxtemplagscomp$month,
  maxtemplagscomp$monthhalf)

maxtemplagstrain <- maxtemplagscomp[sampindeces, ]
maxtemplagstest <- maxtemplagscomp[-sampindeces, ]




modrand <- lmer(weatherval ~ as.factor(monthint) + as.factor(season) +
    (as.factor(season) | city), 
  data = maxtemplagstrain)
ranef(modrand)


pred.fun5day <- function(monthint, season, city) {
  todayinfo2 <- data.frame("monthint" = monthint, "season" = season,
    "city" = city)
  predict(modrand, todayinfo2,
    allow.new.levels = TRUE)
}


maxtemplagstest$newpreds <- with(maxtemplagstest, 
  pred.fun5day(season = season, city = city, monthint = monthint))
maxtemplagstest$oldpreds <- maxtemplagstest$forecastValue
maxtemplagstest$histdata <- maxtemplagstest$weatherval

with(maxtemplagstest, mean((newpreds - histdata), na.rm = TRUE))
with(maxtemplagstest, mean((oldpreds - histdata)))

with(maxtemplagstest, mean((newpreds - histdata)^2, na.rm = TRUE))
with(maxtemplagstest, mean((oldpreds - histdata)^2))


maxtemplagstest$mod.diff <- maxtemplagstest$newpreds - maxtemplagstest$histdata
maxtemplagstest$orig.diff <- maxtemplagstest$oldpreds - maxtemplagstest$histdata
with(maxtemplagstest,
  tapply(mod.diff, INDEX = list(city, season), FUN = mean))
MSPEfun <- function(x) {
  mean(x^2)
}
with(maxtemplagstest,
  tapply(mod.diff, INDEX = list(city, season), FUN = mean))
with(maxtemplagstest,
  tapply(orig.diff, INDEX = list(city, season), FUN = mean))
tab1 <- with(maxtemplagstest,
  tapply(mod.diff, INDEX = list(city, season), FUN = MSPEfun))
tab2 <- with(maxtemplagstest,
  tapply(orig.diff, INDEX = list(city, season), FUN = MSPEfun))

## kind of interesting that model with just month half and season city interaction
## has better prediction for some of the cities in the summer
## ERIN: can you think of ways to modify the model above to make it even better?
## I had trouble thinking about how to incorporate month/date into the model.
tab1 < tab2
mean((newpreds - histdata)^2, na.rm = TRUE)
mean((oldpreds - histdata)^2)

## could try a nonparametric model

loess(weatherval ~ Day, data = subset(maxtemplagstrain,
  city == "Buffalo"))
