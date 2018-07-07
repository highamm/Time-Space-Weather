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

str(maxtemplags)
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
