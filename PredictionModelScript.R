## self-contained file for the predictive function


all.df_completeSub <- read.csv("all_df_completesub.csv")
colnames(all.df_completeSub)[8] <- "forecastValue"
histWeather <- read.csv("histWeather.csv")

library(lubridate)
library(dplyr)
library(ggplot2)
library(lme4)

complete_df <- all.df_completeSub

# include latitude distance in the data set so that it can be included in the model 
complete_df_dists <- merge(x = complete_df, y = dist_locations[ ,c("AirPtCd", "latDist")], 
                           by.x = "AirPtCd", by.y = "AirPtCd", all.x=TRUE)

complete_df <- complete_df_dists

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
    adjmeanvis = lag(Mean_VisibilityMiles),
    adjmaxtemp = lag(Max_TemperatureF))



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
summary(modrand)


# From Erin: I have also included the previous day's max temp. If you plug in humidity and wind in order
# to get a better prediction of tomorrow's max temp, I think it also makes sense to include 
# the max temp from the day before 

modrand2 <- lmer(weatherval ~ forecastValue + adjmeanhum + 
                   adjmeanwind + adjmaxtemp + season  + (season|city), 
                 data = maxtemplagstrain)
ranef(modrand2)
summary(modrand2)


modrand3 <- lmer(weatherval ~ adjmaxtemp + (season|city), 
                 data = maxtemplagstrain)
summary(modrand3)

# model includes latitudinal distance as a fixed covariate - don't think it should be included 
# in the prediction model though, doesn't look significant from t-val or from plot 
modrand4 <- lmer(weatherval ~ forecastValue + adjmeanhum + 
                   adjmeanwind + adjmaxtemp + latDist + season + (season|city), 
                 data = maxtemplagstrain)
summary(modrand4)




# this model only includes the previous day's max temp, not sure it really makes sense to compare
# this model to the mixed effects model though? 
modmaxtemp <- lm(weatherval ~ adjmaxtemp, data = maxtemplagstrain)
summary(modmaxtemp)
qplot(maxtemplagstrain$adjmaxtemp, maxtemplagstrain$weatherval)


# Here I am comparing the mixed effects model and the fixed model that only uses the previous
# day's max temp using LRT. Like I mentioned above, this doesn't seem like something that should generally
# be done, but I just kept the code in here anyways 
dev1 <- -2*logLik(modrand2)
dev0 <- -2*logLik(modmaxtemp)
devdiff <- as.numeric(dev0-dev1)
dfdiff <- attr(dev1,"df")-attr(dev0,"df")
cat('Chi-square =', devdiff, '(df=', dfdiff,'), p =', 
    pchisq(devdiff,dfdiff,lower.tail=FALSE))

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


pred.fun2 <- function(forecastValue, adjmeanhum, adjmeanwind, adjmaxtemp, season,
                     city) {
  todayinfo3 <- data.frame("forecastValue" = forecastValue,
                           "season" = season,
                           "city" = city,
                           "adjmeanhum" = adjmeanhum,
                           "adjmeanwind" = adjmeanwind, 
                           "adjmaxtemp" = adjmaxtemp)
  predict(modrand2, todayinfo3,
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

with(maxtemplagstest, mean((newpreds - histdata)^2, na.rm = TRUE))
mean((maxtemplagstest$oldpreds - maxtemplagstest$histdata)^2)






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

# I don't think we want both season and month included in the model since they are going to be
# confounded, unless you nested month in city (e.g. there are 3 months in each season so within
# the seasons, months went from 1-3)

tab1 < tab2
mean((newpreds - histdata)^2, na.rm = TRUE)
mean((oldpreds - histdata)^2)

## could try a nonparametric model

loess(weatherval ~ Day, data = subset(maxtemplagstrain,
  city == "Buffalo"))
