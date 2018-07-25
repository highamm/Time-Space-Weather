library(lubridate)
library(ggplot2)
library(leaflet)
library(dplyr)
library(maps)
library(geosphere)

complete_df <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/all_df_completesub.csv")

complete_df <- all.df_completeSub
locations <- read.csv("~/Desktop/TimeSpaceExpo/locations.csv")

complete_df <- read.csv("~/Desktop/TimeSpaceExpo/all_df_completesub.csv")
names(complete_df)[8] <- "forecastValue"


## OBTAIN THIS FILE FROM DATA READ
maxtempalldat <- maxtempalldat
maxtempalldatsp <- subset(maxtempalldat, season == "Spring")
maxtempalldatsu <- subset(maxtempalldat, season == "Summer")
maxtempalldatfa <- subset(maxtempalldat, season == "Fall")
maxtempalldatwi <- subset(maxtempalldat, season == "Winter")
maxtempalldatsp$season

## FOR LENGTH FORECAST DAY ONLY == 1
spring_max_error_F1 <- maxtempalldatsp[maxtempalldatsp$LengthForecastDayOnly == 1 &
    complete.cases(maxtempalldatsp[ ,c("Error", "weatherval")]), ] %>% 
##    complete.cases(maxtempalldatsp), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
spring_max_error_F1$TrueValGreater <- spring_max_error_F1$mean_error >= 0
spring_max_error_F1 <- base::merge(x = spring_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
  by.y = "AirPtCd", all.x = TRUE)

spring_max_error_F1$AbsError <- abs(spring_max_error_F1$mean_error)

spring_max_error_F1$SquaredErrorAvg <- data.frame(maxtempalldatsp[maxtempalldatsp$LengthForecastDayOnly==1, ] %>%
    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]
spring_max_error_F1$AbsError
spring_max_error_F1$SquaredErrorAvg
spring_max_error_F1$var <- spring_max_error_F1$SquaredErrorAvg - spring_max_error_F1$AbsError^2
spring_max_error_F1$season <- "Spring"

ggplot(data = spring_max_error_F1, aes(x = city, y = SquaredErrorAvg)) +
  geom_point()
ggplot(data = spring_max_error_F1, aes(x = AbsError^2, y = var)) +
  geom_point() +
  geom_text(aes(label = city))


winter_max_error_F1 <- maxtempalldatwi[maxtempalldatwi$LengthForecastDayOnly==1 & 
    complete.cases(maxtempalldatwi[ ,c("Error", "weatherval")]), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
winter_max_error_F1$TrueValGreater <- winter_max_error_F1$mean_error >= 0
winter_max_error_F1 <- base::merge(x=winter_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
  by.y = "AirPtCd", all.x=TRUE)
winter_max_error_F1$AbsError <- abs(winter_max_error_F1$mean_error)
summary(winter_max_error_F1$AbsError)

winter_max_error_F1$SquaredErrorAvg <- data.frame(maxtempalldatwi[maxtempalldatwi$LengthForecastDayOnly==1, ] %>%
    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]

winter_max_error_F1$AbsError
winter_max_error_F1$SquaredErrorAvg
winter_max_error_F1$var <- winter_max_error_F1$SquaredErrorAvg - winter_max_error_F1$AbsError^2

winter_max_error_F1$season <- "Winter"

ggplot(data = winter_max_error_F1, aes(x = city, y = SquaredErrorAvg)) +
  geom_point()
ggplot(data = subset(winter_max_error_F1, var < 1000000),
  aes(x = AbsError^2, y = var)) +
  geom_point() +
  geom_text(aes(label = AirPtCd))


summer_max_error_F1 <- maxtempalldatsu[maxtempalldatsu$LengthForecastDayOnly==1, ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
summer_max_error_F1$TrueValGreater <- summer_max_error_F1$mean_error >= 0
summer_max_error_F1 <- merge(x=summer_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
  by.y = "AirPtCd", all.x=TRUE)
summer_max_error_F1$AbsError <- abs(summer_max_error_F1$mean_error)
summer_max_error_F1$SquaredErrorAvg <- data.frame(maxtempalldatsu[maxtempalldatsu$LengthForecastDayOnly==1, ] %>%
    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]

qplot(summer_max_error_F1$AbsError) 
summer_max_error_F1$SquaredErrorAvg
summer_max_error_F1$var <- summer_max_error_F1$SquaredErrorAvg - summer_max_error_F1$AbsError^2

summer_max_error_F1$season <- "Summer"

ggplot(data = summer_max_error_F1, aes(x = city, y = SquaredErrorAvg)) +
  geom_point()
ggplot(data = summer_max_error_F1,
  aes(x = AbsError^2, y = var)) +
  geom_point() +
  geom_text(aes(label = city))


fall_max_error_F1 <- maxtempalldatfa[maxtempalldatfa$LengthForecastDayOnly==1, ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
fall_max_error_F1$TrueValGreater <- fall_max_error_F1$mean_error >= 0
fall_max_error_F1 <- merge(x=fall_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
  by.y = "AirPtCd", all.x=TRUE)
fall_max_error_F1$AbsError <- abs(fall_max_error_F1$mean_error)
fall_max_error_F1$SquaredErrorAvg <- data.frame(maxtempalldatfa[maxtempalldatfa$LengthForecastDayOnly==1, ] %>%
    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]

fall_max_error_F1$AbsError
fall_max_error_F1$SquaredErrorAvg
fall_max_error_F1$var <- fall_max_error_F1$SquaredErrorAvg - fall_max_error_F1$AbsError^2

fall_max_error_F1$season <- "Fall"

ggplot(data = fall_max_error_F1, aes(x = city, y = SquaredErrorAvg)) +
  geom_point()
ggplot(data = fall_max_error_F1,
  aes(x = AbsError^2, y = var)) +
  geom_point() +
  geom_text(aes(label = city))
qplot(subset(maxtempalldatfa, city == "Salmon")$Error)

spring_min_error_F1 <- spring_min[spring_min$LengthForecastDayOnly==1 &
    complete.cases(spring_min), ] %>%
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
spring_min_error_F1$TrueValGreater <- spring_min_error_F1$mean_error >= 0  
spring_min_error_F1 <- merge(x=spring_min_error_F1, y=locations[,c("longitude", "latitude", "AirPtCd", "city")], by.x = "AirPtCd", 
  by.y = "AirPtCd", all.x=TRUE)
spring_min_error_F1$AbsError <- abs(spring_min_error_F1$mean_error)
spring_min_error_F1$SquaredErrorAvg <- data.frame(spring_min[spring_min$LengthForecastDayOnly==1 &
    complete.cases(spring_min), ] %>%
    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]

ncol(summer_max_error_F1)
spring_max_error_F1
df <- rbind(spring_max_error_F1, summer_max_error_F1, fall_max_error_F1, winter_max_error_F1)
dfint <- subset(df, city %in% c("Austin", "San Francisco", 
  "Key West", "Miami", "Santa Fe", "Charleston", "Helena",
  "Salmon", "Richfield", "Atlantic City"))

## get rid of one of the Charlestons

dfint <- subset(dfint, AirPtCd != "KCRW")
nrow(dfint)
dfint



##need to update the colour scheme on this graph
ggplot(data = dfint, aes(x = SquaredErrorAvg, y = city, colour = "Error Source")) +
  geom_point(size = 3.4, colour = "chartreuse4") + 
  facet_wrap(~ season, nrow = 1) +
  geom_point(aes(x = AbsError^2, y = city), size = 3.4, colour = "black") +
  geom_segment(aes(x = 0, y = city, xend = AbsError^2, yend = city,
    colour = "Bias Squared"),
    size = 2) +
  geom_segment(aes(x = AbsError^2, y = city, xend = SquaredErrorAvg, yend = city,
    colour = "Variance"),
    size = 2) +
  geom_point(aes(x = 0, y = city), colour = "darkorchid4", size = 3.4) +
  scale_colour_manual("Error Source", values = c("Bias Squared" = "darkorchid4", "Variance" = "chartreuse4")) + 
  theme_grey(base_size = 19) +
  xlab("Mean Square Prediction Error") +
  ylab("City") +
  ggsave("BiasVarGraph.png", width = 14, height = 7)

## begin to look at cities that have strange patterns
dfint

## salmon idaho is just east of the salmon river mountain range....possibly creating
## high temperature error variation because of the difficulty of predicting weather patterns directly east of a mountain range.

## Key West: temperatures very consistent: from weather..com, the record high is 
## between 10 to 15 degrees higher for each month than the average high.
## 
## But then, shouldn't Miami have a low MSPE as well? But, according to weather.com
## the airport is typically a couple of degrees warmer than the city, on average,
## creating bias in our data set (SO IT'S NOT ALWAYS DISTANCE THAT IS INFORMATIVE:
## CONNECTION TO DISTANCE PLOT). Like Key West, Miami has low variance,
## perhaps because of the low variation in temperatures anyway
## 
## San Francisco: similar to Miami (airport, on average, warmer) by a couple of degrees, more so in the summer
## Compare to Columbus, OH (airport temp and city temp, on average,
## are quite similar, differing by about 1 degree)
## 
## Austin highlighted in the distance plot (CONNECTION TO THAT PLOT)
## 
## Compare to Helena, Montana, where the record high is about 30 degrees higher than the average high in most months....much harder to predict! Higher variance and MSPE.

 ## ERIN CHECK OUT THIS PLOT: it doesn't have labels because i did not
 ## want to add them until you have a look and decide if this is satisfactory.
 ## Our blurb would comment on some cities that have a variance with low bias 
 ## (Salmon), cities with a mix of both depending on the season (Austin) and cities
 ## with high bias but low variance (Miami)