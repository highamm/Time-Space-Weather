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

mintempall <- subset(complete_df, weathermeas == "MinTemp")
maxtempall <- subset(complete_df, weathermeas == "MaxTemp")

##maxtempall <- read.csv("~/Desktop/TimeSpaceExpo/maxtempall.csv")
##mintempall <- read.csv("~/Desktop/TimeSpaceExpo/mintempall.csv")

## obtain from ExplroingMaxTempErrorMods file
maxtempall <- rbind(springclean, summerclean, fallclean, winterclean)


maxTemp <- subset(maxtempall, weathermeas == "MaxTemp")
minTemp <- subset(mintempall, weathermeas == "MinTemp")

maxTemp$Date <- as.Date(maxTemp$Date)
minTemp$Date <- as.Date(minTemp$Date)

maxTemp$Error <- maxTemp$weatherval - maxTemp$forecastValue
minTemp$Error <- minTemp$weatherval - minTemp$forecastValue

maxTemp$season <- cut(maxTemp$month, 
  breaks = c(0.5, 2.5, 5.5, 8.5, 11.5, 12.5), 
  labels = c("Winter", "Spring", "Summer", "Fall", "Winter2"), 
  right = FALSE)
minTemp$season <- cut(minTemp$month, 
  breaks = c(0.5, 2.5, 5.5, 8.5, 11.5, 12.5), 
  labels = c("Winter", "Spring", "Summer", "Fall", "Winter2"), 
  right = FALSE)

nrow(unique(maxTemp[c("Date", "AirPtCd", "DateofForecast")]))

# split into seasons
maxTemp$month <- month(as.POSIXlt(maxTemp$Date))
minTemp$month <- month(as.POSIXlt(minTemp$Date))

summary(maxTemp$forecastDiff)
with(maxTemp, qplot(weatherval, forecastValue))

spring_max <- subset(maxTemp, season == "Spring")
summer_max <- subset(maxTemp, season == "Summer")
fall_max <- subset(maxTemp, season == "Fall")
winter_max <- subset(maxTemp, season == "Winter")

spring_min <- subset(minTemp, season == "Spring")
summer_min <- subset(minTemp, season == "Summer")
fall_min <- subset(minTemp, season == "Fall")
winter_min <- subset(minTemp, season == "Winter")

spring_max$SquaredError <- spring_max$Error^2
summer_max$SquaredError <- summer_max$Error^2
fall_max$SquaredError <- fall_max$Error^2
winter_max$SquaredError <- winter_max$Error^2


spring_min$SquaredError <- spring_min$Error^2
summer_min$SquaredError <- summer_min$Error^2
fall_min$SquaredError <- fall_min$Error^2
winter_min$SquaredError <- winter_min$Error^2


spring_max_error_F1 <- spring_max[spring_max$LengthForecastDayOnly==1 & 
    complete.cases(spring_max), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
spring_max_error_F1$TrueValGreater <- spring_max_error_F1$mean_error >= 0
spring_max_error_F1 <- merge(x=spring_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
  by.y = "AirPtCd", all.x=TRUE)
spring_max_error_F1$AbsError <- abs(spring_max_error_F1$mean_error)
spring_max_error_F1$SquaredErrorAvg <- data.frame(spring_max[spring_max$LengthForecastDayOnly==1 &
    complete.cases(spring_max), ] %>%
    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]
spring_max_error_F1$AbsError
spring_max_error_F1$SquaredErrorAvg
spring_max_error_F1$var <- spring_max_error_F1$SquaredErrorAvg - spring_max_error_F1$AbsError^2

ggplot(data = spring_max_error_F1, aes(x = city, y = SquaredErrorAvg)) +
  geom_point()
ggplot(data = spring_max_error_F1, aes(x = AbsError^2, y = var)) +
  geom_point() +
  geom_text(aes(label = city))


winter_max_error_F1 <- winter_max[winter_max$LengthForecastDayOnly==1 & 
    complete.cases(winter_max), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
winter_max_error_F1$TrueValGreater <- winter_max_error_F1$mean_error >= 0
winter_max_error_F1 <- merge(x=winter_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
  by.y = "AirPtCd", all.x=TRUE)
winter_max_error_F1$AbsError <- abs(winter_max_error_F1$mean_error)
winter_max_error_F1$SquaredErrorAvg <- data.frame(winter_max[winter_max$LengthForecastDayOnly==1 &
    complete.cases(winter_max), ] %>%
    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]

winter_max_error_F1$AbsError
winter_max_error_F1$SquaredErrorAvg
winter_max_error_F1$var <- winter_max_error_F1$SquaredErrorAvg - winter_max_error_F1$AbsError^2
winter_max_error_F1 <- subset(winter_max_error_F1, city =! Cheyenne)
ggplot(data = winter_max_error_F1, aes(x = city, y = SquaredErrorAvg)) +
  geom_point()
ggplot(data = subset(winter_max_error_F1, var < 1000000),
  aes(x = AbsError^2, y = var)) +
  geom_point() +
  geom_text(aes(label = city))


summer_max_error_F1 <- summer_max[summer_max$LengthForecastDayOnly==1 & 
    complete.cases(summer_max), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
summer_max_error_F1$TrueValGreater <- summer_max_error_F1$mean_error >= 0
summer_max_error_F1 <- merge(x=summer_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
  by.y = "AirPtCd", all.x=TRUE)
summer_max_error_F1$AbsError <- abs(summer_max_error_F1$mean_error)
summer_max_error_F1$SquaredErrorAvg <- data.frame(summer_max[summer_max$LengthForecastDayOnly==1 &
    complete.cases(summer_max), ] %>%
    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]

summer_max_error_F1$AbsError
summer_max_error_F1$SquaredErrorAvg
summer_max_error_F1$var <- summer_max_error_F1$SquaredErrorAvg - summer_max_error_F1$AbsError^2
summer_max_error_F1 <- subset(summer_max_error_F1, city =! Cheyenne)
ggplot(data = summer_max_error_F1, aes(x = city, y = SquaredErrorAvg)) +
  geom_point()
ggplot(data = subset(summer_max_error_F1, var < 1000000),
  aes(x = AbsError^2, y = var)) +
  geom_point() +
  geom_text(aes(label = city))

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