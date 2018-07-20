library(lubridate)
library(ggplot2)
library(leaflet)
library(dplyr)
library(maps)
library(geosphere)

## Matt only
complete_df <- all.df_completeSub

locations <- read.csv("~/Desktop/DataExpo2018/locations.csv")

complete_df <- read.csv("~/Desktop/DataExpo2018/all_df_completesub.csv")
names(complete_df)[8] <- "forecastValue"

mintempall <- subset(complete_df, weathermeas == "MinTemp")
maxtempall <- subset(complete_df, weathermeas == "MaxTemp")

# File paths for Erin
maxtempall <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/maxtempall.csv")
mintempall <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/mintempall.csv")


# subset the data into sets of only max and min temps 
maxTemp <- subset(maxtempall, weathermeas == "MaxTemp")
minTemp <- subset(mintempall, weathermeas == "MinTemp")

maxTemp$Date <- as.Date(maxTemp$Date)
minTemp$Date <- as.Date(minTemp$Date)

maxTemp$Error <- maxTemp$weatherval - maxTemp$forecastValue
minTemp$Error <- minTemp$weatherval - minTemp$forecastValue

nrow(unique(maxTemp[c("Date", "AirPtCd", "DateofForecast")]))

# split into seasons
maxTemp$month <- month(as.POSIXlt(maxTemp$Date))
minTemp$month <- month(as.POSIXlt(minTemp$Date))

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

# spring_max_avg_F1 <- (spring_max[spring_max$LengthForecastDayOnly==1 & 
#                                    complete.cases(spring_max), ] %>% 
#   group_by(AirPtCd) %>% summarize(mean_max = mean(weatherval)))
# spring_max_avg_F1$forecast <- (spring_max[spring_max$LengthForecastDayOnly==1 & 
#                                             complete.cases(spring_max), ] %>% 
#   group_by(AirPtCd) %>% summarize(mean_forecast = mean(forecastValue)))$mean_forecast
# 
# 
# spring_max_avg_F1 <- merge(x=spring_max_avg_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
#                     by.y = "AirPtCd", all.x=TRUE)


# compute the average of the errors (error = hist temp - forecast temp)
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


winter_min_error_F1 <- winter_min[winter_min$LengthForecastDayOnly==1 & 
                                    complete.cases(winter_min), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
winter_min_error_F1$TrueValGreater <- winter_min_error_F1$mean_error >= 0
winter_min_error_F1 <- merge(x=winter_min_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                             by.y = "AirPtCd", all.x=TRUE)
winter_min_error_F1$AbsError <- abs(winter_min_error_F1$mean_error)
winter_min_error_F1$SquaredErrorAvg <- data.frame(winter_min[winter_min$LengthForecastDayOnly==1 &
                                                               complete.cases(winter_min), ] %>%
                                                    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]


####
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

summer_min_error_F1 <- summer_min[summer_min$LengthForecastDayOnly==1 & 
                                    complete.cases(summer_min), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
summer_min_error_F1$TrueValGreater <- summer_min_error_F1$mean_error >= 0
summer_min_error_F1 <- merge(x=summer_min_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                             by.y = "AirPtCd", all.x=TRUE)
summer_min_error_F1$AbsError <- abs(summer_min_error_F1$mean_error)
summer_min_error_F1$SquaredErrorAvg <- data.frame(summer_min[summer_min$LengthForecastDayOnly==1 &
                                                               complete.cases(summer_min), ] %>%
                                                    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]

####
####
fall_max_error_F1 <- fall_max[fall_max$LengthForecastDayOnly==1 & 
                                    complete.cases(fall_max), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
fall_max_error_F1$TrueValGreater <- fall_max_error_F1$mean_error >= 0
fall_max_error_F1 <- merge(x=fall_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                             by.y = "AirPtCd", all.x=TRUE)
fall_max_error_F1$AbsError <- abs(fall_max_error_F1$mean_error)
fall_max_error_F1$SquaredErrorAvg <- data.frame(fall_max[fall_max$LengthForecastDayOnly==1 &
                                                               complete.cases(fall_max), ] %>%
                                                    group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]


fall_min_error_F1 <- fall_min[fall_min$LengthForecastDayOnly==1 & 
                                complete.cases(fall_min), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
fall_min_error_F1$TrueValGreater <- fall_min_error_F1$mean_error >= 0
fall_min_error_F1 <- merge(x=fall_min_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                           by.y = "AirPtCd", all.x=TRUE)
fall_min_error_F1$AbsError <- abs(fall_min_error_F1$mean_error)
fall_min_error_F1$SquaredErrorAvg <- data.frame(fall_min[fall_min$LengthForecastDayOnly==1 &
                                                           complete.cases(fall_min), ] %>%
                                                  group_by(AirPtCd) %>% summarize(SquaredErrorAvg = mean(SquaredError)))[,2]




# Spring Max Temps Map 1 day forecast
# leaflet(spring_max_avg_F1) %>% addTiles() %>% 
#   addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(forecast^2)*15, 
#              popup=~city) %>%
#   addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(mean_max^2)*15, 
#              popup=~city, col="Green")

pal <- colorFactor(c("navy", "red"), domain = c(TRUE,FALSE))

#### Spring Max

# This map has the sizes of the circles based on the average error for each city in spring
leaflet(spring_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError)*15000, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Average max temp forecast error: Spring")

# This map has the sizes of the circles based on the average error squared for each city in spring
leaflet(spring_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average max temp forecast error: Spring")

# This map has the sizes of the circles based on the average squared error for each city in spring
# (errors were squared first and then averaged, so determining an under or over estimate from
# this map is not possible)
leaflet(spring_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(SquaredErrorAvg)*2000, 
             popup=~city)



# Spring Min

leaflet(spring_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError)*10000, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Average min temp forecast error: Spring")


leaflet(spring_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average min temp forecast error: Spring")

# Summer Max
leaflet(summer_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError)*10000, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Average max temp forecast error: Summer")


leaflet(summer_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Summer")

# Summer Min
leaflet(summer_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError)*10000, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Average min temp forecast error: Summer")

leaflet(summer_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Summer")

# Fall Max
leaflet(fall_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError)*10000, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Average max temp forecast error: Fall")

leaflet(fall_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Fall")

# Fall Min
leaflet(fall_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Fall")

# Winter Max
leaflet(winter_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError)*10000, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Average max temp forecast error: Winter")

leaflet(winter_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Winter")

# Winter Min
leaflet(winter_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError)*10000, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Average min temp forecast error: Winter")

leaflet(winter_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Winter")



# Austin 

Austin_fall_max <- fall_max[fall_max$city == "Austin", ] 

summary(Austin_fall_max$weatherval)
summary(Austin_fall_max$forecastValue)

qplot(Austin_fall_max$weatherval, Austin_fall_max$forecastValue)

Austin_spring_min <- spring_min[spring_min$city == "Austin", ] 
qplot(Austin_spring_min$weatherval, Austin_spring_min$forecastValue)





### Distance qplots (doing this in this file because all of the summarized city datasets are 
### written in here)

## Erin
updlocations <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/updlocations.csv")

## Matt
updlocations <- read.csv("~/Desktop/TimeSpaceExpo/updlocations.csv")

updlocations_withDist <- updlocations

# spherical distances - distGeo function calculates absolute distance in meters 
updlocations_withDist$dists <- distGeo(cbind(updlocations_withDist$longitude, updlocations_withDist$latitude), 
                 cbind(updlocations_withDist$citylons, updlocations_withDist$citylats))

# latitude distances - positive and negative values possible 
updlocations_withDist$latDists <- updlocations_withDist$latitude - updlocations_withDist$citylats


# merge all the temp summary data sets with the distance data sets
# there was totally a quicker way to do this...

subset(updlocations_withDist, city == "Pittsburgh")
subset(locations, city == "Eugene")
## the airport latitudes and longitudes in the data set are not actually
## accurate. This is most obvious for Austin, Nevade, but can also check out
## st. louis:

## google st. louis airport lat/lon to get 38.7503° N, 90.3755° W
subset(locations, city == "St Louis")$latitude ## 38.5833, -90.2
## find distance and convert to miles: 15 miles apart!
(distGeo(c(-90.2, 38.5833), c(-90.3755, 38.7503)) / 1000) * 0.62



spring_max_error_F1 <- merge(x=spring_max_error_F1, y=updlocations_withDist[,c("AirPtCd", "dists", "latDists")], 
              by.x = "AirPtCd", by.y="AirPtCd", all.x = TRUE)
##head(spring_max_error_F1)
##subset(updlocations_withDist, city == "Sault Ste Marie")

spring_min_error_F1 <- merge(x=spring_min_error_F1, y=updlocations_withDist[,c("AirPtCd", "dists", "latDists")], 
                             by.x = "AirPtCd", by.y="AirPtCd", all.x = TRUE)
summer_max_error_F1 <- merge(x=summer_max_error_F1, y=updlocations_withDist[,c("AirPtCd", "dists", "latDists")], 
                             by.x = "AirPtCd", by.y="AirPtCd", all.x = TRUE)
summer_min_error_F1 <- merge(x=summer_min_error_F1, y=updlocations_withDist[,c("AirPtCd", "dists", "latDists")], 
                             by.x = "AirPtCd", by.y="AirPtCd", all.x = TRUE)
fall_max_error_F1 <- merge(x=fall_max_error_F1, y=updlocations_withDist[,c("AirPtCd", "dists", "latDists")], 
                             by.x = "AirPtCd", by.y="AirPtCd", all.x = TRUE)
fall_min_error_F1 <- merge(x=fall_min_error_F1, y=updlocations_withDist[,c("AirPtCd", "dists", "latDists")], 
                           by.x = "AirPtCd", by.y="AirPtCd", all.x = TRUE)
winter_max_error_F1 <- merge(x=winter_max_error_F1, y=updlocations_withDist[,c("AirPtCd", "dists", "latDists")], 
                           by.x = "AirPtCd", by.y="AirPtCd", all.x = TRUE)
winter_min_error_F1 <- merge(x=winter_min_error_F1, y=updlocations_withDist[,c("AirPtCd", "dists", "latDists")], 
                             by.x = "AirPtCd", by.y="AirPtCd", all.x = TRUE)

spring_max_error_F1$season <- rep("spring", 113)
spring_min_error_F1$season <- rep("spring", 113)
summer_max_error_F1$season <- rep("summer", 113)
summer_min_error_F1$season <- rep("summer", 113)
fall_max_error_F1$season <- rep("fall", 113)
fall_min_error_F1$season <- rep("fall", 113)
winter_max_error_F1$season <- rep("winter", 113)
winter_min_error_F1$season <- rep("winter", 113)

spring_max_error_F1$measure <- rep("max", 113)
summer_max_error_F1$measure <- rep("max", 113)
fall_max_error_F1$measure <- rep("max", 113)
winter_max_error_F1$measure <- rep("max", 113)

spring_min_error_F1$measure <- rep("min", 113)
summer_min_error_F1$measure <- rep("min", 113)
fall_min_error_F1$measure <- rep("min", 113)
winter_min_error_F1$measure <- rep("min", 113)


# look at spring max and min temp errors with distance between airport and city

# mean error and spherical distance 
with(spring_max_error_F1, qplot(dists, mean_error))

# average error and latitudinal distance
with(spring_max_error_F1, qplot(latDists, mean_error))

allSeasons_F1 <- rbind(spring_max_error_F1, spring_min_error_F1, 
                    summer_max_error_F1, summer_min_error_F1, 
                    fall_max_error_F1, fall_min_error_F1, 
                    winter_max_error_F1, winter_min_error_F1)

# Matt, write your own copy of this file, it is not very large, but I didn't push it just to be
# safe 
write.csv(allSeasons_F1, "~/Desktop/DataExpo2018/Data Expo 2018/allSeasons_F1.csv")

## Matt only
library(readr)
allSeasons_F1 <- read_csv("~/Desktop/TimeSpaceExpo/allSeasons_F1.csv")

doubleupdlocations <- read_csv("~/Desktop/TimeSpaceExpo/doubleupdlocations.csv")
doubleupdlocations$distance <- distGeo(cbind(doubleupdlocations$newairlon, doubleupdlocations$newairlat), 
  cbind(doubleupdlocations$citylons, doubleupdlocations$citylats))
View(doubleupdlocations)
head(doubleupdlocations); head(allSeasons_F1)
doubleupdlocations[1, 3:12]

## these outliers are accurate
testdf <- merge(allSeasons_F1, doubleupdlocations, by.x = "city", by.y = "city")
head(testdf)
cbind(testdf$dists, testdf$distance)
testdf$londif <- abs(testdf$citylons - testdf$newairlon)
testdf$latdif <- abs(testdf$citylats - testdf$newairlat)

ggplot(data = subset(testdf, distance < 50000), aes(x = latdif, y = abs(mean_error), colour = factor(measure))) +
  geom_point() +
  facet_grid(.~season) + 
  facet_wrap(~season, ncol=2) +
  geom_smooth()


# The bias in max temps being underestimated and min temps being overestimated is also evident
# in the following plots 

# caution: x-axis scales are very different in the following two plots
# the first is absolute distance in meters
# the second is simply the difference in latitudes of airport and city center

ggplot(allSeasons_F1, aes(x=dists, y=mean_error, color=factor(measure))) + 
  geom_point() + 
  facet_grid(.~season) + 
  facet_wrap(~season, ncol=2) + 
  xlab("Distance (m)") + 
  ylab("Mean Forecast Error") + 
  scale_color_discrete(name="Temperature\nMeasure") + 
  ggtitle("Distance between City Center and Airport vs. \nMean Forecast Error")

ggplot(allSeasons_F1, aes(x=latDists, y=mean_error, color=factor(measure))) + 
  geom_point() + 
  facet_grid(.~season) + 
  facet_wrap(~season, ncol=2) +  
  xlab("Latitudinal Distance") + 
  ylab("Mean Forecast Error") + 
  scale_color_discrete(name="Temperature\nMeasure") + 
  ggtitle("Latitudinal distance between City Center and Airport \nvs. Mean Forecast Error")

summary(subset(allSeasons_F1, city == "Austin"))


ggplot(allSeasons_F1, aes(x=dists, y=mean_error, color=factor(measure))) + 
  geom_point() + geom_point(data = subset(allSeasons_F1, city == "Pittsburgh"), colour = "black")
  facet_grid(.~season) + 
  facet_wrap(~season, ncol=2) + 
  xlab("Distance (m)") + 
  ylab("Mean Forecast Error") + 
  scale_color_discrete(name="Temperature\nMeasure") + 
  ggtitle("Distance between City Center and Airport vs. \nMean Forecast Error")


  
  
  
  
##### GGMAPS #######

usa_center <- as.numeric(geocode("United States"))
alaska_center <- as.numeric(geocode("Alaska"))
hawaii_center <- as.numeric(geocode("Hawaii"))
USAMap <- ggmap(get_googlemap(center = usa_center-1, scale = 2, zoom = 4, extent = "normal"))  
USAMap  
AlaskaMap <- ggmap(get_googlemap(center = alaska_center, scale=2, zoom = 4, extent = "normal"))
AlaskaMap  
HawaiiMap <- ggmap(get_googlemap(center = hawaii_center, scale=2, zoom = 7, extent = "normal"))
HawaiiMap


# Spring 

USAMap + geom_point(data = spring_max_error_F1, aes(x = longitude, y = latitude), 
                    size = spring_max_error_F1$AbsError)
                                                    




# may be too complicated to include a shiny app in the poster?


###### START SHINY #############
##### Try to animate maps #######

# maxTemp$season <- cut(maxTemp$month, 
#   breaks = c(0.5, 2.5, 5.5, 8.5, 11.5, 12.5), 
#   labels = c("Winter", "Spring", "Summer", "Fall", "Winter2"), 
#   right = FALSE)
# maxtemp$season[maxtemp$season == "Winter2"] <- "Winter"
# maxtemp$season <- factor(maxtemp$season)

maxTempSummary <- maxTemp %>% group_by(city, season, LengthForecastDayOnly) %>%
  summarize(avgError = mean(Error), longitude = mean(longitude), latitude = mean(latitude))
maxTempSummary$TrueValGreater <- maxTempSummary$avgError >= 0
maxTempSummarysub <- subset(maxTempSummary, city == "Albany")

summary(maxTempSummary$LengthForecastDayOnly)

leaflet(data = maxTempSummary) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat =~latitude, weight=1, radius=~(abs(avgError)^2)*7500)

library(shiny)


ui <- navbarPage("Data Expo",
  tabPanel("Leaflet Map",
  
  fluidPage(titlePanel("Title of Leaflet Map"),
  sidebarLayout(
    sidebarPanel(helpText("Words that we want here"),
    sliderInput("time", label = p("Forecast Length"),
      min = min(maxTempSummary$LengthForecastDayOnly), 
      max = max(maxTempSummary$LengthForecastDayOnly), 
      value = min(maxTempSummary$LengthForecastDayOnly),
      step = 1, animate = T) 
  ),
    mainPanel(
      plotOutput("MaxTempMap"),
      leafletOutput("leafletmap")
      
    )

)
)),
  tabPanel("Possible Second Graph",
    fluidPage(
      titlePanel("Second Title")
    )))

server <- function(input, output, session){
  points <- reactive({
    maxTempSummary[maxTempSummary$season == "Winter", ] %>% 
      dplyr::filter(LengthForecastDayOnly == input$time)
  })

  ##output$maxTempMap <- renderLeaflet({
  ##  leaflet() %>% addTiles() %>% 
  ##    addCircles(lng=~longitude, lat =~latitude, weight=1, radius=~(abs(avgError)^2)*7500)
 ## })
 
  
  output$MaxTempMap <- renderPlot({
   test.df <- points()
   ggplot(data = test.df, aes(x = longitude, y = latitude)) +
     geom_point(aes(colour = avgError))
 })
 
output$leafletmap <- renderLeaflet({
  test.df <- points()
    leaflet(data = test.df) %>% addTiles() %>% 
      addCircles(lng=~longitude, lat =~latitude, weight = 1, 
                 radius=~(abs(avgError)^2)*7500, color=~pal(TrueValGreater))
   })
}


shinyApp(ui,server) 

## other notes: use something like this to switch between columns of data:
##   datasetInput <- reactive({
##switch(input$ts,
##  "Johnson & Johnson quarterly" = jj,
##  "CO2" = co2)
##})
##
##  # reactive graph title
##graphtitle <- reactive({
##  switch(input$ts,
##    "Johnson & Johnson quarterly" = "Johnson & Johnson Quarterly Earnings",
##    "CO2" = "Monthly CO2 concentrations")
##})

# reactive axis label
##axislabel <- reactive({
##  switch(input$ts,
##    "Johnson & Johnson quarterly" = "earnings",
##    "CO2" = "ppm")
##})








