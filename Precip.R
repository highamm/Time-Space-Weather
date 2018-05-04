complete_df <- read.csv("~/Desktop/DataExpo2018/all_df_completesub.csv")

complete_df <- all.df_completeSub
precip <- subset(complete_df, weathermeas == "ProbPrecip")

# very naive approach to just average the percent precip 

library(dplyr)

precip_avg <- precip %>% group_by(Date, AirPtCd) %>% 
  summarize(mean_precip_prob = mean(forecastValue))

precip_avg$weatherval <- (precip %>% group_by(Date, AirPtCd) %>% 
  summarize(weatherval = mean(weatherval)))$weatherval

locations <- read.csv("~/Desktop/DataExpo2018/locations.csv")
histWeather <- read.csv("~/Desktop/DataExpo2018/histWeather.csv")


# subset KAAO

KAAO <- subset(precip_avg, AirPtCd == "KAAO")
library(ggplot2)
ggplot(KAAO, aes(x = mean_precip_prob, y = weatherval)) + geom_point()

# It would be difficult to visualize something like this for each city because we 
# have too many. May want to think of reasonable ways to group cities together:
#    - geographical location
#    - cities that have similar average rainfall, temp, etc.
#    - split by seasons (still going to be large data sets)

# bad 

ggplot(precip_avg, aes(x = mean_precip_prob, y = weatherval)) + geom_point()


# split into seasons 

library(lubridate)

precip_avg$month <- month(as.POSIXlt(precip_avg$Date))

spring <- subset(precip_avg, month %in% c(3,4,5))
summer <- subset(precip_avg, month %in% c(6,7,8))
fall <- subset(precip_avg, month %in% c(9,10,11))
winter <- subset(precip_avg, month %in% c(12,1,2))

ggplot(spring, aes(x = mean_precip_prob, y = weatherval)) + geom_point()
ggplot(summer, aes(x = mean_precip_prob, y = weatherval)) + geom_point()
ggplot(fall, aes(x = mean_precip_prob, y = weatherval)) + geom_point()
ggplot(winter, aes(x = mean_precip_prob, y = weatherval)) + geom_point()

ggplot(precip_avg, aes(x = mean_precip_prob, y = weatherval)) + geom_point(alpha = 0.01)




# make data frame with averages of precip and POP

spring_avg <- spring %>% group_by(AirPtCd) %>% 
  summarize(mean_precip_inches = mean(weatherval))

spring_avg$mean_POP <- (spring %>% group_by(AirPtCd) %>% 
                          summarize(mean_POP = mean(mean_precip_prob)))$mean_POP

spring_avg <- merge(x=spring_avg, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                    by.y = "AirPtCd", all.x=TRUE)

spring_avg$scaled_mean_precip_inches <- scale(spring_avg$mean_precip_inches)
spring_avg$scaled_mean_POP <- scale(spring_avg$mean_POP)

library(leaflet)
library(maps)

# average inches map for spring
leaflet(spring_avg) %>% addTiles %>% 
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = ~mean_precip_inches*750000, popup = ~city)
leaflet(spring_avg) %>% addTiles %>% 
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = ~mean_POP*5000, popup = ~city, color = "Green")

leaflet(spring_avg) %>% addTiles %>% 
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = ~mean_precip_inches*750000, popup = ~city) %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = ~mean_POP*5000, popup = ~city, color = "Green")



leaflet(spring_avg) %>% addTiles %>% 
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = ~abs(scaled_mean_POP), popup = ~city, color = "Green")




# winter
winter_avg <- winter %>% group_by(AirPtCd) %>% 
  summarize(mean_precip_inches = mean(weatherval))

winter_avg$mean_POP <- (winter %>% group_by(AirPtCd) %>% 
                          summarize(mean_POP = mean(mean_precip_prob)))$mean_POP

winter_avg <- merge(x=winter_avg, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                    by.y = "AirPtCd", all.x=TRUE)

winter_avg$scaled_mean_precip_inches <- scale(winter_avg$mean_precip_inches)
winter_avg$scaled_mean_POP <- scale(winter_avg$mean_POP)

leaflet(winter_avg) %>% addTiles %>% 
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = ~mean_precip_inches*750000, popup = ~city) %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = ~mean_POP*5000, popup = ~city, color = "Green")
