complete_df <- read.csv("~/Desktop/DataExpo2018/all_df_completesub.csv")

names(complete_df)[8] <- "forecastValue"
## MATT ONLY
complete_df <- all.df_completeSub


precip <- subset(complete_df, weathermeas == "ProbPrecip")
head(precip)
precip$Date <- as.Date(precip$Date)
precip$DateofForecast <- as.Date(precip$DateofForecast)

library(lubridate)

# very naive approach to just average the percent precip 

library(dplyr)

precip_avg <- precip %>% group_by(Date, AirPtCd) %>% 
  summarize(mean_precip_prob = mean(forecastValue))

precip_avg$weatherval <- (precip %>% group_by(Date, AirPtCd) %>% 
  summarize(weatherval = mean(weatherval)))$weatherval

## ERIN ONLY
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



precip_avg$month <- month(as.POSIXlt(precip_avg$Date))

spring <- subset(precip_avg, month %in% c(3, 4, 5))
summer <- subset(precip_avg, month %in% c(6, 7, 8))
fall <- subset(precip_avg, month %in% c(9, 10, 11))
winter <- subset(precip_avg, month %in% c(12, 1, 2))

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

## getting POP and actual precipitation on a similar scale
spring_avg$scaled_mean_precip_inches <- as.vector(scale(spring_avg$mean_precip_inches))
spring_avg$scaled_mean_POP <- as.vector(scale(spring_avg$mean_POP))
str(spring_avg$scaled_mean_POP)
str(spring_avg$mean_POP)
library(leaflet)
library(maps)

# average inches map for spring

## why are we multiplying by 750,000 and 5,000 for the radius? Is this
## to scale them appropriately?
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


# weird 
leaflet(spring_avg) %>% addTiles %>% 
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, 
             radius = ~scaled_mean_POP*50000, popup = ~city, color = "Green")
?addCircles



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

ggplot(data = spring, aes(x = mean_precip_prob, y = weatherval)) + geom_point() +
  facet_wrap( ~ AirPtCd)







####### Summarize at higher resolution
####### Hoquiam and Spokane seem interesting to look at 
precip$month <- month(as.POSIXlt(precip$Date))
spring_df <- subset(precip, month %in% c(3,4,5))

spring_WA <- subset(spring_df, city %in% c("Hoquiam", "Spokane"))

# subset only forecasts of length 1 (day before)
spring_WA_f1 <- subset(spring_WA, LengthForecastDayOnly== 1)

# only 2016 data for WA spring
spring_WA_f1_16 <- subset(spring_WA_f1, year(as.POSIXlt(spring_WA_f1$Date)) == 2016)

spring_WA_f1_16$ForecastTimeDay <- rep(c("Morning", "Evening"), length.out=nrow(spring_WA_f1_16)) 


library(ggplot2)

# in this plot, each day has two points, one associated with morning prediction, one with evening
ggplot(spring_WA_f1_16, aes(x=forecastValue, y=weatherval, colour=ForecastTimeDay)) + geom_point() + facet_grid(.~ city) +
  xlab("Probability of Precipitation") + ylab("Precipitation (in)") + 
  ggtitle("Spring 2016, 1 Day Forecast")




ggplot(spring_WA_f1_16, aes(x=forecastValue, y=weatherval, colour=ForecastTimeDay)) + 
  geom_violin(aes(x=factor(forecastValue))) + facet_grid(.~ city) +
  xlab("Probability of Precipitation") + ylab("Precipitation (in)") + 
  ggtitle("Spring 2016, 1 Day Forecast")


ggplot(spring_WA_f1_16, aes(x=forecastValue, y=weatherval, colour=ForecastTimeDay)) + 
  geom_dotplot(aes(x=factor(forecastValue)), binaxis = "y", stackdir="center", 
               dotsize=0.5) +
  facet_grid(.~ city) +
  xlab("Probability of Precipitation") + ylab("Precipitation (in)") + 
  ggtitle("Spring 2016, 1 Day Forecast")




## MATT ONLY: I'm not sure where the Value is coming from (not forecastValue)
ggplot(spring_WA_f1_16, aes(x=forecastValue, y=weatherval, colour=ForecastTimeDay)) + geom_point() + facet_grid(.~ city) +
  xlab("Probability of Precipitation") + ylab("Precipitation (in)") + 
  ggtitle("Spring 2016, 1 Day Forecast")

## in this plot or another plot, it might be nice to somehow connect points from
## the same day. Connecting points using lines did not really work though
## so we would have to think of another way to make the connection



## now thinking about adding the "length of forecast" dimension

spring_WA_Spok <- subset(spring_df, city %in% c("Spokane"))

# subset only forecasts of length 1 (day before)
## spring_WA_f1 <- subset(spring_WA, LengthForecastDayOnly== 1)

# only 2016 data for WA spring
##spring_WA_f1_16 <- subset(spring_WA_f1, year(as.POSIXlt(spring_WA_f1$Date)) == 2016)

##spring_WA_f1_16$ForecastTimeDay <- rep(c("Morning", "Evening"), length.out=nrow(spring_WA_f1_16)) 
nrow(spring_WA_Spok) ## odd number of rows here, so not sure which are morning
## and which are evening



Spok.precip <- subset(precip, city %in% c("Spokane") & month %in% c(3, 4, 5))
Spok.precip.summer <- subset(precip, city %in% c("Spokane") & month %in% c(6,7,8))
Spok.precip.fall <- subset(precip, city %in% c("Spokane") & month %in% c(9,10,11))
Spok.precip.winter <- subset(precip, city %in% c("Spokane") & month %in% c(12,1,2))
Spok.precip$ForecastTimeDay <- rep(c("Morning", "Evening"),
  length.out = nrow(Spok.precip)) 

head(Spok.precip)

Spok.precip$precipbinary <- as.numeric(Spok.precip$weatherval > 0.005)
Spok.precip.summer$precipbinary <- as.numeric(Spok.precip.summer$weatherval > 0.005)
Spok.precip.fall$precipbinary <- as.numeric(Spok.precip.fall$weatherval > 0.005)
Spok.precip.winter$precipbinary <- as.numeric(Spok.precip.winter$weatherval > 0.005)
Spok.precip$precipbinary


## for each forecastValue, want to figure out the proportion of days that it actually
## rained

Spok.precip$proprain <- (Spok.precip %>% group_by(forecastValue, 
  LengthForecastDayOnly, ForecastTimeDay) %>% 
    mutate(proprain = mean(precipbinary)))$proprain



Spok.precip$proprain_noMornEv <- (Spok.precip %>% group_by(forecastValue,
                                                           LengthForecastDayOnly) %>%
                                    mutate(proprain_noMornEv = mean(precipbinary)))$proprain_noMornEv

Spok.precip.summer$proprain_noMornEv <- (Spok.precip.summer %>% group_by(forecastValue,
                                                                         LengthForecastDayOnly) %>%
                                           mutate(proprain_noMornEv = mean(precipbinary)))$proprain_noMornEv

Spok.precip.fall$proprain_noMornEv <- (Spok.precip.fall %>% group_by(forecastValue,
                                                                         LengthForecastDayOnly) %>%
                                           mutate(proprain_noMornEv = mean(precipbinary)))$proprain_noMornEv

Spok.precip.winter$proprain_noMornEv <- (Spok.precip.winter %>% group_by(forecastValue,
                                                                     LengthForecastDayOnly) %>%
                                         mutate(proprain_noMornEv = mean(precipbinary)))$proprain_noMornEv


Spok.precip$Numerator_noMornEv <- (Spok.precip %>% group_by(forecastValue,
                                                            LengthForecastDayOnly) %>%
                                     mutate(Numerator_noMornEv = length(precipbinary)))$Numerator_noMornEv

Spok.precip.summer$Numerator_noMornEv <- (Spok.precip.summer %>% group_by(forecastValue, 
                                                                          LengthForecastDayOnly) %>%
                                            mutate(Numerator_noMornEv = length(precipbinary)))$Numerator_noMornEv

Spok.precip.fall$Numerator_noMornEv <- (Spok.precip.fall %>% group_by(forecastValue, 
                                                                          LengthForecastDayOnly) %>%
                                            mutate(Numerator_noMornEv = length(precipbinary)))$Numerator_noMornEv

Spok.precip.winter$Numerator_noMornEv <- (Spok.precip.winter %>% group_by(forecastValue, 
                                                                      LengthForecastDayOnly) %>%
                                          mutate(Numerator_noMornEv = length(precipbinary)))$Numerator_noMornEv



ggplot(data = Spok.precip, aes(x = forecastValue, y = proprain, colour=ForecastTimeDay)) + 
  geom_point() + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly)

# make the scatter plot without time of day since we don't know if it is correct 
ggplot(data = Spok.precip, aes(x = forecastValue/100, y = proprain_noMornEv)) + 
  geom_point(aes(alpha=Numerator_noMornEv)) + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly) +
  ggtitle("Spokane, Spring") + 
  geom_smooth(method="lm", formula = y ~ x + I(x^2), size = 0.5, se = FALSE) +
  scale_alpha_continuous(name = "Number of Obs") + 
  ylab("Proportion of Rainy Days") + 
  xlab("PoP") +
  geom_abline(intercept=0,slope=1)



ggplot(data = Spok.precip.summer, aes(x = forecastValue/100, y = proprain_noMornEv)) + 
  geom_point(aes(alpha=Numerator_noMornEv)) + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly) +
  ggtitle("Spokane, Summer") + 
  geom_smooth(method="lm", formula = y ~ x + I(x^2), size = 0.5, se = FALSE) +
  scale_alpha_continuous(name = "Number of Obs") + 
  ylab("Proportion of Rainy Days") + 
  xlab("PoP") +
  geom_abline(intercept=0,slope=1)


ggplot(data = Spok.precip.fall, aes(x = forecastValue/100, y = proprain_noMornEv)) + 
  geom_point(aes(alpha=Numerator_noMornEv)) + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly) +
  ggtitle("Spokane, Fall") + 
  geom_smooth(method="lm", formula = y ~ x + I(x^2), size = 0.5, se = FALSE) +
  scale_alpha_continuous(name = "Number of Obs") + 
  ylab("Proportion of Rainy Days") + 
  xlab("PoP") +
  geom_abline(intercept=0,slope=1)


ggplot(data = Spok.precip.winter, aes(x = forecastValue/100, y = proprain_noMornEv)) + 
  geom_point(aes(alpha=Numerator_noMornEv)) + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly) +
  ggtitle("Spokane, Winter") + 
  geom_smooth(method="lm", formula = y ~ x + I(x^2), size = 0.5, se = FALSE) +
  scale_alpha_continuous(name = "Number of Obs") + 
  ylab("Proportion of Rainy Days") + 
  xlab("PoP") +
  geom_abline(intercept=0,slope=1)

Spok.precip$season <- rep("Spring", nrow(Spok.precip))
Spok.precip.summer$season <- rep("Summer", nrow(Spok.precip.summer))
Spok.precip.fall$season <- rep("Fall", nrow(Spok.precip.fall))
Spok.precip.winter$season <- rep("Winter", nrow(Spok.precip.winter))

Spok.precip.all <- rbind(Spok.precip[,-c(20,22)], Spok.precip.summer, 
                         Spok.precip.fall, Spok.precip.winter)
                         

ggplot(data = Spok.precip.all, aes(x = forecastValue/100, y = proprain_noMornEv)) + 
  geom_point(aes(alpha=Numerator_noMornEv)) + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly) +
  ggtitle("Spokane, Winter") + 
  geom_smooth(method="lm", formula = y ~ x + I(x^2), size = 0.5, se = FALSE) +
  scale_alpha_continuous(name = "Number of Obs") + 
  ylab("Proportion of Rainy Days") + 
  xlab("PoP") +
  geom_abline(intercept=0,slope=1)





## there is still a problem with using both morning and evening precipitation
## predictions for a single precip response. But, ignoring that issue,
## we would expect "good" forecasts to be a straight line (in that case,
## half of the time 50% of rain was predicted, it actually rained). It's
## interesting that the forecasts don't look to get substantially better
## for precipitation, at least in Spokane.

## SPRING IN SPOKANE
ggplot(data = Spok.precip, aes(x = ForecastTimeDay, y = forecastValue,
  group = Date)) +
  geom_point() + 
  facet_wrap( ~LengthForecastDayOnly) + geom_line(aes(colour = weatherval), alpha=.1)

## some of the dates have two evening forecasts but no morning forecasts: example:
subset(Spok.precip, LengthForecastDayOnly == 6)[c(3, 4), c("Date", "ForecastTimeDay")]

## I'm guessing this is because of something we are doing. Maybe putting in 
## the "ForecastTimeDay" variable so late is creating problems.
## The graph looks really bad because of this. I'm not really sure how much better
## it will look once we correct this though.
## I think this problem is happening because some dates legitimately do not have a morning or evening forecast. So we can't just go every other line for "morning" and "evening".


## EXAMPLE: See how the dat july 16, 2014 only has one ProbPrecip measurement. 
## We don't know if it's morning or evening. And, if we use the every other line
## method for assigning morning and evening, things will get thrown off.

testdf <- subset(precip, LengthForecastDayOnly == 6 & city == "Spokane")
head(testdf)
