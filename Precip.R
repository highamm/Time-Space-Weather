complete_df <- read.csv("~/Desktop/DataExpo2018/all_df_completesub.csv")

precip <- subset(complete_df, weathermeas == "ProbPrecip")

# very naive approach to just average the percent precip 

library(dplyr)
precip_avg <- precip %>% group_by(Date, AirPtCd) %>% 
  summarize(mean_precip_prob = mean(Value))

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
#    - 

# bad 
ggplot(precip_avg, aes(x = mean_precip_prob, y = weatherval)) + geom_point()
