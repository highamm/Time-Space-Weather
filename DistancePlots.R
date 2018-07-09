dist_locations <- read.csv("~/Desktop/DataExpo2018/dist_locations.csv")
allSeasons_F1 <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/allSeasons_F1.csv")

allSeasons_F1_replace <- allSeasons_F1[,-c(10,11)]

allSeason_withDists <- merge(allSeasons_F1_replace, dist_locations, by.x = "AirPtCd", 
                             by.y = "AirPtCd")
  
library(ggplot2)

ggplot(allSeason_withDists, aes(x=distance*0.000621371, y=mean_error, color=factor(measure))) + 
  geom_point() + 
  facet_grid(.~season) +
  facet_wrap(~season, ncol=2) + 
  xlab("Distance (mi)") + 
  ylab("Mean Forecast Error") + 
  scale_color_discrete(name="Temperature\nMeasure") + 
  ggtitle("Distance between City Center and Airport vs. \nMean Forecast Error")




