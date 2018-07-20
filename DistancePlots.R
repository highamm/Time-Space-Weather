dist_locations <- read.csv("~/Desktop/DataExpo2018/dist_locations.csv")
allSeasons_F1 <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/allSeasons_F1.csv")
dist_locations

## Matt only

dist_locations <- read.csv("~/Desktop/TimeSpaceExpo/dist_locations.csv")
allSeasons_F1 <- read.csv("~/Desktop/TimeSpaceExpo/allSeasons_F1.csv")

allSeasons_F1_replace <- allSeasons_F1[,-c(10,11)]

allSeason_withDists <- merge(allSeasons_F1_replace, dist_locations, by.x = "AirPtCd", 
                             by.y = "AirPtCd")

# puts the facet grid in an order that makes sense 
allSeason_withDists$season <- factor(allSeason_withDists$season, levels=c("winter", "spring", 
                                                                          "summer", "fall"))

allSeason_withDists$CityDirection <- rep(0, nrow(allSeason_withDists))

allSeason_withDists$CityDirection[which(allSeason_withDists$latDist < 0)] <- "North"
allSeason_withDists$CityDirection[which(allSeason_withDists$latDist > 0)] <- "South"

library(ggplot2)
library(viridis)

ggplot(allSeason_withDists, aes(x=distance*0.000621371, y=mean_error, color=measure)) + 
  geom_point() + 
  geom_text(data=allSeason_withDists[allSeason_withDists$city.x=="Austin", ], 
             aes(label = city.x), nudge_y = 1.2, color = "black") + 
  facet_grid(.~season) +
  facet_wrap(~season, ncol=2) + 
  xlab("Distance (mi)") + 
  ylab("Mean Forecast Error") + 
  scale_color_manual(name="Temperature\nMeasure", values = c("#00CC99", "#CC6633")) +
  ggtitle("Mean Forecast Error vs.Distance between City Center and Airport  ") 



# If we remove outlier points, still don't see a pattern
ggplot(allSeason_withDists[allSeason_withDists$distance*0.000621371 < 25, ], aes(x=distance*0.000621371, y=mean_error, color=factor(measure))) + 
  geom_point() + 
  facet_grid(.~season) +
  facet_wrap(~season, ncol=2) + 
  xlab("Distance (mi)") + 
  ylab("Mean Forecast Error") + 
  scale_color_discrete(name="Temperature\nMeasure") + 
  ggtitle("Distance between City Center and Airport vs. \nMean Forecast Error") + 
  stat_smooth(se =FALSE)


ggplot(allSeason_withDists, aes(x=latDist, y = mean_error, color = factor(measure))) + 
  geom_point() + 
  facet_grid(. ~ season) +
  facet_wrap(~season, ncol=2) + 
  geom_vline(xintercept = 0) + 
  xlab("Distance (degrees latitude)") + 
  ylab("Mean Forecast Error") + 
  scale_color_discrete(name="Temperature\nMeasure") + 
  ggtitle("Latitudinal Distance between City Center and Airport vs. \nMean Forecast Error") + 
  stat_smooth(se = FALSE)
