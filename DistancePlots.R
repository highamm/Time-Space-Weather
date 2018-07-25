# Erin
dist_locations <- read.csv("~/Desktop/DataExpo2018/dist_locations.csv")
#allSeasons_F1 <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/allSeasons_F1.csv")
dist_locations

## Matt only

dist_locations <- read.csv("~/Desktop/TimeSpaceExpo/dist_locations.csv")
#allSeasons_F1 <- read.csv("~/Desktop/TimeSpaceExpo/allSeasons_F1.csv")


# need maxtempalldat and mintempalldat which are in the data.read.R file
allSeasons <- rbind(maxtempalldat, mintempalldat)
dim(allSeasons)
nrow(maxtempalldat) + nrow(mintempalldat)

allSeasons_F1 <- allSeasons[allSeasons$LengthForecastDayOnly == 1, ]

#allSeasons_F1_replace <- allSeasons_F1[,-c(10,11)]

allSeason_withDists <- merge(allSeasons_F1, dist_locations, by.x = "AirPtCd", 
                             by.y = "AirPtCd")

# puts the facet grid in an order that makes sense 
allSeason_withDists$season <- factor(allSeason_withDists$season, levels=c("Winter", "Spring", 
                                                                          "Summer", "Fall"))

allSeason_withDists$CityDirection <- rep(0, nrow(allSeason_withDists))

allSeason_withDists$CityDirection[which(allSeason_withDists$latDist < 0)] <- "North"
allSeason_withDists$CityDirection[which(allSeason_withDists$latDist > 0)] <- "South"


summary_withDists <- allSeason_withDists[complete.cases(allSeason_withDists$Error), ] %>%
  group_by_(.dots = c("AirPtCd", "weathermeas", "season")) %>%
  summarize(mean_error = mean(Error))
summary_withDists2 <- merge(summary_withDists, dist_locations, by.x = "AirPtCd", 
                            by.y = "AirPtCd")


library(ggplot2)
library(viridis)


max_summary <- bind_rows("Winter" = winter_max_error_F1, 
                         "Spring" = spring_max_error_F1, 
                         "Summer" = summer_max_error_F1, 
                         "Fall" = fall_max_error_F1, 
                         .id = "Season")
min_summary <- bind_rows("Winter" = winter_min_error_F1, 
                         "Spring" = spring_min_error_F1, 
                         "Summer" = summer_min_error_F1, 
                         "Fall" = fall_min_error_F1, 
                         .id = "Season")
overall_summary <- bind_rows("Max" = max_summary, 
                             "Min" = min_summary, 
                             .id = "Metric")
overall_summary_dists <- merge(overall_summary, dist_locations, by.x = "AirPtCd", 
                               by.y = "AirPtCd")
overall_summary_dists$Season <- as.factor(overall_summary_dists$Season)
overall_summary_dists$Season <- factor(overall_summary_dists$Season, 
                                       levels = c("Winter", "Spring", "Summer", "Fall"))

library(lemon)

ggplot(overall_summary_dists, aes(x=distance*0.000621371, y=mean_error, color=Metric)) + 
  geom_point() + 
  geom_text(data=overall_summary_dists[overall_summary_dists$city.x=="Austin", ], 
             aes(label = "Austin, NV"), nudge_y = 1.2, color = "black") + 
  facet_rep_wrap(~Season, ncol=2, repeat.tick.labels = 'bottom') +
  theme_economist() +
  xlab("Distance (mi)") + 
  ylab("Mean Forecast Error (°F)") + 
  scale_color_economist(name="Temperature\nMeasure") +
  ggtitle("Mean Forecast Error vs. Distance \nbetween City Center and Airport") +
  theme(legend.position = c(0.7,1.15), 
        legend.direction = "horizontal",
        strip.text.x = element_text(face="bold", size=15), 
        panel.spacing.x = unit(1,"line"), 
        panel.spacing.y = unit(1,"line"), 
        axis.line.y = element_line(colour="black"), 
        axis.title.y = element_text(size = 15), 
        axis.title.x = element_text(size = 15), 
        plot.title = element_text(size = 18), 
        legend.title = element_text(face="bold"))



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
