# run 1-120 in Prediction Model Script, then run 174-196

library(tidyr)

summary(maxtemplagstest$Date)

maxTempPred_summary <- maxtemplagstest %>% 
  group_by_(.dots = c("AirPtCd", "season")) %>%
  summarize(meanForecastDiff = mean(orig.diff))

maxTempPred_summary$meanModDiff <- data.frame(maxtemplagstest %>%
  group_by_(.dots = c("AirPtCd", "season")) %>%
    summarize(meanModDiff = mean(mod.diff)))[,3]



maxTempPred_summary <- merge(x = maxTempPred_summary, y = locations[, c("AirPtCd", "city", "longitude", 
                                                                        "latitude")], 
                             by.x = "AirPtCd", by.y = "AirPtCd", all.x=TRUE)

maxTempPred_summary$varMod <- data.frame(maxtemplagstest %>%
                                           group_by_(.dots = c("AirPtCd", "season")) %>%
                                           summarize(varMod = mean(mod.diff^2)))[,3]
maxTempPred_summary$varForecast <- data.frame(maxtemplagstest %>%
                                                group_by_(.dots = c("AirPtCd", "season")) %>%
                                                summarize(varForecast = mean(orig.diff^2)))[,3]

maxTempPred_summary$absMeanForecastDiff <- abs(maxTempPred_summary$meanForecastDiff)

maxTempPred_long <- gather(maxTempPred_summary, type, measurement, meanForecastDiff:meanModDiff, 
                           factor_key = TRUE)

maxTempPred_long_var <- gather(maxTempPred_summary, type, measurement, varMod:varForecast, 
                               factor_key = TRUE)

library(ggthemes)
library(ggplot2)

ggplot(maxTempPred_long[maxTempPred_long$season == "Spring", ], aes(x = type, y = measurement, 
                                                                    group = AirPtCd)) + 
  geom_point() + 
  geom_line()


ggplot(maxTempPred_long[maxTempPred_long$season == "Summer", ], aes(x = type, y = measurement, 
                                                                    group = AirPtCd)) + 
  geom_point(alpha=0.4) + 
  geom_line(alpha = 0.4) + 
  theme_economist(base_size= 17) + 
  xlab(" ") + 
  scale_x_discrete(labels = c("Forecast", "Model Prediction")) + 
  ylab("Mean Error (°F)") + 
  ggtitle("Differences in Forecasts and \nModel Predictions (Summer)") +
  theme(axis.title = element_text(size = 17), axis.text = element_text(size = 17), 
        plot.title = element_text(size = 17))



ggplot(maxTempPred_long, aes(x = type, y = measurement, group = AirPtCd)) + 
  geom_point(alpha=0.4) + 
  geom_line(alpha = 0.4) + 
  theme_economist() + 
  scale_x_discrete(labels = c("Forecast", "Model")) + 
  ylab("Mean Error (°F)") + 
  ggtitle("Differences in Forecasts and \nModel Predictions (Max Temp)") +
  theme(axis.title.y = element_text(size = 17), axis.text.x = element_text(size = 10, angle=60, 
                                                                           vjust = 0.8,
                                                                           hjust = 0.9), 
        plot.title = element_text(size = 18), axis.title.x = element_blank()) + 
  facet_grid(.~season) + 
  ggsave("Prediction_All_Seasons_new.png", width = 10, height = 7.5)

  

ggplot(maxTempPred_long_var[maxTempPred_long_var$season == "Summer", ], aes(x = type, y = measurement, 
                                                                    group = AirPtCd)) + 
  geom_point(alpha=0.4) + 
  geom_line(alpha = 0.4) + 
  theme_economist() 


ggplot(maxTempPred_long[maxTempPred_long$season == "Winter", ], aes(x = type, y = measurement, 
                                                                    group = AirPtCd)) + 
  geom_point() + 
  geom_line()

ggplot(maxTempPred_long[maxTempPred_long$season == "Fall", ], aes(x = type, y = measurement, 
                                                                  group = AirPtCd)) + 
  geom_point() + 
  geom_line()
