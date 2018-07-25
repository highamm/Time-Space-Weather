# run 1-120 in Prediction Model Script, then run 174-196


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

maxTempPred_summary$absMeanForecastDiff <- abs(maxTempPred_summary$meanForecastDiff)

maxTempPred_long <- gather(maxTempPred_summary, type, measurement, meanForecastDiff:meanModDiff, 
                           factor_key = TRUE)


ggplot(maxTempPred_long[maxTempPred_long$season == "Spring", ], aes(x = type, y = measurement, 
                                                                    group = AirPtCd)) + 
  geom_point() + 
  geom_line()


ggplot(maxTempPred_long[maxTempPred_long$season == "Summer", ], aes(x = type, y = measurement, 
                                                                    group = AirPtCd)) + 
  geom_point() + 
  geom_line()

ggplot(maxTempPred_long[maxTempPred_long$season == "Winter", ], aes(x = type, y = measurement, 
                                                                    group = AirPtCd)) + 
  geom_point() + 
  geom_line()

ggplot(maxTempPred_long[maxTempPred_long$season == "Fall", ], aes(x = type, y = measurement, 
                                                                  group = AirPtCd)) + 
  geom_point() + 
  geom_line()
