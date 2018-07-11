# run lines 1-104 in PredictionModelScript.R before running the following code

length(fitted(modrand2))
nrow(maxtemplagstrain)
nrow(maxtemplagstrain[complete.cases(maxtemplagstrain), c("forecastValue", "adjmeanhum", 
                                                          "adjmeanwind", "adjmaxtemp", 
                                                          "season", "city")])
# figure out which row is not being used in modrand2 
which(is.na(maxtemplagstrain$forecastValue))
maxtemplagstrain$city[2989]

# remove that row so that each observation has and observed and predicted value
maxTempPred <- maxtemplagstrain[-2989, ]
nrow(maxTempPred)

# add the predicted values from the mixed model to the data set
maxTempPred$modPred <- fitted(modrand2)



# change the order to subtraction to match the idea that Matt had for the map
# this difference is forecast - observed 
# positive value means overestimate in the forecast
# negative value means underestimate in the forecast
maxTempPred$forecastDiff <- maxTempPred$forecastDiff*(-1)

# calculate how far off the mixed model predictions are from the observed weather values
maxTempPred$modDiff <- maxTempPred$modPred - maxTempPred$weatherval

library(dplyr)

maxTempPred_summary <- maxTempPred %>% group_by_(.dots = c("AirPtCd", "season")) %>%
  summarize(meanForecastDiff = mean(forecastDiff))

maxTempPred_summary$meanModDiff <- data.frame(maxTempPred %>% group_by_(.dots = c("AirPtCd", 
                                                                                  "season")) %>%
                                                summarize(meanModDiff = mean(modDiff)))[,3]

maxTempPred_summary <- merge(x = maxTempPred_summary, y = locations[, c("AirPtCd", "city", "citylons", 
                                                "citylats")], 
             by.x = "AirPtCd", by.y = "AirPtCd", all.x=TRUE)

maxTempPred_summary$absMeanForecastDiff <- abs(maxTempPred_summary$meanForecastDiff)

library(leaflet)

# Spring map of mean forecast errors compared to mean model prediction errors
# can easily be changed to any other season
leaflet(maxTempPred_summary[maxTempPred_summary$season == "Spring", ]) %>% addTiles() %>%
  addCircles(lng=~citylons, lat=~citylats, weight=1, radius=~(absMeanForecastDiff)*25000, 
             popup=~city, color = "blue") %>%
  addCircles(lng=~citylons, lat=~citylats, weight=1, radius=~(abs(meanModDiff))*25000, 
             popup=~city, color = "red") %>%
  addLegend("topright", colors=c("blue", "red"), labels=c("Forecast Errors", 
                                                                 "Model Errors"),
            title="Max Temp Errors: Spring")
  


# just playing around with a very simple model here (this is just with the training data)
# basic idea, if all you know is the one day forecast for tomorrow, you should add about
# 2.78 degrees to the forecasted value
simple_mod <- lm(weatherval ~ forecastValue, data = maxTempPred)
summary(simple_mod)
plot(simple_mod) # residuals look fine
