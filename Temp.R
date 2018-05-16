## start working on temperature

library(lubridate)
library(dplyr)

complete_df <- all.df_completeSub

summary(all.df_completeSub)
maxtemp <- subset(complete_df, weathermeas == "MaxTemp")

maxtemp$Date <- as.Date(maxtemp$Date)
maxtemp$DateofForecast <- as.Date(maxtemp$DateofForecast)

maxtemp$month <- month(as.POSIXlt(maxtemp$Date))

springtemp <- subset(maxtemp, month %in% c(3, 4, 5))
summertemp <- subset(maxtemp, month %in% c(6, 7, 8))
falltemp <- subset(maxtemp, month %in% c(9, 10, 11))
wintertemp <- subset(maxtemp, month %in% c(12, 1, 2))

summary(springtemp)
ggplot(springtemp, aes(x = forecastValue, y = weatherval)) + geom_point()
ggplot(summertemp, aes(x = forecastValue, y = weatherval)) + geom_point()
ggplot(falltemp, aes(x = forecastValue, y = weatherval)) + geom_point()
ggplot(wintertemp, aes(x = forecastValue, y = weatherval)) + geom_point()
