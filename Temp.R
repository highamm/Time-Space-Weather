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

## do a bit of data cleaning

## get rid of these points. These are almost surely data entry errors.
## The weather value is 1 degree for the first five days of March while the
## forecast is in the 40s.

## getting rid of the obvious outliers
springclean <- springtemp[-which(springtemp$weatherval < 5 & springtemp$forecastValue > 30), ]
summerclean <- summertemp
fallclean <- falltemp
winterclean <- wintertemp[-which(wintertemp$weatherval > 95), ]
winterclean <- winterclean[-which(winterclean$forecastValue < 32 &
    winterclean$weatherval > 75), ]
winterclean <- winterclean[-which(winterclean$forecastValue > 31 &
    winterclean$weatherval < 5), ]
ggplot(winterclean, aes(x = forecastValue, y = weatherval)) + geom_point()


