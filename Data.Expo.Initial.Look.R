library(readr)

histWeather <- read_csv("~/Desktop/TimeSpaceExpo/histWeather.csv")

locations <- read_csv("~/Desktop/TimeSpaceExpo/locations.csv")

forecast.df <- read.delim("forecast.dat", sep = "") 

# Erin read data 
histWeather <- read.csv("~/Desktop/DataExpo2018/histWeather.csv")
locations <- read.csv("~/Desktop/DataExpo2018/locations.csv")
# forecast.df <- read.delim("~/Desktop/DataExpo2018/forecast.dat", sep="")



extra.obs <- c(1, "2014-07-09", "63", "MinTemp", "2014-07-09")
full.forecast <- rbind(forecast.df, extra.obs)

# Erin don't run following line
write.csv(full.forecast, "forecastdf.csv")
forecastdf <- read_csv("~/Desktop/TimeSpaceExpo/forecastdf.csv")

# Erin only 
write.csv(full.forecast, "~/Desktop/DataExpo2018/forecastdf.csv")
forecastdf <- read.csv("~/Desktop/DataExpo2018/forecastdf.csv")




str(forecastdf)

forecastdfcolnames <- c("Obsnum", "Citynum", "DatePredicted", "Value",
  "Weatherval", "DateofForecast")

colnames(forecastdf) <- forecastdfcolnames
head(forecastdf)

str(data)
str(locations)
str(histWeather)
View(forecast.df)
View(locations)
View(histWeather)

## merge histWeather and locations
merged.df <- merge(locations, histWeather, by = "AirPtCd")

## subsetting data to Eugene only for a single time series
Eug.only <- subset(merged.df, AirPtCd == "KEUG")

## subsetting to a single date so that we have a single spatial process at one
## particular time

July1.only <- subset(merged.df, Date == "2014-07-01")

str(July1.only)

library(ggmap)
usa_center <- as.numeric(geocode("United States"))

USAMap <- ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="normal")
USAMap

## trying to draw a heatmap, but it doesn't make sense for now so ignore.
USAMap +
  geom_density2d(data = July1.only, aes(x = longitude, y = latitude), size = 0.3) + 
  stat_density2d(data = July1.only,
    aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
    size = 0.01, bins = 16, geom = "polygon")
##+
##  scale_fill_gradient(low = "green", high = "red") + 
##  scale_alpha(range = c(0, 0.3), guide = FALSE)

USAMap +
  geom_point(data = July1.only, aes(x = longitude, y = latitude,
    colour = CloudCover)) +
  scale_colour_continuous(low = "cadetblue3", high = "grey")
  ## + 
    ##scale_size_continuous(range = range(merged.df$CloudCover))

## 1 cloud cover data point is negative....typo?
summary(merged.df$CloudCover)
sum(merged.df$CloudCover < 0, na.rm = TRUE)

str(forecastdf)
summary(forecastdf$Citynum)

## try to merge with locations data set
locations$citynum <- as.numeric(rownames(locations))

foreloc.df <- merge(forecastdf, locations, by.x = "Citynum", by.y = "citynum")
nrow(foreloc.df)

## July 9th forecast data for July 10th for Eugene
## IGNORE this line for now
July9fore.df <- subset(foreloc.df, DateofForecast == "2014-07-09" &
    DatePredicted == "2014-07-10" & AirPtCd == "KEUG")
July9fore.df
## there are 4 observations per city (a morning precip, evening precip,
## min temp, and max temp)

## let's look at max temp
July9fore.df <- subset(foreloc.df, DateofForecast == "2014-07-09" &
    DatePredicted == "2014-07-10" & Weatherval == "MaxTemp")

## need to merge with historical data to get the true values...next time
USAMap +
  geom_point(data = July1.only, aes(x = longitude, y = latitude,
    colour = CloudCover)) +
  scale_colour_continuous(low = "cadetblue3", high = "grey")

## each date has more than 1 prediction
July9fore.df <- subset(foreloc.df,
    DatePredicted == "2014-07-12" & Weatherval == "MaxTemp" & AirPtCd == "KBHB")
July9fore.df

## merging historical data with the foreloc data set

library(tidyr)

## first, get rid of unnecessary columns in histWeather
histWeathersub <- histWeather[ ,c("Date", "Max_TemperatureF",
  "Min_TemperatureF", "PrecipitationIn", "AirPtCd")]


## wide to long
histWeatherlong <- gather(histWeathersub, weathermeas, weatherval, 
  c(Max_TemperatureF, Min_TemperatureF, PrecipitationIn),
  factor_key = TRUE)

library(plyr)

## remapping factors to match the names in the forecast data frame
histWeatherlong$weathermeas <- mapvalues(histWeatherlong$weathermeas,
  from = c("Max_TemperatureF", "Min_TemperatureF", "PrecipitationIn"),
  to = c("MaxTemp", "MinTemp", "ProbPrecip"))

## need to figure out why some of these are NAs
str(histWeatherlong$weatherval)
sum(histWeatherlong$weatherval == "T")
which(is.na(as.numeric(histWeatherlong$weatherval == "78")))
histWeatherlong$weatherval[122898]

## convert trace values to 0 inches for now

sum(histWeatherlong$weatherval == "T", na.rm = TRUE) / nrow(histWeatherlong)
histWeatherlong$weatherval[histWeatherlong$weatherval == "T"] <- "0.005"

foreloc.df$Weatherval <- as.factor(foreloc.df$Weatherval)

# is value the forecasted value and weatherval the observed weather
all.df <- merge(histWeatherlong, foreloc.df,
  by.x = c("Date", "AirPtCd", "weathermeas"),
  by.y = c("DatePredicted", "AirPtCd", "Weatherval"),
  all.x = TRUE)


## convert the character vector of historical weather to numeric
all.df$weatherval <- as.numeric(all.df$weatherval)

## let's look at precipitation in Eugene

Eugprec <- subset(all.df, AirPtCd == "KEUG" & weathermeas == "ProbPrecip")

## convert weatherval to rain or no rain

Eugprec$weatherval[Eugprec$weatherval > 0] <- 1
basic.mod <- glm(data = Eugprec, weatherval ~ Value, family = "binomial")
summary(basic.mod)

exp(basic.mod$coefficients[1] + basic.mod$coefficients[2] * 50) / 
  (1 + exp(basic.mod$coefficients[1] + basic.mod$coefficients[2] * 50))

## same model for San Antonio

SAprec <- subset(all.df, AirPtCd == "KSKF" & weathermeas == "ProbPrecip")

## convert weatherval to rain or no rain

SAprec$weatherval[SAprec$weatherval > 0] <- 1
basic.mod <- glm(data = SAprec, weatherval ~ Value, family = "binomial")
summary(basic.mod)

exp(basic.mod$coefficients[1] + basic.mod$coefficients[2] * 50) / 
  (1 + exp(basic.mod$coefficients[1] + basic.mod$coefficients[2] * 50))

## these models have a lot of flaws, so we would probably want to visualize the data first before expanding on these models. Some flaws are:
## Trace is currently counted as a "yes" for precipitation, but should it?
## no spatial effects
## no time effects
## we are not differentiating between morning and evening precipitation while
## the actual observed historical data has precipitation for the entire day


##### Erin Edits 4/19/18
##### Visualization that explores differences in prediction errors for different forecast lengths 
##### MinTemp

# remove any observations that have an NA 
all.df_complete <- all.df[complete.cases(all.df), ]

# make sure dates are recorded as Date objects
all.df_complete$Date <- as.Date(all.df_complete$Date)
all.df_complete$DateofForecast <- as.Date(all.df_complete$DateofForecast)




## putting some of the work you did prepping the Eugene subset so that it applies
## to the whole data set for the purpose of future explorations

# store values as numeric 
all.df_complete$weatherval <- as.numeric(as.character(all.df_complete$weatherval))
all.df_complete$Value <- as.numeric(as.character(all.df_complete$Value))

# add a column that has the difference in forecast and actual min temp

all.df_complete$forecastDiff <- all.df_complete$weatherval - all.df_complete$Value
all.df_complete$absForecastDiff <- abs(all.df_complete$forecastDiff)


library(lubridate)

all.df_complete$Date <- ymd(all.df_complete$Date)
all.df_complete$DateofForecast <- ymd(all.df_complete$DateofForecast)

# record the distance in time between the forecasted date and the observed date
all.df_complete$LengthForecast <- abs(as.duration(all.df_complete$DateofForecast %--% all.df_complete$Date))
all.df_complete$LengthForecastDays <- seconds_to_period(all.df_complete$LengthForecast)

all.df_complete$LengthForecastDayOnly <- as.numeric(substring(all.df_complete$LengthForecastDays, 1, 1))

# remove any observations where the date of forecast is after observed date
# This data set (all.df_completeSub) has dates where the forecasted date was 
# on or before the observed date
all.df_completeSub <- subset(all.df_complete, all.df_complete$DateofForecast <=
                               all.df_complete$Date)
names(all.df_completeSub)[7] <- "forecastValue"

## Erin
write.csv(all.df_completeSub, "~/Desktop/DataExpo2018/all_df_completesub.csv")
all.df_completeSub <- read.csv("~/Desktop/DataExpo2018/all_df_completesub.csv")

## Matt
write.csv(all.df_completeSub, "all_df_completesub.csv")
all.df_completeSub <- read.csv("all_df_completesub.csv")



# subset Eugene and only Min Temps
Eug_mintemp <- subset(all.df_completeSub, AirPtCd == "KEUG" & weathermeas == "MinTemp")

head(Eug_mintemp[complete.cases(Eug_mintemp),])


as.duration(Eug_mintemp$Date[3] %--% Eug_mintemp$DateofForecast[3])

library(ggplot2)
# I think we need a better way to visualize this but should be fine temporarily
ggplot(Eug_mintemp, aes(x = LengthForecastDayOnly, y = absForecastDiff)) +
  geom_jitter(alpha = 0.2)
ggplot(Eug_mintemp, aes(x = LengthForecastDayOnly, y = absForecastDiff)) +
  geom_boxplot(aes(group = LengthForecastDayOnly))
## that's quite strange that some of the forecasts are off by so much. Like I can't
## imagine them being off by almost 30 degrees Farenheit when the
## forecast is made the day before in Eugene.


ggplot(Eug_mintemp, aes(x = LengthForecastDayOnly, y = absForecastDiff)) +
  geom_count() +
  scale_size_area()

## ADDED  4/22
subset(Eug_mintemp, absForecastDiff > 25)
## particularly for these, I find it hard to believe that they would forecast 
## a min temperature of 14 degrees in the middle of April. It could be a problem
## with the merge or could be the result of a few typos in such a large data set.

library(dplyr)

group_by(Eug_mintemp, as.factor(LengthForecastDayOnly)) %>% summarize(m = mean(absForecastDiff))
group_by(Eug_mintemp, as.factor(LengthForecastDayOnly)) %>% summarize(s = sd(absForecastDiff))
## nice to see that forecasts seem to get closer the closer we get to the 
## date to be predicted.

# checking means between different legnths of forecasts
anova(lm(absForecastDiff ~ as.factor(LengthForecastDayOnly), data=Eug_mintemp))



filtered.df <- filter(all.df_complete,
  weathermeas == "MinTemp", LengthForecastDayOnly == 3)

sumbyloc <- group_by(filtered.df, as.factor(LengthForecastDayOnly), AirPtCd) %>%
  summarize(m = mean(absForecastDiff))
## only using forecasts three days out


all_mintempll <- merge(sumbyloc, locations, by = "AirPtCd", all.x = TRUE)
nrow(all_mintempll)

USAMap +
  geom_point(data = all_mintempll, aes(x = longitude, y = latitude,
    colour = m), size = 4) +
  scale_colour_continuous(low = "lightblue", high = "darkblue")


## repeat but now using the true forecastdiff not absolute

sumbylocdiff <- group_by(filtered.df, as.factor(LengthForecastDayOnly), AirPtCd) %>%
  summarize(m = mean(forecastDiff))
## only using forecasts three days out


all_mintemplldiff <- merge(sumbylocdiff, locations, by = "AirPtCd", all.x = TRUE)

USAMap +
  geom_point(data = all_mintemplldiff, aes(x = longitude, y = latitude,
    colour = m), size = 4) +
  scale_colour_gradient2(low = "red", mid = "white", high = "darkblue")
## most places seem to underforecast the minimum temperature value. 