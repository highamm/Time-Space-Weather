library(readr)

histWeather <- read_csv("~/Desktop/TimeSpaceExpo/histWeather.csv")

locations <- read_csv("~/Desktop/TimeSpaceExpo/locations.csv")

forecast.df <- read.delim("forecast.dat", sep = "") 

# Erin read data 
# histWeather <- read.csv("~/Desktop/DataExpo2018/histWeather.csv")
# locations <- read.csv("~/Desktop/DataExpo2018/locations.csv")
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



str(merged.df$Date)
summary(merged.df$Date)


str(forecastdf)
summary(forecastdf$Citynum)

## try to merge with locatiions data set
locations$citynum <- as.numeric(rownames(locations))
str(as.numeric(locations$citynum))
str(forecastdf$Citynum)
foreloc.df <- merge(forecastdf, locations, by.x = "Citynum", by.y = "citynum")
str(foreloc.df)

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
str(July9fore.df)
View(foreloc.df)
## need to merge with historical data to get the true values...next time
USAMap +
  geom_point(data = July1.only, aes(x = longitude, y = latitude,
    colour = CloudCover)) +
  scale_colour_continuous(low = "cadetblue3", high = "grey")



## each date has more than 1 prediction
str(foreloc.df)
July9fore.df <- subset(foreloc.df,
    DatePredicted == "2014-07-12" & Weatherval == "MaxTemp" & AirPtCd == "KBHB")
July9fore.df

## merging historical data with the foreloc data set

library(tidyr)

## first, get rid of unnecessary columns in histWeather
histWeathersub <- histWeather[ ,c("Date", "Max_TemperatureF",
  "Min_TemperatureF", "PrecipitationIn", "AirPtCd")]
str(histWeathersub)
str(histWeather)

## wide to long
histWeatherlong <- gather(histWeathersub, weathermeas, weatherval, 
  c(Max_TemperatureF, Min_TemperatureF, PrecipitationIn),
  factor_key = TRUE)
str(histWeatherlong)

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

## convert trace values to 0.005 inches for now

sum(histWeatherlong$weatherval == "0.005", na.rm = TRUE)
histWeatherlong$weatherval[histWeatherlong$weatherval == "T"] <- "0.005"

str(histWeatherlong)
str(foreloc.df)
foreloc.df$Weatherval <- as.factor(foreloc.df$Weatherval)
?merge

foreloc.sub <- subset(foreloc.df, DatePredicted == "2014-07-12" |
    DatePredicted == "2014-07-13")
str(foreloc.sub)
histweathersub <- subset(histWeatherlong, Date == "2014-07-12" |
    Date == "2014-07-13")
str(histweathersub)
testdf <- merge(histweathersub, foreloc.sub, by.x = c("Date", "AirPtCd",
  "weathermeas"), by.y = c("DatePredicted", "AirPtCd", "Weatherval"))
testdf[3400, ]

# is value the forecasted value and weatherval the observed weather
all.df <- merge(histWeatherlong, foreloc.df, by.x = c("Date", "AirPtCd", 
                                                      "weathermeas"), by.y = c("DatePredicted", "AirPtCd", "Weatherval"),
                all.x = TRUE)

nrow(all.df)
nrow(foreloc.df)
all.df[12000:12050, ]
str(as.numeric(all.df$weatherval))

## convert the character vector of historical weather to numeric
all.df$weatherval <- as.numeric(all.df$weatherval)
all.df[40:100, ]

## seems to have merged correctly I think?

## let's look at precipitation in Eugene

Eugprec <- subset(all.df, AirPtCd == "KEUG" & weathermeas == "ProbPrecip")
str(Eugprec)

## convert weatherval to rain or no rain

Eugprec$weatherval[Eugprec$weatherval > 0] <- 1
basic.mod <- glm(data = Eugprec, weatherval ~ Value, family = "binomial")
summary(basic.mod)

exp(basic.mod$coefficients[1] + basic.mod$coefficients[2] * 50) / 
  (1 + exp(basic.mod$coefficients[1] + basic.mod$coefficients[2] * 50))

## same model for San Antonio

## let's look at precipitation in Eugene

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
## we are not differentiating between morning and evening precipitation







##### Erin Edits 4/19/18
##### Visualization that explores differences in prediction errors for different forecast lengths 
##### MinTemp

# remove any observations that have an NA 
all.df_complete <- all.df[complete.cases(all.df), ]

# make sure dates are recorded as Date objects
all.df_complete$Date <- as.Date(all.df_complete$Date)
all.df_complete$DateofForecast <- as.Date(all.df_complete$DateofForecast)

# remove any observations where the date of forecast is after observed date
# This data set (all.df_completeSub) has dates where the forecasted date was 
# on or before the observed date
all.df_completeSub <- subset(all.df_complete, all.df_complete$DateofForecast <=
                               all.df_complete$Date)









# subset Eugene and only Min Temps
Eug_mintemp <- subset(all.df_completeSub, AirPtCd == "KEUG" & weathermeas == "MinTemp")

head(Eug_mintemp[complete.cases(Eug_mintemp),])

# store values as numeric 
Eug_mintemp$Value <- as.numeric(as.character(Eug_mintemp$Value))

# add a column that has the difference in forecast and actual min temp

Eug_mintemp$forecastDiff <- Eug_mintemp$weatherval - Eug_mintemp$Value
Eug_mintemp$absForecastDiff <- abs(Eug_mintemp$forecastDiff)


library(lubridate)


Eug_mintemp$Date <- ymd(Eug_mintemp$Date)
Eug_mintemp$DateofForecast <- ymd(Eug_mintemp$DateofForecast)

as.duration(Eug_mintemp$Date[3] %--% Eug_mintemp$DateofForecast[3])

Eug_mintemp$LengthForecast <- as.duration(Eug_mintemp$DateofForecast %--% Eug_mintemp$Date)
