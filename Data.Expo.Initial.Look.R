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
write.csv(full.forecast, "forecastdf.csv")

#write.csv(full.forecast, "~/Desktop/DataExpo2018/forecastdf.csv")

forecastdf <- read_csv("~/Desktop/TimeSpaceExpo/forecastdf.csv")
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

testmerge <- merge(histWeatherlong, foreloc.df, by.x = c("Date", "AirPtCd",
  "weathermeas"), by.y = c("DatePredicted", "AirPtCd", "Weatherval"))
nrow(testmerge)
nrow(foreloc.df)
testmerge[1:5, ]
str(as.numeric(testmerge$weatherval))

## convert the character vector of historical weather to numeric
testmerge$weatherval <- as.numeric(testmerge$weatherval)
testmerge[40:100, ]

## seems to have merged correctly I think?

