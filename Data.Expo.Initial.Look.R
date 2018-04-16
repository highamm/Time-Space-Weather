library(readr)

histWeather <- read_csv("~/Desktop/TimeSpaceExpo/histWeather.csv")

locations <- read_csv("~/Desktop/TimeSpaceExpo/locations.csv")

forecast.df <- read.delim("forecast.dat", sep = "") 
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
  geom_density2d(data = merged.df, aes(x = longitude, y = latitude), size = 0.3) + 
  stat_density2d(data = merged.df,
    aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
    size = 0.01, bins = 16, geom = "polygon")
##+
##  scale_fill_gradient(low = "green", high = "red") + 
##  scale_alpha(range = c(0, 0.3), guide = FALSE)

USAMap +
  geom_point(data = merged.df, aes(x = longitude, y = latitude,
    colour = CloudCover), 
    alpha = 0.4) ## + 
    ##scale_size_continuous(range = range(merged.df$CloudCover))

## 1 cloud cover data point is negative....typo?
summary(merged.df$CloudCover)
sum(merged.df$CloudCover < 0, na.rm = TRUE)

ß