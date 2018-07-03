locations <- read_csv("~/Desktop/TimeSpaceExpo/locations.csv")
locations <- read.csv("~/Desktop/DataExpo2018/locations.csv")

locations$citstate <- paste(locations$city, locations$state)
locations$citstate
library(ggmap)
geocode(c("New York", "Chicago"))

citylatlons <- geocode(locations$citstate)
citylatlongs2 <- geocode(locations$city)
citylatlons3 <- geocode(locations$citstate)

row8 <- geocode(locations$citstate[8])
row42 <- geocode(locations$citstate[42])
row45 <- geocode(locations$citstate[45])
row46 <- geocode(locations$citstate[46])
row48 <- geocode(locations$citstate[48])

citylatlons4 <- cbind(rep(NA, length = nrow(locations)),
  rep(NA, length = nrow(locations)))
citylatlons4[8, ] <- as.numeric(row8)
citylatlons4[42, ] <- as.numeric(row42)
citylatlons4[45, ] <- as.numeric(row45)
citylatlons4[46, ] <- as.numeric(row46)
citylatlons4[48, ] <- as.numeric(row48)


testdf <- data.frame(cbind(citylatlons, citylatlongs2, citylatlons3, citylatlons4))
locations$citstate[12]
geocode("Atlantic City")
?geocode
str(testdf)
library(dplyr)
citylons <- coalesce(testdf$lon, testdf$lon.1, testdf$lon.2, testdf$X1)
citylats <- coalesce(testdf$lat, testdf$lat.1, testdf$lat.2, testdf$X2)

locations$citylons <- citylons
locations$citylats <- citylats


library(geosphere)
distGeo(p1 = c(locations$longitude[1], locations$latitude[1]),
  p2 = c(locations$citylons[1], locations$citylats[1]))
dists <- distGeo(cbind(locations$longitude, locations$latitude),
  cbind(locations$citylons, locations$citylats))
qplot(dists)
dists
## fixing a couple of mistakes

locations[31, ]
loc31 <- geocode("Charleston West Virginia")
locations[31, 7] <- loc31[1, 1]; locations[31, 8] <- loc31[1, 2]

## 83, 91, 93
locations[83, ]
loc31 <- geocode("Harve Montana")
loc31
locations[83, 7] <- loc31[1, 1]; locations[83, 8] <- loc31[1, 2]


locations[91, ]
loc31 <- geocode("Richfield Utah")
loc31
locations[91, 7] <- loc31[1, 1]; locations[91, 8] <- loc31[1, 2]


locations[93, ]
loc31 <- geocode("Salmon Idaho")
loc31
locations[93, 7] <- loc31[1, 1]; locations[93, 8] <- loc31[1, 2]

locations[98, ]
loc31 <- geocode("Austin Nevada")
loc31
locations[98, 7] <- loc31[1, 1]; locations[98, 8] <- loc31[1, 2]

dists <- distGeo(cbind(locations$longitude, locations$latitude),
  cbind(locations$citylons, locations$citylats))
qplot(dists)
dists
which(dists > 500000)

write.csv(locations, file = "updlocations.csv")

qplot(dists, spring_max_error_F1$AbsError)
summary(lm(spring_max_error_F1$AbsError ~ dists))







updlocations <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/updlocations.csv")

dists <- distGeo(cbind(updlocations$longitude, updlocations$latitude), 
                 cbind(updlocations$citylons, updlocations$citylats))


