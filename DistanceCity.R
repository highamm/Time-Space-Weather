locations <- read_csv("~/Desktop/TimeSpaceExpo/locations.csv")
locations <- read.csv("~/Desktop/DataExpo2018/locations.csv")

locations$citstate <- paste(locations$city, locations$state)
locations$citstate
library(ggmap)
geocode(c("New York", "Chicago"))

citylatlons <- geocode(locations$citstate)
citylatlongs2 <- geocode(as.character(locations$city))
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


## Matt: do first half of cities





updlocations <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/updlocations.csv")

library(geosphere)
dists <- distGeo(cbind(updlocations$longitude, updlocations$latitude), 
                 cbind(updlocations$citylons, updlocations$citylats))

apts1 <- geocode(as.character(locations$AirPtCd))
apts2 <- geocode(as.character(locations$AirPtCd))
apts3 <- geocode(as.character(locations$AirPtCd))
apts4 <- geocode(as.character(locations$AirPtCd))

apts11 <- geocode("KJFK")
apts11 <- as.numeric(apts11)
apts94 <- geocode(as.character(locations$AirPtCd[94]))
apts94 <- c(-114.6194, 34.7688)
locations[102, ]
apts98 <- c(-116.00556, 39.6014)
apts102 <- c(-117.863, 33.679)

testdfapts <- data.frame(cbind(apts1, apts2, apts3, apts4))
## 11, 94, 98, 102
airlons <- coalesce(testdfapts$lon, testdfapts$lon.1, testdfapts$lon.2, testdfapts$lon.3)

airlats <- coalesce(testdfapts$lat, testdfapts$lat.1, testdfapts$lat.2, testdfapts$lat.3)
airlons[11] <- apts11[1]; airlons[94] <- apts94[1]
airlons[98] <- apts98[1]
airlons[102] <- apts102[1]

airlats[11] <- apts11[2]; airlats[94] <- apts94[2]
airlats[98] <- apts98[2]
airlats[102] <- apts102[2]
airlats[1] <- 44.4450; airlons[1] <- -68.3621

geocode(as.character(locations$AirPtCd[6]))
locations[6, ]

## there's like 30 that are incorrect....I'm dropping these for now! # bye
## testing
cbind(updlocations$newairlon, updlocations$citylons)
which(abs(updlocations$newairlon - updlocations$citylons) > 2)
updlocations$newairlon <- airlons
updlocations$newairlat <- airlats

updlocations


# reduced data set 
updlocationsred <- updlocations[abs(updlocations$newairlon -
    updlocations$citylons) < 2, ]
updlocationsred

# here are the 32 cities that need updated airlat and airlon
nrow(updlocations[abs(updlocations$newairlon - updlocations$citylons) > 2, ])
updlocations[abs(updlocations$newairlon - updlocations$citylons) > 2, ]

## Matt 1st 15 airport changes
row6 <- c(-71.4352, 42.9297)
row9 <- c(-72.8869, 41.2679)
row10 <- c(-73.8055, 42.7487)
row14 <- c(-75.4684, 39.1277)
row16 <- c(-76.0224, 43.9957)
row23 <- c(-80.0382, 32.8943)
row24 <- c(-79.9270, 40.3524)
row31 <- c(-81.5935, 38.3714)
row32 <- c(-81.6850, 30.2200)
row33 <- c(-81.7565, 24.5549)
row35 <- c(-83.0735, 40.0775)
row36 <- c(-83.0102, 42.4093)
row38 <- c(-84.3663, 46.4799)
row43 <- c(-86.2945, 39.8306)
row44 <- c(-86.3626, 32.3791)
row49 <- c(-88.0680, 30.6268)


# Erin remaining cities 
row50 <- c(-89.6781, 39.8435)
row51 <- c(-89.9792, 35.0421)
row52 <- c(-90.0264, 30.0385)
row53 <- c(-90.2222, 32.3345)
row54 <- c(-90.1590, 38.5773)
row61 <- c(-93.6571, 32.5019)
row68 <- c(-97.3866, 35.4148)
row72 <- c(-100.7572, 46.7752)
row74 <- c(-101.7075, 35.2203)
row80 <- c(-106.3824, 31.8053)
row85 <- c(-111.7222, 40.2181)
row86 <- c(-111.6710, 35.1404)
row88 <- c(-112.0735, 43.5124)
row93 <- c(-113.8814, 45.1236)
row112 <- c(-149.8419, 61.2159)


row11 <- c(-73.9667, 40.7834)
row57 <- c(-93.0956, 34.4827)


updlocations[6, c(10, 11)] <- row6
updlocations[9, c(10, 11)] <- row9
updlocations[10, c(10, 11)] <- row10
updlocations[14, c(10, 11)] <- row14
updlocations[16, c(10, 11)] <- row16
updlocations[23, c(10, 11)] <- row23
updlocations[24, c(10, 11)] <- row24
updlocations[31, c(10, 11)] <- row31
updlocations[32, c(10, 11)] <- row32
updlocations[33, c(10, 11)] <- row33
updlocations[35, c(10, 11)] <- row35
updlocations[36, c(10, 11)] <- row36
updlocations[38, c(10, 11)] <- row38
updlocations[43, c(10, 11)] <- row43
updlocations[44, c(10, 11)] <- row44
updlocations[49, c(10, 11)] <- row49

updlocations[50, c(10, 11)] <- row50
updlocations[51, c(10, 11)] <- row51
updlocations[52, c(10, 11)] <- row52
updlocations[53, c(10, 11)] <- row53
updlocations[54, c(10, 11)] <- row54
updlocations[61, c(10, 11)] <- row61
updlocations[68, c(10, 11)] <- row68
updlocations[72, c(10, 11)] <- row72
updlocations[74, c(10, 11)] <- row74
updlocations[80, c(10, 11)] <- row80
updlocations[85, c(10, 11)] <- row85
updlocations[86, c(10, 11)] <- row86
updlocations[88, c(10, 11)] <- row88
updlocations[93, c(10, 11)] <- row93
updlocations[112, c(10, 11)] <- row112

updlocations[11, c(10,11)] <- row11
updlocations[57, c(10,11)] <- row57

updlocations[abs(updlocations$newairlon - updlocations$citylons) > 2, ]

updlocations[1, ]

# don't think we need to write the following line 
write.csv(updlocationsred, "~/Desktop/TimeSpaceExpo/doubleupdlocations.csv")


# create a variable that has the distance between city center and new airport lat longs

distance <- distGeo(cbind(updlocations$citylons, updlocations$citylats), 
                 cbind(updlocations$newairlon, updlocations$newairlat))

updlocations$distance <- distance

max(updlocations$distance)
updlocations[which.max(updlocations$distance), ] 

# data set has airport locations, city locations, and distances between the two 
write.csv(updlocations, "~/Desktop/DataExpo2018/dist_locations.csv")
