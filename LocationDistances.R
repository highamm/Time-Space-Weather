## Adds the distances from city center to airport to the locations dataset

library(ggmap)

locations <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/locations.csv")


locations$cityState <- paste(locations$city, locations$state, sep = " ")

locations$cityLongitude <- geocode(locations$cityState)[1]
locations$cityLatitude <- geocode(locations$cityState)[2]

# input NA's manually 

locations2 <- locations

locations2[locations2$city == "Bangor",]$cityLongitude <- geocode("Bangor, ME")[1]
locations2[locations2$city == "Bangor",]$cityLatitude <- geocode("Bangor ME")[2]
locations2[locations2$city == "Portland" & locations2$state == "Maine",]$cityLongitude <- geocode("Portland ME")[1]
locations2[locations2$city == "Manchester",]$cityLongitude <- geocode("Manchester NH")[1]
locations2[locations2$city == "Montpelier",]$cityLatitude <- geocode("Montpelier VT")[2]
locations2[locations2$city == "Albany",]$cityLongitude <- geocode("Albany NY")[1]
locations2[locations2$city == "Atlantic City",]$cityLatitude <- geocode("Atlantic City NJ")[2]
locations2[locations2$city == "Philadelphia",]$cityLatitude <- geocode("Philadelphia PA")[2]
locations2[locations2$city == "Dover",]$cityLatitude <- geocode("Dover DE")[2]
locations2[locations2$city == "Scranton",]$cityLongitude <- geocode("Scranton PA")[1]
locations2[locations2$city == "Watertown",]$cityLongitude <- geocode("Watertown NY")[1]
locations2[locations2$city == "Watertown",]$cityLatitude <- geocode("Watertown NY")[2]

locations3 <- locations2

locations3$stateAbb <- state.abb[match(locations3$state, state.name)]
locations3$cityStateAbb <- paste(locations3$city, locations3$stateAbb, sep = " ")

locations3[is.na(locations$cityLongitude),]
