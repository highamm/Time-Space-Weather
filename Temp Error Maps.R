library(lubridate)
library(ggplot2)
library(leaflet)
library(dplyr)
library(maps)

## Matt only
complete_df <- all.df_completeSub

locations <- read.csv("~/Desktop/DataExpo2018/locations.csv")

complete_df <- read.csv("~/Desktop/DataExpo2018/all_df_completesub.csv")
names(complete_df)[8] <- "forecastValue"

# File paths for Erin
maxtempall <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/maxtempall.csv")
mintempall <- read.csv("~/Desktop/DataExpo2018/Data Expo 2018/mintempall.csv")

# subset the data into sets of only max and min temps 
maxTemp <- subset(maxtempall, weathermeas == "MaxTemp")
minTemp <- subset(mintempall, weathermeas == "MinTemp")

maxTemp$Date <- as.Date(maxTemp$Date)
minTemp$Date <- as.Date(minTemp$Date)

maxTemp$Error <- maxTemp$weatherval - maxTemp$forecastValue
minTemp$Error <- minTemp$weatherval - minTemp$forecastValue

nrow(unique(maxTemp[c("Date", "AirPtCd", "DateofForecast")]))

# split into seasons
maxTemp$month <- month(as.POSIXlt(maxTemp$Date))
minTemp$month <- month(as.POSIXlt(minTemp$Date))

spring_max <- subset(maxTemp, season == "Spring")
summer_max <- subset(maxTemp, season == "Summer")
fall_max <- subset(maxTemp, season == "Fall")
winter_max <- subset(maxTemp, season == "Winter")

spring_min <- subset(minTemp, season == "Spring")
summer_min <- subset(minTemp, season == "Summer")
fall_min <- subset(minTemp, season == "Fall")
winter_min <- subset(minTemp, season == "Winter")


# spring_max_avg_F1 <- (spring_max[spring_max$LengthForecastDayOnly==1 & 
#                                    complete.cases(spring_max), ] %>% 
#   group_by(AirPtCd) %>% summarize(mean_max = mean(weatherval)))
# spring_max_avg_F1$forecast <- (spring_max[spring_max$LengthForecastDayOnly==1 & 
#                                             complete.cases(spring_max), ] %>% 
#   group_by(AirPtCd) %>% summarize(mean_forecast = mean(forecastValue)))$mean_forecast
# 
# 
# spring_max_avg_F1 <- merge(x=spring_max_avg_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
#                     by.y = "AirPtCd", all.x=TRUE)


# compute the average of the errors (error = hist temp - forecast temp)
spring_max_error_F1 <- spring_max[spring_max$LengthForecastDayOnly==1 & 
                                     complete.cases(spring_max), ] %>% 
                          group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
spring_max_error_F1$TrueValGreater <- spring_max_error_F1$mean_error >= 0
spring_max_error_F1 <- merge(x=spring_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                           by.y = "AirPtCd", all.x=TRUE)
spring_max_error_F1$AbsError <- abs(spring_max_error_F1$mean_error)

spring_min_error_F1 <- spring_min[spring_min$LengthForecastDayOnly==1 &
                                    complete.cases(spring_min), ] %>%
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
spring_min_error_F1$TrueValGreater <- spring_min_error_F1$mean_error >= 0  
spring_min_error_F1 <- merge(x=spring_min_error_F1, y=locations[,c("longitude", "latitude", "AirPtCd", "city")], by.x = "AirPtCd", 
                             by.y = "AirPtCd", all.x=TRUE)
spring_min_error_F1$AbsError <- abs(spring_min_error_F1$mean_error)

winter_max_error_F1 <- winter_max[winter_max$LengthForecastDayOnly==1 & 
                                    complete.cases(winter_max), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
winter_max_error_F1$TrueValGreater <- winter_max_error_F1$mean_error >= 0
winter_max_error_F1 <- merge(x=winter_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                             by.y = "AirPtCd", all.x=TRUE)
winter_max_error_F1$AbsError <- abs(winter_max_error_F1$mean_error)

winter_min_error_F1 <- winter_min[winter_min$LengthForecastDayOnly==1 & 
                                    complete.cases(winter_min), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
winter_min_error_F1$TrueValGreater <- winter_min_error_F1$mean_error >= 0
winter_min_error_F1 <- merge(x=winter_min_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                             by.y = "AirPtCd", all.x=TRUE)
winter_min_error_F1$AbsError <- abs(winter_min_error_F1$mean_error)



####
summer_max_error_F1 <- summer_max[summer_max$LengthForecastDayOnly==1 & 
                                    complete.cases(summer_max), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
summer_max_error_F1$TrueValGreater <- summer_max_error_F1$mean_error >= 0
summer_max_error_F1 <- merge(x=summer_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                             by.y = "AirPtCd", all.x=TRUE)
summer_max_error_F1$AbsError <- abs(summer_max_error_F1$mean_error)

summer_min_error_F1 <- summer_min[summer_min$LengthForecastDayOnly==1 & 
                                    complete.cases(summer_min), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
summer_min_error_F1$TrueValGreater <- summer_min_error_F1$mean_error >= 0
summer_min_error_F1 <- merge(x=summer_min_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                             by.y = "AirPtCd", all.x=TRUE)
summer_min_error_F1$AbsError <- abs(summer_min_error_F1$mean_error)


####
####
fall_max_error_F1 <- fall_max[fall_max$LengthForecastDayOnly==1 & 
                                    complete.cases(fall_max), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
fall_max_error_F1$TrueValGreater <- fall_max_error_F1$mean_error >= 0
fall_max_error_F1 <- merge(x=fall_max_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                             by.y = "AirPtCd", all.x=TRUE)
fall_max_error_F1$AbsError <- abs(fall_max_error_F1$mean_error)

fall_min_error_F1 <- fall_min[fall_min$LengthForecastDayOnly==1 & 
                                complete.cases(fall_min), ] %>% 
  group_by(AirPtCd) %>% summarize(mean_error = mean(Error))
fall_min_error_F1$TrueValGreater <- fall_min_error_F1$mean_error >= 0
fall_min_error_F1 <- merge(x=fall_min_error_F1, y=locations[,c("longitude","latitude", "AirPtCd", "city")], by.x = "AirPtCd",
                           by.y = "AirPtCd", all.x=TRUE)
fall_min_error_F1$AbsError <- abs(fall_min_error_F1$mean_error)



# Spring Max Temps Map 1 day forecast
# leaflet(spring_max_avg_F1) %>% addTiles() %>% 
#   addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(forecast^2)*15, 
#              popup=~city) %>%
#   addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(mean_max^2)*15, 
#              popup=~city, col="Green")

pal <- colorFactor(c("navy", "red"), domain = c(TRUE,FALSE))

# Spring Max
leaflet(spring_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average max temp forecast error: Spring")

# Spring Min
leaflet(spring_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average min temp forecast error: Spring")

# Summer Max
leaflet(summer_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Summer")

# Summer Min
leaflet(summer_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Summer")

# Fall Max
leaflet(fall_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Fall")

# Fall Min
leaflet(fall_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Fall")

# Winter Max
leaflet(winter_max_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Winter")

# Winter Min
leaflet(winter_min_error_F1) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat=~latitude, weight=1, radius=~(AbsError^2)*7500, 
             popup=~city, color=~pal(TrueValGreater)) %>%
  addLegend("topright", colors=c("#000080", "#FF0000"), labels=c("Overestimate", "Underestimate"),
            title="Squared average forecast error: Winter")



# Austin 

Austin_fall_max <- fall_max[fall_max$city == "Austin", ] 

summary(Austin_fall_max$weatherval)
summary(Austin_fall_max$forecastValue)

qplot(Austin_fall_max$weatherval, Austin_fall_max$forecastValue)

Austin_spring_min <- spring_min[spring_min$city == "Austin", ] 
qplot(Austin_spring_min$weatherval, Austin_spring_min$forecastValue)


# may be too complicated to include a shiny app in the poster?


###### START SHINY #############
##### Try to animate maps #######

# maxTemp$season <- cut(maxTemp$month, 
#   breaks = c(0.5, 2.5, 5.5, 8.5, 11.5, 12.5), 
#   labels = c("Winter", "Spring", "Summer", "Fall", "Winter2"), 
#   right = FALSE)
# maxtemp$season[maxtemp$season == "Winter2"] <- "Winter"
# maxtemp$season <- factor(maxtemp$season)

maxTempSummary <- maxTemp %>% group_by(city, season, LengthForecastDayOnly) %>%
  summarize(avgError = mean(Error), longitude = mean(longitude), latitude = mean(latitude))
maxTempSummary$TrueValGreater <- maxTempSummary$avgError >= 0
maxTempSummarysub <- subset(maxTempSummary, city == "Albany")

summary(maxTempSummary$LengthForecastDayOnly)

leaflet(data = maxTempSummary) %>% addTiles() %>% 
  addCircles(lng=~longitude, lat =~latitude, weight=1, radius=~(abs(avgError)^2)*7500)

library(shiny)


ui <- navbarPage("Data Expo",
  tabPanel("Leaflet Map",
  
  fluidPage(titlePanel("Title of Leaflet Map"),
  sidebarLayout(
    sidebarPanel(helpText("Words that we want here"),
    sliderInput("time", label = p("Forecast Length"),
      min = min(maxTempSummary$LengthForecastDayOnly), 
      max = max(maxTempSummary$LengthForecastDayOnly), 
      value = min(maxTempSummary$LengthForecastDayOnly),
      step = 1, animate = T) 
  ),
    mainPanel(
      plotOutput("MaxTempMap"),
      leafletOutput("leafletmap")
      
    )

)
)),
  tabPanel("Possible Second Graph",
    fluidPage(
      titlePanel("Second Title")
    )))

server <- function(input, output, session){
  points <- reactive({
    maxTempSummary[maxTempSummary$season == "Winter", ] %>% 
      dplyr::filter(LengthForecastDayOnly == input$time)
  })

  ##output$maxTempMap <- renderLeaflet({
  ##  leaflet() %>% addTiles() %>% 
  ##    addCircles(lng=~longitude, lat =~latitude, weight=1, radius=~(abs(avgError)^2)*7500)
 ## })
 
  
  output$MaxTempMap <- renderPlot({
   test.df <- points()
   ggplot(data = test.df, aes(x = longitude, y = latitude)) +
     geom_point(aes(colour = avgError))
 })
 
output$leafletmap <- renderLeaflet({
  test.df <- points()
    leaflet(data = test.df) %>% addTiles() %>% 
      addCircles(lng=~longitude, lat =~latitude, weight = 1, 
                 radius=~(abs(avgError)^2)*7500, color=~pal(TrueValGreater))
   })
}


shinyApp(ui,server) 

## other notes: use something like this to switch between columns of data:
##   datasetInput <- reactive({
##switch(input$ts,
##  "Johnson & Johnson quarterly" = jj,
##  "CO2" = co2)
##})
##
##  # reactive graph title
##graphtitle <- reactive({
##  switch(input$ts,
##    "Johnson & Johnson quarterly" = "Johnson & Johnson Quarterly Earnings",
##    "CO2" = "Monthly CO2 concentrations")
##})

# reactive axis label
##axislabel <- reactive({
##  switch(input$ts,
##    "Johnson & Johnson quarterly" = "earnings",
##    "CO2" = "ppm")
##})








