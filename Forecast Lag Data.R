# get maxtempalldat and mintempalldat from data.read.R

# remove unnecessary columns just to make things easier to look at 
lagdat_max <- maxtempalldat[ ,-c(21,22,23,25,28,29,38,39,43,44,45,46,47,48,49,50)]
lagdat_min <- mintempalldat[ ,-c(21,22,23,25,28,29,38,39,43,44,45,46,47,48,49,50)]

# remove forecasts made on the same day or more than 5 days

lagdat_max <- lagdat_max[lagdat_max$LengthForecastDayOnly >= 1 &
                           lagdat_max$LengthForecastDayOnly <= 5, ]
lagdat_min <- lagdat_min[lagdat_min$LengthForecastDayOnly >= 1 &
                           lagdat_min$LengthForecastDayOnly <= 5, ]

lagdat_max_1 <- lagdat_max[lagdat_max$LengthForecastDayOnly == 1, ]
lagdat_max_2 <- lagdat_max[lagdat_max$LengthForecastDayOnly == 2, ]
lagdat_max_3 <- lagdat_max[lagdat_max$LengthForecastDayOnly == 3, ]
lagdat_max_4 <- lagdat_max[lagdat_max$LengthForecastDayOnly == 4, ]
lagdat_max_5 <- lagdat_max[lagdat_max$LengthForecastDayOnly == 5, ]

lagdat_min_1 <- lagdat_min[lagdat_min$LengthForecastDayOnly == 1, ]
lagdat_min_2 <- lagdat_min[lagdat_min$LengthForecastDayOnly == 2, ]
lagdat_min_3 <- lagdat_min[lagdat_min$LengthForecastDayOnly == 3, ]
lagdat_min_4 <- lagdat_min[lagdat_min$LengthForecastDayOnly == 4, ]
lagdat_min_5 <- lagdat_min[lagdat_min$LengthForecastDayOnly == 5, ]

lagdat_max_1_all <- lagdat_max_1 %>% 
  dplyr::group_by_(.dots = c("AirPtCd", "season")) %>%
  dplyr::mutate(lagForecast = lag(weatherval))
lagdat_max_2_all <- lagdat_max_2 %>% 
  dplyr::group_by_(.dots = c("AirPtCd", "season")) %>%
  dplyr::mutate(lagForecast = lag(weatherval,2))
lagdat_max_3_all <- lagdat_max_3 %>% 
  dplyr::group_by_(.dots = c("AirPtCd", "season")) %>%
  dplyr::mutate(lagForecast = lag(weatherval,3))
lagdat_max_4_all <- lagdat_max_4 %>% 
  dplyr::group_by_(.dots = c("AirPtCd", "season")) %>%
  dplyr::mutate(lagForecast = lag(weatherval,4))
lagdat_max_5_all <- lagdat_max_5 %>% 
  dplyr::group_by_(.dots = c("AirPtCd", "season")) %>%
  dplyr::mutate(lagForecast = lag(weatherval,5))

lagdat_min_1_all <- lagdat_min_1 %>% 
  dplyr::group_by_(.dots = c("AirPtCd", "season")) %>%
  dplyr::mutate(lagForecast = lag(weatherval))
lagdat_min_2_all <- lagdat_min_2 %>% 
  dplyr::group_by_(.dots = c("AirPtCd", "season")) %>%
  dplyr::mutate(lagForecast = lag(weatherval,2))
lagdat_min_3_all <- lagdat_min_3 %>% 
  dplyr::group_by_(.dots = c("AirPtCd", "season")) %>%
  dplyr::mutate(lagForecast = lag(weatherval,3))
lagdat_min_4_all <- lagdat_min_4 %>% 
  dplyr::group_by_(.dots = c("AirPtCd", "season")) %>%
  dplyr::mutate(lagForecast = lag(weatherval,4))
lagdat_min_5_all <- lagdat_min_5 %>% 
  dplyr::group_by_(.dots = c("AirPtCd", "season")) %>%
  dplyr::mutate(lagForecast = lag(weatherval,5))

lagdat <- rbind(lagdat_max_1_all,
                lagdat_max_2_all,
                lagdat_max_3_all,
                lagdat_max_4_all,
                lagdat_max_5_all, 
                lagdat_min_1_all,
                lagdat_min_2_all,
                lagdat_min_3_all,
                lagdat_min_4_all,
                lagdat_min_5_all)

# use this data set for predictions using 
lagdat <- lagdat[complete.cases(lagdat$lagForecast), ]

lagdat$errorsimp <- lagdat$lagForecast - lagdat$forecastValue
lagdat$errorsimpsquared <- lagdat$errorsimp^2

lagdat$LengthForecastDayOnly <- as.factor(lagdat$LengthForecastDayOnly)

lagdat_summary <- lagdat %>%
  dplyr::group_by_(.dots = c("AirPtCd", "season", "LengthForecastDayOnly", "weathermeas")) %>%
  dplyr::summarize(mean_error = mean(errorsimp))
lagdat_summary$TrueValGreater <- lagdat_summary$mean_error >= 0
lagdat_summary$MSE <- data.frame(lagdat %>%
  dplyr::group_by_(.dots = c("AirPtCd", "season", "LengthForecastDayOnly", "weathermeas")) %>%
  dplyr::summarize(MSE = mean(errorsimpsquared)))[,5]


library(dplyr)
lagdat_summary$season <- plyr::revalue(lagdat_summary$season, c("Winter"="winter", "Spring"="spring",
  "Fall" = "fall", "Summer" = "summer"))
lagdat_summary$weathermeas <- plyr::revalue(lagdat_summary$weathermeas,
  c("MaxTemp"="max", "MinTemp"="min"))
