Hoq.precip <- subset(precip, city %in% c("Hoquiam") & month %in% c(3, 4, 5))
Hoq.precip.summer <- subset(precip, city %in% c("Hoquiam") & month %in% c(6,7,8))
Hoq.precip.fall <- subset(precip, city %in% c("Hoquiam") & month %in% c(9,10,11))
Hoq.precip.winter <- subset(precip, city %in% c("Hoquiam") & month %in% c(12,1,2))
Hoq.precip$ForecastTimeDay <- rep(c("Morning", "Evening"),
                                   length.out = nrow(Hoq.precip)) 


subset(Hoq.precip, LengthForecastDayOnly %in% 4)$forecastValue

head(Hoq.precip)

Hoq.precip$precipbinary <- as.numeric(Hoq.precip$weatherval > 0.005)
Hoq.precip.summer$precipbinary <- as.numeric(Hoq.precip.summer$weatherval > 0.005)
Hoq.precip.fall$precipbinary <- as.numeric(Hoq.precip.fall$weatherval > 0.005)
Hoq.precip.winter$precipbinary <- as.numeric(Hoq.precip.winter$weatherval > 0.005)
Hoq.precip$precipbinary


## for each forecastValue, want to figure out the proportion of days that it actually
## rained

Hoq.precip$proprain <- (Hoq.precip %>% group_by(forecastValue, 
                                                  LengthForecastDayOnly, ForecastTimeDay) %>% 
                           mutate(proprain = mean(precipbinary)))$proprain



Hoq.precip$proprain_noMornEv <- (Hoq.precip %>% group_by(forecastValue,
                                                           LengthForecastDayOnly) %>%
                                    mutate(proprain_noMornEv = mean(precipbinary)))$proprain_noMornEv

Hoq.precip.summer$proprain_noMornEv <- (Hoq.precip.summer %>% group_by(forecastValue,
                                                                         LengthForecastDayOnly) %>%
                                           mutate(proprain_noMornEv = mean(precipbinary)))$proprain_noMornEv

Hoq.precip.fall$proprain_noMornEv <- (Hoq.precip.fall %>% group_by(forecastValue,
                                                                     LengthForecastDayOnly) %>%
                                         mutate(proprain_noMornEv = mean(precipbinary)))$proprain_noMornEv

Hoq.precip.winter$proprain_noMornEv <- (Hoq.precip.winter %>% group_by(forecastValue,
                                                                         LengthForecastDayOnly) %>%
                                           mutate(proprain_noMornEv = mean(precipbinary)))$proprain_noMornEv


Hoq.precip$Numerator_noMornEv <- (Hoq.precip %>% group_by(forecastValue,
                                                            LengthForecastDayOnly) %>%
                                     mutate(Numerator_noMornEv = length(precipbinary)))$Numerator_noMornEv

Hoq.precip.summer$Numerator_noMornEv <- (Hoq.precip.summer %>% group_by(forecastValue, 
                                                                          LengthForecastDayOnly) %>%
                                            mutate(Numerator_noMornEv = length(precipbinary)))$Numerator_noMornEv

Hoq.precip.fall$Numerator_noMornEv <- (Hoq.precip.fall %>% group_by(forecastValue, 
                                                                      LengthForecastDayOnly) %>%
                                          mutate(Numerator_noMornEv = length(precipbinary)))$Numerator_noMornEv

Hoq.precip.winter$Numerator_noMornEv <- (Hoq.precip.winter %>% group_by(forecastValue, 
                                                                          LengthForecastDayOnly) %>%
                                            mutate(Numerator_noMornEv = length(precipbinary)))$Numerator_noMornEv



ggplot(data = Hoq.precip, aes(x = forecastValue, y = proprain, colour=ForecastTimeDay)) + 
  geom_point() + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly)

# make the scatter plot without time of day since we don't know if it is correct 
ggplot(data = Hoq.precip, aes(x = forecastValue/100, y = proprain_noMornEv)) + 
  geom_point(aes(alpha=Numerator_noMornEv)) + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly) +
  ggtitle("Hoquiam, Spring") + 
  geom_smooth(aes(weight=Numerator_noMornEv)) +
  scale_alpha_continuous(name = "Number of Obs") + 
  ylab("Proportion of Rainy Days") + 
  xlab("PoP") +
  geom_abline(intercept=0,slope=1)



ggplot(data = Hoq.precip.summer, aes(x = forecastValue/100, y = proprain_noMornEv)) + 
  geom_point(aes(alpha=Numerator_noMornEv)) + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly) +
  ggtitle("Hoquiam, Summer") + 
  geom_smooth(method="lm", formula = y ~ x + I(x^2), size = 0.5, se = FALSE) +
  scale_alpha_continuous(name = "Number of Obs") + 
  ylab("Proportion of Rainy Days") + 
  xlab("PoP") +
  geom_abline(intercept=0,slope=1)


ggplot(data = Hoq.precip.fall, aes(x = forecastValue/100, y = proprain_noMornEv)) + 
  geom_point(aes(alpha=Numerator_noMornEv)) + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly) +
  ggtitle("Hoquiam, Fall") + 
  geom_smooth(method="lm", formula = y ~ x + I(x^2), size = 0.5, se = FALSE) +
  scale_alpha_continuous(name = "Number of Obs") + 
  ylab("Proportion of Rainy Days") + 
  xlab("PoP") +
  geom_abline(intercept=0,slope=1)


ggplot(data = Hoq.precip.winter, aes(x = forecastValue/100, y = proprain_noMornEv)) + 
  geom_point(aes(alpha=Numerator_noMornEv)) + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly) +
  ggtitle("Hoquiam, Winter") + 
  geom_smooth(method="lm", formula = y ~ x + I(x^2), size = 0.5, se = FALSE) +
  scale_alpha_continuous(name = "Number of Obs") + 
  ylab("Proportion of Rainy Days") + 
  xlab("PoP") +
  geom_abline(intercept=0,slope=1)

Hoq.precip$season <- rep("Spring", nrow(Hoq.precip))
Hoq.precip.summer$season <- rep("Summer", nrow(Hoq.precip.summer))
Hoq.precip.fall$season <- rep("Fall", nrow(Hoq.precip.fall))
Hoq.precip.winter$season <- rep("Winter", nrow(Hoq.precip.winter))

Hoq.precip.all <- rbind(Hoq.precip[,-c(20,22)], Hoq.precip.summer, 
                         Hoq.precip.fall, Hoq.precip.winter)


ggplot(data = Hoq.precip.all, aes(x = forecastValue/100, y = proprain_noMornEv)) + 
  geom_point(aes(colour = season,alpha=Numerator_noMornEv)) + ylim(c(0, 1)) +
  facet_wrap(~LengthForecastDayOnly) +
  ggtitle("Hoquiam, WA") + 
  geom_smooth(aes(colour = season, weight = Numerator_noMornEv), method="lm", formula = y ~ x + I(x^2), 
              size = 0.5, se = FALSE) +
  #(aes(colour=season))
  scale_alpha_continuous(name = "Number of Obs") + 
  ylab("Proportion of Rainy Days") + 
  xlab("PoP") +
  geom_abline(intercept=0,slope=1)


summary(subset(Hoq.precip, LengthForecastDayOnly == 6)$forecastValue)
