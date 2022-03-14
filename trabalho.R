library(dplyr)
library(na.tools)
library(hms)
library(dlookr)
library(tidyverse)
library('rnoaa')
library(lubridate)
require(devtools)
library(plyr)

#############################################################
#                                                           #
#                                                           #
#   Task 1: Data importation, clean-up and pre-processing   #                                                
#                                                           #
#                                                           #
#############################################################

fires_train <- read.table("fires_train.csv", header = TRUE, sep = ",", na.strings = c("NA", "-", ""), encoding = "UTF-8")
fires_test <- read.table("fires_test.csv", header = TRUE, sep = ",", na.strings = c("NA", "-", ""), encoding = "UTF-8")
weather <- read.table("weather.csv", header = TRUE, sep = ",", na.strings = c("NA", "-", ""), encoding = "UTF-8")
weather_max <- read.table("weather_max.csv", header = TRUE, sep = ",", na.strings = c("NA", "-", ""), encoding = "UTF-8")
weather_test <- read.table("weather_test.csv", header = TRUE, sep = ",", na.strings = c("NA", "-", ""), encoding = "UTF-8")
weather_maxtest <- read.table("weather_maxtest.csv", header = TRUE, sep = ",", na.strings = c("NA", "-", ""), encoding = "UTF-8")


fires_train$intentional_cause = as.factor(fires_train$intentional_cause)
levels(fires_train$intentional_cause) <- c("X0", "X1")

summary(fires_train)

#Remove aler_source collumn
fires_train <- select(fires_train, -"alert_source")
fires_test <- select(fires_test, -"alert_source")

#Change types
fires_train$alert_date <- as.Date(fires_train$alert_date)
fires_train$extinction_date <- as.Date(fires_train$extinction_date)
fires_train$firstInterv_date <- as.Date(fires_train$firstInterv_date)
fires_train$firstInterv_hour <- as_hms(fires_train$firstInterv_hour)
fires_train$alert_hour <- as_hms(fires_train$alert_hour)
fires_train$extinction_hour <- as_hms(fires_train$extinction_hour)

fires_test$alert_date <- as.Date(fires_test$alert_date)
fires_test$extinction_date <- as.Date(fires_test$extinction_date)
fires_test$firstInterv_date <- as.Date(fires_test$firstInterv_date)
fires_test$firstInterv_hour <- as_hms(fires_test$firstInterv_hour)
fires_test$alert_hour <- as_hms(fires_test$alert_hour)
fires_test$extinction_hour <- as_hms(fires_test$extinction_hour)


#check if there are missing values
sum(is.na(fires_train))

sum(is.na(fires_train$region))
sum(is.na(fires_train$extinction_date))
sum(is.na(fires_train$extinction_hour))
sum(is.na(fires_train$firstInterv_date))
sum(is.na(fires_train$firstInterv_hour))

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Deal with missing values for the variable region
for (i in 1:nrow(fires_train)) {
  
  if(is.na(fires_train$region[i])) {
    fires_mode <- (subset(fires_train, municipality==fires_train$municipality[i]))
    result <- getmode(fires_mode$region)
    if (is.na(result)){
      fires_mode <- (subset(fires_train, district==fires_train$district[i]))
      result <- getmode(fires_mode$region)
    }
    fires_train$region[i] <- result
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(is.na(fires_test$region[i])) {
    fires_mode <- (subset(fires_test, municipality==fires_test$municipality[i]))
    result <- getmode(fires_mode$region)
    if (is.na(result)){
      fires_mode <- (subset(fires_test, district==fires_test$district[i]))
      result <- getmode(fires_mode$region)
    }
    fires_test$region[i] <- result
  }
}

#Deal with missing values for the variable firstInterv_date
for (i in 1:nrow(fires_train)) {
  
  if(is.na(fires_train$firstInterv_date[i])) {
    fires_train$firstInterv_date[i] <- fires_train$alert_date[i]
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(is.na(fires_test$firstInterv_date[i])) {
    fires_test$firstInterv_date[i] <- fires_test$alert_date[i]
  }
}

#Deal with missing values for the variable firstInterv_hour
for (i in 1:nrow(fires_train)) {
  
  if(is.na(fires_train$firstInterv_hour[i])) {
    fires_train$firstInterv_hour[i] <- fires_train$alert_hour[i]
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(is.na(fires_test$firstInterv_hour[i])) {
    fires_test$firstInterv_hour[i] <- fires_test$alert_hour[i]
  }
}

#Deal with missing values for the variable extinction_date
for (i in 1:nrow(fires_train)) {
  
  if(is.na(fires_train$extinction_date[i])) {
    fires_train$extinction_date[i] <- fires_train$firstInterv_date[i]
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(is.na(fires_test$extinction_date[i])) {
    fires_test$extinction_date[i] <- fires_test$firstInterv_date[i]
  }
}

#Deal with missing values for the variable extinction_hour
for (i in 1:nrow(fires_train)) {
  
  if(is.na(fires_train$extinction_hour[i])) {
    fires_train$extinction_hour[i] <- fires_train$firstInterv_hour[i]
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(is.na(fires_test$extinction_hour[i])) {
    fires_test$extinction_hour[i] <- fires_test$firstInterv_hour[i]
  }
}

#check if there are missing values
sum(is.na(fires_train))
sum(is.na(fires_test))

# split latitude and longitude values in order to get the correct representation

fires_train <- fires_train %>% separate(lat, c("lat_hours", "lat_minutes", "lat_seconds"))
fires_train <- fires_train %>% separate(lon, c("lon_hours", "lon_minutes", "lon_seconds"))


fires_test <- fires_test %>% separate(lat, c("lat_hours", "lat_minutes", "lat_seconds"))
fires_test <- fires_test %>% separate(lon, c("lon_hours", "lon_minutes", "lon_seconds"))


# create new subset without the 00 and 1900 lan_hours 
fires_lanlon <- (subset(fires_train, lat_hours!="00"))
fires_lanlon <- (subset(fires_lanlon, lat_hours!="1900"))

fires_lanlon2 <- (subset(fires_test, lat_hours!="00"))
fires_lanlon2 <- (subset(fires_lanlon2, lat_hours!="1900"))

for(i in 1:nrow(fires_train)) {
  if(is.na(fires_train$lat_hours[i]) || fires_train$lat_hours[i] == "00" || fires_train$lat_hours[i] == "1900"){
    fires_mode <- (subset(fires_lanlon, parish==fires_train$parish[i]))
    if(is.na(getmode(fires_mode$lat_hours))){
      fires_mode <- (subset(fires_lanlon, municipality==fires_train$municipality[i]))
    }
    if(is.na(getmode(fires_mode$lat_hours))){
      fires_mode <- (subset(fires_lanlon, district==fires_train$district[i]))
    }
    fires_train$lat_hours[i] <- getmode(fires_mode$lat_hours)
    fires_train$lat_minutes[i] <- getmode(fires_mode$lat_minutes)
    fires_train$lat_seconds[i] <- getmode(fires_mode$lat_seconds)
    fires_train$lon_hours[i] <- getmode(fires_mode$lon_hours)
    fires_train$lon_minutes[i] <- getmode(fires_mode$lon_minutes)
    fires_train$lon_seconds[i] <- getmode(fires_mode$lon_seconds)
  }
}

for(i in 1:nrow(fires_test)) {
  if(is.na(fires_test$lat_hours[i]) || fires_test$lat_hours[i] == "00" || fires_test$lat_hours[i] == "1900"){
    fires_mode <- (subset(fires_lanlon2, parish==fires_test$parish[i]))
    if(is.na(getmode(fires_mode$lat_hours))){
      fires_mode <- (subset(fires_lanlon2, municipality==fires_test$municipality[i]))
    }
    if(is.na(getmode(fires_mode$lat_hours))){
      fires_mode <- (subset(fires_lanlon2, district==fires_test$district[i]))
    }
    fires_test$lat_hours[i] <- getmode(fires_mode$lat_hours)
    fires_test$lat_minutes[i] <- getmode(fires_mode$lat_minutes)
    fires_test$lat_seconds[i] <- getmode(fires_mode$lat_seconds)
    fires_test$lon_hours[i] <- getmode(fires_mode$lon_hours)
    fires_test$lon_minutes[i] <- getmode(fires_mode$lon_minutes)
    fires_test$lon_seconds[i] <- getmode(fires_mode$lon_seconds)
  }
}

# get correct latitude and longitude values
for(i in 1:nrow(fires_train)) {
  segundos_lat <- as.numeric(fires_train$lat_seconds[i])/60
  segundos_lat <- round(segundos_lat,digits=5)
  minutos_lat <- (segundos_lat+as.numeric(fires_train$lat_minutes[i]))/60
  fires_train$lat[i] <- minutos_lat + as.numeric(fires_train$lat_hours[i])
  
  segundos_lon <- as.numeric(fires_train$lon_seconds[i])/60
  segundos_lon <- round(segundos_lon,digits=5)
  minutos_lon <- (segundos_lon+as.numeric(fires_train$lon_minutes[i]))/60
  fires_train$lon[i] <- (minutos_lon + as.numeric(fires_train$lon_hours[i]))*-1
}

for(i in 1:nrow(fires_test)) {
  segundos_lat <- as.numeric(fires_test$lat_seconds[i])/60
  segundos_lat <- round(segundos_lat,digits=5)
  minutos_lat <- (segundos_lat+as.numeric(fires_test$lat_minutes[i]))/60
  fires_test$lat[i] <- minutos_lat + as.numeric(fires_test$lat_hours[i])
  
  segundos_lon <- as.numeric(fires_test$lon_seconds[i])/60
  segundos_lon <- round(segundos_lon,digits=5)
  minutos_lon <- (segundos_lon+as.numeric(fires_test$lon_minutes[i]))/60
  fires_test$lon[i] <- (minutos_lon + as.numeric(fires_test$lon_hours[i]))*-1
}

fires_train <- subset(fires_train, lon!=is.na(fires_train$lon))

fires_testna <- fires_test %>% drop_na(lon)


for(i in 1:nrow(fires_test)) {
  if(is.na(fires_test$lon[i])){
    fires_test$lon[i] <- median(fires_testna$lon)
  }
}

#remove lat and lon seconds, minutes and hours
fires_train <- select(fires_train, -"lat_seconds", -"lon_seconds", -"lat_hours", -"lon_hours", -"lat_minutes", -"lon_minutes")
fires_test <- select(fires_test, -"lat_seconds", -"lon_seconds", -"lat_hours", -"lon_hours", -"lat_minutes", -"lon_minutes")

#check if there are missing values
sum(is.na(fires_test))
sum(is.na(fires_train))

fires_train <- select(fires_train, -"firstInterv_date", -"village_area", -"farming_area", -"village_veget_area")
fires_test <- select(fires_test, -"firstInterv_date", -"village_area", -"farming_area", -"village_veget_area")



##################################################

weather <- select(weather, -"qflag")
weather_max <- select(weather_max, -"qflag", -"mflag")

weather_test <- select(weather_test, -"qflag")
weather_maxtest <- select(weather_maxtest, -"qflag", -"mflag")


fires_train <- full_join(fires_train,weather, by="id")
fires_train <- full_join(fires_train,weather_max, by="id")

fires_test <- full_join(fires_test,weather_test, by="id")
fires_test <- full_join(fires_test,weather_maxtest, by="id")

for (i in 1:nrow(fires_train)) {
  
  if(is.na(fires_train$sflag.y[i])) {
    fires_train$sflag.y[i] <- fires_train$sflag.x[i]
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(is.na(fires_test$sflag.y[i])) {
    fires_test$sflag.y[i] <- fires_test$sflag.x[i]
  }
}

fires_train <- select(fires_train, -"id", -"sflag.x")
fires_test <- select(fires_test, -"sflag.x")

for (i in 1:nrow(fires_train)) {
  
  if(is.na(fires_train$tavg[i])) {
    fires_tavg <- (subset(fires_train, alert_date==fires_train$alert_date[i]))
    fires_tavg <- fires_tavg %>% drop_na(tavg)
    result <- round(mean(fires_tavg$tavg))
    if (is.na(result)){
      fires_tavg <- (subset(fires_train, district==fires_train$district[i]))
      fires_tavg <- fires_tavg %>% drop_na(tavg)
      result <- round(mean(fires_tavg$tavg))
    }
    fires_train$tavg[i] <- result
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(is.na(fires_test$tavg[i])) {
    fires_tavg <- (subset(fires_test, alert_date==fires_test$alert_date[i]))
    fires_tavg <- fires_tavg %>% drop_na(tavg)
    result <- round(mean(fires_tavg$tavg))
    if (is.na(result)){
      fires_tavg <- (subset(fires_test, district==fires_test$district[i]))
      fires_tavg <- fires_tavg %>% drop_na(tavg)
      result <- round(mean(fires_tavg$tavg))
    }
    fires_test$tavg[i] <- result
  }
}

for (i in 1:nrow(fires_train)) {
  
  if(is.na(fires_train$tmax[i])) {
    fires_max <- (subset(fires_train, alert_date==fires_train$alert_date[i]))
    fires_max <- fires_max %>% drop_na(tmax)
    result <- round(mean(fires_max$tmax))
    if (is.na(result)){
      fires_max <- (subset(fires_train, district==fires_train$district[i]))
      fires_max <- fires_max %>% drop_na(tmax)
      result <- round(mean(fires_max$tmax))
    }
    fires_train$tmax[i] <- result
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(is.na(fires_test$tmax[i])) {
    fires_max <- (subset(fires_test, alert_date==fires_test$alert_date[i]))
    fires_max <- fires_max %>% drop_na(tmax)
    result <- round(mean(fires_max$tmax))
    if (is.na(result)){
      fires_max <- (subset(fires_test, district==fires_test$district[i]))
      fires_max <- fires_max %>% drop_na(tmax)
      result <- round(mean(fires_max$tmax))
    }
    fires_test$tmax[i] <- result
  }
}

fires_train$mflag = as.factor(fires_train$mflag)
fires_train$sflag.y = as.factor(fires_train$sflag.y)
levels(fires_train$mflag) <- c("H", "N")
levels(fires_train$sflag.y) <- c("E", "N","S")

fires_test$mflag = as.factor(fires_test$mflag)
fires_test$sflag.y = as.factor(fires_test$sflag.y)
levels(fires_test$mflag) <- c("H", "N")
levels(fires_test$sflag.y) <- c("E", "N","S")

for (i in 1:nrow(fires_train)) {
  
  if(is.na(fires_train$mflag[i])) {
    fires_flag <- (subset(fires_train, alert_date==fires_train$alert_date[i]))
    fires_flag <- fires_flag %>% drop_na(mflag)
    result <- getmode(fires_flag$mflag)
    if (is.na(result)){
      fires_flag <- (subset(fires_train, district==fires_train$district[i]))
      fires_flag <- fires_flag %>% drop_na(mflag)
      result <- getmode(fires_flag$mflag)
    }
    fires_train$mflag[i] <- result
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(is.na(fires_test$mflag[i])) {
    fires_flag <- (subset(fires_test, alert_date==fires_test$alert_date[i]))
    fires_flag <- fires_flag %>% drop_na(mflag)
    result <- getmode(fires_flag$mflag)
    if (is.na(result)){
      fires_flag <- (subset(fires_test, district==fires_test$district[i]))
      fires_flag <- fires_flag %>% drop_na(mflag)
      result <- getmode(fires_flag$mflag)
    }
    fires_test$mflag[i] <- result
  }
}

sum(is.na(fires_train$mflag))
sum(is.na(fires_test$mflag))

for (i in 1:nrow(fires_train)) {
  
  if(is.na(fires_train$sflag.y[i])) {
    fires_flag <- (subset(fires_train, alert_date==fires_train$alert_date[i]))
    fires_flag <- fires_flag %>% drop_na(sflag.y)
    result <- getmode(fires_flag$sflag.y)
    if (is.na(result)){
      fires_flag <- (subset(fires_train, district==fires_train$district[i]))
      fires_flag <- fires_flag %>% drop_na(sflag.y)
      result <- getmode(fires_flag$sflag.y)
    }
    fires_train$sflag.y[i] <- result
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(is.na(fires_test$sflag.y[i])) {
    fires_flag <- (subset(fires_test, alert_date==fires_test$alert_date[i]))
    fires_flag <- fires_flag %>% drop_na(sflag.y)
    result <- getmode(fires_flag$sflag.y)
    if (is.na(result)){
      fires_flag <- (subset(fires_test, district==fires_test$district[i]))
      fires_flag <- fires_flag %>% drop_na(sflag.y)
      result <- getmode(fires_flag$sflag.y)
    }
    fires_test$sflag.y[i] <- result
  }
}

sum(is.na(fires_train$sflag.y))
sum(is.na(fires_test$sflag.y))

sum(is.na(fires_train))
sum(is.na(fires_test))

# add seasons column
fires_train['seasons'] <- NA
fires_test['seasons'] <- NA

for (i in 1:nrow(fires_train)) {
  
  if(fires_train$alert_date[i] <= "2014-03-20" || (fires_train$alert_date[i] >= "2015-01-01" && fires_train$alert_date[i] <= "2015-03-20") )
    fires_train$seasons[i] <- "winter"
  else if((fires_train$alert_date[i] >= "2014-03-21" && fires_train$alert_date[i] <= "2014-06-20") || (fires_train$alert_date[i] >= "2015-03-21" && fires_train$alert_date[i] <= "2015-06-20"))
    fires_train$seasons[i] <- "spring"
  else if((fires_train$alert_date[i] >= "2014-06-21" && fires_train$alert_date[i] <= "2014-09-20") || (fires_train$alert_date[i] >= "2015-06-21" && fires_train$alert_date[i] <= "2015-09-20"))
    fires_train$seasons[i] <- "summer"
  else
    fires_train$seasons[i] <- "fall"
}

for (i in 1:nrow(fires_test)) {
  
  if(fires_test$alert_date[i] <= "2014-03-20" || (fires_test$alert_date[i] >= "2015-01-01" && fires_test$alert_date[i] <= "2015-03-20") )
    fires_test$seasons[i] <- "winter"
  else if((fires_test$alert_date[i] >= "2014-03-21" && fires_test$alert_date[i] <= "2014-06-20") || (fires_test$alert_date[i] >= "2015-03-21" && fires_test$alert_date[i] <= "2015-06-20"))
    fires_test$seasons[i] <- "spring"
  else if((fires_test$alert_date[i] >= "2014-06-21" && fires_test$alert_date[i] <= "2014-09-20") || (fires_test$alert_date[i] >= "2015-06-21" && fires_test$alert_date[i] <= "2015-09-20"))
    fires_test$seasons[i] <- "summer"
  else
    fires_test$seasons[i] <- "fall"
}

# add seasons column
fires_train['day_night'] <- NA
fires_test['day_night'] <- NA

for (i in 1:nrow(fires_train)) {
  
  if(fires_train$alert_hour[i] >= "11" && (fires_train$alert_hour[i] <= "18"))
    fires_train$day_night[i] <- "day"
  else {
    fires_train$day_night[i] <- "night"
  }
}

for (i in 1:nrow(fires_test)) {
  
  if(fires_test$alert_hour[i] >= "11" && (fires_test$alert_hour[i] <= "18"))
    fires_test$day_night[i] <- "day"
  else{
    fires_test$day_night[i] <- "night"
  }
}


# split date into year month day
fires_train <- fires_train %>% separate(alert_date, c("year", "month", "day"), sep = "-")
fires_test <- fires_test %>% separate(alert_date, c("year", "month", "day"), sep = "-")


# split date into year month day
fires_train <- fires_train %>% separate(alert_hour, c("alert_hour", "alert_minutes", "alert_seconds"), sep = ":")
fires_test <- fires_test %>% separate(alert_hour, c("alert_hour", "alert_minutes", "alert_seconds"), sep = ":")



# split latitude and longitude values in order to get the correct representation
fires_train <- fires_train %>% separate(firstInterv_hour, c("fi_hours", "fi_minutes", "fi_seconds"))
fires_train <- fires_train %>% separate(extinction_hour, c("ex_hours", "ex_minutes", "ex_seconds"))

# split latitude and longitude values in order to get the correct representation
fires_test <- fires_test %>% separate(firstInterv_hour, c("fi_hours", "fi_minutes", "fi_seconds"))
fires_test <- fires_test %>% separate(extinction_hour, c("ex_hours", "ex_minutes", "ex_seconds"))


# add season´s column
fires_train['time'] <- NA
fires_test['time'] <- NA

fires_train$ex_hours <- as.numeric(fires_train$ex_hours)
fires_train$fi_hours <- as.numeric(fires_train$fi_hours)
fires_train$ex_minutes <- as.numeric(fires_train$ex_minutes)
fires_train$fi_minutes <- as.numeric(fires_train$fi_minutes)
fires_test$ex_hours <- as.numeric(fires_test$ex_hours)
fires_test$fi_hours <- as.numeric(fires_test$fi_hours)
fires_test$ex_minutes <- as.numeric(fires_test$ex_minutes)
fires_test$fi_minutes <- as.numeric(fires_test$fi_minutes)

for (i in 1:nrow(fires_train)) {
  h <- abs(fires_train$ex_hours[i] - fires_train$fi_hours[i])
  mi <- abs(fires_train$ex_minutes[i] - fires_train$fi_minutes[i]) /10
  t = h + mi
  fires_train$time[i] <- t
}

for (i in 1:nrow(fires_test)) {
  h <- abs(fires_test$ex_hours[i] - fires_test$fi_hours[i])
  mi <- abs(fires_test$ex_minutes[i] - fires_test$fi_minutes[i]) /10
  t = h + mi
  fires_test$time[i] <- t
}

#remove lat and lon seconds, minutes and hours
fires_train <- select(fires_train, -"fi_hours", -"fi_minutes", -"fi_seconds", -"ex_hours", -"ex_minutes", -"ex_seconds")
fires_test <- select(fires_test, -"fi_hours", -"fi_minutes", -"fi_seconds", -"ex_hours", -"ex_minutes", -"ex_seconds")


fires_train$month <- as.numeric(fires_train$month)
fires_train$day <- as.numeric(fires_train$day)
fires_train$origin <- as.factor(fires_train$origin)
fires_train$year <- as.factor(fires_train$year)
fires_train$alert_hour <- as.numeric(fires_train$alert_hour)
fires_train$alert_minutes <- as.numeric(fires_train$alert_minutes)
fires_train$seasons <- as.factor(fires_train$seasons)
fires_train$day_night <- as.factor(fires_train$day_night)
fires_train <- select(fires_train, -"alert_seconds")

fires_test$month <- as.numeric(fires_test$month)
fires_test$day <- as.numeric(fires_test$day)
fires_test$origin <- as.factor(fires_test$origin)
fires_test$year <- as.factor(fires_test$year)
fires_test$alert_hour <- as.numeric(fires_test$alert_hour)
fires_test$alert_minutes <- as.numeric(fires_test$alert_minutes)
fires_test$seasons <- as.factor(fires_test$seasons)
fires_test$day_night <- as.factor(fires_test$day_night)
fires_test <- select(fires_test, -"alert_seconds")

summary(fires_test)


#############################################################
#                                                           #
#                                                           #
#             Task 2: Data exploratory analysis             #                                                
#                                                           #
#                                                           #
#############################################################

fires_train %>% group_by(intentional_cause) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(intentional_cause, -n), y = n)) + 
  geom_bar(stat = 'identity', fill = 'orange') + 
  labs(x = '', y = '# of instances (thousands)', title = 'Contagem das variáveis alvo') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(fires_train, aes(day_night, ..count..)) + geom_bar(aes(fill = intentional_cause), position = "dodge", ) + labs(x = '', y = '', title = 'Horas de maior calor')

ggplot(fires_train, aes(seasons, ..count..)) + geom_bar(aes(fill = intentional_cause), position = "dodge", ) + labs(x = '', y = '', title = 'Causas por estações')
fires_flag <- (subset(fires_train, origin=="firepit"))

ggplot(fires_flag, aes(mflag, ..count..)) + geom_bar(aes(fill = intentional_cause), position = "dodge", )

ggplot(fires_train,aes(x=intentional_cause,y=tavg)) + geom_boxplot() + coord_flip() +  coord_cartesian() + labs(x = '', y = '', title = 'Temperatura média')

ggplot(fires_train,aes(x=intentional_cause,y=tmax)) + geom_boxplot() + coord_flip() +  coord_cartesian() + labs(x = '', y = '', title = 'Temperatura máxima')

ggplot(data = fires_train) +
  geom_boxplot(mapping = aes(x = reorder(intentional_cause, vegetation_area, FUN = median), y = tavg)) +
  coord_flip()

ggplot(fires_train, aes(origin, ..count..)) + geom_bar(aes(fill = intentional_cause), position = "dodge", )

ggplot(fires_train, aes(mflag, ..count..)) + geom_bar(aes(fill = intentional_cause), position = "dodge", )

ggplot(fires_train, aes(sflag.y, ..count..)) + geom_bar(aes(fill = intentional_cause), position = "dodge", )

ggplot(fires_train, aes(alert_hour, ..count..)) + geom_bar(aes(fill = intentional_cause), position = "dodge", )

ggplot(fires_train, aes(tmax, ..count..)) + geom_bar(aes(fill = intentional_cause), position = "dodge", )

ggplot(fires_train, aes(tavg, ..count..)) + geom_bar(aes(fill = intentional_cause), position = "dodge", )

ggplot(fires_train, aes(x=time, fill=intentional_cause)) +    geom_histogram(binwidth=2) + labs(fill="")

#############################################################
#                                                           #
#                                                           #
#             Task 3: Predictive modelling                  #                                                
#                                                           #
#                                                           #
#############################################################



library(caret)
library(doParallel)
library(e1071)
library(caretEnsemble) 
library(cutpointr)
library(caTools)
library(DMwR)

library(randomForest)
library(readr)
library(tidyimpute)
library(imputeTS)
library(readxl)

#setting the seed for replicatable results and randomizing entries
set.seed(222)
dataset <- select(fires_train , total_area, lat, lon, month, vegetation_area, origin, alert_hour, tavg, seasons, intentional_cause)
dataset<-na_ma(dataset, k=1)

#create test set and trainning set (using Synthetic minority class oversample to balance the trainnig set)
index = createDataPartition(dataset$intentional_cause, p = .80, list = FALSE)
train = dataset[index, ]
test = dataset[-index, ]



#Setup parallel processing and cross validation
registerDoParallel(4)
getDoParWorkers()

control <- trainControl(method = "repeatedcv",
                        number = 10,
                        index = createFolds(train$intentional_cause, 10),
                        repeats = 3,
                        savePredictions = "final",
                        allowParallel = TRUE,
                        classProbs = TRUE)

set.seed(222)
models <- caretList(intentional_cause ~., data = train,
                    trControl = control,
                    methodList = c("knn","svmLinear","rf","naive_bayes","nnet"),
                    tuneList = NULL,
                    continue_on_fail = FALSE, 
                    preProcess = c("center","scale"))


resamples <- resamples(models)
dotplot(resamples, metric = "Accuracy")


#############################################################
#                                                           #
#                                                           #
#             random forest                                 #                                                
#                                                           #
#                                                           #
#############################################################

treeBoston <- randomForest(intentional_cause ~ total_area + lat + lon + month + vegetation_area + parish + origin + district + alert_hour + tavg + municipality + seasons, fires_train,  ntree = 700, mtry = 2)
print(treeBoston)
pred <- predict(treeBoston, fires_test, importance=T)


importance(treeBoston)
varImpPlot(treeBoston,main="Feature Relevance Scores")

id <- fires_test$id
#id

output.df <- as.data.frame(id)
output.df$intentional_cause <- pred

levels(output.df$intentional_cause) <- c("0", "1")

write.csv(output.df,"kaggle.csv", row.names = FALSE)





