
getwd()
setwd("/Users/hensanity/Desktop/Climate and Marsh Production/Data")

# data was downloaded from NOAA climate at a glance "https://www.ncdc.noaa.gov/cdo-web/orders?email=beckstar44@gmail.com&id=2224468"
# this table has data from 1995-2019 from both the Painter and Wallops stations 

library(dplyr)
library(lubridate)
library(gridExtra)
library(tidyr)
library(ggplot2)

#Wallops 
#Station name = WALLOPS ISLAND NASA TEST FACILITY, VA US
#LAT = 37.93718
#LONG = -75.46629	
#Elevation = 14m

#### Painter
#Station name = PAINTER 2 W, VA US
#LAT = 37.5844	
#LONG = -75.8217	
#Elevation = 9.1m

#get this new table to look like the old tables for Painter and Wallops I used then write two new csvs


wallopsData <- read.csv("wallopsAttributes.csv") %>% select(-c(X, STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, PRCP_ATTRIBUTES,TMAX_ATTRIBUTES, TMIN_ATTRIBUTES))
#converts Date column to class date (shows up as unknown for some reason)
wallopsData$DATE <- as.Date(wallopsData$DATE) 
# how many NAs present in each column?
colSums(is.na(wallopsData[,2:4]))

#reads Painter Data csv into R
painterData <- read.csv("painterAttributes.csv") %>% select(-c(X, STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, PRCP_ATTRIBUTES,TMAX_ATTRIBUTES, TMIN_ATTRIBUTES))
#converts Date column to class date (shows up as unknown for some reason)
painterData$DATE <- as.Date(painterData$DATE) 
# how many NAs present in each column?
colSums(is.na(painterData[,2:4]))

#creates a data frame of each row containing an NA using Dplyr
painterNAs <- painterData %>% filter_all(any_vars(is.na(.))) 
#create vector with Painter dates that need to be filled in with Wallops data
missingDays <- painterNAs$DATE
# creates table without any NAs
painterChopped <- painterData[complete.cases(painterData),]
#creates data frame of wallops data of dates missing in painter data
wallopsFill <- select(filter(wallopsData, DATE %in% missingDays))
#combines wallops data with painter data 
painterComplete <- rbind(painterChopped, wallopsFill)

#adds Temp mean column
painterComplete$TMEAN <- rowMeans(painterComplete[,c("TMAX", "TMIN")])
#create a month column for filtering
painterComplete$Month <- month(painterComplete$DATE)
#create a year column for filtering 
painterComplete$Year <- year(painterComplete$DATE)
#filter out the growing seasons for the desired years and take the mean, sd, and sum of desired columns 
growingSZN <- painterComplete %>% filter(Month %in% c(4:8)) %>% filter(Year %in% c(1999:2019)) %>% 
      group_by(Year=year(DATE)) %>% summarise(meanTMIN=mean(TMIN),
                                              meanTMAX=mean(TMAX),
                                              meanTMEAN=mean(TMEAN),
                                              sdTMEAN=sd(TMEAN),
                                              sdTMIN=sd(TMIN),
                                              sdTMAX=sd(TMAX),
                                              sdPRCP=sd(PRCP),
                                              sumPRCP=sum(PRCP))

###########


#Kiptopeke station ID 
#8632200

#Wachapreague station ID
#8631044

install.packages("rnoaa")

library(rnoaa)
library(lubridate)
library(dplyr)
library(gridExtra)
library(ggplot2)



Kiptopeke <- data.frame()

for (i in c(1999:2019)){
      
      print(paste0("Start: ", i))
      new <- coops_search(station_name = 8632200, begin_date = paste0(i,"0101"), end_date = paste0((i),"1231"), product = "high_low", datum = "STND")
      Kiptopeke <- rbind(Kiptopeke, new$data)
}

colnames(Kiptopeke) <-  c("Date", "waterLevel", "Tide", "Inferred")

Kiptopeke <- Kiptopeke %>% mutate(Inferred = case_when( Inferred == "0,0" ~ F, Inferred == "1,0" ~ T ))

#Inferred <- Kiptopeke[Kiptopeke$Inferred == T,]
#inferredSplit <- split(Inferred, Inferred$Tide)

Kiptopeke$Tide[Kiptopeke$Tide == "HH"] <- "H"
Kiptopeke$Tide[Kiptopeke$Tide == "LL"] <- "L"
Kiptopeke$Tide[Kiptopeke$Tide == "H "] <- "H"
Kiptopeke$Tide[Kiptopeke$Tide == "L "] <- "L"

Kiptopeke$Tide <- as.factor(Kiptopeke$Tide)

Kiptopeke$Month <- month(Kiptopeke$Date)

Kiptopeke$Year <- year(Kiptopeke$Date)

kiptopekeGrowing <- Kiptopeke %>% filter(Month %in% c(4:8)) %>% filter(Year %in% c(1999:2019)) 

kiptopekeSplit <- split(Kiptopeke, Kiptopeke$Tide) 

KiptopekeHigh <- kiptopekeSplit[["H"]] %>% group_by(Year) %>% summarise(meanH=mean(waterLevel), sdH=sd(waterLevel))

KiptopekeHigh$hSD <- kiptopekeSplit[["H"]] %>% group_by(Year) %>% summarise(meanH=mean(waterLevel))

tideFinal <- select(KiptopekeHigh, c(meanH, sdH))

#############


meanH <- ggplot(KiptopekeHigh, aes(Year, meanH,)) + geom_line(color = "Green") + ggtitle("Mean High Tide April-August 1999-2019") + 
      ylab("Height Above Station Datum (m)") + scale_x_continuous(breaks = seq(1999, 2019, by = 2)) + scale_y_continuous(breaks = seq(1.9, 2.7, by = 0.05))


sumPRCP <- ggplot(growingSZN, aes(Year, sumPRCP,)) + geom_line(color = "Red") + ggtitle("Sum of Precipitation April-August 1999-2019") + 
      ylab("Precipitation (mm)") + scale_x_continuous(breaks = seq(1999, 2019, by = 2)) + scale_y_continuous(breaks = seq(200, 950, by = 100))

temp <- ggplot(growingSZN, aes(Year, meanTMEAN,)) + geom_line(color = "Blue") + ggtitle("Mean Daily Temp April-August 1999-2019") + 
      ylab("Temperature (C)") + scale_x_continuous(breaks = seq(1999, 2019, by = 2)) + scale_y_continuous(breaks = seq(19, 24, by = 1))


grid.arrange(temp, sumPRCP, meanH, ncol=1, nrow = 3)




