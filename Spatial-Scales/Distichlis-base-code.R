setwd("~/Desktop/Harlan-VCR-Project/Data")
library(ggplot2)
library(dplyr)
library(tidyr)
library(rsq)
library(broom)
library(writexl)

EOYBdata <- read.csv("EOYBdata.csv") 

#filter out zones 3 and 4 from sites with more than 5 years data with only the desired columns

tableFull <- EOYBdata %>% select(EOYBYear, marshName, locationID, speciesName, Transect, Replicate, unknownMass, totalMass) %>% 
  filter(locationID %in% c(3,4), Transect %in% c("A","B","C","D"), 
         marshName %in% c("Box Tree", "Cushmans Landing", "Gator Tract", "Hog Island North", 
                          "Hog Island South", "Indiantown", "Oyster", "Steelmans Landing", 
                          "Upper Phillips"))

#Combine both brownsvilles into Brownsville North Left
Brownsville <- select(filter(EOYBdata, locationID %in% c(3,4), marshName %in% c("Brownsville North", "N. Brownsville Left")), c(EOYBYear, marshName, locationID, speciesName, Transect, Replicate, unknownMass, totalMass))
Brownsville$marshName <- "Brownsville North Left"

###################################### Upper Phillips Creek 

UPC <- read.csv("UPC.csv")

UPChighMarsh <- select(filter(UPC, marshRegion == 4), c(EOYBYear, marshRegion, speciesName, locationName, Replicate, unknownMass, totalMass))
UPChighMarsh$marshRegion <- 3
UPChighMarsh <- rename(UPChighMarsh, Transect = locationName)

UPCT <- read.csv("UPC 2.csv")

UPCtransition <- select(filter(UPCT), c(EOYBYear, marshRegion, speciesName, Transect, Replicate, unknownMass, totalMass))
UPCtransition$marshRegion <- 4

UPCtransition <- UPCtransition %>% mutate(Transect = case_when(
  Transect == "PP2" ~ "A", 
  Transect == "E5" ~ "B", 
  Transect == "E1" ~ "C", 
  Transect == "S1" ~ "D"))

upperPhillips <- rbind(UPChighMarsh, UPCtransition)
upperPhillips$marshName <- ("Upper Phillips")
upperPhillips <- rename(upperPhillips, locationID = marshRegion)

#################

tableComplete <- rbind(Brownsville, tableFull, upperPhillips)

tableComplete[ tableComplete == "NA" ] = NA
tableComplete$Transect <- as.character(tableComplete$Transect)
tableComplete$Replicate <- as.character(tableComplete$Replicate)
tableComplete$speciesName <- as.character(tableComplete$speciesName)
tableComplete$totalMass <- as.numeric(tableComplete$totalMass)

tableComplete <- na.omit(tableComplete)

tableComplete <-tableComplete[!(tableComplete$unknownMass > 0),]
tableComplete <- tableComplete[!(tableComplete$totalMass == 0),]

#convert all total biomass values to grams per meter squared 
tableComplete$massPerMeter <- tableComplete$totalMass * 16 

tableComplete$speciesName[tableComplete$speciesName == "Distichlis spicata"] <- "Distichlis.spicata"
tableComplete$speciesName[tableComplete$speciesName == "Spartina patens"] <- "Spartina.patens"

marshes <- c("Box Tree", "Cushmans Landing", "Gator Tract", "Hog Island North", 
             "Hog Island South", "Indiantown", "Oyster", "Steelmans Landing", 
             "Upper Phillips", "Brownsville North Left")

#################### Replicate Scale

HotStart <- select(filter(tableComplete, locationID %in% c("3", "4")), 
                   c(EOYBYear, marshName, locationID, Transect, Replicate, massPerMeter, speciesName)) 

#group by all variables excpet total mass, extract rows with repeated identifiers into new dataframe
repeatKeys <- HotStart %>% group_by_at(vars(-massPerMeter)) %>% filter(n() >1)

#extract all unique rows into its own dataframe
uniqueKeys <- HotStart %>% group_by_at(vars(-massPerMeter)) %>% filter(n() == 1)

#spread wide 
replicateWide <- spread(uniqueKeys, speciesName, massPerMeter)

#convert all NAs in the table to 0s
replicateWide[is.na(replicateWide)] <-0

#create column AllSpeciesBiomass with row sums excluding columns 1:5
replicateWide$AllSpeciesBiomass <- apply(replicateWide[,-c(1:5)], 1, sum) 

#subtract distichlis biomass from total species biomass 
replicateWide$AllSpeciesBiomass <-replicateWide$AllSpeciesBiomass - replicateWide$Distichlis.spicata

#remove rows with less than 2 replicates
replicateRemover <- replicateWide %>% group_by(EOYBYear, marshName, locationID, Transect) %>% filter(n() > 1)

#average the total species mass and distichlis of each replicate within transects
transectScale <- replicateRemover %>% group_by(EOYBYear, marshName, locationID, Transect) %>% summarise_at(c("AllSpeciesBiomass", "Distichlis.spicata"), mean, na.rm = TRUE)

#remove years if there isn't at least two transects
transectRemover <- transectScale %>% group_by(EOYBYear, marshName, locationID) %>% filter(n() > 1)

#average the total species mass and percent distichlis of each transect within a year
zoneScale <- transectRemover %>% group_by(EOYBYear, marshName, locationID) %>% summarise_at(c("AllSpeciesBiomass", "Distichlis.spicata"), mean, na.rm = TRUE)

#filter out high marsh values only
highMarshZone <- zoneScale %>% filter(locationID == 3)

#filter out transition values only
transitionZone <- zoneScale %>% filter(locationID == 4)

#removes year if both zones do not have data
zoneRemover <- zoneScale %>% group_by(EOYBYear, marshName) %>% filter(n() > 1)

#average the total species mass and percent distichlis of each zone within a year
siteScale <- zoneRemover %>% group_by(EOYBYear, marshName) %>% summarise_at(c("AllSpeciesBiomass", "Distichlis.spicata"), mean, na.rm = TRUE)


###################

#this function finds the max value above the mean
SDMax <- function(x){
  Max <- (mean(x)+(2*sd(x)))
  return(Max)
}

#this function finds the min value below the mean
SDMin <- function(x){
  Min <- (mean(x)-(2*sd(x)))
  return(Min)
}
