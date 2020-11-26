#Subsetting data table
###################################
setwd("~/Desktop/VCR_EOYB_Descriptive_Analysis/Data copy")
library(gridExtra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rsq)
library(broom)
library(vegan)
library(writexl)
library(stringr)

#read main EOYB data table into R
EOYBdata <- read.csv("EOYBdata.csv") 

#filter out zones 3 and 4 from sites with more than 5 years data with only the desired columns

tableFull <- EOYBdata %>% select(EOYBYear, marshName, locationID, speciesName, Transect, Replicate, unknownMass, totalMass) %>% 
      filter(locationID %in% c(1,2,3,4), Transect %in% c("A","B","C","D"), 
             marshName %in% c("Box Tree", "Cushmans Landing", "Gator Tract", "Hog Island North", 
                              "Hog Island South", "Indiantown", "Oyster", "Steelmans Landing", 
                              "Upper Phillips"))

#Combine both brownsvilles into Brownsville North Left
Brownsville <- select(filter(EOYBdata, locationID %in% c(3,4), marshName %in% c("Brownsville North", "N. Brownsville Left")), c(EOYBYear, marshName, locationID, speciesName, Transect, Replicate, unknownMass, totalMass))
Brownsville$marshName <- "Brownsville North Left"

###################################### Upper Phillips Creek 

UPC <- read.csv("UPC.csv")

UPCzones <- select(filter(UPC, marshRegion != 3, locationName %in% c("A","B","C","D")), c(EOYBYear, marshRegion, speciesName, locationName, Replicate, unknownMass, totalMass))
UPCzones$marshRegion[ UPCzones$marshRegion == 4 ] = 3
UPCzones <- rename(UPCzones, Transect = locationName)

UPCT <- read.csv("UPC 2.csv")

UPCtransition <- select(filter(UPCT), c(EOYBYear, marshRegion, speciesName, Transect, Replicate, unknownMass, totalMass))
UPCtransition$marshRegion <- 4

UPCtransition <- UPCtransition %>% mutate(Transect = case_when(
      Transect == "PP2" ~ "A", 
      Transect == "E5" ~ "B", 
      Transect == "E1" ~ "C", 
      Transect == "S1" ~ "D"))

upperPhillips <- rbind(UPCzones, UPCtransition)
upperPhillips$marshName <- ("Upper Phillips")
upperPhillips <- rename(upperPhillips, locationID = marshRegion)

######################################

tableComplete <- rbind(Brownsville, upperPhillips, tableFull)

tableComplete[ tableComplete == "NA" ] = NA
tableComplete$Transect <- as.character(tableComplete$Transect)
tableComplete$Replicate <- as.character(tableComplete$Replicate)
tableComplete$speciesName <- as.character(tableComplete$speciesName)
tableComplete$totalMass <- as.numeric(tableComplete$totalMass)
tableComplete$EOYBYear <- as.character(tableComplete$EOYBYear)
tableComplete$locationID <- as.character(tableComplete$locationID)

tableComplete <- na.omit(tableComplete)

tableComplete <-tableComplete[!(tableComplete$unknownMass > 0),]
tableComplete <- tableComplete[!(tableComplete$totalMass == 0),]

#convert all total biomass values to grams per meter squared 
tableComplete$massPerMeter <- tableComplete$totalMass * 16 

marshes <- c("Box Tree", "Cushmans Landing", "Gator Tract", "Hog Island North", 
             "Hog Island South", "Indiantown", "Oyster", "Steelmans Landing", 
             "Brownsville North Left")

################################ Percent Calculations ################



#### full table all zones

massTotal <- sum(tableComplete$massPerMeter)

bySpecies <- split(tableComplete, tableComplete$speciesName)

percentRemover <- function(x){
      
      j <- ncol(x)
      
      k <- as.vector(apply(x[j], 2, sum))
      
      percent <- (k/massTotal)*100
      
}

#calculates the percent mass of each species
percentSpecies <- as.data.frame(lapply(bySpecies, percentRemover))
#shifts the table from all columns to all rows
percentSpecies <- t(percentSpecies)
#converts matrix into data frame
percentSpecies <- as.data.frame(percentSpecies)
#makes a column of species names
percentSpecies$Species <- row.names(percentSpecies)
#adds column names
colnames(percentSpecies) <- c("percentMass", "Species")
#makes an index od the order from highest percent to lowest percent
ndx <- order(percentSpecies$percentMass, decreasing=T)
#creates a data frame with the ordered percents
ordered <- percentSpecies[ndx,]
row.names(ordered) <- NULL







########### zone level ###########################

percentRemover <- function(x){
      
      j <- ncol(x)
      
      k <- as.vector(apply(x[j], 2, sum))
      
      percent <- (k/zoneMassTotal)*100
      
}


byZone <- split(tableComplete, tableComplete$locationID)

zone <- byZone[[4]]

h <- ncol(zone)

zoneMassTotal <- as.vector(apply(zone[h], 2, sum))

zoneSpecies <- split(zone, zone$speciesName)


#calculates the percent mass of each species
percentSpecies <- as.data.frame(lapply(zoneSpecies, percentRemover))
#shifts the table from all columns to all rows
percentSpecies <- t(percentSpecies)
#converts matrix into data frame
percentSpecies <- as.data.frame(percentSpecies)
#makes a column of species names
percentSpecies$Species <- row.names(percentSpecies)
#adds column names
colnames(percentSpecies) <- c("percentMass", "Species")
#makes an index od the order from highest percent to lowest percent
ndx <- order(percentSpecies$percentMass, decreasing=T)
#creates a data frame with the ordered percents
ordered <- percentSpecies[ndx,]
row.names(ordered) <- NULL

write.csv(ordered, "TransitionPercent.csv")
