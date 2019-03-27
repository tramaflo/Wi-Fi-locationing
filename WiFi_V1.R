# WiFi Locationing #
# Floriana Trama #
# Data analysis department #
# Feasibility of using WiFi Fingerprinting to determine a person's location within a building #
# MArch/April 2019 #



# Libraries ---------------------------------------------------------------

library(readr)

library(caret)

library(dplyr)

library(tidyverse)

library(anytime)


# Uploading training and testing dataset ----------------------------------

TrainData <- read_csv("C:/Users/T450S/Desktop/Floriana/Ubiqum/IoT/WiFi/trainingData.csv")

TestData <- read_csv("C:/Users/T450S/Desktop/Floriana/Ubiqum/IoT/WiFi/validationData.csv")

summary(TrainData)

summary(TestData)

str(TrainData)

str(TestData)


# Change time variable from integer to an actual datetime  ----------------

TrainData$TIMESTAMP <- anytime(TrainData$TIMESTAMP)

TestData$TIMESTAMP <- anytime(TestData$TIMESTAMP)



# Subsetting the Waps -----------------------------------------------------

Sub_WAPS <- select(TrainData, WAP001:WAP520)


if(Sub_WAPS<-80 || Sub_WAPS > -30) {
  
}

# Replacing the value of 100 to -106 as 100 is no signal and values -------
# higher than 90 mean that the signal is very low  

Sub_WAPS <- replace(Sub_WAPS, Sub_WAPS==100, -106)





# Subsetting for BUILDING 0

TrainDataBUILDING <- TrainData %>% 
  filter(BUILDINGID == 0)

summary(TrainDataBUILDING$FLOOR)


# Create DF of building 0 per each floors ---------------------------------

BuildingZero0F <- TrainDataBUILDING %>% 
  filter(FLOOR == 0)
  
BuildingZero1F <- TrainDataBUILDING %>% 
  filter(FLOOR == 1)

BuildingZero2F <- TrainDataBUILDING %>% 
  filter(FLOOR == 2)

BuildingZero3F <- TrainDataBUILDING %>% 
  filter(FLOOR == 3)


