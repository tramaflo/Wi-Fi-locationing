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

library(tidyr)

library(ggplot2)


# Uploading training and testing dataset ----------------------------------

TrainData <- read_csv("C:/Users/T450S/Desktop/Floriana/Ubiqum/IoT/WiFi/trainingData.csv")

TestData <- read_csv("C:/Users/T450S/Desktop/Floriana/Ubiqum/IoT/WiFi/validationData.csv")

summary(TrainData)

summary(TestData)

str(TrainData)

str(TestData)


# Data exploration --------------------------------------------------------

hist(TrainData$BUILDINGID)

hist(TrainData$RELATIVEPOSITION)

hist(TrainData$FLOOR)

grouped_bars <- ggplot(TrainData) +
  geom_bar(aes(FLOOR),stat="count", position="dodge") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_fill_manual(values = c("#1380A1", "#FAAB18")) +
  labs(title="Floors frequency by buildings") +
  facet_wrap(~BUILDINGID)

grouped_bars

# Change time variable from integer to an actual datetime  ----------------

TrainData$TIMESTAMP <- anytime(TrainData$TIMESTAMP)

TestData$TIMESTAMP <- anytime(TestData$TIMESTAMP)


# Subsetting the Waps -----------------------------------------------------

Sub_WAPS <- select(TrainData, WAP001:WAP520)

Sub_WAPS[Sub_WAPS > -30 ] <- -106

Sub_WAPS[Sub_WAPS < -80 ] <- -106

listofNoSignalWaps <- apply(Sub_WAPS, 2,var) == 0

listofNoSignalWaps2 <- apply(Sub_WAPS, 1,var) == 0

Sub_WAPS_withvar <- Sub_WAPS[!listofNoSignalWaps2,!listofNoSignalWaps]








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


