# WiFi Locationing #
# Floriana Trama #
# Data analysis department #
# Feasibility of using WiFi Fingerprinting to determine a person's location within a building #
# MArch/April 2019 #



# Libraries ---------------------------------------------------------------

library(readr)

library(caret)



# Uploading dataset -------------------------------------------------------

TrainData <- read_csv("C:/Users/T450S/Desktop/Floriana/Ubiqum/IoT/WiFi/trainingData.csv")

summary(TrainData)

str(TrainData)
