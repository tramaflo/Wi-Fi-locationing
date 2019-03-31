# WiFi Locationing #
# Floriana Trama #
# Data analysis department #
# Feasibility of using WiFi Fingerprinting to determine a person's location within a building #
# MArch/April 2019 #
# Floor predictions



# Libraries ---------------------------------------------------------------

library(readr)

library(caret)

library(dplyr)

library(tidyverse)

library(anytime)

library(tidyr)

library(ggplot2)


# Uploading Training and Validation dataset -------------------------------

TrainData <- read_csv("C:/Users/T450S/Desktop/Floriana/Ubiqum/IoT/WiFi/trainingData.csv")

ValData <- read_csv("C:/Users/T450S/Desktop/Floriana/Ubiqum/IoT/WiFi/validationData.csv")

summary(TrainData)

summary(ValData)

str(TrainData)

str(ValData)


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

plot(TrainData$FLOOR)

plot(TrainData$BUILDINGID)

plot(TrainData$SPACEID)

plot(TrainData$RELATIVEPOSITION)

plot(TrainData$USERID)

plot(TrainData$PHONEID)

plot(TrainData$TIMESTAMP)

plot(TrainData$LONGITUDE)

plot(TrainData$LATITUDE)

plot(TrainData$BUILDINGID, TrainData$LATITUDE)

plot(TrainData$BUILDINGID, TrainData$LONGITUDE)

ggplot(TrainData, aes(LONGITUDE, LATITUDE, color = as.factor(BUILDINGID)))+ 
  geom_jitter(alpha = 0.5)


# Subsetting no signal Waps and Observations in the Training set ----------

Sub_WAPS <- select(TrainData, WAP001:WAP520)

Sub_WAPS[Sub_WAPS > -30 ] <- -106

Sub_WAPS[Sub_WAPS < -80 ] <- -106

listofNoSignalWaps <- apply(Sub_WAPS, 2,var) == 0

Sub_WAPS_Var <- Sub_WAPS[,!listofNoSignalWaps]


# Subsetting no signal Waps and Observations in the Validation set --------

Sub_WAPSTest <- select(ValData, WAP001:WAP520)

Sub_WAPSTest[Sub_WAPSTest > -30 ] <- -106

Sub_WAPSTest[Sub_WAPSTest < -80 ] <- -106

listofNoSignalWapsTest <- apply(Sub_WAPSTest, 2,var) == 0

Sub_WAPS_VarTest <- Sub_WAPSTest[,!listofNoSignalWapsTest]


# Making same columns for Testing and Validation set ----------------------
# Validation set will have more columns but it is not a problem
# If Training set has more column than the Traning one, the additional columns have to be deleted

Sub_WAPS_Var <- Sub_WAPS_Var[, which(colnames(Sub_WAPS_Var)%in% colnames(Sub_WAPS_VarTest))]


# Create a new Training set with the modified WAPs variables --------------

ExtractTrain <- select(TrainData, LONGITUDE, LATITUDE, FLOOR, 
                       BUILDINGID)

TrainData2 <- bind_cols(Sub_WAPS_Var, ExtractTrain) 

rm(ExtractTrain)

rm(Sub_WAPS_Var)


# Create a new Validation set with the modified WAPs variables ------------

ExtractVal <- select(ValData, LONGITUDE, LATITUDE, FLOOR, 
                     BUILDINGID)

ValData2 <- bind_cols(Sub_WAPS_VarTest, ExtractVal) 

rm(ExtractVal)

rm(Sub_WAPS_VarTest)


# Remove rows with zero variance ------------------------------------------

listZVRowsTrain <- apply(TrainData2 %>% select(starts_with("WAP")),
                         1, 
                         var ) == 0

TrainData2 <- TrainData2[!listZVRowsTrain,]

listZVRowsVal <- apply(ValData2 %>% select(starts_with("WAP")),
                       1, 
                       var ) == 0

ValData2 <- ValData2[!listZVRowsVal,]


# Transform "Building" and "Floor" as factor ------------------------------

TrainData2$BUILDINGID <- as.factor(TrainData2$BUILDINGID) 

TrainData2$FLOOR <- as.factor(TrainData2$FLOOR)

ValData2$BUILDINGID <- as.factor(ValData2$BUILDINGID)

ValData2$FLOOR <- as.factor(ValData2$FLOOR)


# Set seed ----------------------------------------------------------------

set.seed(123)


# Cross Validation --------------------------------------------------------

CrossValidation <- trainControl(method = "repeatedcv",
                                number = 10,
                                repeats = 1,
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)

# Training KNN Model ------------------------------------------------------

KNN_FLOOR <- train(FLOOR~., 
                   data = TrainData2, 
                   method = "knn", 
                   tuneLength = 1,
                   trControl = CrossValidation)


# Training results --------------------------------------------------------

KNN_FLOOR


# Predictions on the Validation set ---------------------------------------

FinalPrediction <- predict(KNN_FLOOR, ValData2)

FinalPrediction

Specialtable <- cbind(ValData2, FinalPrediction )

summary(FinalPrediction)


# Calculate error ---------------------------------------------------------
# Attention, after transforming floor in numeric, they range between 1 to 5 and not 0 to 4

Specialtable <- cbind(ValData2, FinalPrediction)

Specialtable$FLOOR <- as.numeric(Specialtable$FLOOR)

Specialtable$FinalPrediction <- as.numeric(Specialtable$FinalPrediction)

error <- abs(Specialtable$FLOOR - Specialtable$FinalPrediction)

Errorplot <- cbind(Specialtable, error)

plot(Specialtable$FinalPrediction, Specialtable$FLOOR)


# GGplot for errors -------------------------------------------------------

ggplot(Errorplot, aes(Specialtable$KNNPrediction2, Specialtable$FLOOR, color = as.factor(BUILDINGID)))+ 
  geom_jitter(alpha = 0.5)


# Error check -------------------------------------------------------------

# Tot errors
TotErrors <- Errorplot %>% 
  filter(error !=0)

hist(Errorplot$error)

# Errors per building
ErrorsBuild0 <- Errorplot %>% 
  filter(BUILDINGID == 0 & error !=0)

Build0 <- Errorplot %>% 
  filter(BUILDINGID == 0)

ErrorsBuild1 <- Errorplot %>% 
  filter(BUILDINGID == 1 & error !=0)

Build1 <- Errorplot %>% 
  filter(BUILDINGID == 1)

ErrorsBuild2 <- Errorplot %>% 
  filter(BUILDINGID == 2 & error !=0)

Build2 <- Errorplot %>% 
  filter(BUILDINGID == 2)

# Errors building 1 per floor (floors from 1 to 4)
ErrorsBuild1Floor1 <- Errorplot %>% 
  filter(BUILDINGID == 1 & FLOOR == 1 & error != 0)

ErrorsBuild1Floor2 <- Errorplot %>% 
  filter(BUILDINGID == 1 & FLOOR == 2 & error != 0)

ErrorsBuild1Floor3 <- Errorplot %>% 
  filter(BUILDINGID == 1 & FLOOR == 3 & error != 0)

ErrorsBuild1Floor4 <- Errorplot %>% 
  filter(BUILDINGID == 1 & FLOOR == 4 & error != 0)

# Errors building 2 per floor (floors from 1 to 5)
ErrorsBuild2Floor1 <- Errorplot %>% 
  filter(BUILDINGID == 2 & FLOOR == 1 & error != 0)

ErrorsBuild2Floor2 <- Errorplot %>% 
  filter(BUILDINGID == 2 & FLOOR == 2 & error != 0)

ErrorsBuild2Floor3 <- Errorplot %>% 
  filter(BUILDINGID == 2 & FLOOR == 3 & error != 0)

ErrorsBuild2Floor4 <- Errorplot %>% 
  filter(BUILDINGID == 2 & FLOOR == 4 & error != 0)

ErrorsBuild2Floor5 <- Errorplot %>% 
  filter(BUILDINGID == 2 & FLOOR == 5 & error != 0)

# Errors building 0 per floor (floors from 1 to 4)
ErrorsBuild0Floor1 <- Errorplot %>% 
  filter(BUILDINGID == 0 & FLOOR == 1 & error != 0)

ErrorsBuild0Floor2 <- Errorplot %>% 
  filter(BUILDINGID == 0 & FLOOR == 2 & error != 0)

ErrorsBuild0Floor3 <- Errorplot %>% 
  filter(BUILDINGID == 0 & FLOOR == 3 & error != 0)

ErrorsBuild0Floor4 <- Errorplot %>% 
  filter(BUILDINGID == 0 & FLOOR == 4 & error != 0)
