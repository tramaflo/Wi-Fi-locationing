# WiFi Locationing #
# Floriana Trama #
# Data analysis department #
# Feasibility of using WiFi Fingerprinting to determine a person's location within a building #
# MArch/April 2019 #
# Building predictions #
# Automatic Random Forest # 


# Libraries ---------------------------------------------------------------

if(!require(pacman)) install.packages("pacman")

pacman::p_load(readr, caret, tidyverse, anytime, magrittr, BBmisc)


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

ggplot(TrainData, aes(LONGITUDE, LATITUDE, color = as.factor(BUILDINGID)))+ 
  geom_jitter(alpha = 0.5)

ggplot(TrainData, aes(BUILDINGID, FLOOR, color = as.factor(FLOOR)))+ 
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


# Transform "Building" as factor ------------------------------------------

TrainData2$BUILDINGID <- as.factor(TrainData2$BUILDINGID) 

ValData2$BUILDINGID <- as.factor(ValData2$BUILDINGID)


# Set seed ----------------------------------------------------------------

set.seed(123)


# Create 75%/25% training and test sets -----------------------------------

inTraining <- createDataPartition(TrainData2$BUILDINGID, p = .75, list = FALSE)

training <- TrainData2[inTraining,]

testing <- TrainData2[-inTraining,]


# Cross Validation --------------------------------------------------------

CrossValidation <- trainControl(method = "repeatedcv",
                                number = 3,
                                repeats = 1,
                                preProc = c("center", "scale", "range"), 
                                verboseIter = TRUE)

# Training RF Model ------------------------------------------------------

rf_BUILDING <- train(BUILDINGID~., 
                data = training, 
                method = "rf", trControl=CrossValidation, 
                tuneLength = 1)


# Training results --------------------------------------------------------

rf_BUILDING


# Predictions -------------------------------------------------------------

rfPrediction <- predict(rf_BUILDING, testing)

rfPrediction

postResample(rfPrediction, testing$BUILDINGID)


# Predictions on the Validation set ---------------------------------------

FinalPrediction <- predict(rf_BUILDING, ValData2)

FinalPrediction

Specialtable <- cbind(ValData2, FinalPrediction )

summary(FinalPrediction)

confusionMatrix( data = FinalPrediction,  reference = ValData2$BUILDINGID)
