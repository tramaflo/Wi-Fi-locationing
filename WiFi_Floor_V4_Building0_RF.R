# WiFi Locationing #
# Floriana Trama #
# Data analysis department #
# Feasibility of using WiFi Fingerprinting to determine a person's location within a building #
# MArch/April 2019 #
# Floor predictions



# Libraries ---------------------------------------------------------------

if(!require(pacman)) install.packages("pacman")

pacman::p_load(readr, caret, tidyverse, anytime)


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
                       BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP)

TrainData2 <- bind_cols(Sub_WAPS_Var, ExtractTrain) 

rm(ExtractTrain)

rm(Sub_WAPS_Var)


# Create a new Validation set with the modified WAPs variables ------------

ExtractVal <- select(ValData, LONGITUDE, LATITUDE, FLOOR, 
                     BUILDINGID, SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP)

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


# B0 observations distribution in Training and Validation  ----------------

Floor0T <- TrainData2 %>% 
  filter(BUILDINGID == 0 & FLOOR == 0)

Floor1T <- TrainData2 %>% 
  filter(BUILDINGID == 0 & FLOOR == 1)

Floor2T <- TrainData2 %>% 
  filter(BUILDINGID == 0 & FLOOR == 2)

Floor3T <- TrainData2 %>% 
  filter(BUILDINGID == 0 & FLOOR == 3)

Floor0V <- ValData2 %>% 
  filter(BUILDINGID == 0 & FLOOR == 0)

Floor1V <- ValData2 %>% 
  filter(BUILDINGID == 0 & FLOOR == 1)

Floor2V <- ValData2 %>% 
  filter(BUILDINGID == 0 & FLOOR == 2)

Floor3V <- ValData2 %>% 
  filter(BUILDINGID == 0 & FLOOR == 3)

DistrFloorT <- TrainData2 %>% 
  filter(BUILDINGID ==0)

hist(DistrFloorT$FLOOR)

DistrFloorV <- ValData2 %>% 
  filter(BUILDINGID ==0)

hist(DistrFloorV$FLOOR)


# Building 0 --------------------------------------------------------------

# Set seed
set.seed(123)


# Cross Validation
CrossValidationB0 <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats = 1,
                                  preProc = c("center", "scale", "range"), 
                                  verboseIter = TRUE)

# Training KNN Model
KNN_FLOORB0 <- train(as.factor(FLOOR) ~., 
                     data = TrainData2 %>% 
                       select(starts_with("WAP"), FLOOR, LONGITUDE, LATITUDE, BUILDINGID) %>% 
                       filter(BUILDINGID == 0), 
                     method = "rf", 
                     tuneLength = 1,
                     trControl = CrossValidationB0)


# Training results
KNN_FLOORB0

Val <- ValData2 %>% 
  filter(BUILDINGID == 0)

Val$FLOOR <- as.factor(Val$FLOOR)

# Predictions on the b0 Validation set
FinalPredictionB0 <- predict(KNN_FLOORB0, Val)

FinalPredictionB0

confusionMatrix( data = FinalPredictionB0,  reference = Val$FLOOR)

summary(FinalPredictionB0)


# Calculate error for B0
# Attention, after transforming floor in numeric, they range between 1 to 5 and not 0 to 4

SpecialtableB0 <- cbind(Val, FinalPredictionB0)

SpecialtableB0$FLOOR <- as.numeric(SpecialtableB0$FLOOR)

SpecialtableB0$FinalPredictionB0 <- as.numeric(SpecialtableB0$FinalPredictionB0)

errorB0 <- abs(SpecialtableB0$FLOOR - SpecialtableB0$FinalPredictionB0)

ErrorplotB0 <- cbind(SpecialtableB0, errorB0)

plot(SpecialtableB0$FinalPredictionB0, SpecialtableB0$FLOOR)


# Error check
# Tot errors

TotErrorsB0 <- ErrorplotB0 %>% 
  filter(errorB0 !=0)

# F1
B0F1 <- ErrorplotB0 %>% 
  filter(FLOOR == 1 & errorB0 !=0)

# F2
B0F2 <- ErrorplotB0 %>% 
  filter(FLOOR == 2 & errorB0 !=0)

# F3
B0F3 <- ErrorplotB0 %>% 
  filter(FLOOR == 3 & errorB0 !=0)

# F4
B0F4 <- ErrorplotB0 %>% 
  filter(FLOOR == 4 & errorB0 !=0)

# ErrorplotB0F1
ErrorplotB0F1 <- ErrorplotB0 %>% 
  filter(FLOOR == 1)

# ErrorplotB0F2
ErrorplotB0F2 <- ErrorplotB0 %>% 
  filter(FLOOR == 2)

# ErrorplotB0F3
ErrorplotB0F3 <- ErrorplotB0 %>% 
  filter(FLOOR == 3)

# ErrorplotB0F4
ErrorplotB0F4 <- ErrorplotB0 %>% 
  filter(FLOOR == 4)


# Floor level ggplot
ggplot(ErrorplotB0F1, aes(LONGITUDE, LATITUDE, color = as.factor(errorB0)))+ 
  geom_jitter(alpha = 0.5)

ggplot(ErrorplotB0F2, aes(LONGITUDE, LATITUDE, color = as.factor(errorB0)))+ 
  geom_jitter(alpha = 0.5)

ggplot(ErrorplotB0F3, aes(LONGITUDE, LATITUDE, color = as.factor(errorB0)))+ 
  geom_jitter(alpha = 0.5)

ggplot(ErrorplotB0F4, aes(LONGITUDE, LATITUDE, color = as.factor(errorB0)))+ 
  geom_jitter(alpha = 0.5)


# Building level ggplot
ggplot(ErrorplotB0, aes(LONGITUDE, LATITUDE, color = as.factor(errorB0)))+ 
  geom_jitter(alpha = 0.5)


# Delete no signal Waps for wrong predictions
listB0F1NoSignal <- apply(B0F1 %>% select(starts_with("WAP")),
                          2, 
                          var ) == 0

B0F1YesSignal <- B0F1[,!listB0F1NoSignal]


# No errors total
NoErrorsB0 <- ErrorplotB0 %>% 
  filter(errorB0 == 0)


# No errors B0F1
NoErrorsB0F1 <-  NoErrorsB0 %>% 
  filter(FLOOR == 1)


# Delete no signal Waps for correct predictions
listB0F1NoSignal2 <- apply(NoErrorsB0F1 %>% select(starts_with("WAP")),
                           2, 
                           var ) == 0

B0F1YesSignal2 <- NoErrorsB0F1[,!listB0F1NoSignal2]


# Plot wrong-correct predictions
# Errors and correct predictions
plot_ly(ErrorplotB0, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#30b1d1')) %>%
  add_markers(color = ~errorB0 == 0, size = 0.05, opacity = 0.5) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))

# Only errors
plot_ly(TotErrorsB0, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorB0 == 0, size = 2) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))


# Change time variable from integer to an actual datetime  ----------------

NoErrorsB0F1$TIMESTAMP <- anytime(NoErrorsB0F1$TIMESTAMP)

TotErrorsB0$TIMESTAMP <- anytime(TotErrorsB0$TIMESTAMP)

write.csv(NoErrorsB0F1, file = "NoErrorsB0F1RF.csv")

write.csv(TotErrorsB0, file = "TotErrorsB0RF.csv")
