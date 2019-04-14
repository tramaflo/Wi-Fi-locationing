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


# B1 observations distribution in Training and Validation  ----------------

Floor0T <- TrainData2 %>% 
  filter(BUILDINGID == 1 & FLOOR == 0)

Floor1T <- TrainData2 %>% 
  filter(BUILDINGID == 1 & FLOOR == 1)

Floor2T <- TrainData2 %>% 
  filter(BUILDINGID == 1 & FLOOR == 2)

Floor3T <- TrainData2 %>% 
  filter(BUILDINGID == 1 & FLOOR == 3)

Floor0V <- ValData2 %>% 
  filter(BUILDINGID == 1 & FLOOR == 0)

Floor1V <- ValData2 %>% 
  filter(BUILDINGID == 1 & FLOOR == 1)

Floor2V <- ValData2 %>% 
  filter(BUILDINGID == 1 & FLOOR == 2)

Floor3V <- ValData2 %>% 
  filter(BUILDINGID == 1 & FLOOR == 3)

DistrFloorT <- TrainData2 %>% 
  filter(BUILDINGID ==1)

hist(DistrFloorT$FLOOR)

DistrFloorV <- ValData2 %>% 
  filter(BUILDINGID ==1)

hist(DistrFloorV$FLOOR)


# Building 1 --------------------------------------------------------------

# Set seed
set.seed(123)


# Cross Validation
CrossValidationB1 <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats = 1,
                                  preProc = c("center", "scale", "range"), 
                                  verboseIter = TRUE)

# Training KNN Model
KNN_FLOORB1 <- train(as.factor(FLOOR) ~., 
                     data = TrainData2 %>% 
                       select(starts_with("WAP"), FLOOR, LONGITUDE, LATITUDE, BUILDINGID) %>% 
                       filter(BUILDINGID == 1), 
                     method = "rf", 
                     tuneLength = 1,
                     trControl = CrossValidationB1)


# Training results
KNN_FLOORB1

Val <- ValData2 %>% 
  filter(BUILDINGID == 1)

Val$FLOOR <- as.factor(Val$FLOOR)

# Predictions on the B1 Validation set
FinalPredictionB1 <- predict(KNN_FLOORB1, Val)

FinalPredictionB1

confusionMatrix( data = FinalPredictionB1,  reference = Val$FLOOR)

summary(FinalPredictionB1)


# Calculate error for B1
# Attention, after transforming floor in numeric, they range between 1 to 5 and not 0 to 4

SpecialtableB1 <- cbind(Val, FinalPredictionB1)

SpecialtableB1$FLOOR <- as.numeric(SpecialtableB1$FLOOR)

SpecialtableB1$FinalPredictionB1 <- as.numeric(SpecialtableB1$FinalPredictionB1)

errorB1 <- abs(SpecialtableB1$FLOOR - SpecialtableB1$FinalPredictionB1)

ErrorplotB1 <- cbind(SpecialtableB1, errorB1)

plot(SpecialtableB1$FinalPredictionB1, SpecialtableB1$FLOOR)


# Error check
# Tot errors
TotErrorsB1 <- ErrorplotB1 %>% 
  filter(errorB1 !=0)

# F1
B1F1 <- ErrorplotB1 %>% 
  filter(FLOOR == 1 & errorB1 !=0)

# F2
B1F2 <- ErrorplotB1 %>% 
  filter(FLOOR == 2 & errorB1 !=0)

# F3
B1F3 <- ErrorplotB1 %>% 
  filter(FLOOR == 3 & errorB1 !=0)

# F4
B1F4 <- ErrorplotB1 %>% 
  filter(FLOOR == 4 & errorB1 !=0)

# ErrorplotB1F1
ErrorplotB1F1 <- ErrorplotB1 %>% 
  filter(FLOOR == 1)

# ErrorplotB1F2
ErrorplotB1F2 <- ErrorplotB1 %>% 
  filter(FLOOR == 2)

# ErrorplotB1F3
ErrorplotB1F3 <- ErrorplotB1 %>% 
  filter(FLOOR == 3)

# ErrorplotB1F4
ErrorplotB1F4 <- ErrorplotB1 %>% 
  filter(FLOOR == 4)


# Floor level ggplot
ggplot(ErrorplotB1F1, aes(LONGITUDE, LATITUDE, color = as.factor(errorB1)))+ 
  geom_jitter(alpha = 0.5)

ggplot(ErrorplotB1F2, aes(LONGITUDE, LATITUDE, color = as.factor(errorB1)))+ 
  geom_jitter(alpha = 0.5)

ggplot(ErrorplotB1F3, aes(LONGITUDE, LATITUDE, color = as.factor(errorB1)))+ 
  geom_jitter(alpha = 0.5)

ggplot(ErrorplotB1F4, aes(LONGITUDE, LATITUDE, color = as.factor(errorB1)))+ 
  geom_jitter(alpha = 0.5)


# Building level ggplot
ggplot(ErrorplotB1, aes(LONGITUDE, LATITUDE, color = as.factor(errorB1)))+ 
  geom_jitter(alpha = 0.5)


# Delete no signal Waps for wrong predictions
listB1F1NoSignal <- apply(B1F1 %>% select(starts_with("WAP")),
                          2, 
                          var ) == 0

B1F1YesSignal <- B1F1[,!listB1F1NoSignal]


# No errors total
NoErrorsB1 <- ErrorplotB1 %>% 
  filter(errorB1 == 0)


# No errors B1F1
NoErrorsB1F1 <-  NoErrorsB1 %>% 
  filter(FLOOR == 1)

# No errors B1F2
NoErrorsB1F2 <-  NoErrorsB1 %>% 
  filter(FLOOR == 2)

# No errors B1F3
NoErrorsB1F3 <-  NoErrorsB1 %>% 
  filter(FLOOR == 3)

# No errors B1F4
NoErrorsB1F4 <-  NoErrorsB1 %>% 
  filter(FLOOR == 4)


# Delete no signal Waps for correct predictions
# F1
listB1F1NoSignal2 <- apply(NoErrorsB1F1 %>% select(starts_with("WAP")),
                           2, 
                           var ) == 0

B1F1YesSignal2 <- NoErrorsB1F1[,!listB1F1NoSignal2]

#F2
listB1F2NoSignal2 <- apply(NoErrorsB1F2 %>% select(starts_with("WAP")),
                           2, 
                           var ) == 0

B1F2YesSignal2 <- NoErrorsB1F2[,!listB1F2NoSignal2]

# F3
listB1F3NoSignal2 <- apply(NoErrorsB1F3 %>% select(starts_with("WAP")),
                           2, 
                           var ) == 0

B1F3YesSignal2 <- NoErrorsB1F3[,!listB1F3NoSignal2]

# F4
listB1F4NoSignal2 <- apply(NoErrorsB1F4 %>% select(starts_with("WAP")),
                           2, 
                           var ) == 0

B1F4YesSignal2 <- NoErrorsB1F4[,!listB1F4NoSignal2]


# Plot wrong-correct predictions
# Errors and correct predictions
plot_ly(ErrorplotB1, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#30b1d1')) %>%
  add_markers(color = ~errorB1 == 0, size = 0.05, opacity = 0.5) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))

# Only errors
plot_ly(TotErrorsB1, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorB1 == 0, size = 2) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))


# Change time variable from integer to an actual datetime  ----------------
# F1
NoErrorsB1F1$TIMESTAMP <- anytime(NoErrorsB1F1$TIMESTAMP)

B1F1$TIMESTAMP <- anytime(B1F1$TIMESTAMP)

# F2
NoErrorsB1F2$TIMESTAMP <- anytime(NoErrorsB1F2$TIMESTAMP)

B1F2$TIMESTAMP <- anytime(B1F2$TIMESTAMP)

# F3
NoErrorsB1F3$TIMESTAMP <- anytime(NoErrorsB1F3$TIMESTAMP)

B1F3$TIMESTAMP <- anytime(B1F3$TIMESTAMP)

# F4
NoErrorsB1F4$TIMESTAMP <- anytime(NoErrorsB1F4$TIMESTAMP)

B1F4$TIMESTAMP <- anytime(B1F4$TIMESTAMP)

# Tot
NoErrorsB1$TIMESTAMP <- anytime(NoErrorsB1$TIMESTAMP)

TotErrorsB1$TIMESTAMP <- anytime(TotErrorsB1$TIMESTAMP)


# Export in csv
# F1
write.csv(NoErrorsB1F1, file = "NoErrorsB1F1.csv")

write.csv(B1F1, file = "B1F1.csv")

# F2
write.csv(NoErrorsB1F2, file = "NoErrorsB1F2.csv")

write.csv(B1F2, file = "B1F2.csv")

# F3
write.csv(NoErrorsB1F3, file = "NoErrorsB1F3.csv")

write.csv(B1F3, file = "B1F3.csv")

# F4
write.csv(NoErrorsB1F4, file = "NoErrorsB1F4.csv")

write.csv(B1F4, file = "B1F4.csv")

# Tot
write.csv(NoErrorsB1, file = "NoErrorsB1RF.csv")

write.csv(TotErrorsB1, file = "TotErrorsB1RF.csv")
