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

# B2 observations distribution in Training and Validation  ----------------

Floor0T <- TrainData2 %>% 
  filter(BUILDINGID == 2 & FLOOR == 0)

Floor1T <- TrainData2 %>% 
  filter(BUILDINGID == 2 & FLOOR == 1)

Floor2T <- TrainData2 %>% 
  filter(BUILDINGID == 2 & FLOOR == 2)

Floor3T <- TrainData2 %>% 
  filter(BUILDINGID == 2 & FLOOR == 3)

Floor4T <- TrainData2 %>% 
  filter(BUILDINGID == 2 & FLOOR == 4)

Floor0V <- ValData2 %>% 
  filter(BUILDINGID == 2 & FLOOR == 0)

Floor1V <- ValData2 %>% 
  filter(BUILDINGID == 2 & FLOOR == 1)

Floor2V <- ValData2 %>% 
  filter(BUILDINGID == 2 & FLOOR == 2)

Floor3V <- ValData2 %>% 
  filter(BUILDINGID == 2 & FLOOR == 3)

Floor4V <- ValData2 %>% 
  filter(BUILDINGID == 2 & FLOOR == 4)

DistrFloorT <- TrainData2 %>% 
  filter(BUILDINGID ==2)

hist(DistrFloorT$FLOOR)

DistrFloorV <- ValData2 %>% 
  filter(BUILDINGID ==2)

hist(DistrFloorV$FLOOR)


# Building 2 --------------------------------------------------------------

# Set seed
set.seed(123)


# Cross Validation
CrossValidationB2 <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats = 1,
                                  preProc = c("center", "scale", "range"), 
                                  verboseIter = TRUE)

# Training KNN Model
KNN_FLOORB2 <- train(as.factor(FLOOR) ~., 
                     data = TrainData2 %>% 
                       select(starts_with("WAP"), FLOOR, LONGITUDE, LATITUDE, BUILDINGID) %>% 
                       filter(BUILDINGID == 2), 
                     method = "rf", 
                     tuneLength = 1,
                     trControl = CrossValidationB2)


# Training results
KNN_FLOORB2

Val <- ValData2 %>% 
  filter(BUILDINGID == 2)

Val$FLOOR <- as.factor(Val$FLOOR)

# Predictions on the B2 Validation set
FinalPredictionB2 <- predict(KNN_FLOORB2, Val)

FinalPredictionB2

confusionMatrix( data = FinalPredictionB2,  reference = Val$FLOOR)

summary(FinalPredictionB2)


# Calculate error for B2
# Attention, after transforming floor in numeric, they range between 1 to 5 and not 0 to 4

SpecialtableB2 <- cbind(Val, FinalPredictionB2)

SpecialtableB2$FLOOR <- as.numeric(SpecialtableB2$FLOOR)

SpecialtableB2$FinalPredictionB2 <- as.numeric(SpecialtableB2$FinalPredictionB2)

errorB2 <- abs(SpecialtableB2$FLOOR - SpecialtableB2$FinalPredictionB2)

ErrorplotB2 <- cbind(SpecialtableB2, errorB2)

plot(SpecialtableB2$FinalPredictionB2, SpecialtableB2$FLOOR)


# Error check
# Tot errors
TotErrorsB2 <- ErrorplotB2 %>% 
  filter(errorB2 !=0)

# F1
B2F1 <- ErrorplotB2 %>% 
  filter(FLOOR == 1 & errorB2 !=0)

# F2
B2F2 <- ErrorplotB2 %>% 
  filter(FLOOR == 2 & errorB2 !=0)

# F3
B2F3 <- ErrorplotB2 %>% 
  filter(FLOOR == 3 & errorB2 !=0)

# F4
B2F4 <- ErrorplotB2 %>% 
  filter(FLOOR == 4 & errorB2 !=0)

# F5
B2F5 <- ErrorplotB2 %>% 
  filter(FLOOR == 5 & errorB2 !=0)

# ErrorplotB2F1
ErrorplotB2F1 <- ErrorplotB2 %>% 
  filter(FLOOR == 1)

# ErrorplotB2F2
ErrorplotB2F2 <- ErrorplotB2 %>% 
  filter(FLOOR == 2)

# ErrorplotB2F3
ErrorplotB2F3 <- ErrorplotB2 %>% 
  filter(FLOOR == 3)

# ErrorplotB2F4
ErrorplotB2F4 <- ErrorplotB2 %>% 
  filter(FLOOR == 4)

# ErrorplotB2F5
ErrorplotB2F5 <- ErrorplotB2 %>% 
  filter(FLOOR == 5)


# Floor level ggplot
ggplot(ErrorplotB2F1, aes(LONGITUDE, LATITUDE, color = as.factor(errorB2)))+ 
  geom_jitter(alpha = 0.5)

ggplot(ErrorplotB2F2, aes(LONGITUDE, LATITUDE, color = as.factor(errorB2)))+ 
  geom_jitter(alpha = 0.5)

ggplot(ErrorplotB2F3, aes(LONGITUDE, LATITUDE, color = as.factor(errorB2)))+ 
  geom_jitter(alpha = 0.5)

ggplot(ErrorplotB2F4, aes(LONGITUDE, LATITUDE, color = as.factor(errorB2)))+ 
  geom_jitter(alpha = 0.5)

ggplot(ErrorplotB2F5, aes(LONGITUDE, LATITUDE, color = as.factor(errorB2)))+ 
  geom_jitter(alpha = 0.5)


# Building level ggplot
ggplot(ErrorplotB2, aes(LONGITUDE, LATITUDE, color = as.factor(errorB2)))+ 
  geom_jitter(alpha = 0.5)


# Delete no signal Waps for wrong predictions
listB2F1NoSignal <- apply(B2F1 %>% select(starts_with("WAP")),
                          2, 
                          var ) == 0

B2F1YesSignal <- B2F1[,!listB2F1NoSignal]


# No errors total
NoErrorsB2 <- ErrorplotB2 %>% 
  filter(errorB2 == 0)


# No errors B2F1
NoErrorsB2F1 <-  NoErrorsB2 %>% 
  filter(FLOOR == 1)

# No errors B2F2
NoErrorsB2F2 <-  NoErrorsB2 %>% 
  filter(FLOOR == 2)

# No errors B2F3
NoErrorsB2F3 <-  NoErrorsB2 %>% 
  filter(FLOOR == 3)

# No errors B2F4
NoErrorsB2F4 <-  NoErrorsB2 %>% 
  filter(FLOOR == 4)

# No errors B2F5
NoErrorsB2F5 <-  NoErrorsB2 %>% 
  filter(FLOOR == 5)


# Delete no signal Waps for correct predictions
# F1
listB2F1NoSignal2 <- apply(NoErrorsB2F1 %>% select(starts_with("WAP")),
                           2, 
                           var ) == 0

B2F1YesSignal2 <- NoErrorsB2F1[,!listB2F1NoSignal2]

#F2
listB2F2NoSignal2 <- apply(NoErrorsB2F2 %>% select(starts_with("WAP")),
                           2, 
                           var ) == 0

B2F2YesSignal2 <- NoErrorsB2F2[,!listB2F2NoSignal2]

# F3
listB2F3NoSignal2 <- apply(NoErrorsB2F3 %>% select(starts_with("WAP")),
                           2, 
                           var ) == 0

B2F3YesSignal2 <- NoErrorsB2F3[,!listB2F3NoSignal2]

# F4
listB2F4NoSignal2 <- apply(NoErrorsB2F4 %>% select(starts_with("WAP")),
                           2, 
                           var ) == 0

B2F4YesSignal2 <- NoErrorsB2F4[,!listB2F4NoSignal2]

# F5
listB2F5NoSignal2 <- apply(NoErrorsB2F5 %>% select(starts_with("WAP")),
                           2, 
                           var ) == 0

B2F5YesSignal2 <- NoErrorsB2F5[,!listB2F5NoSignal2]


# Plot wrong-correct predictions
# Errors and correct predictions
plot_ly(ErrorplotB2, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#30b1d1')) %>%
  add_markers(color = ~errorB2 == 0, size = 0.05, opacity = 0.5) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))

# Only errors
plot_ly(TotErrorsB2, 
        x = ~LATITUDE, y = ~LONGITUDE, z = ~FLOOR, colors = c('#ff0000','#0800ff')) %>%
  add_markers(color = ~errorB2 == 0, size = 2) %>%
  layout(title = "Wrongly predicted FLOORS",
         scene = list(xaxis = list(title = 'LATITUDE'),
                      yaxis = list(title = 'LONGITUDE'),
                      zaxis = list(title = 'FLOOR')))


# Change time variable from integer to an actual datetime  ----------------
# F1
NoErrorsB2F1$TIMESTAMP <- anytime(NoErrorsB2F1$TIMESTAMP)

B2F1$TIMESTAMP <- anytime(B2F1$TIMESTAMP)

# F2
NoErrorsB2F2$TIMESTAMP <- anytime(NoErrorsB2F2$TIMESTAMP)

B2F2$TIMESTAMP <- anytime(B2F2$TIMESTAMP)

# F3
NoErrorsB2F3$TIMESTAMP <- anytime(NoErrorsB2F3$TIMESTAMP)

B2F3$TIMESTAMP <- anytime(B2F3$TIMESTAMP)

# F4
NoErrorsB2F4$TIMESTAMP <- anytime(NoErrorsB2F4$TIMESTAMP)

B2F4$TIMESTAMP <- anytime(B2F4$TIMESTAMP)

# F5
NoErrorsB2F5$TIMESTAMP <- anytime(NoErrorsB2F5$TIMESTAMP)

B2F5$TIMESTAMP <- anytime(B2F5$TIMESTAMP)

# Tot
NoErrorsB2$TIMESTAMP <- anytime(NoErrorsB2$TIMESTAMP)

TotErrorsB2$TIMESTAMP <- anytime(TotErrorsB2$TIMESTAMP)


# Export in csv
# F1
write.csv(NoErrorsB2F1, file = "NoErrorsB2F1.csv")

write.csv(B2F1, file = "B2F1.csv")

# F2
write.csv(NoErrorsB2F2, file = "NoErrorsB2F2.csv")

write.csv(B2F2, file = "B2F2.csv")

# F3
write.csv(NoErrorsB2F3, file = "NoErrorsB2F3.csv")

write.csv(B2F3, file = "B2F3.csv")

# F4
write.csv(NoErrorsB2F4, file = "NoErrorsB2F4.csv")

write.csv(B2F4, file = "B2F4.csv")

# F5
write.csv(NoErrorsB2F5, file = "NoErrorsB2F5.csv")

write.csv(B2F5, file = "B2F5.csv")

# Tot
write.csv(NoErrorsB2, file = "NoErrorsB2.csv")

write.csv(TotErrorsB2, file = "TotErrorsB2.csv")
