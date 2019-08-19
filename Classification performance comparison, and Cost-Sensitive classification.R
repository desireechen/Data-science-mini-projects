library(kernlab) # this is for the SVM algorithm
library(caret) # this is for the function "trainControl"
library(rpart)
# Set working directory to where csv file is located.
setwd("~/MSBA/IS5152 Decision Making Technologies/Assignment 2/G12_e0056094_e0056268")
########## Load and prepare data for QUESTION 1
mydata <- read.csv("pima-indians-diabetes.csv")
summary(mydata)
mydata$Class <- as.factor(mydata$Class)
# Remove observations in the dataset where it does not make sense to have zero values
newdata <- mydata[which(mydata$GlucoseTest != 0 & mydata$BloodPressure != 0 
                        & mydata$SkinThickness != 0 & mydata$SerumInsulin != 0
                        & mydata$BMI),]
summary(newdata)
########## Decision Tree algorithm
# train the model to get preliminary best parameter
set.seed(674)
train1_control <- trainControl(method="cv", number=100)
model.dt <- train(Class~., data=newdata, trControl=train1_control, method="rpart")
# summarize result
print(model.dt) # Preliminary best parameter value was cp = 0.03076923
########## Decision Tree algorithm best parameter
# grid search for selecting parameters
grid.dt <- expand.grid(.cp = c(0.01,0.025,0.03076923))
# train the model built using algorithm, and calculate total training time
set.seed(674)
train1_control <- trainControl(method="cv", number=100)
Sys.time() 
model.train.dt <- train(Class~., data=newdata, trControl=train1_control, method="rpart", tuneGrid=grid.dt) 
Sys.time() # It took 2 seconds.
print(model.train.dt) # Final best parameter for decision tree algorithm was cp = 0.025
########## Neural Network algorithm
set.seed(674)
train1_control <- trainControl(method="cv", number=100)
model.nn <- train(Class~., data=newdata, trControl=train1_control, method ="nnet")
print(model.nn) # Preliminary best parameter values were size = 5 and decay = 0.1
########## Neural Network algorithm best parameter
grid.nn <- expand.grid(.size = c(5,6,7), .decay =c(0.1,0.5,1))
set.seed(674)
train1_control <- trainControl(method="cv", number=100)
NN.start <- Sys.time() 
model.train.nn <- train(Class~., data=newdata, trControl=train1_control, method="nnet", tuneGrid=grid.nn) 
NN.end <- Sys.time() # It took 1 min 19 seconds.
print(model.train.nn) # Final best parameters for neural net algorithm were size = 5 and decay = 0.5
########## Support Vector Machine algorithm
set.seed(674)
train1_control <- trainControl(method="cv", number=100)
model.svm <- train(Class~., data=newdata, trControl=train1_control, method ="svmRadial")
print(model.svm) # Preliminary best parameter values were sigma = 0.1256422 and C = 1
########## Support Vector Machine algorithm best parameter
grid.svm <- expand.grid(.sigma = 0.1256422, .C=c(1,1.25,1.5))
set.seed(674)
train1_control <- trainControl(method="cv", number=100)
Sys.time() 
model.train.svm <- train(Class~., data=newdata, trControl=train1_control, method="svmRadial", tuneGrid=grid.svm)
Sys.time() # It took 8 seconds.
print(model.train.svm) # Final best parameter for svm algorithm was C = 1
########## Load and prepare data for QUESTION 2
mydata2 <- read.csv("A2_Q2.csv")
summary(mydata2)
mydata2$X3 <- as.factor(mydata2$X3)
mydata2$X4 <- as.factor(mydata2$X4)
########## Best classifier chosen by Accuracy
# train the model to get best parameters
set.seed(674)
train_control <- trainControl(method="cv", number=10)
model2a <- train(Y~., data=mydata2, trControl=train_control, method="nnet")
# summarize result
print(model2a) # Best parameter values were size = 5 and decay = 1e-04
# Confusion Matrix
probs.model2a <- predict(model2a, mydata2, type = "prob")
threshold <- 0.5
pred.model2a <- factor(ifelse(probs.model2a[, "yes"] > threshold, "yes", "no") )
confusionMatrix(pred.model2a, mydata2$Y) # The accuracy was 0.9939, Kappa was 0.588 and total cost was -6100
########## Best classifier chosen by Kappa
set.seed(674)
train_control <- trainControl(method="cv", number=10)
model2b <- train(Y~., data=mydata2, trControl=train_control, metric="Kappa", method="nnet")
print(model2b) # Best parameter values were size = 5 and decay = 0.1
probs.model2b <- predict(model2b, mydata2, type = "prob")
threshold <- 0.5
pred.model2b <- factor(ifelse(probs.model2b[, "yes"] > threshold, "yes", "no") )
confusionMatrix(pred.model2b, mydata2$Y) # The accuracy was 0.9934, Kappa was 0.539 and total cost was -6600
########## Re-sampling by SMOTE
library(DMwR) # for SMOTE, resampling method to handle unbalanced dataset
set.seed(674)
data2c <- SMOTE(Y ~ ., mydata2, perc.over = 9900, perc.under = 1) # to get 10500Yes and 103No, so that misclassification costs are balanced for No and Yes
summary(data2c$Y)
########## Classifier for re-sampled data using best parameter values chosen by Accuracy
# Place in grid the best parameter values from Method A
grid2ca <- expand.grid(.size = 5, .decay = 0.0001)
set.seed(674)
train_control <- trainControl(method="cv", number=10)
model2ca <- train(Y~., data=data2c, trControl=train_control, method="nnet", tuneGrid=grid2ca)
print(model2ca)  
probs.model2ca <- predict(model2ca, mydata2, type = "prob") # testing is done on the original dataset mydata2
threshold <- 0.5
pred.model2ca <- factor(ifelse(probs.model2ca[, "yes"] > threshold, "yes", "no") )
confusionMatrix(pred.model2ca, mydata2$Y) # The accuracy was 0.0553, Kappa was 0.001 and total cost was -9447
########## Classifier for re-sampled data using best parameter values chosen by Kappa
# Place in grid the best parameter values from Method B
grid2ck <- expand.grid(.size = 5, .decay = 0.1)
set.seed(674)
train_control <- trainControl(method="cv", number=10)
model2ck <- train(Y~., data=data2c, trControl=train_control, metric="Kappa", method="nnet", tuneGrid=grid2ck)
print(model2ck)  
probs.model2ck <- predict(model2ck, mydata2, type = "prob") # testing is done on the original dataset mydata2
threshold <- 0.5
pred.model2ck <- factor(ifelse(probs.model2ck[, "yes"] > threshold, "yes", "no") )
confusionMatrix(pred.model2ck, mydata2$Y) # The accuracy was 0.0776, Kappa was 0.0015 and total cost was -9224
########## Best classifier chosen by Accuracy and then built by thresholding
threshold <- 0.0222 # Best threshold that minimises total cost is 0.0222
pred.model2da <- factor(ifelse(probs.model2a[, "yes"] > threshold, "yes", "no") )
confusionMatrix(pred.model2da, mydata2$Y) # The accuracy was 0.7472, Kappa was 0.0577 and total cost was -2528
########## Best classifier chosen by Kappa and then built by thresholding
threshold <- 0.057 # Best threshold that minimises total cost is 0.057
pred.model2dk <- factor(ifelse(probs.model2b[, "yes"] > threshold, "yes", "no") )
confusionMatrix(pred.model2dk, mydata2$Y) # The accuracy was 0.9565, Kappa was 0.3114 and total cost was -534
########## Performance of classifier to minimise total costs
# Divide data into No and Yes
data.no <- mydata2[which(mydata2$Y == 'no'),]
data.yes <- mydata2[which(mydata2$Y == 'yes'),]
# Find the number of rows
n.row.no <- nrow(data.no)
n.row.yes <- nrow(data.yes)
# Get index for 90% of train and validation each
set.seed(674) # To check robustness, we can change the seed here
train.index.no <- sample(n.row.no, round(0.9 * n.row.no), replace = FALSE)  # sample 90%
train.index.yes <- sample(n.row.yes, round(0.9 * n.row.yes), replace = FALSE)  # sample 90%
# Get 90% of data each for No and Yes
train.no <- data.no[train.index.no,]
train.yes <- data.yes[train.index.yes,]
# Get index for undersampled No data
set.seed(746)
train.index.sample.no <- sample(length(train.index.no), nrow(train.yes), replace = FALSE)
# Get undersampled No data
train.sample.no <- train.no[train.index.sample.no,]
# Rbind the undersampled No data and the train Yes data
train.data <- rbind(train.sample.no, train.yes)
# Get the validation data for No and Yes, and Rbind the data
test.no <- data.no[-train.index.no,]
test.yes <- data.yes[-train.index.yes,]
test.data <- rbind(test.no, test.yes)
# Train a model on the Training Data
set.seed(467)
train_control <- trainControl(method="cv", number=10)
model3 <- train(Y~., data=train.data, trControl=train_control, metric="Kappa", method="nnet")
print(model3) # Best parameter values were size = 5 and decay = 0.1
# Use the trained model to predict on the Training Data and compare misclassification
probs.train.model3 <- predict(model3, train.data, type = "prob")
threshold <- 0.4 # Best threshold that minimises total cost is 0.4
pred.train.model3 <- factor(ifelse(probs.train.model3[, "yes"] > threshold, "yes", "no") )
confusionMatrix(pred.train.model3, train.data$Y) # The accuracy was 0.9309, Kappa was 0.8617 and total cost was -13
# Use the trained model to predict on the Validation Data and compare misclassification
probs.test.model3 <- predict(model3, test.data, type = "prob")
pred.test.model3 <- factor(ifelse(probs.test.model3[, "yes"] > threshold, "yes", "no"))
confusionMatrix(pred.test.model3, test.data$Y) # The accuracy was 0.789, Kappa was 0.075 and total cost was -211
########## DATA CHALLENGE CODES (to use these codes on the simulated test dataset)
mydata2 <- read.csv("A2_Q2.csv") # to replace the csv filename with the simulated test dataset filename
mydata2$X3 <- as.factor(mydata2$X3)
mydata2$X4 <- as.factor(mydata2$X4)
# Divide data into No (majority class) and Yes (minority class)
data.no <- mydata2[which(mydata2$Y == 'no'),]
data.yes <- mydata2[which(mydata2$Y == 'yes'),]
# Find the number of rows
n.row.no <- nrow(data.no)
n.row.yes <- nrow(data.yes)
# Get index for undersampled No data
set.seed(746)
challenge.index.no <- sample(nrow(data.no), nrow(data.yes), replace = FALSE)
# Get undersampled No data
challenge.no <- data.no[challenge.index.no,]
# Rbind the undersampled No data and the Yes data
challenge.data <- rbind(challenge.no, data.yes)
# Train a model on the re-sampled Challenge Data
set.seed(674) 
train3_control <- trainControl(method="cv", number=10)
model.challenge <- train(Y~., data=challenge.data, trControl=train3_control, metric="Kappa", method="nnet")
print(model.challenge)
## Use the trained model to predict on the re-sampled Challenge Data, use tuned threshold to compare misclassification
## probs.model.challenge <- predict(model.challenge, challenge.data, type = "prob")
## threshold <- 0.4 # This is the tuned threshold.
## pred.model.challenge <- factor(ifelse(probs.model.challenge[, "yes"] > threshold, "yes", "no") )
## confusionMatrix(pred.model.challenge, challenge.data$Y) # The accuracy was 0.9238, Kappa was 0.8476 and total cost was -16
# Use the trained model to predict on the simulated test dataset, use tuned threshold to compare misclassification
probs.simulated <- predict(model.challenge, mydata2, type = "prob")
threshold <- 0.4 # This is the tuned threshold.
pred.simulated <- factor(ifelse(probs.simulated[, "yes"] > threshold, "yes", "no") )
confusionMatrix(pred.simulated, mydata2$Y)