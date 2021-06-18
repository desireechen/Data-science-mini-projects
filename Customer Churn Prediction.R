## Load the dataset.
cust.churn <- read.csv("Data Exercise - Customer Churn.csv")
# Look at the structure of the dataset.
str(cust.churn)
# Get the summary. 
summary(cust.churn)
## Data cleaning
# customerID is not required to predict Churn. I will remove customerID column.
cust.churn <- cust.churn[,-c(1)]
# Change SeniorCitizen from integer to factor.
cust.churn$SeniorCitizen <- as.factor(cust.churn$SeniorCitizen)
# Remove NAs. There are other ways to handle missing values such as imputing the average. In this assignment, I will remove missing values.
df.no.NAs <- na.omit(cust.churn) # there are other ways to handle missing values
summary(df.no.NAs)
## Plot.
# Gender and StreamingMovies are not very telling of Churn.
plot(df.no.NAs$gender, df.no.NAs$Churn)
plot(df.no.NAs$StreamingMovies, df.no.NAs$Churn)
# Those with Partner are less likely to Churn.
plot(df.no.NAs$Partner, df.no.NAs$Churn)
# Those with dependents are less likely to Churn.
plot(df.no.NAs$Dependents, df.no.NAs$Churn)
# Seniors are more likely to Churn.
plot(df.no.NAs$SeniorCitizen, df.no.NAs$Churn)
# Those with month-to-month contract are more likely to Churn.
plot(df.no.NAs$Contract, df.no.NAs$Churn)
## Make the dataset balanced.
# install.packages('DMwR')
library(DMwR)
d <- 9876
set.seed(d)
newData <- SMOTE(Churn ~ ., df.no.NAs, perc.over=100, perc.under=200)
summary(newData)
## Separate data into train and validation data.
data.no <- newData[which(newData$Churn == 'No'),]
data.yes <- newData[which(newData$Churn == 'Yes'),]
n.row.no <- nrow(data.no)
n.row.yes <- nrow(data.yes)
# In this assignment, I used 80-20. Other values such as 90-10, 75-25, 50-50 can also be used.
proportion <- 0.8
set.seed(d)
train.index.no <- sample(n.row.no, round(proportion*n.row.no), replace = FALSE)
set.seed(d)
train.index.yes <- sample(n.row.yes, round(proportion*n.row.yes), replace = FALSE)
train.no <- data.no[train.index.no,]
train.yes <- data.yes[train.index.yes,]
df.train <- rbind(train.no, train.yes)
test.no <- data.no[-train.index.no,]
test.yes <- data.yes[-train.index.yes,]
df.test <- rbind(test.no, test.yes)
## Logistic Regression
lr <- glm(Churn ~ ., data=df.train, family=binomial(link = "logit"))
summary(lr)
# Use stepAIC to select the model. There are forward, backward and both directions.
library("MASS")
lr.best <- stepAIC(lr, direction="backward")
summary(lr.best)
# Predict using the best lr model.
df.test$prob.lr <- predict(lr.best, newdata=df.test, type="response")
df.test$pred.lr <- as.factor(ifelse(df.test$prob.lr > 0.5, 1, 0))
table(df.test$pred.lr, df.test$Churn)
## Decision Tree.
library("tree")
tree1 <- tree(Churn ~ ., data=df.train)
# View details of each decision node. Asterisks are the terminal nodes or leaves.
tree1
summary(tree1)
# Use cross-validation to determine optimal tree size.
set.seed(d)
tree1.cv <- cv.tree(tree1, method="misclass")
tree1.cv
plot(tree1.cv)
optimal <- which.min(tree1.cv$dev)
optimal.size <- tree1.cv$size[optimal]
# Prune the tree using the optimal tree size determined by cross-validation.
tree1.prune <- prune.tree(tree1, best=optimal.size, method="misclass")
plot(tree1.prune)
text(tree1.prune)
# Predict using the pruned tree model.
df.test$prob.tree <- predict(tree1.prune, newdata=df.test, type="vector")[, 2]
df.test$pred.tree <- predict(tree1.prune, newdata=df.test, type="class")
table(df.test$pred.tree, df.test$Churn)
## Random Forest
library("randomForest")
set.seed(d)
rf <- randomForest(Churn ~ ., data=df.train, mtry=1, ntree=500, xtest=df.test[, 1:19], y.test=df.test[, 20], keep.forest=TRUE)
plot(rf)
# Predict using the random forest model.
df.test$prob.rf <- rf$test$votes[ ,2]
df.test$pred.rf <- rf$test$predicted
table(df.test$pred.rf, df.test$Churn)
## Bagging (this is randomForest when mtry is equal to the number of variables in x)
set.seed(d)
bag <- randomForest(Churn ~ ., data=df.train, mtry=19, ntree=500, xtest=df.test[, 1:19], y.test=df.test[, 20], keep.forest=TRUE)
# Predict using the bagging model.
df.test$prob.bag <- bag$test$votes[ ,2]
df.test$pred.bag <- bag$test$predicted
table(df.test$pred.bag, df.test$Churn)
## The next 3 models uses package "glmnet" that requires data to be in matrix form.
# Make data into a matrix form.
x <- model.matrix(Churn ~ ., df.train)[,-1]
y <- df.train$Churn
df.test2 <- rbind(test.no, test.yes)
x.test <- model.matrix(Churn ~ ., df.test2)[,-1]
y.test <- df.test$Churn
## Ridge Regression
# install.packages('glmnet')
library("glmnet")
model.ridge <- glmnet(x, y, family="binomial", alpha=0)
set.seed(d)
ridge.cv <- cv.glmnet(x, y, family="binomial", type.measure="class", alpha=0)
ridge.lam <- ridge.cv$lambda.min
# Use lambda.1se as the optimal lambda for a more parsimonious model
ridge.lam2 <- ridge.cv$lambda.1se
ridge.lam2
ridge.prob <- predict(model.ridge, newx=x.test, s=ridge.lam2, family="binomial", type="response", exact=TRUE) 
head(ridge.prob)
summary(ridge.prob)
# Get predictions from Ridge.
ridge.pred2 = ifelse(ridge.prob > 0.5, 1, 0)
# Create confusion mat for Ridge.
confusion.mat.ridge <- table(ridge.pred2, y.test)
misclass.ridge <- (confusion.mat.ridge[2, 1] + confusion.mat.ridge[1, 2]) / nrow(x.test)
misclass.ridge
## LASSO
model.lasso <- glmnet(x, y, family="binomial", alpha=1)
set.seed(d)
lasso.cv <- cv.glmnet(x, y, family="binomial", type.measure="class", alpha=1)
lasso.lam <- lasso.cv$lambda.min
lasso.lam2 <- lasso.cv$lambda.1se
lasso.lam2
lasso.prob <- predict(model.lasso, newx=x.test, s=lasso.lam2, family="binomial", type="response", exact=TRUE) 
head(lasso.prob)
summary(lasso.prob)
# Get predictions from Lasso.
lasso.pred2 = ifelse(lasso.prob > 0.5, 1, 0)
# Create confusion mat for Lasso.
confusion.mat.lasso <- table(lasso.pred2, y.test)
misclass.lasso <- (confusion.mat.lasso[2, 1] + confusion.mat.lasso[1, 2]) / nrow(x.test)
misclass.lasso
## Elastic Net
K <- 7
n <- nrow(x)
fold <- rep(0, n)
set.seed(d)
shuffled.index <- sample(n, n, replace=FALSE)
fold[shuffled.index] <- rep(1:K, length.out=n)
table(fold) # each of the datapoints is going into a chunk
fold # to see which chunk each of the data points goes to
alphas <- seq(0, 1, 0.1)
en2.cv.error <- data.frame(alpha=alphas)
for (i in 1:length(alphas)){
  en2.cv <- cv.glmnet(x, y, alpha=alphas[i], family="binomial", type.measure="class", foldid=fold)
  en2.cv.error[i, "lambda.min"] <- en2.cv$lambda.min
  en2.cv.error[i, "error.min"] <- min(en2.cv$cvm)
  en2.cv.error[i, "lambda.1se"] <- en2.cv$lambda.1se
  en2.cv.error[i, "error.1se"] <- min(en2.cv$cvm) + en2.cv$cvsd[which.min(en2.cv$cvm)]
}
en2.cv.error
en2.lam2 <- en2.cv.error[which.min(en2.cv.error$error.1se), "lambda.1se"]
en2.lam2
en2.alpha2 <- en2.cv.error[which.min(en2.cv.error$error.1se), "alpha"]
en2.alpha2
en2.mod <- glmnet(x, y, family="binomial", alpha=en2.alpha2)
en2.prob <- predict(en2.mod, newx=x.test, s=en2.lam2, family="binomial", type="response", exact=TRUE)
head(en2.prob)
summary(en2.prob)
# Get predictions from Elastic Net.
en2.pred2 = ifelse(en2.prob > 0.5, 1, 0)
# Create confusion mat for Elastic Net.
confusion.mat.en2 <- table(en2.pred2, y.test)
misclass.en2 <- (confusion.mat.en2[2, 1] + confusion.mat.en2[1, 2]) / nrow(x.test)
misclass.en2
## Compare error rates, ROC and AUC values.
library('ROCR')
# Create prediction objects.
lr.pred <- prediction(df.test$prob.lr, df.test$Churn)
tree.pred <- prediction(df.test$prob.tree, df.test$Churn)
rf.pred <- prediction(df.test$prob.rf, df.test$Churn)
bag.pred <- prediction(df.test$prob.bag, df.test$Churn)
ridge.pred <- prediction(ridge.prob, y.test)
lasso.pred <- prediction(lasso.prob, y.test)
en2.pred <- prediction(en2.prob, y.test)
# Create performance objects for the misclassification errors.
lr.error <- performance(lr.pred, measure="err")
tree.error <- performance(tree.pred, measure="err")
rf.error <- performance(rf.pred, measure="err")
bag.error <- performance(bag.pred, measure="err")
ridge.error <- performance(ridge.pred, measure="err")
lasso.error <- performance(lasso.pred, measure="err")
en2.error <- performance(en2.pred, measure="err")
# Plot misclassification errors.
plot(lr.error, ylim=c(0.1,0.6), col="red")
plot(tree.error, add=TRUE, col="green")
plot(rf.error, add=TRUE, col="blue")
plot(bag.error, add=TRUE, col="grey")
plot(ridge.error, add=TRUE, col="yellow")
plot(lasso.error, add=TRUE, col="purple")
plot(en2.error, add=TRUE, col="black")
# Create performance objects for the ROCs.
lr.ROC <- performance(lr.pred, measure="tpr", x.measure="fpr")
tree.ROC <- performance(tree.pred, measure="tpr", x.measure="fpr")
rf.ROC <- performance(rf.pred, measure="tpr", x.measure="fpr")
bag.ROC <- performance(bag.pred, measure="tpr", x.measure="fpr")
ridge.ROC <- performance(ridge.pred, measure="tpr", x.measure="fpr")
lasso.ROC <- performance(lasso.pred, measure="tpr", x.measure="fpr")
en2.ROC <- performance(en2.pred, measure="tpr", x.measure="fpr")
# Plot ROC curves.
plot(lr.ROC, col="red")
plot(tree.ROC, add=TRUE, col="green")
plot(rf.ROC, add=TRUE, col="blue")
plot(bag.ROC, add=TRUE, col="grey")
plot(ridge.ROC, add=TRUE, col="yellow")
plot(lasso.ROC, add=TRUE, col="purple")
plot(en2.ROC, add=TRUE, col="black")
abline(a=0, b=1, lty=2) # diagonal line
# AUC values
as.numeric(performance(lr.pred, "auc")@y.values)
as.numeric(performance(tree.pred, "auc")@y.values)
as.numeric(performance(rf.pred, "auc")@y.values)
as.numeric(performance(bag.pred, "auc")@y.values)
as.numeric(performance(ridge.pred, "auc")@y.values) 
as.numeric(performance(lasso.pred, "auc")@y.values) 
as.numeric(performance(en2.pred, "auc")@y.values)
## Bagging model has the highest AUC.
## Further work to be done: Tune the model. How many trees should we have?
# Need to test for robustness. Does this model work on future data?
# Can explore ensemble method such as SVM and bagging.