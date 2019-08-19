# Install and load rpart package.
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("DMwR")
library("rpart")
library("rpart.plot")
library(DMwR)
set.seed(674)
get_mase = function(a, x)
{ mae = 0
  b = x$E1
  c = x$price1
  count = 0
  for (i in 1:nrow(x))
  { tmp = abs(b[i] - a[i])/c[i]
    if (!is.na(tmp))
    { mae = mae + tmp
      count = count + 1
    }
  }
  return(mae/count)
}
error_sum_1 = c(0, 0, 0, 0, 0, 0)
# Set working directory to where csv file is located.
setwd("~/MSBA/IS5152 Decision Making Technologies/Assignment 1/G12_e0056268_e0056094")
mydata <- read.csv("IS5152_A1_SIC2K2 - G12.csv")
mydata$NegE <- as.factor(mydata$NegE)
# Split the dataset for training and testing.
train.data <- subset(mydata, mydata$year <= "2010" & mydata$year > "2000")
train.data.noblankE1 <- subset(train.data, E1 != "")
test.data <- subset(mydata, mydata$year >= "2011")
test.data.noblankE1 <- subset(test.data, E1 != "")
# Install a package for statistic description which provides the number of missing values.
# install.packages("pastecs")
library(boot)
library(pastecs)
stat.desc(train.data, basic=TRUE, desc=FALSE)
stat.desc(train.data.noblankE1, basic=TRUE, desc=FALSE)
stat.desc(test.data, basic=TRUE, desc=FALSE)
stat.desc(test.data.noblankE1, basic=TRUE, desc=FALSE)
# We can tell that attributes B and TACC have missing values nbr.na
#################
# Build the decision tree for METHOD2A which is drop attribute B
method2a = E1 ~ E + NegE + ENegE + TACC
train.model2a <- rpart(method2a, data = train.data.noblankE1, method = "anova", control = rpart.control(cp = 0.0001))
## summary(train.model2a)
## printcp(train.model2a)
## rpart.plot(train.model2a)
# Prediction performance on training dataset.
pred.train.model2a <- vector(mode = "list",length =nrow(train.data.noblankE1))
pred.train.model2a = predict(train.model2a, train.data.noblankE1, type="vector")
# length(pred.train.model2a)
error_sum_1[1]=get_mase(pred.train.model2a, train.data.noblankE1)
#################
# Build the decision tree for METHOD 2B which is drop attribute TACC
method2b = E1 ~ E + NegE + ENegE + B
train.model2b <- rpart(method2b, data = train.data.noblankE1, method = "anova", control = rpart.control(cp = 0.0001))
# Prediction performance on training dataset.
pred.train.model2b <- vector(mode = "list",length =nrow(train.data.noblankE1))
pred.train.model2b = predict(train.model2b, train.data.noblankE1, type="vector")
error_sum_1[2]=get_mase(pred.train.model2b, train.data.noblankE1)
#################
train.data.model4.noblankE1 = train.data.noblankE1
tmp_data = 0
for(j in c(17, 18))
{ mis = which(is.na(train.data.model4.noblankE1[,j]))
for(i in mis)
  { end = train.data.model4.noblankE1[,13][i]
    sep = end - 2000
    if(sep != 0)
    { year = train.data.model4.noblankE1[,13][i]
      tmp_data = train.data.model4.noblankE1[train.data.model4.noblankE1[,13] == year,]
      value = mean(tmp_data[,j], na.rm = T)
      train.data.model4.noblankE1[,j][i] = value
    }
  }
}
# Build the decision tree for METHOD4.
method = E1 ~ E + NegE + ENegE + B + TACC
train.model4 <- rpart(method, data = train.data.model4.noblankE1, method = "anova", control = rpart.control(cp = 0.0001))
# Prediction performance on training dataset.
pred.train.model4 <- vector(mode = "list",length =nrow(train.data.noblankE1))
pred.train.model4 = predict(train.model4, train.data.model4.noblankE1, type="vector")
error_sum_1[3]=get_mase(pred.train.model4, train.data.noblankE1)
#################
train.data.model6.noblankE1 = train.data.noblankE1
tmp_data = 0
tmp_data2 = 0
for(j in c(17, 18))
{ mis = which(is.na(train.data.model6.noblankE1[,j]))
for(i in mis)
  { end = train.data.model6.noblankE1[,13][i]
    sep = end - 2001
    if(sep > 1)
    { year = train.data.model6.noblankE1[,13][i]
    tmp_data = train.data.model6.noblankE1[train.data.model6.noblankE1[,13] == (year-1),]
    value1 = mean(tmp_data[,j], na.rm = T)
    tmp_data2 = train.data.model6.noblankE1[train.data.model6.noblankE1[,13] == (year-2),]
    value2 = mean(tmp_data2[,j], na.rm = T)
    value = (value1 + value2) / 2
    train.data.model6.noblankE1[,j][i] = value
    }
  }
}
# Build the decision tree for METHOD6.
train.model6 <- rpart(method, data = train.data.model6.noblankE1, method = "anova", control = rpart.control(cp = 0.0001))
# Prediction performance on training dataset.
pred.train.model6 <- vector(mode = "list",length =nrow(train.data.noblankE1))
pred.train.model6 = predict(train.model6, train.data.model6.noblankE1, type="vector")
error_sum_1[4]=get_mase(pred.train.model6, train.data.noblankE1)
#################
# Build the tree for METHOD8A where we predict values of B
train.model8a <- rpart(B ~ E + NegE + ENegE + TACC, data = train.data[!is.na(train.data$B), ], 
                       method="anova", na.action=na.omit, control = rpart.control(cp = 0.0001))
# Prune the tree using the best cp.
bestcp.train.model8a <- train.model8a$cptable[which.min(train.model8a$cptable[,"xerror"]),"CP"]
tree.pruned.train.model8a <- prune(train.model8a, cp = bestcp.train.model8a)
# Predict 2098 values of B using pruned tree.
pred.Bs <- vector(mode = "list",length =nrow(train.data))
pred.Bs = predict(tree.pruned.train.model8a, train.data, type="vector")
# Predict 2079 values of available B using pruned tree.
pred.B <- predict(tree.pruned.train.model8a, train.data[!is.na(train.data$B), ], type="vector")
# Performance of rpart in predicting values of B
regr.eval(train.data[!is.na(train.data$B), ]$B, pred.B) # comparing available B and the predicted 2079 values of B
#################
# Build the tree for METHOD8B where we predict values of TACC
train.model8b <- rpart(TACC ~ E + NegE + ENegE + B, data = train.data[!is.na(train.data$TACC), ], 
                       method="anova", na.action=na.omit, control = rpart.control(cp = 0.0001))
# Prune the tree using the best cp.
bestcp.train.model8b <- train.model8b$cptable[which.min(train.model8b$cptable[,"xerror"]),"CP"]
tree.pruned.train.model8b <- prune(train.model8b, cp = bestcp.train.model8b)
# Predict 2098 values of TACC using pruned tree.
pred.TACCs <- vector(mode = "list",length =nrow(train.data))
pred.TACCs = predict(tree.pruned.train.model8b, train.data, type="vector")
# Predict 1865 values of available TACC using pruned tree.
pred.TACC <- predict(tree.pruned.train.model8b, train.data[!is.na(train.data$TACC), ], type="vector")
# Performance of rpart in predicting values of TACC
regr.eval(train.data[!is.na(train.data$TACC), ]$TACC, pred.TACC) # comparing available TACC and the predicted 1865 values of TACC
#################
train.data.model8 = train.data
impute <- function (a, a.impute){ 
  ifelse (is.na(a), a.impute, a)
}
train.data.model8$B = impute (train.data.model8$B, pred.Bs) 
train.data.model8$TACC = impute (train.data.model8$TACC, pred.TACCs) 
train.data.model8.noblankE1 <- subset(train.data.model8, E1 != "")
# Build the decision tree for METHOD8.
train.model8 <- rpart(method, data = train.data.model8.noblankE1, method = "anova", control = rpart.control(cp = 0.0001))
# Prediction performance on training dataset.
pred.train.model8 <- vector(mode = "list",length =nrow(train.data.noblankE1))
pred.train.model8 = predict(train.model8, train.data.model8.noblankE1, type="vector")
error_sum_1[5]=get_mase(pred.train.model8, train.data.noblankE1)
#################
# Build the decision tree for METHOD9.
train.model9 <- rpart(method, data = train.data.noblankE1, method = "anova", control = rpart.control(cp = 0.0001))
# Prediction performance on training dataset.
pred.train.model9 <- vector(mode = "list",length =nrow(train.data.noblankE1))
pred.train.model9 = predict(train.model9, train.data.noblankE1, type="vector")
error_sum_1[6]=get_mase(pred.train.model9, train.data.noblankE1)
################# 
error_sum_2 = c(0, 0, 0, 0, 0, 0, 0)
test.data.model2a = test.data.noblankE1[,-17]
# Prediction performance on validation dataset using METHOD2A comparing with similar suitable tree from training dataset.
pred.test.model2a <- vector(mode = "list",length =nrow(test.data.noblankE1))
pred.test.model2a = predict(train.model2a, test.data.noblankE1, type="vector")
error_sum_2[1]=get_mase(pred.test.model2a, test.data.noblankE1)
#################
test.data.model2b = test.data.noblankE1[,-18]
# Prediction performance on validation dataset using METHOD2B comparing with similar suitable tree from training dataset.
pred.test.model2b <- vector(mode = "list",length =nrow(test.data.noblankE1))
pred.test.model2b <- predict(train.model2b, test.data.noblankE1, type="vector")
error_sum_2[2]=get_mase(pred.test.model2b, test.data.noblankE1)
#################
test.data.model4 = test.data.noblankE1
tmp_data = 0
for(j in c(17, 18))
{ mis = which(is.na(test.data.model4[,j]))
  for(i in mis)
    { year = test.data.model4[,13][i]
      tmp_data = test.data.model4[test.data.model4[,13] == year,]
      value = mean(tmp_data[,j], na.rm = T)
      test.data.model4[,j][i] = value
    }
}
# Prediction performance on validation dataset using METHOD4.
pred.test.model4 <- vector(mode = "list",length =nrow(test.data.noblankE1))
pred.test.model4 = predict(train.model6, test.data.model4, type="vector")
error_sum_2[3]=get_mase(pred.test.model4, test.data.noblankE1)
#################
test.data.model6 = test.data.noblankE1
tmp_data = 0
for(j in c(17, 18))
{ mis = which(is.na(test.data.model6[,j]))
  for(i in mis)
    { year = test.data.model6[,13][i]
      tmp_data = test.data.model6[test.data.model6[,13] == (year-1),]
      value1 = mean(tmp_data[,j], na.rm = T)
      tmp_data2 = test.data.model6[test.data.model6[,13] == (year-2),]
      value2 = mean(tmp_data2[,j], na.rm = T)
      value = (value1 + value2) / 2
      test.data.model6[,j][i] = value
    }
}
# Prediction performance on validation dataset using METHOD6.
pred.test.model6 <- vector(mode = "list",length =nrow(test.data.noblankE1))
pred.test.model6 = predict(train.model6, test.data.model6, type="vector")
error_sum_2[4]=get_mase(pred.test.model6, test.data.noblankE1)
#################
# Build the tree for TESTMETHOD8A where we predict values of B
test.model8a <- rpart(B ~ E + NegE + ENegE + TACC, data = test.data[!is.na(test.data$B), ], 
                       method="anova", na.action=na.omit, control = rpart.control(cp = 0.0001))
# Prune the tree using the best cp.
bestcp.test.model8a <- test.model8a$cptable[which.min(test.model8a$cptable[,"xerror"]),"CP"]
tree.pruned.test.model8a <- prune(test.model8a, cp = bestcp.test.model8a)
# Predict 638 values of B using pruned tree.
pred.testBs <- vector(mode = "list",length =nrow(test.data))
pred.testBs = predict(tree.pruned.test.model8a, test.data, type="vector")
# Predict 635 values of available B using pruned tree.
pred.testB <- predict(tree.pruned.test.model8a, test.data[!is.na(test.data$B), ], type="vector")
# Performance of rpart in predicting values of B
regr.eval(test.data[!is.na(test.data$B), ]$B, pred.testB) # comparing available B and the predicted 635 values of B
#################
# Build the tree for TESTMETHOD8B where we predict values of TACC
test.model8b <- rpart(TACC ~ E + NegE + ENegE + B, data = test.data[!is.na(test.data$TACC), ], 
                      method="anova", na.action=na.omit, control = rpart.control(cp = 0.0001))
# Prune the tree using the best cp.
bestcp.test.model8b <- test.model8b$cptable[which.min(test.model8b$cptable[,"xerror"]),"CP"]
tree.pruned.test.model8b <- prune(test.model8b, cp = bestcp.test.model8b)
# Predict 638 values of TACC using pruned tree.
pred.testTACCs <- vector(mode = "list",length =nrow(test.data))
pred.testTACCs = predict(tree.pruned.test.model8b, test.data, type="vector")
# Predict 552 values of available TACC using pruned tree.
pred.testTACC <- predict(tree.pruned.test.model8b, test.data[!is.na(test.data$TACC), ], type="vector")
# Performance of rpart in predicting values of TACC
regr.eval(test.data[!is.na(test.data$TACC), ]$TACC, pred.testTACC) # comparing available TACC and the predicted 552 values of TACC
#################
test.data.model8 = test.data
impute <- function (a, a.impute){ 
  ifelse (is.na(a), a.impute, a)
}
test.data.model8$B = impute (test.data.model8$B, pred.testBs) 
test.data.model8$TACC = impute (test.data.model8$TACC, pred.testTACCs) 
test.data.model8.noblankE1 <- subset(test.data.model8, E1 != "")
# Prediction performance on validation dataset using METHOD8.
pred.test.model8 <- vector(mode = "list",length =nrow(test.data.model8.noblankE1))
pred.test.model8 = predict(train.model6, test.data.model8.noblankE1, type="vector")
error_sum_2[5]=get_mase(pred.test.model8, test.data.model8.noblankE1)
#################
# Build the decision tree for TESTMETHOD9.
test.model9 <- rpart(method, data = test.data.noblankE1, method = "anova", control = rpart.control(cp = 0.0001))
# Prediction performance on validation dataset using METHOD9.
pred.test.model9 <- vector(mode = "list",length =nrow(test.data.noblankE1))
pred.test.model9 <- predict(test.model9, test.data.noblankE1, type="vector")
error_sum_2[6]=get_mase(pred.test.model9, test.data.noblankE1)
#################
# EXTRA Using BEST CLASSIFIER without any imputation method and obtain prediction performance on validation dataset.
pred.test <- vector(mode = "list",length =nrow(test.data.noblankE1))
pred.test = predict(train.model6, test.data.noblankE1, type="vector")
error_sum_2[7]=get_mase(pred.test, test.data.noblankE1)
#################
newdata <- mydata[c(4, 6, 7, 8, 10, 12, 13, 14, 15, 16, 17, 18, 19, 21)]
# Split the dataset for training and testing.
train.data3 <- subset(newdata, newdata$year <= "2010" & newdata$year > "2000")
test.data3 <- subset(newdata, newdata$year >= "2011")
test.data3.noblankE1 <- subset(test.data3, E1 != "")
# Impute missing values of xad, xsga, B and TACC using Method6.
train.data5 = train.data3
tmp_data = 0
for(j in c(4, 5, 11, 12))
{ mis = which(is.na(train.data5[,j]))
for(i in mis)
  { end = train.data5[,7][i]
    sep = end - 2001
    if(sep > 1)
    { year = train.data5[,7][i]
      tmp_data = train.data5[train.data5[,7] == (year-1),]
      value1 = mean(tmp_data[,j], na.rm = T)
      tmp_data2 = train.data5[train.data5[,7] == (year-2),]
      value2 = mean(tmp_data2[,j], na.rm = T)
      value = (value1 + value2) / 2
      train.data5[,j][i] = value
    }
  }
}
# Then, impute remaining missing values of xad, xsga, B and TACC using Method4.
train.data7 = train.data5
tmp_data = 0
for(j in c(4, 5, 11, 12))
{ mis = which(is.na(train.data7[,j]))
for(i in mis)
  { end = train.data7[,7][i]
    sep = end - 2000
    if(sep != 0)
    { year = train.data7[,7][i]
      tmp_data = train.data7[train.data7[,7] == year,]
      value = mean(tmp_data[,j], na.rm = T)
      train.data7[,j][i] = value
    }
  }
}
# Create new variable profitmargin in training dataset.
train.data7$profitmargin<-(train.data7$ni/train.data7$sale)
# Fit a regression model to see which variables are statistically significant.
lm.model <- lm(E1 ~ . , data = train.data7)
summary(lm.model) # variables sale, year, E, B and price1 are statistically significant
#################
error_sum_3 = c(0, 0, 0)
# Build the decision tree, using statistically significant variables and include the new variable profitmargin
target = E1 ~ sale + year + E + B + price1 + profitmargin
train.q3 <- rpart(target, data = train.data7, method = "anova", control = rpart.control(cp = 0.0001))
# Using decision tree from training dataset (combined imputation methods and 
# only statistically significant attributes) to predict on training dataset
pred.train.q3 <- vector(mode = "list",length =nrow(train.data7))
pred.train.q3 = predict(train.q3, train.data7, type="vector")
error_sum_3[1]=get_mase(pred.train.q3, train.data7)
#################
# Build the tree for TESTMETHOD8A where we predict values of B
test3.model8a <- rpart(B ~ . - E1, data = test.data3[!is.na(test.data3$B), ], 
                      method="anova", na.action=na.omit, control = rpart.control(cp = 0.0001))
# Prune the tree using the best cp.
bestcp.test3.model8a <- test3.model8a$cptable[which.min(test3.model8a$cptable[,"xerror"]),"CP"]
tree.pruned.test3.model8a <- prune(test3.model8a, cp = bestcp.test3.model8a)
# Predict 638 values of B using pruned tree.
pred3.testBs <- vector(mode = "list",length =nrow(test.data3))
pred3.testBs = predict(tree.pruned.test3.model8a, test.data3, type="vector")
# Predict 635 values of available B using pruned tree.
pred3.testB <- predict(tree.pruned.test3.model8a, test.data3[!is.na(test.data3$B), ], type="vector")
# Performance of rpart in predicting values of B
regr.eval(test.data3[!is.na(test.data3$B), ]$B, pred3.testB)
#################
# Build the tree for TESTMETHOD8B where we predict values of TACC
test3.model8b <- rpart(TACC ~ . - E1, data = test.data3[!is.na(test.data3$TACC), ], 
                       method="anova", na.action=na.omit, control = rpart.control(cp = 0.0001))
# Prune the tree using the best cp.
bestcp.test3.model8b <- test3.model8b$cptable[which.min(test3.model8b$cptable[,"xerror"]),"CP"]
tree.pruned.test3.model8b <- prune(test3.model8b, cp = bestcp.test3.model8b)
# Predict 638 values of TACC using pruned tree.
pred3.testTACCs <- vector(mode = "list",length =nrow(test.data3))
pred3.testTACCs = predict(tree.pruned.test3.model8b, test.data3, type="vector")
# Predict 552 values of available TACC using pruned tree.
pred3.testTACC <- predict(tree.pruned.test3.model8b, test.data3[!is.na(test.data3$TACC), ], type="vector")
# Performance of rpart in predicting values of TACC
regr.eval(test.data3[!is.na(test.data3$TACC), ]$TACC, pred3.testTACC)
#################
# Build the tree for TESTMETHOD8C where we predict values of invt
test3.model8c <- rpart(invt ~ . - E1, data = test.data3[!is.na(test.data3$invt), ], 
                       method="anova", na.action=na.omit, control = rpart.control(cp = 0.0001))
# Prune the tree using the best cp.
bestcp.test3.model8c <- test3.model8c$cptable[which.min(test3.model8c$cptable[,"xerror"]),"CP"]
tree.pruned.test3.model8c <- prune(test3.model8c, cp = bestcp.test3.model8c)
# Predict 638 values of invt using pruned tree.
pred3.testinvts <- vector(mode = "list",length =nrow(test.data3))
pred3.testinvts = predict(tree.pruned.test3.model8c, test.data3, type="vector")
# Predict 637 values of available invt using pruned tree.
pred3.testinvt <- predict(tree.pruned.test3.model8c, test.data3[!is.na(test.data3$invt), ], type="vector")
# Performance of rpart in predicting values of invt
regr.eval(test.data3[!is.na(test.data3$invt), ]$invt, pred3.testinvt)
#################
# Build the tree for TESTMETHOD8D where we predict values of xad
test3.model8d <- rpart(xad ~ . - E1, data = test.data3[!is.na(test.data3$xad), ], 
                       method="anova", na.action=na.omit, control = rpart.control(cp = 0.0001))
# Prune the tree using the best cp.
bestcp.test3.model8d <- test3.model8d$cptable[which.min(test3.model8d$cptable[,"xerror"]),"CP"]
tree.pruned.test3.model8d <- prune(test3.model8d, cp = bestcp.test3.model8d)
# Predict 638 values of xad using pruned tree.
pred3.testxads <- vector(mode = "list",length =nrow(test.data3))
pred3.testxads = predict(tree.pruned.test3.model8d, test.data3, type="vector")
# Predict 274 values of available xad using pruned tree.
pred3.testxad <- predict(tree.pruned.test3.model8d, test.data3[!is.na(test.data3$xad), ], type="vector")
# Performance of rpart in predicting values of xad
regr.eval(test.data3[!is.na(test.data3$xad), ]$xad, pred3.testxad)
#################
test.data3.model8 = test.data3
impute <- function (a, a.impute){ 
  ifelse (is.na(a), a.impute, a)
}
test.data3.model8$B = impute (test.data3.model8$B, pred3.testBs) 
test.data3.model8$TACC = impute (test.data3.model8$TACC, pred3.testTACCs) 
test.data3.model8$invt = impute (test.data3.model8$invt, pred3.testinvts) 
test.data3.model8$xad = impute (test.data3.model8$xad, pred3.testxads) 
test.data3.model8.noblankE1 <- subset(test.data3.model8, E1 != "")
# Create new variable profitmargin in validation dataset.
test.data3.model8.noblankE1$profitmargin<-(test.data3.model8.noblankE1$ni/test.data3.model8.noblankE1$sale)
# Using decision tree from training dataset (combined imputation methods and 
# only statistically significant attributes) to predict on validation dataset inputted using METHOD8
pred.test.q3 <- vector(mode = "list",length =nrow(test.data3.model8.noblankE1))
pred.test.q3 = predict(train.q3, test.data3.model8.noblankE1, type="vector")
error_sum_3[2]=get_mase(pred.test.q3, test.data3.model8.noblankE1)
# Using BEST CLASSIFIER from Question 1 to predict on validation dataset inputted using METHOD8
pred.test.q3best <- vector(mode = "list",length =nrow(test.data3.model8.noblankE1))
pred.test.q3best = predict(train.model6, test.data3.model8.noblankE1, type="vector")
error_sum_3[3]=get_mase(pred.test.q3best, test.data3.model8.noblankE1)
#################
error_sum_1
error_sum_2
error_sum_3
