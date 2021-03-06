## Load the "Mixture" dataset.
library("ElemStatLearn")
# Extract the components that we want.
x <- mixture.example$x
y <- mixture.example$y
xnew <- mixture.example$xnew
# px1 <- mixture.example$px1
# px2 <- mixture.example$px2
prob <- mixture.example$prob

## Plot training data. First, we need to make a dataframe for ggplot use.
df.train <- data.frame(x1=x[ , 1], x2=x[ ,2], y=y)
# Make y to be a factor.
df.train$y <- as.factor(df.train$y)
summary(df.train)
## Plot the true boundary.Again, we need to make a dataframe for ggplot use.
df.grid <- data.frame(x1=xnew[ ,1], x2=xnew[ ,2])
df.grid$prob <- prob
summary(df.grid)

library("ggplot2")
# To plot 200 training points.
# ggplot() + geom_point(data=df.train, aes(x=x1, y=x2, color=y), size=4) + scale_color_manual(values=c("green", "red")) + theme_bw()
# To plot boundary.
# stat_contour(data=df.grid, aes(x=x1, y=x2, z=prob), breaks=c(0.5))
# Plot 200 training points and boundary.
ggplot() + geom_point(data=df.train, aes(x=x1, y=x2, color=y), size=4) + scale_color_manual(values=c("green", "red")) + theme_bw() + stat_contour(data=df.grid, aes(x=x1, y=x2, z=prob), breaks=c(0.5))

## Generate test data.
set.seed(9876)
# 1st 5000 rows are random numbers betw 1 & 10, last 5000 rows are random numbers betw 11 & 20.
centers <- c(sample(1:10, 5000, replace=TRUE), sample(11:20, 5000, replace=TRUE))
# means consists of 20 different combinations of mixture centers. 
means <- mixture.example$means
# If 3rd and 10th integer of centers is 2, the 3rd and 10th row of means will take the 2nd combination of mixture centers.
means <- means[centers, ]
# Now the 10000 rows of means have 20 unique combinations.
library("mvtnorm")
x.test <- rmvnorm(10000, c(0, 0), 0.2 * diag(2))
x.test <- x.test + means
# replicate zeros and ones 5000 times each
y.test <- c(rep(0, 5000), rep(1, 5000)) 
# Make a dataframe containing the 10000 test values.
df.test <- data.frame(x1=x.test[ ,1], x2=x.test[ ,2], y=y)
df.test$y <- as.factor(df.test$y)
summary(df.test)

## Calculate irreducible error which is the error that comes from the data generating model.
bayes.error <- sum(mixture.example$marginal * (prob * I(prob < 0.5) + (1-prob) * I(prob >= 0.5)))

## K-Nearest Neighbour Classification with specific k value, for example k = 9.
library("FNN")
knn9 <- knn(x, xnew, y, k=9, prob=TRUE)
# Display the structure.
str(knn9)
# Get prob attribute.
prob.knn9 <- attr(knn9, "prob")
prob.knn9 <- ifelse(knn9 == "1", prob.knn9, 1 - prob.knn9)
# Get pred y.
y_hat9 <- ifelse(knn(x, x, y, k=9) == "1", 1, 0)
# Calculate number of times that pred y is not equal to the actual y.
sum(y_hat9 != y)
# Misclassification rate in training.
sum(y_hat9 != y) / length(y)

## K-Nearest Neighbour Classification with multiple k values.
# Enumerate multiple k values and measure misclassification rates of each.
ks <- c(1,3,5,7,9,11,15,23,27,35,45,55,75,99)
# Create placeholders to store training errors, testing errors and Cross-Validation misclassification rates.
misclass.train <- numeric(length=length(ks))
misclass.test <- numeric(length=length(ks))
misclass.cv <- numeric(length=length(ks))
for (i in seq(along=ks)) {
  # Apply KNN classification on the training set.
  model.train <- knn(x, x, y, k=ks[i])
  # Apply KNN classification on the test set.
  # For each of the 10000 test points, look at the k nearest training points found and determine the colour of the test point.
  model.test <- knn(x, x.test, y, k=ks[i])
  # Apply KNN classification leave-one-out (of the 200 points) cross validation on the training set.
  # Leave-one-out cv is deterministic, hence no need to set seed.
  model.cv <- knn.cv(x, y, k=ks[i])
  misclass.train[i] <- sum(model.train != y) / length(y)
  misclass.test[i] <- sum(model.test != y.test) / length(y.test)
  misclass.cv[i] <- sum(model.cv != y) / length(y)
}
# Place the misclassification values into a dataframe.
misclass <- data.frame(k=ks, train=misclass.train, test=misclass.test, cv=misclass.cv)
misclass
# Plot the MSEs.
plot.mse <- ggplot() + geom_line(aes(x=ks, y=misclass.train), color="red") + geom_point(aes(x=ks, y=misclass.train)) + geom_line(aes(x=ks, y=misclass.test), color="blue") + geom_point(aes(x=ks, y=misclass.test)) + geom_line(aes(x=ks, y=misclass.cv), color="green") + geom_point(aes(x=ks, y=misclass.cv)) + geom_hline(aes(yintercept = bayes.error), linetype="dashed") + scale_x_reverse(lim=c(100, 1)) + theme_bw()
plot.mse
