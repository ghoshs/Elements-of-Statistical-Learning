####################################
##  ESL Problem Set 6  ##
####################################
## Problem 4 ##

## a.
library(ISLR)
data <- data.frame(Hitters)

summary(data) # 322 obs with 20 variables
sum(is.na(data$Salary)) # 59 obs with no Salary information
data <- subset(data, is.na(Salary) == FALSE) # remove 59 obs.
str(data) # 263 obs. with 20 variables

data$LogSalary <- log(data$Salary)
data.train <- data[1:200,] # Train set
data.test <- data[201:nrow(data),] # Test set

## b. 
library(gbm)
library(tidyr)
boost.MSE.train <- rep(0, 5)
boost.MSE.test <- rep(0, 5)
# shrinkage.val <- c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1)
shrinkage.val <- seq(0.00001,1, by=0.001)
for(i in c(1:length(shrinkage.val))){
  set.seed(1)
  boost.model = gbm(LogSalary~.-Salary, data=data.train, distribution="gaussian", 
                 n.trees=1000, interaction.depth=4, shrinkage=shrinkage.val[i])
  summary(boost.model)
  boost.MSE.train[i] <- mean((predict(boost.model, newdata=data.train, n.trees=1000) - 
                         data.train$LogSalary)^2)
  boost.MSE.test[i] <- mean((predict(boost.model, newdata=data.test, n.trees=1000) - 
                              data.test$LogSalary)^2)
}

boost.MSE <- data.frame(train=boost.MSE.train, test=boost.MSE.test, shrinkage=shrinkage.val)
boost.MSE <- gather(data=boost.MSE, key=condition, value=MSE, -shrinkage)
library(ggplot2)
jpeg('boostingMSE.jpg')
ggplot(data=boost.MSE, aes(x=log10(shrinkage), y=MSE, col=factor(condition)))+
  geom_point()+
  geom_line()
dev.off()

## Best model for shrinkage = 0.01
set.seed(1)
boost.bestmodel <- gbm(LogSalary~.-Salary, data=data.train, distribution="gaussian", 
                       n.trees=1000, interaction.depth=4, shrinkage=shrinkage.val[3])
summary(boost.bestmodel)

## c.
boost.MSE[8,3]
# Test MSE Boost = 2.672439e-01
# Train MSE Boost = 6.307699e-02
# Least squares fit
set.seed(1)
lm.model = lm(data=data.train, LogSalary~.-Salary)
lm.MSE.test = mean((predict(lm.model, newdata=data.test) - data.test$LogSalary)^2)
lm.MSE.test ## = 0.4917959
lm.MSE.train = mean((predict(lm.model, newdata=data.train) - data.train$LogSalary)^2)
lm.MSE.train ## 0.3203734

# Ridge regression
library(glmnet)
x <- model.matrix(LogSalary~.-Salary, data.train)[, -1]
y <- data.train$LogSalary
set.seed(1)
ridge.mod = cv.glmnet(x, y, alpha=0, lambda=10^seq (4,-2, length =1000))
ridge.minlambda <- ridge.mod$lambda.min
x.test <- model.matrix(LogSalary~.-Salary, data.test)[, -1]
ridge.MSE.test <- mean((predict(ridge.mod, s=ridge.minlambda, newx=x.test) - 
                      data.test$LogSalary)^2)
ridge.MSE.test ## =  0.4521156
ridge.MSE.train <-mean((predict(ridge.mod, s=ridge.minlambda, newx=x) - y)^2)
ridge.MSE.train ##  0.3560562

## d.
jpeg('influence.jpg')
summary(boost.bestmodel)
dev.off()

d<-ridge.mod$glmnet.fit
L<-length(d$lambda)
x<-log(d$lambda[L])
y<-d$beta[, L]
labs<-names(y)
jpeg('ridge.jpg')
plot(d, "lambda", col=rainbow(ncol(data.train)-2))
text(x, y, labels=labs)
legend("bottomright", title="Predictors", labs, fill=rainbow(ncol(data.train)-2), cex=0.75)
dev.off()
coefficients(ridge.mod, s= ridge.minlambda)

summary(lm.model)
### Intercept > Hits > Putouts > Walks > AtBat > Years > CWalks

## e.
library(randomForest)
ntrees <- seq(5,100,5)
bagging.MSE.train <- rep(0, 20)
bagging.MSE.test <- rep(0, 20)
set.seed(1)
for(i in c(1:20)){
  #set.seed(1)
  bagging.model = randomForest(LogSalary~.-Salary, data=data.train, mtry=19, ntree=i, maxnodes=1)
  bagging.MSE.train[i] = mean((predict(bagging.model, newdata=data.train)-data.train$LogSalary)^2)
  bagging.MSE.test[i] = mean((predict(bagging.model, newdata=data.test)-data.test$LogSalary)^2)
}

bag.MSE <- data.frame(train=bagging.MSE.train, test=bagging.MSE.test, ntree=ntrees)
bag.MSE <- gather(data=bag.MSE, key=condition, value=MSE, -ntree)
jpeg('baggingMSE.jpg')
ggplot(data=bag.MSE, aes(x=ntree, y=MSE, col=factor(condition)))+
  geom_point()+
  geom_line()
dev.off()

## f.
rf.MSE.train <- rep(0, 20)
rf.MSE.test <- rep(0, 20)
set.seed(1)
for(i in c(1:20)){
  rf.model = randomForest(LogSalary~.-Salary, data=data.train, mtry=6, ntree=i, importance=T)
  rf.MSE.train[i] = mean((predict(rf.model, newdata=data.train)-data.train$LogSalary)^2)
  rf.MSE.test[i] = mean((predict(rf.model, newdata=data.test)-data.test$LogSalary)^2)
}

rf.MSE <- data.frame(train=rf.MSE.train, test=rf.MSE.test, ntree=ntrees)
rf.MSE <- gather(data=rf.MSE, key=condition, value=MSE, -ntree)
jpeg('rfMSE.jpg')
ggplot(data=rf.MSE, aes(x=ntree, y=MSE, col=factor(condition)))+
  geom_point()+
  geom_line()
dev.off()

set.seed(1)
rf.model = randomForest(LogSalary~.-Salary, data=data.train, mtry=6, ntree=95, importance=T)
importance(rf.model)
jpeg('rfimportance.jpg')
varImpPlot(rf.model)
dev.off()
