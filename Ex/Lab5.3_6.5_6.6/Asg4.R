####################################
##  ESL Problem Set 4  ##
####################################
## Problem 4 ##

## a.
library(MASS)
data <- read.table('prostate.txt')
str(data)
summary(data)
head(data)

## 97observations with 10 columns (col 1 in text file is ignored here)
## Outcome is in 9th col
## 10th col denotes TRUE (67 obs.) if observation belongs to train data
## FALSE (30 obs.) if observation belongs to test data.

train <- subset(data, train == TRUE, select=-c(train))
str(train)
dim(train)

colMeans(train)
apply(train, 2, sd)

train[ ,1:8] <- sapply(train[ ,1:8], function(x) (x - mean(x))/sd(x))
colMeans(train)
apply(train, 2, sd)

test <- subset(data, train == FALSE, select = -c(train))
dim(test)

colMeans(test)
apply(test, 2, sd)

test[ ,1:8] <- sapply(test[ ,1:8], function(x) (x-mean(x))/sd(x))
colMeans(test)
apply(test, 2, sd)

## b.
library(leaps)
sum(is.na(train)) # checking for any missing values in train
sum(is.na(test))  # and test set

regfit.full.model <- regsubsets(lpsa ~ ., train)
regfit.full.summary <- summary(regfit.full.model)
names(regfit.full.summary)

regfit.full.summary$outmat

maxr2 <- which.max(regfit.full.summary$rsq)
maxadjr2 <- which.max(regfit.full.summary$adjr2)
mincp <- which.min(regfit.full.summary$cp)
minbic <- which.min(regfit.full.summary$bic)

jpeg('plot1.jpg')

par(mfrow =c(2,2))
plot(regfit.full.summary$rsq, xlab=" Number of Variables ",ylab=" R sqaured", type='l')
points (maxr2, regfit.full.summary$rsq[maxr2], col ="red",cex =2, pch =20)

plot(regfit.full.summary$adjr2, xlab=" Number of Variables ",ylab="Adjusted R sqaured", type='l')
points (maxadjr2, regfit.full.summary$adjr2[maxadjr2], col ="red",cex =2, pch =20)

plot(regfit.full.summary$cp, xlab=" Number of Variables ",ylab="Cp", type='l')
points (mincp, regfit.full.summary$cp[mincp], col ="red",cex =2, pch =20)

plot(regfit.full.summary$bic, xlab=" Number of Variables ",ylab="BIC", type='l')
points (minbic, regfit.full.summary$bic[minbic], col ="red",cex =2, pch =20)
dev.off()

# plot(regfit.full.model, scale='r2')

## how to select model?
## Best model for Adjusted R sqr and Cp is with 7 variables, but for BIC
## best model is the 2 variable model
## We can see that BIC does not change much from 2 to 4 variables but thereafter increases sharply
## The Cp value for 4 variable model is also close to the 7 variable model.
## Hence for this data I choose the 4 variable model with predictors
## lcavol, lweight, svi and lbph

coef(regfit.full.model, 4)
#(Intercept)      lcavol     lweight        lbph         svi 
#2.4523451   0.6281553   0.2568064   0.2049279   0.2821691 

best.model <- lm(formula = lpsa ~ lcavol + lweight + svi + lbph, data = train)
# we obtain same coeff
train.predict <- predict(best.model, train)
train.MSE <- mean((train.predict - train$lpsa)^2)
# 0.489776
test.predict <- predict(best.model, test)
test.MSE <- mean((test.predict - test$lpsa)^2)
# 0.4895071

## c.
library(broom)
library(ggplot2)

ridge.model <- lm.ridge(formula=lpsa~.,data=train,lambda=10^seq(4, -2, length =1000))
#plot(ridge.model)
select(ridge.model)
lambda_low = 4.905584
#abline(v=lambda_low)

n <- tidy(ridge.model)
head(n)

jpeg('plot2.jpg')
ggplot(n, aes(log(lambda), estimate, color = term)) + geom_line() + 
  geom_vline(xintercept=log(lambda_low))
dev.off()

jpeg('plot3.jpg')
ggplot(n, aes(lambda, estimate, color = term)) + geom_line() + 
  geom_vline(xintercept=lambda_low)
dev.off()

# gleason and age predictors coef are drivento zero early.

## d.
library(glmnet)

set.seed(17)
x <- model.matrix(lpsa ~ .,train)[,-1]
y <- train$lpsa
ridge.model.k5 <- cv.glmnet (x, y, nfolds=5, alpha =0, lambda=10^seq(4, -2, length =1000))
plot(ridge.model.k5)
bestlam <- ridge.model.k5$lambda.min
bestlam ## 0.2543346

train.error.minlambda <- predict(ridge.model.k5, s=bestlam, newx=x)
train.MSE.minlambda <- mean((train.error.minlambda - y)^2)
train.MSE.minlambda ## 0.4741574

x.test <- model.matrix(lpsa ~ .,test)[,-1]
y.test <- test$lpsa
test.error.minlambda <- predict(ridge.model.k5, s=bestlam, newx=x.test)
test.MSE.minlambda <- mean((test.error.minlambda - y.test)^2)
test.MSE.minlambda ## 0.5038159

## e.
set.seed(17)
lasso.model <- glmnet(x, y, alpha = 1, lambda=10^seq(4, -4, length =1000))

jpeg('plot4.jpg')
plot(lasso.model, col=1:8, label=TRUE)
legend("bottomleft", title="Predictors", colnames(x), fill=1:8, cex=0.75)
dev.off()
# predictors are driven exactly to zero.

## f.
set.seed(17)
lasso.model.k5 <- cv.glmnet(x, y, nfolds=5, alpha =1, lambda=10^seq(4, -4, length =1000))
plot(lasso.model.k5)
bestlam.lasso <- lasso.model.k5$lambda.min
bestlam.lasso ## 0.02219966

train.error.lasso <- predict(lasso.model.k5, s=bestlam.lasso, newx=x)
train.MSE.lasso <- mean((train.error.lasso - y)^2)
train.MSE.lasso ## 0.4488017

test.error.lasso <- predict(lasso.model.k5, s=bestlam.lasso, newx=x.test)
test.MSE.lasso <- mean((test.error.lasso - y.test)^2)
test.MSE.lasso ## 0.5056859

lasso.coef <- predict(lasso.model.k5, type = "coefficients", s=bestlam.lasso)
lasso.coef

#(Intercept)  2.45234509
#lcavol       0.64800530
#lweight      0.27558580
#age         -0.08738984
#lbph         0.18691948
#svi          0.25836928
#lcp         -0.13949499
#gleason      .         
#pgg45        0.18194508

ridge.coef <- predict(ridge.model.k5, type = "coefficients", s=bestlam)
ridge.coef
#(Intercept)  2.45234509
#lcavol       0.50003239
#lweight      0.26986097
#age         -0.07133449
#lbph         0.18425292
#svi          0.25551229
#lcp         -0.04928486
#gleason      0.03403757
#pgg45        0.15568431

## g.
lm.model <- lm(lpsa~., train)
train.lm.predict <- predict(lm.model, train) 
train.lm.MSE <- mean((train.lm.predict - train$lpsa)^2)
train.lm.MSE ## 0.4391998

test.lm.predict <- predict(lm.model, test) 
test.lm.MSE <- mean((test.lm.predict - test$lpsa)^2)
test.lm.MSE ##  0.5491941
