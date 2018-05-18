# (a) Familiarize with R 
# (b) load data
load("ozone.RData")
# number of observations
nrow(ozone)
# column names
colnames(ozone)
# datatypes
class(ozone)
class(ozone$ozone)
class(ozone$radiation)
class(ozone$temperature)
class(ozone$wind)
# or obtain above information using str()
str(ozone)

# (c) range, mean, standard deviation
print("c. Range")
sapply(ozone, range)
print(" Mean")
sapply(ozone, mean)
print(" Standard Deviation")
sapply(ozone, sd)

# (d) scatter plots for each pair of variables in the dataframe
# is useful to determine is any pair of (predictor/response) variables have 
# a linear relationship
pairs(ozone)

# (e) rss function
rss <- function(vec1, vec2) { sum((vec1-vec2)^2) }
# (f) create model and predict values
prediction <- predict.lm(lm(ozone~radiation+temperature+wind, data=ozone[trainset,]), ozone[testset,2:4])
# print RSS and correlation
trueval <- ozone[testset,1]
print("f. RSS")
rss(prediction, trueval)
print(" Correlation")
cor(prediction, trueval)
plot(trueval, prediction)

# (g) perform kNN for k=1,2,...,30
# load library FNN
library(FNN)
prediction<-list() 
k <- c(1:30)
# to store prediction for entire dataset
for(i in k){ 
  # use scale() for normalizing train and test?? 
	knnmodel <- knn.reg(ozone[trainset,2:4], test=ozone[,2:4], y=ozone[trainset,1], k=i)
    prediction[[i]] <- knnmodel$pred         
}
prediction <- matrix(unlist(prediction), ncol=111, byrow=TRUE)
rss_train <- c()
rss_test <- c()
for(i in k){
	rss_train[i] <- rss(prediction[i,trainset], ozone[trainset,1])
	rss_test[i] <- rss(prediction[i,testset], ozone[testset,1])
}
# plot
plot(k, rss_train, type="b", col="yellow", pch=19, xlab="k", ylab="rss")
lines(k, rss_test, type="b", col="blue", pch=19)
legend("bottomright", legend=c("Train", "Test"), col=c("yellow", "blue"), pch=c(19, 19), bty="n")
# (h) correlation and rss for k-6
print("Correlation for k=6")
cor(prediction[6, testset], ozone[testset, 1])
print("RSS for k=6")
rss_test[6]