####################################
##  ESL Problem Set 5  ##
####################################
## Problem 4 ##

## a.
load('prostate.Rdata')
summary(prostate.train)
# 67 obs. 9 var
summary(prostate.test)
# 30 obs. 9 var
sapply(prostate.test, sd)
sapply(prostate.train, sd)
# data is standardized
library(pls)

pcr.fit=pcr(lpsa~., data=prostate.train, validation = "CV")
summary(pcr.fit)

#validationplot(pcr.fit, val.type='MSEP')
train.error <- rep(0, 8)
test.error <- rep(0, 8)
for(i in 1:8){
  pcr.pred.train = predict(pcr.fit, prostate.train, ncomp = i)
  pcr.pred.test = predict(pcr.fit, prostate.test, ncomp = i)
  train.error[i] <- mean((pcr.pred.train - prostate.train[,9])^2)
  test.error[i] <- mean((pcr.pred.test - prostate.test[,9])^2)
}
library(ggplot2)
library(tidyr)
error <- data.frame(ncomp=1:8, train=train.error, test=test.error)
error <- gather(error, condition, error, -ncomp)
jpeg('pcr_ncomp8.jpg')
ggplot(data=error, aes(x=ncomp, y=error, col=condition))+geom_point()+geom_line()
dev.off()

#b.
pls.fit=plsr(lpsa~., data=prostate.train, validation = "CV")
summary(pls.fit)

#validationplot(psl.fit, val.type='MSEP')
pls.train.error <- rep(0, 8)
pls.test.error <- rep(0, 8)
for(i in 1:8){
  pls.pred.train = predict(pls.fit, prostate.train, ncomp = i)
  pls.pred.test = predict(pls.fit, prostate.test, ncomp = i)
  pls.train.error[i] <- mean((pls.pred.train - prostate.train[,9])^2)
  pls.test.error[i] <- mean((pls.pred.test - prostate.test[,9])^2)
}
pls.error <- data.frame(ncomp=1:8, train=pls.train.error, test=pls.test.error)
pls.error <- gather(pls.error, condition, error, -ncomp)
jpeg('pls_ncomp8.jpg')
ggplot(data=pls.error, aes(x=ncomp, y=error, col=condition))+geom_point()+geom_line()
dev.off()

#c.
data.full <- rbind(prostate.train, prostate.test)
pr.full.out <- prcomp(data.full[, 1:8], scale=TRUE)
pr.train.out <- prcomp(prostate.train[, 1:8], scale=TRUE)
#names(pr.out)
#pr.out$center
#pr.out$sdev
#pr.out$rotation
#dim(pr.out$x)
jpeg('PCA_score_fulldata.jpg')
plot(pr.full.out$x,  col = ifelse(pr.full.out$x< 2.5,'red','blue'))
dev.off()
jpeg('PCA_score_fulldata.jpg')
plot(pr.train.out$x,  col = ifelse(pr.train.out$x< 2.5,'red','blue'))
dev.off()

#d.
#full data
pls.full.data<-predict(pls.fit, data.full, ncomp=4)
jpeg('pls_proj_full.jpg')
plot(pls.full.data, col = ifelse(pls.fit$scores< 2.5,'red','blue'))
dev.off()

# train data
jpeg('pls_proj_train.jpg')
plot(pls.fit$scores, col = ifelse(pls.fit$scores< 2.5,'red','blue'))
dev.off()
