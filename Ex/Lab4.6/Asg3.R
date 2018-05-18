####################################
##  ESL Problem Set 3  ##
####################################
## Problem 4 ##

## a.
phoneme = read.csv('phoneme.csv')
summary(phoneme)
names(phoneme)
dim(phoneme)
## 256 predictors (x.1 - x.256) with 4509 observations 
## g: output variable with 5 values (aa, ao, dcl, iy, sh)
## speaker variable : 4 strings separated by '.'. 1st part says whether train or test data

train <- which(grepl('train', phoneme$speaker))
##train <- which(substr(phoneme$speaker, 1, 5) == 'train')
length(train)

test <- which(grepl('test', phoneme$speaker))
##test <- which(substr(phoneme$speaker, 1, 4) == 'test')
length(test)

## b.
library(MASS)
?lda

x_1 <- which(colnames(phoneme) == 'x.1')
x_256 <-which(colnames(phoneme) == 'x.256')
formula = as.formula(paste(c('g',paste(names(phoneme)[x_1:x_256], collapse =" + ")), collapse=' ~ '))
# formula = g~x.1+x.2+...+x.256 ## g is predicted using the 256 predictor variables
lda.model = lda(formula, data=phoneme, subset=train)
#Prior probabilities of groups:
#   aa        ao       dcl        iy        sh 
# 0.1553892 0.2272455 0.1682635 0.2550898 0.1940120

lda.train = predict(lda.model, phoneme[train, x_1:x_256])
lda.test = predict(lda.model, phoneme[test, x_1:x_256])

col_g <- which(colnames(phoneme) == 'g')
table(lda.train$class,phoneme[train, col_g])
mean(lda.train$class != phoneme[train, col_g])
## Train error - 0.05598802
table(lda.test$class,phoneme[test, col_g])
mean(lda.test$class != phoneme[test, col_g])
## Test error - 0.08041061

## c.
## projection of the training data onto the first two canonical coordinates of the LDA
jpeg('plot1.jpg')
plot(lda.model, dimen=2, col=1:5)
dev.off()
jpeg('plot2.jpg')
plot(lda.model, dimen=1)
dev.off()
jpeg('plot3.jpg')
plot(lda.model, dimen=3, col=1:5)
dev.off()
jpeg('plot4.jpg')
plot(lda.model, dimen=4, col=1:5)
dev.off()

#plot(lda.model)

## d.
library(dplyr)
phoneme2 <- filter(phoneme, (g == 'aa' | g == 'ao')) ## select observations for 2 phonemes
phoneme2$g <- factor(phoneme2$g) ## change factor levels
train2 <- which(grepl('train', phoneme2$speaker)) ## re-create train and test 
test2 <- which(grepl('test', phoneme2$speaker))
length(train2)
length(test2)
lda.model2 = lda(formula, data=phoneme2, subset=train2)
#Prior probabilities of groups:
#  aa        ao 
#0.4061033 0.5938967 

lda.train2 = predict(lda.model2, phoneme2[train2, x_1:x_256])
lda.test2 = predict(lda.model2, phoneme2[test2, x_1:x_256])

table(lda.train2$class,phoneme2[train2, col_g])
mean(lda.train2$class != phoneme2[train2, col_g])
## Train error - 0.1064163
table(lda.test2$class,phoneme2[test2, col_g])
mean(lda.test2$class != phoneme2[test2, col_g])
## Test error - 0.214123

## e.
qda.model = qda(formula, data=phoneme, subset=train)
## Prior probabilities of groups:
##   aa        ao       dcl        iy        sh 
## 0.1553892 0.2272455 0.1682635 0.2550898 0.1940120
qda.train = predict(qda.model, phoneme[train, x_1:x_256])
qda.test = predict(qda.model, phoneme[test, x_1:x_256])

table(qda.train$class,phoneme[train, col_g])
mean(qda.train$class != phoneme[train, col_g])
## Train error - 0
table(qda.test$class,phoneme[test, col_g])
mean(qda.test$class != phoneme[test, col_g])
## Test error - 0.1582549

qda.model2 = qda(formula, data=phoneme2, subset=train2)
# Prior probabilities of groups:
#   aa        ao 
# 0.4061033 0.5938967 

qda.train2 = predict(qda.model2, phoneme2[train2, x_1:x_256])
qda.test2 = predict(qda.model2, phoneme2[test2, x_1:x_256])

table(qda.train2$class,phoneme2[train2, col_g])
mean(qda.train2$class != phoneme2[train2, col_g])
## Train error - 0
table(qda.test2$class,phoneme2[test2, col_g])
mean(qda.test2$class != phoneme2[test2, col_g])
## Test error - 0.3394077

## f.
## LDA train - all classes
table(lda.train$class,phoneme[train, col_g])
## LDA test - all classes
table(lda.test$class,phoneme[test, col_g])

## LDA train - 2 classes
table(lda.train2$class,phoneme2[train2, col_g])
## LDA test - all classes
table(lda.test2$class,phoneme2[test2, col_g])

## QDA train - all classes
table(qda.train$class,phoneme[train, col_g])
## QDA test - all classes
table(qda.test$class,phoneme[test, col_g])

## QDA train - 2 classes
table(qda.train2$class,phoneme2[train2, col_g])
## QDA test - 2 classes
table(qda.test2$class,phoneme2[test2, col_g])