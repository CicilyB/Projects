# Dataset: https://archive.ics.uci.edu/ml/datasets/Ionosphere

data1 <- read.csv("C:/Users/cib17001/OneDrive - University of Connecticut/PDrive/BIST 5615/ionosphere.data", header=FALSE)


#Libraries

library(ISLR2)
library(class)
library(ROCR)
library(MASS)
library(plyr)


#Overview of Data

head(data1)
summary(data1)


# Removing the column with only 0 values
summary(data1$V2)
data1$V2 <- NULL


# Creating the factor levels for class
data1$V35 <- factor(data1$V35, levels=c("b", "g"), labels = c(0, 1))


# Using a 70/30 split
train <- sample(nrow(data1), 0.7*nrow(data1))


# Training Data
data.train <- data1[train,]
View(data.train)
count(data.train$V35)


# Test Data for Validation
data.test <- data1[-train, ]


# Logistic

glm.fit <- glm(V35~., family="binomial", data=data.train)
summary(glm.fit)

glm.prob <- predict(glm.fit, data.test, type="response")
glm.pred <- rep(0, length(data.test$V35))
glm.pred[glm.prob > .3] <- 1

table(glm.pred, data.test$V35)
mean(glm.pred == data.test$V35)


# Naive Bayes

library(e1071)
nb.fit <- naiveBayes(V35~., data = data.train)
nb.fit
nb.class <- predict(nb.fit , data.test)
table(nb.class , data.test$V35)
mean(nb.class == data.test$V35)


# LDA

lda.fit <- lda(V35~., data = data.train)
lda.fit

lda.pred <- predict(lda.fit, data.test)
lda.class <- lda.pred$class
table(lda.class, data.test$V35)
mean(lda.class == data.test$V35)


#SVM

fit.svm <- svm(V35~., data=data1[train,])
svm.pred <- predict(fit.svm, na.omit(data1[-train,]))
svm.perf <- table(na.omit(data1[-train,])$V35, 
                  svm.pred, dnn=c("Actual", "Predicted"))
sum(diag(svm.perf))/sum(svm.perf)







