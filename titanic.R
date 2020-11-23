cat("\014")
train <- read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\R ML\\Reg Assignment\\Regression Assignment\\Titanic\\train.csv")
test <- read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\R ML\\Reg Assignment\\Regression Assignment\\Titanic\\test.csv")

View(train)
View(test)

train<- train[,-11]
train <- train [,-9]
train <- train [,-4]
train <- train[,-1]

str(train)
summary(train)

test<- test[,-10]
test <- test [,-8]
test <- test [,-3]
test <- test[,-1]

str(test)
summary(test)

train$Age[is.na(train$Age)] <-median(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age)] <- median(test$Age, na.rm = TRUE)
test$Fare[is.na(test$Fare)] <- median(test$Fare, na.rm = TRUE)

summary(train)
summary(test)

which(train$Embarked == "")

train<- train[-62,]
train<- train[-829,]

pairs(Survived~ Pclass + Age + SibSp + Parch +Fare, data = train)
barplot(table(train$Sex))
barplot(table(train$Embarked))

library(dummies)
train<- dummy.data.frame(train)
test<- dummy.data.frame(test)

train<- train[,-11]
train<- train[,-3]

test <- test[,-10]
test <- test[,-2]

library(caTools)
split = sample.split(train, SplitRatio = 0.8)
traindata<- subset(train, split== TRUE)
testdata<- subset(train, split== FALSE)
trainfit<- glm(Survived~., data = traindata, family = binomial)
testprobs<- predict(trainfit, testdata, type = "response")
testpred<- rep(0,198)
testpred[testprobs>= 0.5] <- 1
table(testpred, testdata$Survived)
#Accuracy = 105+57/198 = 162/198= 82%

#Bulding the model
library(glmnet)
train.fit<- glm(Survived~., data = train, family = binomial )
test.probs<- predict(train.fit, test, type = "response")

test.pred <- rep(0,418)
test.pred[test.probs>=0.5] <- 1
test.pred

test$Survived <- test.pred
table(test.pred, test$Survived)

#LDA Method
library(MASS)

lda.fit<- lda(Survived~., data = traindata)
lda.pred<- predict(lda.fit, testdata)
lda.pred$class
table(lda.pred$class, testdata$Survived)
#Accuracy = 156/ 198 = 78.8%

lda.fit1<- lda(Survived~., data = train)
lda.pred1 = predict(lda.fit1, test)
lda.pred1$class

#KNN Classifier
#FOr KNN Classifier we need to find the 4 parameters to insert in the knn function
library(class)
trainx<- traindata[,-1] #only independent vars #1st parameter
trainy<- traindata$Survived #dependent vars #2nd parameter
testx<- testdata[,-1] #testdata independent vars #3rd parameter
testy<- testdata$Survived
#testy is not taken in the parameter because we need to predict it

k=5 #4th parameter

#Now we need to scale all the numeric data so that when the dist is calculated it comes out to be the same
trainxs<- scale(trainx)
testxs<- scale(testx)

set.seed(1234)
knn.pred<- knn(trainxs, testxs, trainy, k= k)
table(knn.pred, testy) #Accuracy= 157/198 = 79.29%%

trainx1<- train[,-1]
trainxs1<- scale(trainx1)
trainy1<- train$Survived
testxs1<- scale(test)

k=5

set.seed(1234)
knn.pred1<- knn(trainxs1, testxs1, trainy1, k=k)
knn.pred1

test$Survivedknn<- knn.pred1
