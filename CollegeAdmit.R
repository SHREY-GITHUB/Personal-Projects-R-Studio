library(readxl)
Admission_Predict <- read.csv("Admission_Predict.csv")
View(Admission_Predict)

df<- Admission_Predict

summary(df)
str(df)

#on data exploration we do not find any NA values as well as no outliers

#lets plot the graphs for more relationship
pairs(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating + SOP + CGPA, data = df )

barplot(table(df$Chance.of.Admit))
barplot(table(df$LOR))
barplot(table(df$University.Rating))
barplot(table(df$Research))
df$Research<- factor(df$Research)


df<- df[,-1]
str(df)

library(dummies)
df<- dummy.data.frame(df)
df<- df[,-7]

df$Chance.of.Admit[df$Chance.of.Admit >=0.7 ] <- 1
df$Chance.of.Admit[df$Chance.of.Admit < 0.7] <- 0

#Logistics Regression
library(caTools)
set.seed(1234)
split =sample.split(df, SplitRatio = 0.8)
traindata<- subset(df, split== TRUE)
testdata<- subset(df, split== FALSE)

#Bulding the model
library(glmnet)
train.fit<- glm(Chance.of.Admit~., data = traindata, family = binomial )
test.probs<- predict(train.fit, testdata, type = "response")

test.pred <- rep(0,100)
test.pred[test.probs>=0.7] <- 1
table(test.pred, testdata$Chance.of.Admit)
#Accuracy = 32+49/100 = 81/100 = 81%
plot(test.pred)
lines(testdata$Chance.of.Admit, col = "red")

#LDA Method
library(MASS)
lda.fit <- lda(Chance.of.Admit~., data = df)
lda.pred <- predict(lda.fit, df)
lda.pred$class
lda.pred$posterior

lda.pred[lda.pred$posterior>=0.7] <-1

table(lda.pred$class, df$Chance.of.Admit) #130+209/400 = 339/400 = 85%

split = sample.split(df, SplitRatio = 0.8)
traindata = subset(df, split == TRUE)
testdata = subset(df, split== FALSE)

lda.fit1<- lda(Chance.of.Admit~., data = traindata)
lda.pred1<- predict(lda.fit1, testdata)

lda.pred1[lda.pred1$posterior>=0.7] <- 1

table(lda.pred1$class, testdata$Chance.of.Admit) #accuracy = 85/100 = 85%

#KNN Classifier
#FOr KNN Classifier we need to find the 4 parameters to insert in the knn function
library(class)
trainx<- traindata[,-8] #only independent vars #1st parameter
trainy<- traindata$Chance.of.Admit #dependent vars #2nd parameter
testx<- testdata[,-8] #testdata independent vars #3rd parameter
testy<- testdata$Chance.of.Admit
#testy is not taken in the parameter because we need to predict it

k=3 #4th parameter

#Now we need to scale all the numeric data so that when the dist is calculated it comes out to be the same
trainxs<- scale(trainx)
testxs<- scale(testx)

set.seed(1234)

knn.pred<- knn(trainxs, testxs, trainy, k= k)

table(knn.pred, testy) #Accuracy= 30+53/100 = 83%

#So comparing all the 3 classifier technique we find out that LDA has the highest accuracy of 85% and then knn with 83% followed by logistics reg with 81%
