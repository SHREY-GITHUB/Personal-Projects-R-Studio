df<- read.csv("C:\\Users\\shrey\\OneDrive\\Desktop\\R ML\\Reg Assignment\\Regression Assignment\\car data.csv")
View(df)

#EDA:
summary(df)
str(df)


#Selling Price, Present Price and Kms driven have many outliers present
#No NA values

#Some Variable Transformation

df$Owner<- factor(df$Owner)
df$Year<- factor(df$Year)
df$Car_Name<- as.character(df$Car_Name)
df$Year<- as.Date(df$Year, format = "%Y")

hist(df$Kms_Driven)
pairs(~Selling_Price+ Present_Price+ Kms_Driven , data = df)
barplot(table(df$Owner))
barplot(table(df$Year))
barplot(table(df$Fuel_Type))
barplot(table(df$Seller_Type))
barplot(table(df$Transmission))


#OUtlier imputation
lv<- 0.3*quantile(df$Selling_Price, 0.01)
df$Selling_Price[df$Selling_Price<lv]<- lv
uv<- 3*quantile(df$Selling_Price, 0.99) 
df$Selling_Price[df$Selling_Price>uv] <- uv

lv<- 0.3*quantile(df$Present_Price, 0.01)
df$Present_Price[df$Present_Price<lv] <- lv
uv<- 3*quantile(df$Present_Price,0.99)
df$Present_Price[df$Present_Price>uv] <- uv

lv<- 0.05*quantile(df$Kms_Driven,0.01)
df$Kms_Driven[df$Kms_Driven<lv] <- lv
uv<- 5*quantile(df$Kms_Driven,0.99)
df$Kms_Driven[df$Kms_Driven>uv] <- uv

summary(df)

#Removing names of the car as we dont want it in our modellig  
df<- df[,-1]

#Creating dummy vars for categorical variables
library(dummies)
df<- dummy.data.frame(df)

#Removing extra dummy vars
df<- df[,-14]
df<- df[,-11]
df<-df[,-9]
df<- df[,-5]

df2<- df[,-1]
round(cor(df2),2)
df2<- df2[,-2]

#Building simple reg model
simplemodel<- lm(Selling_Price~Kms_Driven, data = df2)
summary(simplemodel)
plot(df2$Selling_Price, df2$Kms_Driven)
abline(simple_model)

#Building mmultimodel with all the variables in consideration
multimodel<- lm(Selling_Price~., data = df)
summary(multimodel) #pvalue is less than 0.05 hence the model is good to go


library(caTools)
set.seed(0)

#Multimodel Linear Regression
split = sample.split(df,SplitRatio = 0.8)
training_set = subset(df,split == TRUE)
test_set = subset(df, split == FALSE)

lm_a = lm(Selling_Price~.,data=training_set)

train_a = predict(lm_a,training_set)
test_a = predict(lm_a,test_set)

mean((training_set$Selling_Price -train_a)^2)
mean((test_set$Selling_Price-test_a)^2)

plot(test_set$Selling_Price ,test_a)
abline(lm_a)

plot(test_a)
lines(ts(test_set$Selling_Price), col = "red")

?lines

#Subset Selection
library(leaps)
#in this we will create 9 subsets(no of independent variables) and then we will take the subset with the highest value of Adj R2
lm_best <- regsubsets(Selling_Price~., data = df, nvmax = 9) 
summary(lm_best)
summary(lm_best)$adjr2 #looking at the values of Adj R2 from the 9 subsets

which.max(summary(lm_best)$adjr2) #8th subset has the max value of adj R2
coef(lm_best, 8) #taking out the coeff of 8th subset, so this is the most significant coeffs we need to keep in our equation to get least error prediction of the output variables

#3rd method is the Forward subset where we take one variable and then subsequently start increasing the vars till we get the most optimised value
lm_forward<- regsubsets(Selling_Price~., data = df, nvmax= 9, method = "forward")
which.max(summary(lm_forward)$adjr2)
coef (lm_forward, which.max(summary(lm_forward)$adjr2))

#4th method is BAckward subset method
lm_backward<- regsubsets(Selling_Price~., data = df, nvmax = 9, method = "backward")

coef(lm_backward, which.max(summary(lm_backward)$adjr2))     

#5th method. Ridge Regression
#In this, we will try to minimise the coeff of the independent vars so that they do not effect the results to a larger extent
library(glmnet) #glm~ genereal linear modelling
#now lets allot x as matrix of independent vars and y as dependent vars
x = model.matrix(Selling_Price~., data = df)[,-1]
y= df$Selling_Price

#Now there is a factor of lambda(k) in which we need to select the optimum k so that it gives the min values of the coefficients
#We will have to make a matrix of k from values 10^10 to 10^-2
grid = 10^seq(10,-2, length = 100)
grid

lm_ridge = glmnet(x,y, alpha = 0, lambda = grid) #alpha 0 for ridge, 1 for lasso
#Now applying cross validation technque to fit all the k values so that we select k with lowest mean square

cv_ridge<- cv.glmnet(x,y, alpha = 0, lambda = grid)
plot(cv_ridge) #we will select the value of lambda from the min value of mean sq error

opt_lambda<- cv_ridge$lambda.min #opt lambda= 0.2848035

#Predicting the value of y now
y_a<- predict(lm_ridge, s = opt_lambda, newx = x)

tss= sum((y-mean(y))^2)
rss = sum((y_a-y)^2)

rsq= 1-rss/tss  #rsq = 0.88189 which is a very high value

lines(y_a, col = "blue")
lines(y, col = "red")

#Now lets do this with lasso req technique
#In this technique we try to bring the coeffs to zero so that the effect of the vars is completely eliminated

#building the model
lm_lasso <- glmnet(x,y, alpha = 1, lambda = grid) #alpha 1 for lasso

#fitting the lambda with cross validation technique
cv_lasso <- cv.glmnet(x,y, alpha= 1, lambda = grid)

#Finding the optimum lambda with the lowest Mean Sq Error
opt_lambda1<- cv_lasso$lambda.min # 0.1232 which is very less that opt_lambda

#Lets predict the values of y
y_a1 = predict(lm_lasso, s = opt_lambda1, newx = x) 
rm(lasso_predict)

plot(y)
lines(y_a1, col = "blue")

tss = sum((y-mean(y))^2)
rss = sum((y_a1-y)^2)
rsq2 = 1- rss/tss #0.87897

#rsq: Ridge = 0.8819, Lasso: 0.8789
#Both are very close to each other hence we can chose anyone. Not going to make much difference. Since Ridge has a bit higher rsq hence I will go with Ridge Regression
