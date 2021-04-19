library(MASS)
library(ISLR)

head(Boston)
summary(Boston)
  
#Exploratory Data Analysis
#check relation between independent with dependent variable 
#Here dependent variable-medv
#independent variable-lstat
plot(medv~lstat,Boston)

#To show relationship of dependent variable with all independent variable
pairs(~medv + ptratio + black + lstat + dis + rm + crim, data=Boston, main= "Boston Data")
#conclusion
#lstat,dis and rm are good linear variables but things like 
#crim,ptratio ,black is not linear
#in fact the relationship is quite complicated.

#simple linear regression lm() -->parameter constructor
#objectname=lm(parameter1,para2,para3,...)
#we train model here we use lm(y~x.datasetname)
fit1=lm(medv~lstat,data=Boston) #fit1 user defined object
summary(fit1)

#we visualize the data after train
plot(medv~lstat,Boston) #scatter plot
abline(fit1,col="red")

confint(fit1)

#To test our model use predict()
predict(fit1,data.frame(lstat=c(10,20,30)),interval = "confidence")

#Multiple linear regression 
#we train our model with 2 independent variables
#fit2 user defined object of linear regression
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)

#we train our model with all independent variables
#fit3 user defined object of linear regression
fit3=lm(medv~.,Boston) #lm(y~.) . means all independent variables
summary(fit3)

#Nonlinear terms
#we train our model with increase the degree of x
fit4=lm(medv~lstat +I(lstat^2),Boston)
summary(fit4)

#use polynomial function
attach(Boston)
fit5=lm(medv~poly(lstat,4)) #degree =4
plot(mdev~lstat)
points(lstat,fitted(fit5),col="Blue",pch=20)
summary(fit5)

#Interaction
fit6=lm(medv~lstat*age,Boston)
summary(fit6)

#final
fit7=lm(medv~lstat+crim+rm+dis+black+chas+nox+rad+tax+ptratio+I(lstat^2)+I(rm^2))
summary(fit7)
plot(fit7)


