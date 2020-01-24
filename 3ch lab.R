#SIMPLE LINEAR REGRESSION
library(MASS)
load.table("Boston.data")
fix(Boston)
names(Boston) #shows names of independet variables

lm.fit=lm(medv~lstat, data=Boston)
attach(Boston) #alternative to the perious line of code
lm.fit=lm(medv~lstat)
lm.fit #gives some basic info about the model
summary(lm.fit) #more detailed information about the model
names(lm.fit) #shows what pieces of information are stored
lm.fit$coefficients #extracting coefficients
coef(lm.fit) #shows the estimated coefficients of the model
confint(lm.fit) #to obtain confidence interval (95% CI by default)

#predict() function is used to produce confidence intervals and prediction intervals
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="confidence")
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="prediction")
#prediction and confidence intervals are centered around the fitted values, but prediction interval is wider

attach(Boston)
plot(medv, lstat)
abline(lm.fit) #abline() function can be used to draw any line not just regression
abline(lm.fit, lwd=3) #lwd=3 increases the width of the line by 3 factors
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
#pch creates different plotting symbols
plot(lstat, medv, pch=11)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20, col="pink")

#Next we examine diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit), col="aquamarine4")
plot(predict(lm.fit), rstudent(lm.fit), col="aquamarine4") #rstudent return studentisized residuals 

plot(hatvalues(lm.fit), col="aquamarine4") #hatvalues computes levearge statistics for any number of predictors
which.max(hatvalues(lm.fit)) #shows the observation with the maximum 


#MULTIPLE LINEAR REGRESSION
lm.fit=lm(crim~lstat+age, data=Boston)
summary(lm.fit)
lm.fit=lm(crim~., data=Boston)
summary(lm.fit)
summary(lm.fit)$r.sq #shows R-squared
summary(lm.fit)$sigma #shows RSE

install.packages("car")
library(car)
vif(lm.fit) #variance inflation factors
lm.fit1=lm(medv~.-age, data=Boston) #takes all variables as predictors apart from age
lm.fit1=update(lm.fit, ~.-age) #alternative for the previous line of  code
summary(lm.fit1)


#INTERACTION TERMS
lm.fit2=lm(medv~lstat:black, data=Boston)#includes an interaction between lstat and black
summary(lm.fit2)
lm.fit3=lm(medv~lstat*black, data=Boston)#includes both lstat, black, as well as interaction between them 
summary(lm.fit3)


#NON-LINEAR TRANSFORMATION OF THE PREDICTORS
lm.fit4=lm(medv~lstat+I(lstat^2), data=Boston) #I() is needed because ^ has a special meaning in a formula
summary(lm.fit4)
lm.fit=lm(medv~lstat, data=Boston)
anova(lm.fit, lm.fit4)
#Anova  test compares 2 models. The null hypothesis is that both models fit data equally well. The alternative hypothesis is that the the fuller model is superior.
par(mfrow=c(2,2))
plot(lm.fit4) #there is little discernible pattern in the residuals

lm.fit5=lm(medv~poly(lstat,5), data=Boston)
summary(lm.fit5) #poly() is a better function to create polynomial within lm()
#investigations of the data reveals that no polynomial terms beyond fifth order
#have significant p-values in a regression fit
lm.fit6=lm(medv~log(lstat), data=Boston)
summary(lm.fit6)

#QUALITATIVE PREDICTORS
library(ISLR)
fix(Carseats)
names(Carseats)

lm.fit7=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit7)

attach(Carseats)
contrasts(ShelveLoc) #contrasts() returns the coding that R uses for the dummy variables



#WRITING FUNCTIONS
LoadLibraries=function(){ #{ tells R that multiple commands are to be inputed
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
} #tells R no further functions will be inputed

LoadLibraries #tells what function is
LoadLibraries() #executes the function