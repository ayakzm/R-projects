#THE VALIDATION SET APPROACH
library(ISLR)
set.seed(1)
train=sample(392, 196)
lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)
#Train index below selects only the observations that are not in the training set
#The code below estimates MSE for the liner regression
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2=lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

#If we set another seed, the results for MSE will be different
set.seed(2)
train=sample(392, 196)
lm.fit4=lm(mpg~horsepower, subset=train)
mean((mpg-predict(lm.fit4, Auto))[-train]^2)

lm.fit5=lm(mpg~poly(horsepower, 2), subset = train)
mean((mpg-predict(lm.fit5, Auto))[-train]^2)

lm.fit6=lm(mpg~poly(horsepower, 3), subset=train)
mean((mpg-predict(lm.fit6, Auto))[-train]^2)

#LOOCV 
#The LOOCV estimate can be automatically computed for any generalized
#linear model using the glm() and cv.glm() functions
#The glm() function is used to perform logistic regression by passing
#in the family="binomial" argument. But if we use glm() to fit a model
#without passing in the family argument, then it performs linear regression,
#just like the lm() function.
glm.fit=glm(mpg~horsepower, data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower, data=Auto)
coef(lm.fit)

#In this lab, we will perform linear
#regression using the glm() function rather than the lm() function because
#the latter can be used together with cv.glm(). The cv.glm() function is
#part of the boot library.
library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto, glm.fit)
cv.err$delta
#The cv.glm() function produces a list with several components.
#The two numbers in the delta vector contain the cross-validation results.

cv.error=rep(0,5)
for(i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}
cv.error


#K-FOLD CROSS-VALIDATION
set.seed(17)
cv.error.10=rep(0, 10)
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i]=cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10


#BOOTSTRAP
#we must first create
#a function, alpha.fn(), which takes as input the (X, Y ) data as well as
#a vector indicating which observations should be used to estimate ??. The
#function then outputs the estimate for ?? based on the selected observations.
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)

#The next command uses the sample() function to randomly select 100 observations 
#from the range 1 to 100, with replacement. This is equivalent
#to constructing a new bootstrap data set and recomputing ???? based on the
#new data set
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))

#We can implement a bootstrap analysis by performing this command many
#times, recording all of the corresponding estimates for ??, and computing
#the resulting standard deviation. However, the boot() function automates
#this approach. Below we produce R = 1, 000 bootstrap estimates for ??.
boot(Portfolio, alpha.fn, R=1000)

#We first create a simple function, boot.fn(), which takes in the Auto data
#set as well as a set of indices for the observations, and returns the intercept
#and slope estimates for the linear regression model. We then apply this
#function to the full set of 392 observations in order to compute the estimates of 
#??0 and ??1 on the entire data set using the usual linear regression
#coefficient estimate formulas
boot.fn=function(data, index)
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
boot.fn(Auto, 1:392)

#The boot.fn() function can also be used in order to create bootstrap esti-
#mates for the intercept and slope terms by randomly sampling from among
#the observations with replacement.
set.seed(1)
boot.fn(Auto, sample(392,392, replace = T))

#We use the boot()function to compute the standard errors of
#1000 bootstrap estimates for the intercept and slope terms
boot(Auto, boot.fn, 1000)


#The standard error estimates for ????0 and ????1 obtained using the formulas summary() function.
#Interestingly, these are somewhat different from the estimates obtained
#using the bootstrap. Does this indicate a problem with the bootstrap? In
#fact, it suggests the opposite. Recall that the standard formulas given in
#Equation 3.8 on page 66 rely on certain assumptions. For example, they
#depend on the unknown parameter ??2 , the noise variance. We then estimate
#??2 using the RSS. Now although the formula for the standard errors do not
#rely on the linear model being correct, the estimate for ??2 does. We see in
#Figure 3.8 on page 91 that there is a non-linear relationship in the data, and
#so the residuals from a linear fit will be inflated, and so will ????2 . Secondly,
#the standard formulas assume (somewhat unrealistically) that the xi are
#fixed, and all the variability comes from the variation in the errors i . The
#bootstrap approach does not rely on any of these assumptions, and so it is
#likely giving a more accurate estimate of the standard errors of ????0 and ????1
#than is the summary() function.
summary(lm(mpg~horsepower, data=Auto))$coef


#The same process is done for quadratic equation
boot.fn=function(data, index)
  coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index))
set.seed(1)
boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef

