#APPLIED EXCERCISES
8.
library(ISLR)
fix(Auto)
na.omit(Auto)
lm.fit=lm(mpg~horsepower, data=Auto)
#IS THERE A RELATIONSHIP BETWEEN PREDICTOR AND RESPONSE VARIABLE?
summary(lm.fit)
#Yes, there is a relationship between horsepower and mpg as deterined by
#testing the null hypothesis of all regression coefficients equal to zero.
#Since the F-statistic is far larger than 1 and the p-value of the F-statistic
#is close to zero we can reject the null hypothesis and state there is a
#statistically significant relationship between horsepower and mpg.
#HOW STRONG IS THE RELATIONSHIP?
attach(Auto)
mean(mpg)
summary(lm.fit)$sigma #RSE
#To calculate the residual error relative to the response we use the mean of
#the response and the RSE. The mean of mpg is 23.4459. The RSE of the lm.fit
#was 4.906 which indicates a percentage error of 20.9248%. The R2 of the
#lm.fit was about 0.6059, meaning 60.5948% of the variance in mpg is explained
#by horsepower.
#The relationship between mpg and horsepower is negative. The more
#horsepower an automobile has the linear regression indicates the less mpg
#fuel efficiency the automobile will have.
#The relationship between mpg and horsepower is negative. The more horsepower an automobile has the linear regression indicates the less mpg fuel efficiency the automobile will have.
#PREDICT MPG WHEN HORSEPOWER IS 98
predict(lm.fit, data.frame(horsepower=98), interval = "confidence")
predict(lm.fit, data.frame(horsepower=98), interval="prediction")
predict(lm.fit, data.frame(horsepower=98)) #without intervals
plot(mpg, horsepower)
abline(lm.fit, lwd=7)
par(mfrow=c(2,2))
plot(lm.fit) #diagnostic plots of the least square regression fit
#Based on the residuals plots, there is some evidence of non-linearity.
9.
pairs(Auto, col="aquamarine4")
cor(subset(Auto, select=-name))
Auto1=subset(Auto, select = -name)
lm.fit1=lm(mpg~., data=Auto1)
summary(lm.fit1)
#Yes, there is a relatioship between the predictors and the response by testing
#the null hypothesis of whether all the regression coefficients are zero. The F -statistic
#is far from 1 (with a small p-value), indicating evidence against the null hypothesis.
#Looking at the p-values associated with each predictor’s t-statistic, we see that
#displacement, weight, year, and origin have a statistically significant relationship,
#while cylinders, horsepower, and acceleration do not.
#The regression coefficient for year, 0.7508, suggests that for every one year, mpg increases
#by the coefficient. In other words, cars become more fuel efficient every year by almost 1 mpg / year.
plot(lm.fit1)
cor(subset(Auto, select=-name))

plot(lm.fit1)
#The fit does not appear to be accurate, because there is a discernible curve pattern to the 
#residuals plots. From the leverage plot, point 14 appears to have high leverage, although not 
#a high magnitude residual.

lm.fit2=lm(mpg~displacement:cylinders, data=Auto)
summary(lm.fit2)
#Interaction between displacement and cylinders appears to be statistically significant

lm.fit4=lm(mpg~horsepower:weight, data=Auto)
summary(lm.fit4)
lm.fit5=lm(mpg~origin:year, data=Auto)
summary(lm.fit5)
lm.fit3=lm(mpg~displacement*cylinders, data=Auto)
summary(lm.fit3)

lm.fit7=lm(mpg~log(weight)+sqrt(horsepower)+acceleration+I(acceleration^2))
summary(lm.fit7)
plot(lm.fit7)
plot(predict(lm.fit7), rstudent(lm.fit7))
#log(weight), sqrt(horsepower), and acceleration^2 all have statistical 
#significance of some sort. The residuals plot has less of a discernible 
#pattern than the plot of all linear regression terms. The studentized residuals 
#displays potential outliers (>3). The leverage plot indicates more than three 
#points with high leverage.
#However, 2 problems are observed from the above plots: 1) the residuals vs 
#fitted plot indicates heteroskedasticity (unconstant variance over mean) in
#the model. 2) The Q-Q plot indicates somewhat unnormality of the residuals.
#So, a better transformation need to be applied to our model. From the 
#correlation matrix in 9a., displacement, horsepower and weight show a similar 
#nonlinear pattern against our response mpg. This nonlinear pattern is very close
#to a log form. So in the next attempt, we use log(mpg) as our response variable.
#The outputs show that log transform of mpg yield better model fitting 
#(better R^2, normality of residuals).

10.
library(ISLR)
fix(Carseats)
sales.fit=lm(Sales~Price+Urban+US, data=Carseats)
summary(sales.fit)
#The linear regression suggests a relationship between price and sales given the low p-value of the t-statistic. 
#The coefficient states a negative relationship between Price and Sales: as Price increases, Sales decreases.

#The linear regression suggests that there isn’t a relationship between the location of the store and the number 
#of sales based on the high p-value of the t-statistic.

#The linear regression suggests there is a relationship between whether the store is in the US or not and the 
#amount of sales. The coefficient states a positive relationship between USYes and Sales: if the store is in 
#the US, the sales will increase by approximately 1201 units.

#Sales = 13.04 + -0.05 Price + -0.02 UrbanYes + 1.20 USYes

#The null hypothesis Ho: Beta=0 for Price and USYes can be rejected, based on the p-values, F-statistic, and p-value of the F-statistic.

sales.fit1=lm(Sales~Price+US, data=Carseats)
summary(sales.fit1)
attach(Carseats)
Error1=summary(sales.fit1)$sigma/mean(Sales)
Error1 
summary(sales.fit1)$r.sq
Error=summary(sales.fit)$sigma/mean(Sales)
Error 
summary(sales.fit)$r.sq    
#They both fit data similarly, but sales.fit1 does it slightly better, based on R squared and RSE.

confint(sales.fit1)
plot(predict(sales.fit1), rstudent(sales.fit1))
#bounded by -3 to 3, so not potential outliers are suggested from the linear regression.
plot(sales.fit1)
#There are a few observations that greatly exceed (p+1)/n (0.0076) on the leverage-statistic plot that suggest
#that the corresponding points have high leverage.

11. 
# The following code generates predictor x and response y
set.seed(1)
x=rnorm(100)
y=2*x+rnorm(100) 
fit=lm(y~x+0) #regresssion without an intercept 

summary(fit)
#An estimate of slope is 1.9939, standard error 0.1065. T value is 18.73 and p-value 
#associated with T value is close to 0, meaning that estimate is significant. Null hypothesis is regected

fit1=lm(x~y+0)
summary(fit1)
#The estimate of the coefficient is -.3911, st error is 0.02089, t value is 18.73 and p value 
#associated with t value is close to 0. Null hypothesis is rejected.

#Both results in (a) and (b) reflect the same line created in 11a. In other words, y=2x+?? could also be written x=0.5???(y?????).

#The following formula calculates t-value statistic for the null hypothesis. It gives the same result as t-value in summary
sqrt(length(x)-1)*sum(x*y)/sqrt((sum(x*x)*sum(y*y)-sum(x*y)*sum(x*y)))

#Using the previous formula, If you swap t(x,y) as t(y,x), then you will find t(x,y) = t(y,x).

# when regression is performed with an intercept, the t-statistic for H0 : ??1 = 0 is the same for the regression of y
#onto x as it is for the regression of x onto y.
fit2=lm(y~x)
summary(fit2)
fit3=lm(x~y)
summary(fit3)

12.
#When the sum of the squares of the observed y-values are equal to the sum of the squares of the observed x-values, coefficient estimates x~y and y~x will be equal

set.seed(1)
x=rnorm(100)
y=2*x
lm.fit=lm(y~x+0)
lm.fit1=lm(x~y+0)
summary(lm.fit)
summary(lm.fit1)
#Estimates are different

set.seed(1)
x=rnorm(100)
y= -sample(x, 100)
lm.fit2=lm(y~x+0)
lm.fit3=lm(x~y+0)
summary(lm.fit2)
summary(lm.fit3)
#Estimates are the same

13. 
set.seed(1)
x=rnorm(100)
eps=rnorm(100, 0, sqrt(0.25))
y=-1+0.5*x+eps
#Length of the vector y is 100. b0 is -1 and B1 is 0.5.

plot(x, y)

fit=lm(y~x)
summary(fit)
#The estimates are close to the true values of coefficients
abline(fit, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

fit1=lm(y~x+I(x^2))
summary(fit1)
#There is evidence that model fit has increased over the training data given the slight increase in R squared and RSE.
#P-values and t-value suggest there is no relationship between y and x^2.


14. 
set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2*x1+0.3*x2+rnorm(100)
cor(x1, x2)
plot(x1, x2)
fit=lm(y~x1+x2)
summary(fit)
fit1=lm(y~x1)
summary(fit1)
fit2=lm(y~x2)
summary(fit2)

#No, because x1 and x2 have collinearity, it is hard to distinguish their effects when regressed 
#upon together. When they are regressed upon separately, the linear relationship between y and each predictor is indicated more clearly.

x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y, 6)
fit3=lm(y~x1+x2)
summary(fit3)
fit4=lm(y~x1)
summary(fit4)
fit5=lm(y~x2)
summary(fit5)
par(mfrow=c(2,2))
plot(fit3)
plot(fit4)
plot(fit5)
plot(predict(lm.fit3), rstudent(lm.fit3))
plot(predict(lm.fit4), rstudent(lm.fit4))
plot(predict(lm.fit5), rstudent(lm.fit5))

#Looking at the studentized residuals, we don’t observe points too far from the |3| value cutoff, except for the second linear regression: y ~ x1.

15.
library(MASS)
fix(Boston)
lm.zn=lm(crim~zn, data=Boston)
summary(lm.zn) #yes

lm.indus=lm(crim~indus, data=Boston)
summary(lm.indus) #yes

lm.chas=lm(crim~chas, data=Boston)
summary(lm.chas) #no

lm.nox=lm(crim~nox, data=Boston)
summary(lm.nox) #yes

lm.rm=lm(crim~rm, data=Boston)
summary(lm.rm) #yes

lm.age=lm(crim~age, data=Boston)
summary(lm.age) #yes

lm.dis=lm(crim~dis, data=Boston)
summary(lm.dis) #yes

lm.rad=lm(crim~rad, data=Boston)
summary(lm.rad) #yes

lm.tax=lm(crim~tax, data=Boston)
summary(lm.tax) #yes

lm.ptratio=lm(crim~ptratio, data=Boston)
summary(lm.ptratio) #yes

lm.black=lm(crim~black, data=Boston)
summary(lm.black) #yes

lm.lstat=lm(crim~lstat, data=Boston)
summary(lm.lstat) #yes

lm.medv=lm(crim~medv, data=Boston)
summary(lm.medv) #yes




crim.fit=lm(crim~., data=Boston)
summary(crim.fit)
#The null hypothesis can be rejected for zn, dis, rad, black, medv at 5% significance level.

x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(crim.fit)[2:14]
plot(x, y)


lm.zn=lm(crim~poly(zn,3))
summary(lm.zn) # 1, 2

lm.indus=lm(crim~poly(indus,3))
summary(lm.indus) # 1, 2
