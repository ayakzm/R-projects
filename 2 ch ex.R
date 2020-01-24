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
#by the coefficient. In other words, cars become more fuel efficient every year by almost 1 mpg
#/ year.

plot(lm.fit1)
#The fit does not appear to be accurate because there is a discernible 
#curve pattern to the residuals plots. From the leverage plot, point 14
#appears to have high leverage, although not a high magnitude residual.

lm.fit2=lm(mpg~displacement*cylinders, data=Auto)
lm.fit3=lm(mpg~displacement:cylinders, data=Auto)
