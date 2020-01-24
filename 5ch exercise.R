#APPLIED EXCERCISES
8. set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
n = 100 p = 2
Y=X−2X^2+ϵ
plot(x,y)
#The scatteplot gives concave down curve. This is a quadratic curve. The x is from -2 to 2 and y is from -8 to 2.

library(boot)
data=data.frame(x,y)
set.seed(1)
glm.fit=glm(y~x, data=data)
cv.err=cv.glm(data, glm.fit)$delta
cv.err

glm.fit1=glm(y~poly(x,2), data=data)
cv.err=cv.glm(data, glm.fit1)$delta
cv.err

glm.fit2=glm(y~poly(x,3), data=data)
cv.err=cv.glm(data, glm.fit2)$delta
cv.err

glm.fit3=glm(y~poly(x,4), data=data)
cv.err=cv.glm(data, glm.fit3)$delta
cv.err

#Now we are trying out the same thing with setting another seed
data1=data.frame(x,y)
set.seed(17)
glm.fit4=glm(y~x, data=data1)
cv.glm(data1, glm.fit4)$delta

glm.fit5=glm(y~poly(x,2), data=data1)
cv.glm(data1, glm.fit5)$delta

glm.fit6=glm(y~poly(x,3), data=data1)
cv.glm(data1, glm.fit6)$delta

glm.fit7=glm(y~poly(x,4), data=data1)
cv.glm(data1, glm.fit7)$delta
#Exact same, because LOOCV will be the same since it evaluates n folds of a single observation.

#The quadratic specification has the lowest LOOCV error. It was expected because the shape of scatterplot

summary(glm.fit1)
#p-values show statistical significance of linear and quadratic terms, which agrees with the CV results.


9.
library(MASS)
summary(Boston)
attach(Boston)
set.seed(1) #WHY SET.SEED() is even needed?
mean.medv=mean(medv)

medv.err=sd(medv)/sqrt(length(medv))
medv.err


boot.fn=function(data, index) return(mean(data[index]))
library(boot)
bstrap=boot(medv, boot.fn, 1000)
bstrap
#Bootstrap standard error is similar to the previous one

t.test(medv)
c(bstrap$t0-2*0.4119, bstrap$t0+2*0.4119) #You can approximate a 95 % confidence interval using the formula [μ̂ − 2SE(μ̂), μ̂ + 2SE(μ̂)].

med.medv=median(medv)
med.medv
boot.fn=function(data, index) return(median(data[index]))
bstrap=boot(medv, boot.fn, 1000)
bstrap
#Median of 21.2 with SE of 0.380. Small standard error relative to median value.


q10.medv=quantile(medv,0.1)
q10.medv
boot.fn = function(data, index) return(quantile(data[index], c(0.1)))
boot(medv, boot.fn, 1000)
#enth-percentile of 12.75 with SE of 0.511. Small standard error relative to tenth-percentile value.