#EIGHTTH EXCERCISE
setwd("C:/Users/77785/Desktop/Datababe/R/Datasets")
college <- read.csv("college.csv")
fix(college)
rownames(college)=college[, 1] #R has given each row a name coreesponding to each university name
college <- college[,-1] #removing the column with university names
summary(college)
pairs(college[, 1:10], col="aquamarine4")

attach(college)
Private=as.factor(college$Private)
plot(Private, Outstate, col="aquamarine4")

#Creating new variable
Elite=rep("No", nrow(college)) #replicates No for all rows in college dataset 
Elite[college$Top10perc>50]="Yes" #Yes if condition is fulfilled
Elite=as.factor(Elite)
college=data.frame(college,Elite)
summary(college) #There are 78 elite universities
plot(Elite, Outstate)
hist(Grad.Rate, breaks = 10)
hist(Apps, breaks=10)
hist(Accept, breaks=10)
hist(Enroll, breaks = 10)
par(mfrow=c(2,2)) #divides the print window into 4 regions so that 4 graphs can be displayed

#NINTH EXCERCISE
library(ISLR)
Auto=read.table("Auto.data", header=T, na.strings ="?")
na.omit(Auto)
fix(Auto)
#Name and origin are the qualitative predictors, the rest are quantitative
attach(Auto)
range(mpg)
range(cylinders)
range(displacement)
range(horsepower)
range(weight)
range(acceleration)
range(year) #instead of doing this, the method below is more efficient
sapply(Auto[,1:7], range)
sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)

#Removing 10th to 85th observation, calculating range, mean and sd for quantitative variables again 
auto <- Auto[-(10:85),]
sapply(Auto[,1:7], range)
sapply(Auto[,1:7], mean)
sapply(Auto[,1:7], sd)

pairs(Auto)
plot(displacement, mpg)
plot(horsepower, mpg)
plot(weight, mpg)
plot(year, mpg)


#TENTH EXERCISE
install.packages("MASS")
library(MASS)
load.table("Boston.data")
fix(Boston) #The dataset has 506 rows and 14 columns. Columns represent variables and rows represent obresvations
pairs(Boston)
?Boston
attach(Boston)
hist(crim)
hist(tax)
hist(ptratio)
range(crim)
range(tax)
range(ptratio)