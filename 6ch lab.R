#SUBSET SELECTION METHODS
#BEST SUBSET SELECTION
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
install.packages("leaps")
library(leaps)
#Regsubsets() function performs best subset selection by identifying the best model that contains a given number of predictors
regfit.full=regsubsets(Salary~., Hitters)
#Summary() outputs the best set of variables for each model size
summary(regfit.full)
#By default, regsubsets() only reports results up to the best eight-variable model.
regfit.full=regsubsets(Salary~., data=Hitters, nvmax = 19)
reg.summary=summary(regfit.full)
#The summary() function also returns R-squared, RSS, adjusted R-squared, Cp and BIC
names(reg.summary)
reg.summary$rsq
reg.summary$rss

#Plotting RSS, adjusted R2, C p , and BIC for all of the models at once will help us decide which model to select.
par(mfrow=c(2, 2))
#Type="l" option tells R to connect the plotted points with lines
plot(reg.summary$rss, xlab = "Number of Variables", ylab="RSS", type = "l", col="aquamarine4")
plot(reg.summary$adjr2, xlab = "Number of Vatiables", ylab="R2", type = "l", col="aquamarine4")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col="aquamarine4", cex=2, pch=20)
which.min(reg.summary$rss)
points(19, reg.summary$rss[19], col="aquamarine4", cex=5, pch=20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp",type = "l", col="aquamarine4")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], cex=2, pch=20, col="aquamarine4")
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", col="aquamarine4")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], cex=2,pch=20, col="aquamarine4")


#ASK DAVIDD
#The regsubsets() function has a built-in plot() command which can be used to display the selected variables for the best model with a given number of predictors, ranked according to the BIC, Cp , adjusted R2 , or AIC. 
plot(regfit.full, scale="r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale = "bic")

coef(regfit.full, 6)
