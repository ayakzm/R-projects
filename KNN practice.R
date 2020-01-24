df <- data(iris)
head(iris)
#Generating a random number that is 90% of the total number of rows in dataset
ran <- sample(1:nrow(iris), 0.9*nrow(iris))

#The normalization function is created
nor <- function(x){(x-min(x))/(max(x)-min(x))}

iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
summary(iris_norm)

#extracting training set
iris_train <- iris_norm[ran,]

#extracting testing set
iris_test <- iris_norm[-ran,]

#extract 5th column of train dataset because it will be used as 'cl'
#argument in knn function
iris_target_category <- iris[ran,5]

#extract 5th column if test dataset to measure the accuracy
iris_test_category <-iris[-ran,5]

install.packages("class")
library(class)

#run knn function
pr <- knn(iris_train,iris_test,cl=iris_target_category,k=13)

##create confusion matrix
tab <- table(pr,iris_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


install.packages("ggplot2")
library(ggplot2)
data(diamonds)
dia <- data.frame(diamonds)
ran <- sample(1:nrow(dia), 0.9*nrow(dia))

#The normalization function is created
nor <- function(x){(x-min(x))/(max(x)-min(x))}

dia_norm <- as.data.frame(lapply(dia[,c(1,5,6,7,8,9,10)], nor))
summary(dia_norm)

dia_train <- dia_norm[ran,]

#extracting testing set
dia_test <- dia_norm[-ran,]

#extract 5th column of train dataset because it will be used as 'cl'
#argument in knn function
dia_target_category <- dia[ran,2]

#extract 5th column if test dataset to measure the accuracy
dia_test_category <-dia[-ran,2]

library(class)

#run knn function
pr <- knn(dia_train,dia_test,cl=dia_target_category,k=13)

##create confusion matrix
tab <- table(pr,dia_test_category)

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

