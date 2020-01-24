#Doing some initial exploratory analysis
data <- read.csv("AB_NYC_2019.csv")
class(data)
dim(data)
summary(data)
library(ggplot2)
library(tidyverse)

#Renaming columns
data$host_count <- data$calculated_host_listings_count
data$boroughs <- data$neighbourhood_group #supposed to be replaced, but new column was created
data <- data[-c(5, 15)] #deleting replicated column
data <- data[c(1:4, 16, 5:13, 15, 14)] #reordering columns

#Here I visualise quantitative data in form of histograms
boxplot(data$price)
boxplot(data$minimum_nights)
boxplot(data$number_of_reviews)
boxplot(data$reviews_per_month)
boxplot(data$host_count)
boxplot(data$availability_365)

#Correcting errors

#Type conversions
data$name <- as.character(data$name)
data$host_name <- as.character(data$host_name)


#Trimming whitespaces
library(stringr)
data$name <- str_trim(data$name)
data$last_review <- str_trim(data$last_review)

#Replacing outliers with the median
vec1 <- boxplot.stats(data$price)$out
data$price[data$price %in% vec1] <- median(data$price) 

vec2 <- boxplot.stats(data$minimum_nights)$out
data$minimum_nights[data$minimum_nights %in% vec2] <- median(data$minimum_nights)

vec3 <- boxplot.stats(data$number_of_reviews)$out
data$number_of_reviews[data$number_of_reviews %in% vec3] <- median(data$number_of_reviews)

vec4 <- boxplot.stats(data$reviews_per_month)$out
data$reviews_per_month[data$reviews_per_month %in% vec4] <- median(data$reviews_per_month)

#checking for the presence of missing values in entire dataframe
any(is.na(data)) 
sum(is.na(data))
sum(is.na(data$reviews_per_month)) #All missing values are in this column
#Replacing missing values with the median
data$reviews_per_month[is.na(data$reviews_per_month)] <- median(data$reviews_per_month)
    

#Some data analysis
data %>% group_by(boroughs) %>% summarise(avg_price=mean(price)) 
#Manhattan has highest prices for apartments and rooms on average
cor(data$price, data$number_of_reviews) #The correlation coefficient coefficient is very close to 0.
cor(data$price, data$host_count) #Small, positive correlation between price and number of hosts


fit <- lm(minimum_nights~price+room_type+number_of_reviews, data=data)
summary(fit)
