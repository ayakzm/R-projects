#Assignment2

library(tidyverse)
library(dplyr)
library(ggplot2)
library(fivethirtyeight)

#Select 
select(data, time, gender)

#Filter 
filter(data, spend_amount>999)

#Arrange 
arrange(data, time)

#Mutate 
mutate(data, dollars_spent=spend_amount/100)

#Summarize 
summarise(data, average_spend=mean(spend_amount)) 
summarise(data, average_spend=mean(spend_amount)/100) 

#Without pipe
data2 <-mutate(data, dollars_spent=spend_amount/100)
data3 <-group_by(data2, gender) 
summarise(data3, mean=mean(dollars_spent), median=median(dollars_spent), n=n())
Yes, there is a difference. Women spend 2 cents less on average comparing to men.

#With pipe
data %>% mutate(dollar_amount=spend_amount/100) %>% group_by(gender) %>%
         summarise(mean = mean(dollar_amount), n = n())


data %>% filter(gender!="UNKNOWN", age_group!="UNKNOWN") %>% 
  group_by(gender, age_group) %>%  summarise(mean=mean(spend_amount), n=n()) %>% 
  ungroup(gender, age_group) %>% arrange(gender, age_group)
#Yes, women of different age groups spend different amounts. The same is with the men.


data %>%group_by(user_id) %>% arrange(time) %>% mutate(spend_number=row_number()) %>% 
  ungroup(user_id) %>% group_by(spend_number) %>% summarise(mean=mean(spend_amount), median=median(spend_amount), n=n())
#I made everything following instructions, but I didn't really understand the logic.


#Case Study
US_births_1994_2003 %>%
       select(-date) %>%
       filter(date_of_month %in% c(6, 13, 20)) %>%
       spread(key=date_of_month, value = births) %>%
       mutate(mean620=(`6`+`20`)/2) %>%
       mutate(avg_dif=((`13`-mean620)/mean620)*100) %>%
       group_by(day_of_week) %>% 
       summarise(mean=mean(avg_dif)) %>%
       ggplot() + geom_col(aes(x=day_of_week, y=mean), fill="purple")

