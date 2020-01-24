library(ggplot2)
library(tidyverse)
library(lubridate)

setwd("C:/Users/_____/Desktop")
data <- read.csv('polls.csv', sep = ';')

data %>% 
  mutate(Net_approv=Approval-Disapproval) %>% 
  select(-No.opinion) %>% 
  mutate(Date=mdy(Date)) %>%
  ggplot() + geom_line(aes(x=Date, y=Net_approv))  + 
  annotate("text", x=mdy("03 23 2010"), y=-0.05, label="Affordable Care Act signed", angle=120, hjust = 0, vjust =1) +
  annotate("text", x=mdy("05 20 2010"), y=0.0, label="Deepwater Horizon explosion", angle=60, hjust = 0, vjust =1) +
  annotate("text", x=mdy("11 02 2010"), y=-0.03, label="Republicans sweep midterm elections", angle=60, hjust =0, vjust=1) +
  annotate("text", x=mdy("05 01 2011"), y= 0.0, label="Osama bin Laden killed", angle=60, hjust = 0, vjust =1) +
  annotate("text", x=mdy("11 06 2012"), y= 0.05, label="Re-elected to second term", angle=60, hjust = 0, vjust =1)
  
             