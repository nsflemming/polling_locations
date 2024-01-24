#Nathaniel Flemming
# 24/1/24

# Regressions

library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet

# read in data
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2<-read.csv('L2_join_poll_place.csv')
L2 <- subset(L2, select = -c(CountyEthnic_LALEthnicCode_2018, 
                             CountyEthnic_Description_2018))
## create mini dataset for troubleshooting
L2_mini <- L2[sample(nrow(L2), 1000),]


################################## Logistic Regression
#### Probability of turning out
#m1 <- lm(~)
