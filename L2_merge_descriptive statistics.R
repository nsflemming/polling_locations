#Nathaniel Flemming
# 12/6/23

# Descriptive statistics
library(tidyverse)
library(data.table) #read in data selectively

# read in data
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2<-read.csv('L2_join_poll_place.csv')

## create mini dataset for troubleshooting
L2_mini <- L2[sample(nrow(L2), 1000),]

##### geographic distribution of missingness
## dataframe of rows with any missing data
L2_missing<-L2[!complete.cases(L2),]
## create mini dataset for troubleshooting
L2_miss_mini <- L2_missing[sample(nrow(L2), 1000),]

##### Cross tabs of demographic characteristics and polling place categories


##### Missingness of religious description in L2



