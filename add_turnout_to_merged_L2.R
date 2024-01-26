#Nathaniel Flemming
# 24/1/24

# Descriptive statistics
library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet

############################################################# read in data
### L2 Data
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2<-read.csv('L2_join_poll_place.csv')
L2 <- subset(L2, select = -c(CountyEthnic_LALEthnicCode_2018, 
                             CountyEthnic_Description_2018))
## create mini dataset for troubleshooting
L2_mini <- L2[sample(nrow(L2), 1000),]

### PA voterfile
setwd('C:/Users/natha/Desktop/Polling Places/data/FVEData')
VF18 <- read.csv('voterturnout18.csv')

### merge with L2 records based on name, age, gender, county
test<-left_join(L2_mini, VF18, by=c())






