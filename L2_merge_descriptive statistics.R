#Nathaniel Flemming
# 12/6/23

# Descriptive statistics
library(tidyverse)
library(data.table) #read in data selectively

# read in data
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2<-read.csv('L2_join_poll_place.csv')
L2 <- subset(L2, select = -c(CountyEthnic_LALEthnicCode_2018, 
                                       CountyEthnic_Description_2018))

## create mini dataset for troubleshooting
L2_mini <- L2[sample(nrow(L2), 1000),]


##### geographic distribution of missingness
## dataframe of rows with any missing data
L2_missing<-L2[!complete.cases(L2),]
### remove any entirely missing columns

## create mini dataset for troubleshooting
L2_miss_mini <- L2_missing[sample(nrow(L2), 1000),]

### missingness by county
L2_missing$county <- factor(L2_missing$county)
temp<-count(L2_missing, county)
## normalize by number of registered voters in each county
temp$reg_voter_pop<-count(L2, county)$n
temp$prop_miss<-temp$n/temp$reg_voter_pop
write.csv(temp, 'any_missingness_by_county.csv')

##### Missingness of religious description in L2



##### Cross tabs of demographic characteristics and polling place categories






