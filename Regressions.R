#Nathaniel Flemming
# 24/1/24

# Regressions

library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet

# read in data
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2<-read.csv('L2_join_poll_turnout.csv')
#L2 <- subset(L2, select = -c(CountyEthnic_LALEthnicCode_2018, 
#                             CountyEthnic_Description_2018))
## binarize vote method to get vote/didn't vote
L2$Voted <- ifelse(is.na(L2$VoteMethod), 0, 1)
## create mini dataset for troubleshooting
L2_mini <- L2[sample(nrow(L2), 10000),]
### Full dataset too big for regression on this laptop

################################## Logistic Regression
#### Probability of turning out, location category as predictor
m_base <- glm(data=L2_mini, 
              Voted~CommercialData_Education_2018+
                CommercialData_EstimatedHHIncomeAmount_2018+
                EthnicGroups_EthnicGroup1Desc_2018+
                location_category
                , family = "binomial")
summary(m_base)
### Probability of turning out, if has/lacks child and is voting at a school
