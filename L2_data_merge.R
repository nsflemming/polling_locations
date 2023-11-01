#Nathaniel Flemming
# 1/11/23

# Merge L2 data to polling locations
library(tidyverse)
library(data.table)

#read in data
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2<-read.csv('PA_combined.csv', nrows = 1)
test<-fread('PA_combined.csv', 
            select = c('LALVOTERID','Voters_FirstName_2016','Voters_MiddleName_2016',
                       'Voters_LastName_2016','Voters_NameSuffix_2016',
                                          ))
poll<-read.csv('polllocation_structure_and_keyword.csv')







