#Nathaniel Flemming
# 1/11/23

# Merge L2 data to polling locations
library(tidyverse)
library(data.table)

#read in data
setwd("C:/Users/natha/Desktop/Polling Places/data")
row1<-read.csv('PA_combined.csv', nrows = 1)
L2<-fread('PA_combined.csv', 
            select = c('LALVOTERID',
                       'Residence_Addresses_AddressLine_2020','Residence_Addresses_ExtraAddressLine_2020',
                       'Residence_Addresses_City_2020','Residence_Addresses_State_2020',
                       'Residence_Addresses_Zip_2020',
                       'Residence_Addresses_Latitude_2020','Residence_Addresses_Longitude_2020',
                       'Voters_Gender_2020','Voters_Age_2020','Parties_Description_2020',
                       'Religions_Description_2020','Voters_OfficialRegDate_2020',
                       'MaritalStatus_Description_2020','CommercialData_PresenceOfChildrenCode_2020',
                       'CommercialData_EstimatedHHIncomeAmount_2020',
                       'CommercialData_Education_2020','County_2020','Voters_FIPS_2020',
                       'Voters_Active_2020','CountyEthnic_LALEthnicCode_2020',
                       'EthnicGroups_EthnicGroup1Desc_2020','Ethnic_Description_2020',
                       'CountyEthnic_Description_2020'
                                          ))
poll<-read.csv('polllocation_structure_and_keyword.csv')







