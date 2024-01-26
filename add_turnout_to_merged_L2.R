#Nathaniel Flemming
# 24/1/24

# Descriptive statistics
library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet
library(eeptools) #convert dob to age

############################################################# read in data
### L2 Data
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2<-read.csv('L2_join_poll_place.csv')
L2 <- subset(L2, select = -c(CountyEthnic_LALEthnicCode_2018, 
                             CountyEthnic_Description_2018))
## create mini dataset for troubleshooting
L2_mini <- L2[sample(nrow(L2), 1000),]


### merge with L2 records based on name, address, gender
## change strings to all uppercase
L2_mini$Voters_FirstName_2018<-toupper(L2_mini$Voters_FirstName_2018)
L2_mini$Voters_MiddleName_2018<-toupper(L2_mini$Voters_MiddleName_2018)
L2_mini$Voters_LastName_2018<-toupper(L2_mini$Voters_LastName_2018)
L2_mini$Residence_Addresses_AddressLine_2018<-toupper(L2_mini$Residence_Addresses_AddressLine_2018)
## convert DOB to age?
test <- as.Date(VF18$DOB, "%m/%d/%Y")
election <- as.Date('11/06/2018', "%m/%d/%Y")
end18<-as.Date('12/31/2018', "%m/%d/%Y")
round(age_calc(test[1], end18, unit='years'))
## combine VF address lines
VF18$AddressLine1 <- paste(VF18$HouseNumber, VF18$StreetName)

#left join
test<-left_join(L2_mini, VF18, by=c('Voters_FirstName_2018' = 'FirstName', 
                                    'Voters_MiddleName_2018' = 'MiddleName',
                                    'Voters_LastName_2018' = 'LastName',
                                    'county' = 'County',
                                    'Residence_Addresses_AddressLine_2018' = 'AddressLine1',
                                    'Voters_Gender_2018' = 'Gender'
                                    ))
#save
setwd("C:/Users/natha/Desktop/Polling Places/data")
write.csv(test, file='L2_join_poll_turnout.csv')




