#Nathaniel Flemming
# 24/1/24

# Descriptive statistics
library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet
library(lubridate) #convert dob to age

############################################################# read in data
### L2 Data
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2<-read.csv('L2_join_poll_place.csv')
L2 <- subset(L2, select = -c(CountyEthnic_LALEthnicCode_2018, 
                             CountyEthnic_Description_2018))
## create mini dataset for troubleshooting
L2_mini <- L2[sample(nrow(L2), 1000),]

### Voter file data
setwd("C:/Users/natha/Desktop/Polling Places/data/FVEData")
VF18 = read.csv('voterturnout18.csv')

### merge with L2 records based on name, address, gender
## change strings to all uppercase
L2$Voters_FirstName_2018<-toupper(L2$Voters_FirstName_2018)
L2$Voters_MiddleName_2018<-toupper(L2$Voters_MiddleName_2018)
L2$Voters_LastName_2018<-toupper(L2$Voters_LastName_2018)
L2$Residence_Addresses_AddressLine_2018<-toupper(L2$Residence_Addresses_AddressLine_2018)

## convert VF DOB to age?
VF18$DOB_date <- as.Date(VF18$DOB, "%m/%d/%Y")
#election <- as.Date('11/06/2018', "%m/%d/%Y")
end18<-as.Date('12/31/2018', "%m/%d/%Y")
#calculate age at end of 2018
VF18<- VF18 %>%
  mutate(Voters_Age_2018 = interval(DOB_date, end18) %/% years(1))

## combine VF address lines
VF18$AddressLine1 <- paste(VF18$HouseNumber, VF18$StreetName)

##### REMOVE POTENTIAL voter file DUPLICATES
VF18 <- VF18 %>%
  select(c(FirstName, MiddleName, LastName, County, AddressLine1, Gender, 
           Voters_Age_2018, Zip, VoteMethod)) %>%
  #group by characteristics
  group_by(FirstName, MiddleName, LastName, County, AddressLine1, Gender, 
             Voters_Age_2018, Zip)%>%
  #overwrite empty vote method with first non empty vote method
  mutate(VoteMethod = ifelse(all(VoteMethod == ""), NA, 
                             replace(VoteMethod, VoteMethod == "", 
                                     VoteMethod[VoteMethod != ""][1]))) %>%
  ungroup()%>%
  distinct()

#rename some variables
L2 <- L2 %>%
  rename('FirstName'='Voters_FirstName_2018', 
          'MiddleName'='Voters_MiddleName_2018',
          'LastName'='Voters_LastName_2018',
         'County' = 'county_name')
#change int to character
L2$Residence_Addresses_Zip_2018 = as.character(L2$Residence_Addresses_Zip_2018)
#left join
test<-left_join(L2, VF18, by=c('FirstName','MiddleName','LastName',
                                    'County',
                                    'Residence_Addresses_AddressLine_2018' = 'AddressLine1',
                                    'Voters_Gender_2018' = 'Gender',
                                    'Voters_Age_2018',
                                    'Residence_Addresses_Zip_2018' = 'Zip'
                                    ))

#VF18%>%
#  filter(FirstName=='GROVER' & MiddleName=='C'& LastName=='BEASLEY'&
#         County=='ADAMS' & Voters_Age_2018==76)

#save
setwd("C:/Users/natha/Desktop/Polling Places/data")
write.csv(test, file='L2_join_poll_turnout.csv')




