#Nathaniel Flemming
# 1/11/23

# Merge L2 data to polling locations
library(tidyverse)
library(data.table)

######################### Prepare data for arcGIS spatial join
#read in data
setwd("C:/Users/natha/Desktop/Polling Places/data")
#row1<-read.csv('PA_combined.csv', nrows = 1)
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
#read in poll location data
#### Voting precinct list matches 2018 polling location list better than 2020
poll<-read.csv('Pennsylvania_2018-11-06.csv')
## drop unnecessary columns
poll <- subset(poll, select = c(county_name, precinct_name, precinct_id))


### remove obs if missing address
L2 <- L2 %>%
  drop_na(Residence_Addresses_AddressLine_2020)

###combine voter coordinates into one so arcGIS can split it
L2$voterLongLat<-paste0(L2$Residence_Addresses_Latitude_2020,',',L2$Residence_Addresses_Longitude_2020)
L2$voterLongLat<-str_remove(L2$voterLongLat, " , ")

### get just id and coordinates
L2_mini<-subset(L2, select = c(LALVOTERID, voterLongLat))

### save to csv
setwd("C:/Users/natha/Desktop/Polling Places/data")
write.csv(L2_mini, 'L2_2020_coords.csv')

############################ Read in joined data after ArcGIS processing
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2_join <- read.csv('vtds_L2_join.csv')
#### reformat county-precinct naming in join and poll files to agree
# convert L2 to all uppercase
L2_join$county_pre<-toupper(L2_join$county_pre)
# join poll county and precinct strings
poll$county_pre = paste0(poll$county_name,' - ',poll$precinct_name)
#verify
poll$county_pre %in% L2_join$county_pre

### 
recode_sheet <- unique(subset(L2_join, select = c(county_pre)))
ref_sheet <- unique(subset(poll, select = c(county_name, precinct_name, precinct_id)))
## join recode and reference sheets relying on alphabetical order to match
# alphabetize and add index row
recode_sheet<-recode_sheet %>%
  arrange(county_pre) %>%
  # add index row
  mutate(index=1:nrow(recode_sheet))
ref_sheet<-ref_sheet %>%
  # alphabetize on county and precinct name
  arrange(county_name,precinct_name)%>%
  # add index row
  mutate(index=1:nrow(ref_sheet))
#join on index column
joined_sheet = left_join(recode_sheet, ref_sheet, by='index')
#### manually correct mismatches
# add apostrophe to prevent ids being converted to dates
joined_sheet$precinct_id = 
paste0('\'',joined_sheet$precinct_id)
setwd("C:/Users/natha/Desktop/Polling Places/data")
write.csv(joined_sheet, 'joined_sheet_mismatched.csv')



L2_test<-sample_n(L2_join, 10000)
#get precinct name from county-pre? closest to poll naming?
L2_test$precinct_name = str_extract_all(L2_test$county_pre, '(?<=\\-\\s).*')
## manually match L2 precinct name to poll location precinct ids
#L2_test<-L2_test%>%
#  mutate(precinct_id = case_when(
#  )

  
  
  
  
  
  
  
  