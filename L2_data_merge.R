#Nathaniel Flemming
# 1/11/23

# Merge L2 data to polling locations
library(tidyverse)
library(data.table) #read in L2 data selectively

######################### Prepare data for arcGIS spatial join
#read in data
setwd("C:/Users/natha/Desktop/Polling Places/data")
row1<-read.csv('PA_combined.csv', nrows = 1)
L2<-fread('PA_combined.csv', 
            select = c('LALVOTERID',
                       'Residence_Addresses_AddressLine_2018',
                       'Residence_Addresses_Latitude_2018',
                       'Residence_Addresses_Longitude_2018'))
#read in poll location data
#### Voting precinct list matches 2018 polling location list better than 2020
poll<-read.csv('Pennsylvania_2018-11-06.csv')
## drop unnecessary columns
poll <- subset(poll, select = c(county_name, precinct_name, precinct_id))


### remove obs if missing address
L2 <- L2 %>%
  drop_na(Residence_Addresses_AddressLine_2018)

###combine voter coordinates into one so arcGIS can split it
L2$voterLongLat<-paste0(L2$Residence_Addresses_Latitude_2018,',',L2$Residence_Addresses_Longitude_2018)
L2$voterLongLat<-str_remove(L2$voterLongLat, " , ")

### get just id and coordinates
L2_mini<-subset(L2, select = c(LALVOTERID, voterLongLat))

### save to csv
setwd("C:/Users/natha/Desktop/Polling Places/data")
write.csv(L2_mini, 'L2_2018_coords.csv')

############################ Export and process in ArcGIS

############################ Read in joined data after ArcGIS processing
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2_join <- read.csv('vtds_L2_join.csv')
#### merge L2 demographic data back in
L2_demog<-fread('PA_combined.csv', 
          select = c('LALVOTERID',
                     'Residence_Addresses_AddressLine_2018','Residence_Addresses_ExtraAddressLine_2018',
                     'Residence_Addresses_City_2018','Residence_Addresses_State_2018',
                     'Residence_Addresses_Zip_2018',
                     'Residence_Addresses_Latitude_2018','Residence_Addresses_Longitude_2018',
                     'Voters_Gender_2018','Voters_Age_2018','Parties_Description_2018',
                     'Religions_Description_2018','Voters_OfficialRegDate_2018',
                     'MaritalStatus_Description_2018','CommercialData_PresenceOfChildrenCode_2018',
                     'CommercialData_HHComposition',
                     'CommercialData_EstimatedHHIncomeAmount_2018',
                     'CommercialData_Education_2018','County_2018','Voters_FIPS_2018',
                     'Voters_Active_2018','CountyEthnic_LALEthnicCode_2018',
                     'EthnicGroups_EthnicGroup1Desc_2018','Ethnic_Description_2018',
                     'CountyEthnic_Description_2018'
          ))
L2_join <- left_join(L2_join, L2_demog, by='LALVOTERID')
rm(L2_demog)
#### reformat county-precinct naming in join and poll files to match better
# convert L2 to all uppercase
L2_join$county_pre<-toupper(L2_join$county_pre)
# join poll county and precinct strings
#poll$county_pre = paste0(poll$county_name,' - ',poll$precinct_name)
##verify
#poll$county_pre %in% L2_join$county_pre

############################################### Create sheet for manual matching
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
joined_sheet$precinct_id = paste0('\'',joined_sheet$precinct_id)
setwd("C:/Users/natha/Desktop/Polling Places/data")
write.csv(joined_sheet, 'joined_sheet_mismatched.csv')

################## Manually match L2 precinct name to poll location precinct ids


############## Read manually matched L2 and poll location precinct names back in
joined_sheet_matched<-read.csv('joined_sheet_matched.csv')
#remove the apostrophe from the precinct ids
joined_sheet_matched$precinct_id = str_extract(joined_sheet_matched$precinct_id,
                                               "(?<=\\')(.*)")
# find number of unmatched precincts
9235-sum(complete.cases(joined_sheet_matched))
#percentage
(9235-sum(complete.cases(joined_sheet_matched)))/9235
#2%-ish
#subset out unmatched precincts
joined_sheet_unmatched<-joined_sheet_matched[!complete.cases(joined_sheet_matched),]
######### unmatched precincts seem to be mainly due to mixing up 2018 and 2020
# write to csv
setwd("C:/Users/natha/Desktop/Polling Places/data")
write.csv(joined_sheet_unmatched, 'joined_sheet_unmatched.csv')


################# merge L2 data with matched precincts
## merge L2 with joined_sheet on L2 precinct names
joined_sheet_matched<-subset(joined_sheet_matched, select = c(county_pre, county_name,
                                                              precinct_name,precinct_id))
L2_join<-left_join(L2_join, joined_sheet_matched, by='county_pre')
# find number of unmatched L2 voters
8104701-sum(complete.cases(L2_join))
#percentage
(8104701-sum(complete.cases(L2_join)))/8104701
#1.5%-ish

################# merge in polling place categories
# location/category data
setwd("C:/Users/natha/Desktop/Polling Places/data")
poll_cat<-read.csv('polllocation_structure_and_keyword.csv')
## remove unneeded columns
poll_cat <- subset(poll_cat, select = c(county_name, precinct_name, location_category))
L2_join <- left_join(L2_join, poll_cat, by=c('county_name', 'precinct_name'))
#sample = L2_join[sample(nrow(L2_join), 1000), ]
#drop duplicated and unneeded columns
## (should drop more depending on use case)
#L2_join <- L2_join %>% select(-contains(".x"))
#L2_join <- L2_join %>% select(-c(location_count, source_notes, notes, source_date,
#                                 location_type, polling_place_id, X, X.1))

################# write to csv
setwd("C:/Users/natha/Desktop/Polling Places/data")
write.csv(L2_join, 'L2_join_poll_place.csv')  
  