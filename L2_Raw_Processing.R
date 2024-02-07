#Nathaniel Flemming
#6/2/24

library(tidyverse)
library(data.table) #read in L2 data selectively
library(stringr) #string manipulation

# Read in L2 data selectively
get_L2_data <- function(L2_dir, filename, vars){
  setwd(L2_dir)
  L2<-fread(filename, sep='\t', header=T, select = vars)
  return(L2)
}

# Read in precinct mapping crosswalk
get_match_data <- function(match_dir, filename, vars){
  setwd(match_dir)
  match<-read.csv(filename)
  match<-subset(match, select = vars)
  #remove the apostrophe from the precinct ids
  match$precinct_id = str_extract(match$precinct_id, "(?<=\\')(.*)")
  return(match)
}

# Read in poll location data
get_poll_data <- function(poll_dir, filename, vars){
  setwd(poll_dir)
  poll<-read.csv(filename)
  ## remove unneeded columns
  poll <- subset(poll, select = vars)
  return(poll)
  }



#################################################################

# set directories
data_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places\\data'
L2_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places\\data\\VM2_PA_2019_08_23'
# Set variable lists
vote_vars<-c('LALVOTERID', 'General_2018_11_06')
demog_vars<-c('LALVOTERID', 
              #address/location
              'Residence_Addresses_AddressLine',
              'Residence_Addresses_Latitude',
              'Residence_Addresses_Longitude',
              'County','Voters_FIPS','Precinct',
              #Demographics
              'Voters_Gender', 'Voters_Age', 'EthnicGroups_EthnicGroup1Desc',
              'Religions_Description', 'MaritalStatus_Description',
              'CommercialData_Education', 'CommercialData_HHComposition',
              'CommercialData_LikelyUnion', 'CommercialData_OccupationGroup',
              'CommercialData_OccupationIndustry',
              #Political
              'Parties_Description',
              #Misc
              'MilitaryStatus_Description'
              )

# Read in data
L2votehist <-get_L2_data(L2_dir, 'VM2--PA--2019-08-22-VOTEHISTORY.tab', vote_vars)
L2demog<-get_L2_data(L2_dir, 'VM2--PA--2019-08-22-DEMOGRAPHIC.tab', demog_vars)
# Combine L2 data
L2demog<-left_join(L2demog, L2votehist, by = 'LALVOTERID')
### drop voterhistory
rm(L2votehist)

################# merge L2 data with matched precincts
### Read manually matched L2 and poll location precinct names back in
match<-get_match_data(data_dir, 'joined_sheet_matched.csv', 
                    c('county_pre', 'county_name', 'precinct_name', 'precinct_id'))
### Merge precinct and county names in L2 data frame
L2demog$county_pre = paste0(L2demog$County,' - ',L2demog$Precinct)
## merge L2 with joined_sheet on L2 precinct names
L2demog<-left_join(L2demog, match, by='county_pre')
## Drop crosswalk
rm(match)

################# merge in polling place categories
# Read in location/category data
poll <- get_poll_data(data_dir, 'polllocation_structure_and_keyword.csv', 
              c('county_name', 'precinct_name', 'location_category'))
# Join in poll location data
L2demog <- left_join(L2demog, poll, by=c('county_name', 'precinct_name'))
## drop location 
rm(poll)

################# write to csv
setwd(data_dir)
write.csv(L2demog, 'L2PA_full.csv')




