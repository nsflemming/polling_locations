#Nathaniel Flemming
#6/2/24

library(tidyverse)
library(data.table) #read in L2 data selectively
library(stringr) #string manipulation

########## Functions

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



################################################################# Main

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
              'CommercialData_EstimatedHHIncomeAmount',
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

### Edit precinct names to match poll location file
match$county_pre <- str_remove(match$county_pre, pattern = '#')
### Merge precinct and county names in L2 data frame
L2demog$county_pre = paste0(L2demog$County,' - ',L2demog$Precinct)
### Combine L2 and poll location county precinct sheets
county_pre<-as.data.frame(unique(L2demog$county_pre))
county_pre$index<-row.names(county_pre)
match$index<-row.names(match)
match<-left_join(match, county_pre, by='index')
write.csv(match, 'L2_poll_precincts_mismatch.csv')
######## Align county_precinct names manually
## read back in
match_aligned<-read.csv('L2_poll_precincts_aligned.csv')
#rename columns
match_aligned<-match_aligned%>%
  rename(L2_county_pre = unique.L2demog.county_pre.)
L2demog<-rename(L2demog, L2_county_pre = county_pre)

## merge L2 with joined_sheet on L2 precinct names
L2demog<-left_join(L2demog, match_aligned, by='L2_county_pre')
## Drop crosswalk
rm(match)
rm(match_aligned)
7677867-sum(is.na(L2demog$precinct_id))

################# merge in polling place categories
# Read in location/category data
poll <- get_poll_data(data_dir, 'polllocation_structure_and_keyword.csv', 
              c('county_name', 'precinct_name', 'location_category'))
# Join in poll location data
L2demog <- left_join(L2demog, poll, by=c('county_name', 'precinct_name'))
## drop location 
rm(poll)
7677867-sum(is.na(L2demog$precinct_id))
##### Drop extraneous variables
L2demog <- subset(L2demog, select = -c(X, index))

################# write to csv
setwd(data_dir)
write.csv(L2demog, 'L2PA_full.csv')

mini_data <- L2demog[sample(nrow(L2demog), 100000),]


