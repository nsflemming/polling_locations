#Nathaniel Flemming
#18/10/24

library(tidyverse)
library(data.table) #read in L2 data selectively
library(stringr) #string manipulation
#library(fuzzyjoin) #fuzzy match precinct names
#library(stringdist) #fuzzy match precinct names

########## Create voter history files from L2 data and combined voter and poll location files from Combine_voterfile_pollocation

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
data_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data'
race_data_dir<-'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\predicted_race_data'
## adjust as needed based on year desired
### Need to use subsequent year for general election of polling location data year
#L2_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\VM2--PA--2018-08-22'
#L2_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\VoterMapping--PA--HEADERS--2017-08-05'

#set polling location data year
poll_year=2017

# Set variable lists
demog_vars<-c('LALVOTERID','Voters_StateVoterID','County','Voters_FIPS','Precinct')

# Read in data
L2loc<-get_L2_data(L2_dir, 'VoterMapping--PA--HEADERS--08-04-2017-HEADERS.tab', demog_vars)

################# merge L2 data with polling location data
# Read in location/category data
poll <- get_poll_data(data_dir, paste0('FVE_',poll_year,'_polllocation.csv'), 
                      c('VOTERID','County', 'PrecinctName', 'location_category',
                        'Description'))
### Replace '-' in L2 precinct names to better match government format
L2loc<-L2loc%>%
  mutate(across('Precinct',\(x) str_replace(x,'-', ' ') ))#deprecated syntax?

## merge on state voter id
merged<-left_join(L2loc, poll, by=c('County','Voters_StateVoterID'='VOTERID'))
sum(is.na(merged$PrecinctName))
# missing 695,180 location categories, about 74,000 more than missing from poll 
# 2020 missing 124,313 poll location data rows, 1.5% missing
# 2019 missing 139,617 poll location data rows, 1.8% missing
# 2018 missing 10,163 poll location data rows, 0.1% missing
# 2017 missing

########## write to csv
setwd(data_dir)
year_str<-as.character(poll_year)
write.csv(merged, paste0('L2PA_poll_loc_VM2_',str_sub(year_str,start=-2),'.csv'))

######################### Extract voterhistory
# Set variable lists
## adjust as needed based on elections of interest/contained in L2 file
vote_vars<-c('LALVOTERID','General_2016_11_08','General_2017_11_07',
            'General_2018_11_06')
#,'General_2019_11_05')
#vote_vars<-c('LALVOTERID','General_2017_11_07')
#set polling location data year
poll_year=2019

L2_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\VM2_PA_2019_08_23'

L2votehist19<-get_L2_data(L2_dir, 
                          'VM2--PA--2019-08-22-VOTEHISTORY.tab', 
                          vote_vars)

########### write to csv
setwd(data_dir)
year_str<-as.character(poll_year)
write.csv(L2votehist19, paste0('L2PA_votehist_VM2_',str_sub(year_str,start=-2),'.csv'))



