#Nathaniel Flemming
#6/2/24

# Get address history information and demographics from the L2 files
## don't need voting history since the 2018 general election is in the 2019 file

#Packages
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

### Add year to predicted race data frames
add_year<-function(data, year){
  data<-data%>%
    rename_with(~paste0(.x,'_',year),
                starts_with('pred'))
}

### Create race indicator for most probable race
most_prob_race<-function(data){
  # remove rows with all missing race predictions
  data <- data[complete.cases(data),]
  # create column taking value of most probable race
  ## skip first row: voter id
  data <- mutate(data, pred_race = names(data[,-1])[max.col(data[,-1])])
  return(data)
}


## convert dollars to numeric
dollar_to_num <- function(data, inc_var){
  #remove $
  data[[inc_var]] = str_extract(data[[inc_var]], "(?<=\\$)(.*)")
  # convert to numeric
  data[[inc_var]] = as.numeric(data[[inc_var]])
  return(data)
}

## Convert education to ordinal
educ_to_ord <- function(data, educ_var, mapping){
  data[[educ_var]] <- mapping[as.character(data[[educ_var]])]
  #leave blank = missing/unknown
  return(data)
}


################################################################# Main

# set directories
data_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data'
race_data_dir<-'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\predicted_race_data'
## adjust as needed based on year desired
L2_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\VM2__PA__2020_10_01'
#set year
year=2017

# Set variable lists
## adjust as needed based on elections of interest
vote_vars<-c('LALVOTERID','General_2016_11_08','General_2017_11_07',
             'General_2018_11_06','General_2019_11_05')
demog_vars<-c('LALVOTERID', 
              #address/location
              'Residence_Addresses_AddressLine',
              'Residence_Addresses_Latitude',
              'Residence_Addresses_Longitude',
              'County','Voters_FIPS','Precinct',
              #Demographics
              'Voters_Gender', 'Voters_Age', 'EthnicGroups_EthnicGroup1Desc',
              'Residence_Families_HHCount', 'Residence_HHGender_Description',
              'Religions_Description', 
              ###'MaritalStatus_Description',
              'CommercialData_Education', 'CommercialData_HHComposition',
              'CommercialData_EstimatedHHIncomeAmount',
              'CommercialData_EstimatedIncomeAmount',
              'CommercialData_LikelyUnion', 'CommercialData_OccupationGroup',
              'CommercialData_OccupationIndustry',
              #Political
              'Parties_Description'
              )

# Read in data
L2votehist <-get_L2_data(L2_dir, 'VM2--PA--2020-10-01-VOTEHISTORY.tab', vote_vars)
L2demog<-get_L2_data(L2_dir, 'VM2--PA--2020-10-01-DEMOGRAPHIC.tab', demog_vars)

## race estimates
#setwd(race_data_dir)
#race16<-read.csv('pa2016.csv')
#race17<-read.csv(paste0(race_data_dir,'\\pa2017.csv'))
#race18<-read.csv(paste0(race_data_dir,'\\pa2018.csv'))
### (2018 has two rows for LALPA182948529)
#race19<-read.csv(paste0(race_data_dir,'\\pa2019.csv'))
### (2019 has two row for LALPA680572)
race<-read.csv(paste0(race_data_dir,'\\pa',year,'.csv'))


## Remove repeated/duplicated voters (keeps first entry)
#race18<-distinct(race18, LALVOTERID, .keep_all=T)
#race19<-distinct(race19, LALVOTERID, .keep_all=T)
race<-distinct(race, LALVOTERID, .keep_all=T)

# add year indicator to column names
#race18<-add_year(race18, '2018')
#race19<-add_year(race19, '2019')
race<-add_year(race, year)
# create column with most probable race for each voter
#race18<-most_prob_race(race18)
#race19<-most_prob_race(race19)
race<-most_prob_race(race)
# Create black dummy variable
#race18$pred_black<-ifelse(race18$pred_race=='pred.bla_2018', TRUE, FALSE)
#race19$pred_black<-ifelse(race19$pred_race=='pred.bla_2019', TRUE, FALSE)
race$pred_black<-ifelse(race$pred_race==paste0('pred.bla_',year), TRUE, FALSE)
# convert race to factor(? not sure necessary)
#race18$pred_race<-as.factor(race18$pred_race)
#race19$pred_race<-as.factor(race19$pred_race)
race$pred_race<-as.factor(race$pred_race)

# Combine L2 data
test<-left_join(L2demog, L2votehist, by = 'LALVOTERID')
## remove duplicate rows
#race18<-unique(race18)
## merge in race estimates
test<-left_join(test, race, by='LALVOTERID')
### drop voterhistory
rm(L2votehist)
### drop race imputations
#rm(race18)
rm(race)

####################### Process L2 data to more usable forms
# Convert income to numeric
L2demog<-dollar_to_num(L2demog, 'CommercialData_EstimatedHHIncomeAmount')
# Convert education to ordinal
## level to numeric map
educ_map <- c("Less than HS Diploma - Ex Like"=1,
              "Less than HS Diploma - Likely"=1,
              'HS Diploma - Extremely Likely'=2, "HS Diploma - Likely"=2,
              "Some College -Extremely Likely"=3, "Some College - Likely"=3, 
              "Vocational Technical Degree - Extremely Likely"=4,
              "Bach Degree - Extremely Likely"=5, "Bach Degree - Likely"=5,
              "Grad Degree - Extremely Likely"=6, "Grad Degree - Likely"=6
)
L2demog<-educ_to_ord(L2demog, 'CommercialData_Education', educ_map)
## Create factor variables
# Convert location category to factor
L2demog$location_category <- as.factor(L2demog$location_category)
# Convert parties to factor
L2demog$Parties_Description <- as.factor(L2demog$Parties_Description)
# Replace blanks with 'no' for union membership
L2demog$CommercialData_LikelyUnion[L2demog$CommercialData_LikelyUnion==''] <- 'No'
## factor
L2demog$CommercialData_LikelyUnion<-as.factor(L2demog$CommercialData_LikelyUnion)
# Replace blank with 'unknown' for occupation industry
L2demog$CommercialData_OccupationIndustry[L2demog$CommercialData_OccupationIndustry==''] <- 'Unknown'
## factor
L2demog$CommercialData_OccupationIndustry<-as.factor(L2demog$CommercialData_OccupationIndustry)
# Replace blank with 'unknown' for occupation group
L2demog$CommercialData_OccupationGroup[L2demog$CommercialData_OccupationGroup==''] <- 'Unknown'
## factor
L2demog$CommercialData_OccupationGroup<-as.factor(L2demog$CommercialData_OccupationGroup)

## Create Binary Variables
# Convert household composition to child yes/no
L2demog$has_child <- str_detect(L2demog$CommercialData_HHComposition, 'Children|children')
# Convert religious description to know religious yes/no, set blank to no
L2demog$known_religious <- L2demog$Religions_Description!=""
# Convert religious description to Catholic binary variable
L2demog$known_catholic <- L2demog$Religions_Description=="Catholic"
# Convert gender to M/F, set blank to missing
L2demog$Voters_Gender <- ifelse(L2demog$Voters_Gender=="",NA,L2demog$Voters_Gender)
## Create Location dummy variables
L2demog$pub_loc<-L2demog$location_category=='public'
L2demog$pub_just <- L2demog$location_category=='public_justice'
L2demog$other<-L2demog$location_category=='other'
L2demog$relig_loc <- (L2demog$location_category=='religious'|L2demog$location_category=='religious_school')
L2demog$school <- L2demog$location_category=='school' 
L2demog$multiple <- L2demog$location_category=='multiple'
L2demog$justice_loc <- (L2demog$location_category=='justice'|L2demog$location_category=='public_justice')
L2demog$library<-L2demog$location_category=='library'
L2demog$relig_school <- L2demog$location_category=='religious_school' 

### Create known government employee dummy variable
L2demog$known_gov_emp <- ifelse(L2demog$CommercialData_OccupationIndustry=="Civil Servant",TRUE,FALSE)
### Create known Republican dummy variable
L2demog$known_repub <- ifelse(L2demog$Parties_Description=="Republican",TRUE,FALSE)


################# write to csv
setwd(data_dir)
write.csv(L2demog, paste0('L2PA_',year,'_address_in_',year%%100,'.csv'))

#mini_data <- L2demog[sample(nrow(L2demog), 100000),]


