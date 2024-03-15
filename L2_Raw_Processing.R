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
              'Residence_Families_HHCount', 'Residence_HHGender_Description',
              'Religions_Description', 
              ###'MaritalStatus_Description',
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
## race estimates
setwd(data_dir)
load('PA_2016_race.Rdata')
race16<-df.pred
load('PA_2017_race.Rdata')
race17<-df.pred
load('PA_2018_race.Rdata')
race18<-df.pred
rm(df.pred)
# Combine L2 data
L2demog<-left_join(L2demog, L2votehist, by = 'LALVOTERID')
## merge in race estimates
L2demog<-left_join(L2demog, race16, by='LALVOTERID')
L2demog<-left_join(L2demog, race17, by='LALVOTERID')
L2demog<-left_join(L2demog, race18, by='LALVOTERID')
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

## Create Binary Variables
# Convert household composition to child yes/no
L2demog$has_child <- str_detect(L2demog$CommercialData_HHComposition, 'Children|children')
# Convert religious description to know religious yes/no, set blank to no
L2demog$known_religious <- L2demog$Religions_Description!=""
# Convert gender to M/F, set blank to missing
L2demog$Voters_Gender <- ifelse(L2demog$Voters_Gender=="",NA,L2demog$Voters_Gender)
## Create Location dummy variables
L2demog$school <- L2demog$location_category=='school' 
L2demog$relig_loc <- (L2demog$location_category=='religious'|L2demog$location_category=='religious_school')
L2demog$justice_loc <- (L2demog$location_category=='justice'|L2demog$location_category=='public_justice')
L2demog$pub_loc <- L2demog$location_category=='public' 
### Create black dummy variable
## most probable race category
L2demog_sub<-L2demog[,c('pred.whi_2018', 'pred.bla_2018', 'pred.his_2018',
                              'pred.asi_2018', 'pred.oth_2018')]
L2demog <- mutate(L2demog, pred_race = names(L2demog_sub)[max.col(L2demog_sub)])
L2demog$pred_black<-ifelse(L2demog$pred_race=='pred.bla_2018', TRUE, FALSE)
L2demog$pred_race<-as.factor(L2demog$pred_race)
### Create known government employee dummy variable
L2demog$known_gov_emp <- ifelse(L2demog$CommercialData_OccupationIndustry=="Civil Servant",TRUE,FALSE)
### Create known Republican dummy variable
L2demog$known_repub <- ifelse(L2demog$Parties_Description=="Republican",TRUE,FALSE)


################# write to csv
setwd(data_dir)
write.csv(L2demog, 'L2PA_full.csv')

mini_data <- L2demog[sample(nrow(L2demog), 100000),]


