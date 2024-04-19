#Nathaniel Flemming
#6/2/24

library(tidyverse)
library(data.table) #read in L2 data selectively
library(stringr) #string manipulation
library(fuzzyjoin) #fuzzy match precinct names
library(stringdist) #fuzzy match precinct names

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

### fuzzy match precinct names within counties
fuzzy_match_precincts<-function(df1, df2, prec_var='Precinct',prec_var2='PrecinctName',
                                county_var='County'){
  # get list of counties
  counties<-unique(df1[[county_var]])
  # create crosswalk data frame
  crosswalk<-data.frame()
  for(county in counties){
    # get list of precincts w/in county
    list1<-unique(df1[[prec_var]][df1[[county_var]]==county])
    list2<-unique(df2[[prec_var2]][df2[[county_var]]==county])
    ## calculate string distance b/t each precinct
    result<-stringdistmatrix(list1, list2, method='jw')
    ## get index of col with lowest value for each row
    closest_match<-apply(result, 1, which.min)
    # create crosswalk for the county
    temp<-as.data.frame(cbind('County'=county,'Precinct'=list1,list2[closest_match]))
    print(county)
    # bind into larger crosswalk
    crosswalk<-rbind(crosswalk, temp)
  }
  return(crosswalk)
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
vote_vars<-c('LALVOTERID', 'General_2018_11_06','General_2017_11_07')
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
              'Parties_Description'
              #Misc
              #'MilitaryStatus_Description'
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
### drop race imputations
rm(race16, race17, race18)

################# merge L2 data with polling location data
# Read in location/category data
poll <- get_poll_data(data_dir, 'poll_struct_key_govsource18.csv', 
                      c('CountyName', 'PrecinctName', 'location_category'))
## rename variables to match L2
poll<-poll%>%
  rename('County'='CountyName')
### Replace '-' in L2 precinct names to better match government format
L2demog<-L2demog%>%
  mutate(across('Precinct', str_replace, '-', ' '))#deprecated syntax?
### Convert government county names to all caps to match L2
poll$County<-toupper(poll$County)

## Create a crosswalk by fuzzy matching precinct names
crosswalk<-fuzzy_match_precincts(L2demog, poll)
####### Double check inexact matches
# rename columns to match other data sets
crosswalk<-crosswalk%>%
  rename('PrecinctName'='V3')
## Join the crosswalk into L2 
L2demog<-left_join(L2demog, crosswalk, by=c('County','Precinct'))

#### join the poll location categories in
L2demog<-left_join(L2demog, poll, by=c('County','PrecinctName'))
## drop location 
rm(poll)
#7677867-sum(is.na(L2demog$precinct_id))
##### Drop extraneous variables
#L2demog <- subset(L2demog, select = -c(X, index))

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

## Create Binary Variables
# Convert household composition to child yes/no
L2demog$has_child <- str_detect(L2demog$CommercialData_HHComposition, 'Children|children')
# Convert religious description to know religious yes/no, set blank to no
L2demog$known_religious <- L2demog$Religions_Description!=""
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

#mini_data <- L2demog[sample(nrow(L2demog), 100000),]


