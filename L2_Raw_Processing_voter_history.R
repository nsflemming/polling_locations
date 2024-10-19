#Nathaniel Flemming
#18/10/24
#18/10/24

library(tidyverse)
library(data.table) #read in L2 data selectively
library(stringr) #string manipulation
library(fuzzyjoin) #fuzzy match precinct names
library(stringdist) #fuzzy match precinct names

########## Create voter history files from L2 data

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
data_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data'
race_data_dir<-'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\predicted_race_data'
## adjust as needed based on year desired
L2_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\VM2__PA__2020_10_01'

# Set variable lists
## adjust as needed based on elections of interest
vote_vars<-c('LALVOTERID','County','Voters_FIPS','Precinct','General_2016_11_08','General_2017_11_07',
             'General_2018_11_06','General_2019_11_05')

# Read in data
L2votehist <-get_L2_data(L2_dir, 'VM2--PA--2020-10-01-VOTEHISTORY.tab', vote_vars)

################# merge L2 data with polling location data
# Read in location/category data
poll <- get_poll_data(data_dir, 'poll_struct_key_govsource19.csv', 
                      c('CountyName', 'PrecinctName', 'location_category'))
## rename variables to match L2
poll<-poll%>%
  rename('County'='CountyName')
### Replace '-' in L2 precinct names to better match government format
L2votehist<-L2votehist%>%
  mutate(across('Precinct', str_replace, '-', ' '))#deprecated syntax?
### Convert government county names to all caps to match L2
poll$County<-toupper(poll$County)

## Create a crosswalk by fuzzy matching precinct names (replace with something more official)
### match on voterid instead?
crosswalk<-fuzzy_match_precincts(L2votehist, poll)
####### Double check inexact matches
# rename columns to match other data sets
crosswalk<-crosswalk%>%
  rename('PrecinctName'='V3')
## Join the crosswalk into L2 
L2votehist<-left_join(L2votehist, crosswalk, by=c('County','Precinct'))

#### join the poll location categories in
L2votehist<-left_join(L2votehist, poll, by=c('County','PrecinctName'))
## drop location 
rm(poll)
#7677867-sum(is.na(L2votehist$precinct_id))
##### Drop extraneous variables
#L2votehist <- subset(L2votehist, select = -c(X, index))

####################### Process L2 data to more usable forms
# Convert income to numeric
L2votehist<-dollar_to_num(L2votehist, 'CommercialData_EstimatedHHIncomeAmount')
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
L2votehist<-educ_to_ord(L2votehist, 'CommercialData_Education', educ_map)
## Create factor variables
# Convert location category to factor
L2votehist$location_category <- as.factor(L2votehist$location_category)
# Convert parties to factor
L2votehist$Parties_Description <- as.factor(L2votehist$Parties_Description)
# Replace blanks with 'no' for union membership
L2votehist$CommercialData_LikelyUnion[L2votehist$CommercialData_LikelyUnion==''] <- 'No'
## factor
L2votehist$CommercialData_LikelyUnion<-as.factor(L2votehist$CommercialData_LikelyUnion)
# Replace blank with 'unknown' for occupation industry
L2votehist$CommercialData_OccupationIndustry[L2votehist$CommercialData_OccupationIndustry==''] <- 'Unknown'
## factor
L2votehist$CommercialData_OccupationIndustry<-as.factor(L2votehist$CommercialData_OccupationIndustry)
# Replace blank with 'unknown' for occupation group
L2votehist$CommercialData_OccupationGroup[L2votehist$CommercialData_OccupationGroup==''] <- 'Unknown'
## factor
L2votehist$CommercialData_OccupationGroup<-as.factor(L2votehist$CommercialData_OccupationGroup)

## Create Binary Variables
# Convert household composition to child yes/no
L2votehist$has_child <- str_detect(L2votehist$CommercialData_HHComposition, 'Children|children')
# Convert religious description to know religious yes/no, set blank to no
L2votehist$known_religious <- L2votehist$Religions_Description!=""
# Convert gender to M/F, set blank to missing
L2votehist$Voters_Gender <- ifelse(L2votehist$Voters_Gender=="",NA,L2votehist$Voters_Gender)
## Create Location dummy variables
L2votehist$pub_loc<-L2votehist$location_category=='public'
L2votehist$pub_just <- L2votehist$location_category=='public_justice'
L2votehist$other<-L2votehist$location_category=='other'
L2votehist$relig_loc <- (L2votehist$location_category=='religious'|L2votehist$location_category=='religious_school')
L2votehist$school <- L2votehist$location_category=='school' 
L2votehist$multiple <- L2votehist$location_category=='multiple'
L2votehist$justice_loc <- (L2votehist$location_category=='justice'|L2votehist$location_category=='public_justice')
L2votehist$library<-L2votehist$location_category=='library'
L2votehist$relig_school <- L2votehist$location_category=='religious_school' 

### Create known government employee dummy variable
L2votehist$known_gov_emp <- ifelse(L2votehist$CommercialData_OccupationIndustry=="Civil Servant",TRUE,FALSE)
### Create known Republican dummy variable
L2votehist$known_repub <- ifelse(L2votehist$Parties_Description=="Republican",TRUE,FALSE)


################# write to csv
setwd(data_dir)
write.csv(L2votehist, '')

#mini_data <- L2votehist[sample(nrow(L2votehist), 100000),]


