#Nathaniel Flemming
#18/10/24
#19/10/24

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
vote_vars<-c('LALVOTERID','General_2016_11_08','General_2017_11_07',
             'General_2018_11_06','General_2019_11_05')
demog_vars<-c('LALVOTERID','County','Voters_FIPS','Precinct')

# Read in data
L2votehist <-get_L2_data(L2_dir, 'VM2--PA--2020-10-01-VOTEHISTORY.tab', vote_vars)
L2demog<-get_L2_data(L2_dir, 'VM2--PA--2020-10-01-DEMOGRAPHIC.tab', demog_vars)
# Combine L2 data
L2votehist<-left_join(L2votehist, L2demog, by = 'LALVOTERID')

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

################# write to csv
setwd(data_dir)
write.csv(L2votehist, 'L2PA_votehist_VM2_20.csv')

#mini_data <- L2votehist[sample(nrow(L2votehist), 100000),]


