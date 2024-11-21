#Nathaniel Flemming
#18/10/24
#19/10/24

library(tidyverse)
library(data.table) #read in L2 data selectively
library(stringr) #string manipulation
library(fuzzyjoin) #fuzzy match precinct names
library(stringdist) #fuzzy match precinct names

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
L2_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\VM2_PA_2019_08_23'

#set polling location data year
poll_year=2019

# Set variable lists
## adjust as needed based on elections of interest
#vote_vars<-c('LALVOTERID','General_2016_11_08','General_2017_11_07',
#             'General_2018_11_06','General_2019_11_05')
vote_vars<-c('LALVOTERID','General_2016_11_08','General_2017_11_07',
             'General_2018_11_06')
demog_vars<-c('LALVOTERID','Voters_StateVoterID','County','Voters_FIPS','Precinct')

# Read in data
L2votehist <-get_L2_data(L2_dir, 'VM2--PA--2019-08-22-VOTEHISTORY.tab', vote_vars)
L2demog<-get_L2_data(L2_dir, 'VM2--PA--2019-08-22-DEMOGRAPHIC.tab', demog_vars)
# Combine L2 data
L2votehist<-left_join(L2votehist, L2demog, by = 'LALVOTERID')

################# merge L2 data with polling location data
# Read in location/category data
poll <- get_poll_data(data_dir, paste0('FVE_',poll_year,'_polllocation.csv'), 
                      c('VOTERID','County', 'PrecinctName', 'location_category'))
### Replace '-' in L2 precinct names to better match government format
L2votehist<-L2votehist%>%
  mutate(across('Precinct',\(x) str_replace(x,'-', ' ') ))#deprecated syntax?

## merge on state voter id
merged<-left_join(L2votehist, poll, by=c('County','Voters_StateVoterID'='VOTERID'))
sum(is.na(merged$PrecinctName))
# missing 695,180 location categories, about 74,000 more than missing from poll 
# 2020 missing 124,313 poll location data rows, 1.5% missing
# 2019 missing 139,617 poll location data rows, 1.8% missing

# 
# ### examine missing
# notinL2<-poll[!(poll$VOTERID%in%L2votehist$Voters_StateVoterID),]
# notinpoll<-L2votehist[!(L2votehist$Voters_StateVoterID%in%poll$VOTERID),]
# 
# 
# voterid<-L2votehist$Voters_StateVoterID[L2votehist$County=='ADAMS']
# pollid<-poll$VOTERID[poll$County=='ADAMS']
# sum(pollid%in%voterid)
# 
# notin<-pollid[!(pollid%in%voterid)]
# notinp<-voterid[!(voterid%in%pollid)]
# 
# notin<-poll[!(poll$VOTERID%in%L2votehist$Voters_StateVoterID),]
# notin<-str_sub(notin$VOTERID, 2)
# sum(notin%in%L2votehist$Voters_StateVoterID)
# 
# voterid<-as.integer(substring(voterid, 6))
# voterid=sort(voterid)
# # remove 5 leading characters? so length matches L2 ids? 
# pollid<-str_sub(pollid, 3)
# sum(pollid%in%voterid)
# pollid=sort(pollid)

#crosswalk<-fuzzy_match_precincts(L2votehist, poll)


################# write to csv
setwd(data_dir)
write.csv(merged, 'L2PA_votehist_VM2_19.csv')

#mini_data <- L2votehist[sample(nrow(L2votehist), 100000),]


