#Nathaniel Flemming
# 27/9/24

# Preprocessing data for the difference in difference regressions
## uses data files made by L2_Raw_Processing scripts

library(did) #difference in difference package
library(tidyverse) #convenience
library(data.table) #read in data selectively
library(readxl) #read in excel sheets


########## Functions
## Binarize vote variables
binarize_vote <- function(data, vote_var, yes_vote_value){
  data[[vote_var]] <- ifelse(data[[vote_var]]==yes_vote_value,1,0)
  return(data)
}

### read in a csv and add a year column
read_add_year<-function(dir, filename, year){
  temp<-read.csv(paste0(dir, '\\', filename))
  temp$year<-year
  return(temp)
}

### Replace NAs with -Infs (for predicted race)
replace_na_with_neg_inf<-function(data,variables){
  for(variable in variables){
    data[variable][is.na(data[variable])]<- -Inf
  }
  return(data)
}

### Create race indicator for most probable race
create_most_prob_race_var<-function(data, race_vars){
  # create column taking value of most probable race
  ## ties or all missing(-Inf) are set randomly
  data <- mutate(data, pred_race = names(data[,race_vars])
                 [max.col(data[,race_vars])])
  return(data)
}

### Original function to create indicator for most probable race
most_prob_race<-function(data, race_vars){
  # remove rows with all missing race predictions
  firstcol = which(colnames(data)=="pred.whi")
  lastcol = which(colnames(data)=="pred.oth")
  data <- data[rowSums(is.na(data[,firstcol:lastcol]))<5,]
  #data <- data[complete.cases(data),]
  # create column taking value of most probable race
  data <- mutate(data, pred_race = names(data[,race_vars])
                 [max.col(data[,race_vars])])
  return(data)
}

########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"

### read in vote history and poll location data
## adding year indicator
hist_poll_2020<-read_add_year(data_dir, 'L2PA_votehist_VM2_20.csv',2020)
hist_2019<-read_add_year(data_dir, 'L2PA_votehist_VM2_19.csv',2019)
### separate into polling location info and voting history
#poll_2020<-hist_poll_2020%>%select(-c(General_2016_11_08, General_2017_11_07, General_2018_11_06, General_2019_11_05))
hist_2020<-hist_poll_2020%>%select(c(LALVOTERID, General_2016_11_08, General_2017_11_07, General_2018_11_06, General_2019_11_05))
rm(hist_poll_2020)

# read in poll location data
poll_2019<-read_add_year(data_dir, 'L2PA_poll_loc_VM2_19.csv',2019)
poll_2018<-read_add_year(data_dir, 'L2PA_poll_loc_VM2_18.csv',2018)
# Read in Religious census data
#setwd(data_dir)
#relig_census<-read_excel('2020_USRC_Group_Detail.xlsx', sheet = '2020 Group by County')
# Read in distance to polling station data (distance in degrees)
dist_data18<-read.csv(paste0(data_dir,'\\voterloc_pollloc_dist_2018.csv'))
dist_data19<-read.csv(paste0(data_dir,'\\voterloc_pollloc_dist_2019.csv'))

### read in voter demographic and address data
## remove county and precinct columns since that's in the voter history/polling location files
demog_addr_2018<-read.csv(paste0(data_dir,'\\L2PA_2018_address_in_18.csv'))%>%
  select(-c(X, County, Precinct, Voters_FIPS, pub_loc, pub_just,
            other, relig_loc, school, multiple, justice_loc, library, relig_school))
demog_addr_2019<-read.csv(paste0(data_dir,'\\L2PA_2019_address_in_19.csv'))%>%
  select(-c(X, County, Precinct, Voters_FIPS, pub_loc, pub_just,
            other, relig_loc, school, multiple, justice_loc, library, relig_school))
#### Merge poll location data with voter demographic data on L2 voterid
demog_addr_2018<-left_join(demog_addr_2018, poll_2018, by='LALVOTERID')
demog_addr_2019<-left_join(demog_addr_2019, poll_2019, by='LALVOTERID')
# remove data frames to free up memory
rm(poll_2018, poll_2019)

#### Merge distance to poll station data with voter demographic data on L2 voterid
demog_addr_2018<-left_join(demog_addr_2018, dist_data18, by='LALVOTERID')
demog_addr_2019<-left_join(demog_addr_2019, dist_data19, by='LALVOTERID')
# remove data frames to free up memory
rm(dist_data18, dist_data19)

#### rbind years together
## make column names and number match (check that income variables are equivalent)
demog_addr_2018<-demog_addr_2018%>%
  rename(pred.whi=pred.whi_2018, pred.bla=pred.bla_2018, pred.his=pred.his_2018, 
         pred.asi=pred.asi_2018, pred.oth=pred.oth_2018)
demog_addr_2019<-demog_addr_2019%>%
  rename(pred.whi=pred.whi_2019, pred.bla=pred.bla_2019, pred.his=pred.his_2019, 
         pred.asi=pred.asi_2019, pred.oth=pred.oth_2019)
poll_demog_addr_all<-rbind(demog_addr_2018, demog_addr_2019)
## create race variable for cross-sectional logistic regression
#poll_demog_addr_all<- replace_na_with_neg_inf(poll_demog_addr_all,c("pred.whi","pred.bla","pred.his",
#                                      "pred.asi","pred.oth"))
#poll_demog_addr_all<-create_most_prob_race_var(poll_demog_addr_all,c("pred.whi","pred.bla","pred.his",
#                                       "pred.asi","pred.oth"))
poll_demog_addr_all<-most_prob_race(poll_demog_addr_all,c("pred.whi","pred.bla","pred.his",
                                           "pred.asi","pred.oth"))
# remove data frames to free up memory
rm(demog_addr_2018, demog_addr_2019)

#### Merge in voting history on L2 voterid
vote_poll_demog_addr_all<-left_join(poll_demog_addr_all, hist_2020, by='LALVOTERID')
# remove data frames to free up memory
rm(hist_2020, poll_demog_addr_all)

### merge in 2018 election records from 2019 file to replace missings in 2020 file
hist_2019<-select(hist_2019, c('LALVOTERID', 'General_2018_11_06'))
vote_poll_demog_addr_all<-vote_poll_demog_addr_all%>%
  # join on L2 voterid
  inner_join(hist_2019, by='LALVOTERID')%>%
  # coalesce to replace missing values
  mutate(General_2018_11_06 = coalesce(General_2018_11_06.x, General_2018_11_06.y),
         Voters_StateVoterID = coalesce(Voters_StateVoterID.x, Voters_StateVoterID.y))%>%
  # remove duplicate variables
  select(-c(General_2018_11_06.x, General_2018_11_06.y, X, Voters_StateVoterID.x, 
            Voters_StateVoterID.y))

### Merge in religious census data on county name
# filter religious data to just PA... 
# relig_census<-relig_census%>%
#   filter(`State Name`=='Pennsylvania', `Group Name`=='Catholic Church')%>%
#   #and select county name, group, congregations, and adherents in population
#   select(c(`County Name`,`Group Name`,Congregations, `Adherents as % of Total Population`))
# # and bring county names into line with L2 and government data
# relig_census$`County Name`<-str_extract(relig_census$`County Name`, '[:alpha:]*(?=[:space:])')
# relig_census$County<-toupper(relig_census$`County Name`)
# # merge in 
# vote_poll_demog_addr_all<-left_join(vote_poll_demog_addr_all, relig_census, by='County')

### Convert vote to binary
vote_poll_demog_addr_all <- binarize_vote(vote_poll_demog_addr_all, 'General_2016_11_08', 'Y')
vote_poll_demog_addr_all <- binarize_vote(vote_poll_demog_addr_all, 'General_2017_11_07', 'Y')
vote_poll_demog_addr_all <- binarize_vote(vote_poll_demog_addr_all, 'General_2018_11_06', 'Y')
vote_poll_demog_addr_all <- binarize_vote(vote_poll_demog_addr_all, 'General_2019_11_05', 'Y')

# Create treatment indicators (takes a very long time)
## remove voters with only 1 year of data
# vote_poll_demog_addr_all<-vote_poll_demog_addr_all%>%
#   filter(complete.cases(.))%>% #remove rows with missing data
#   group_by(LALVOTERID)%>% 
#   filter(n()>1)%>% # remove voters with only one year
#   ungroup()
## create treatment indicators
vote_poll_demog_addr_all<-vote_poll_demog_addr_all%>%
  group_by(LALVOTERID)%>%
  mutate(
    ## set to TRUE when address doesn't match previous
    changed_address = (Residence_Addresses_AddressLine != lag(Residence_Addresses_AddressLine)),
    ## set to TRUE when polling location address doesn't match previous
    changed_poll_loc = (Description != lag(Description))
    )%>%
    ## Once treated, all subsequent values = treated
    ### need extra step to avoid NAs in 2017?
    #changed_prec = ifelse(is.na(lag(changed_prec)), changed_prec, 
    #                      ifelse(lag(changed_prec), T, changed_prec)),
    #changed_address = ifelse(is.na(lag(changed_address)), changed_address, 
    #                         ifelse(lag(changed_address), T, changed_address)))%>%
  group_by(year)%>%
  mutate(
    ## set missing to FALSE (didn't change as far as we know)
    changed_address = ifelse(is.na(changed_address), F, changed_address),
    changed_poll_loc = ifelse(is.na(changed_poll_loc), F, changed_poll_loc))%>%
  ungroup()
# Create treatment indicators based on first indicators
vote_poll_demog_addr_all$moved_new_poll_loc=((vote_poll_demog_addr_all$changed_address==TRUE) & (vote_poll_demog_addr_all$changed_poll_loc==TRUE))
vote_poll_demog_addr_all$moved_old_poll_loc=((vote_poll_demog_addr_all$changed_address==TRUE) & (vote_poll_demog_addr_all$changed_poll_loc==FALSE))
vote_poll_demog_addr_all$no_move_new_poll_loc=((vote_poll_demog_addr_all$changed_address==FALSE) & (vote_poll_demog_addr_all$changed_poll_loc==TRUE))

# Extract years registered from calculated registration date
vote_poll_demog_addr_all$year_reg<-str_sub(vote_poll_demog_addr_all$Voters_CalculatedRegDate, start=-4)

# drop variables not in Regression and DiD Models
vote_poll_demog_addr_all<-select(vote_poll_demog_addr_all, c(LALVOTERID, year, 
                                                             County, Precinct,
                                   General_2016_11_08,General_2017_11_07,
                                   General_2018_11_06,General_2019_11_05,
                                   changed_address,
                                   #changed_poll_cat,
                                   changed_poll_loc,
                                   #moved_new_poll_cat,
                                   #no_move_new_poll_cat,
                                   #moved_old_poll_cat,
                                   moved_new_poll_loc,
                                   no_move_new_poll_loc,
                                   moved_old_poll_loc,
                                   location_category,
                                   Voters_Gender,Voters_Age,
                                   CommercialData_EstimatedHHIncomeAmount,
                                   Residence_Families_HHCount,has_child,
                                   Religions_Description,known_religious,
                                   known_catholic,
                                   CommercialData_LikelyUnion,
                                   CommercialData_OccupationIndustry,
                                   CommercialData_OccupationGroup,
                                   Parties_Description,
                                   pred.whi,pred.bla,pred.his,pred.asi,pred.oth,
                                   pred_race,
                                   Shape_Length,
                                   year_reg))

# Modify variables to fit DiD package requirements
## ID
vote_poll_demog_addr_all$VOTERID<-str_sub(vote_poll_demog_addr_all$LALVOTERID, 
                                          6, nchar(vote_poll_demog_addr_all$LALVOTERID))
vote_poll_demog_addr_all$VOTERID<-as.numeric(vote_poll_demog_addr_all$VOTERID)

## treatment
vote_poll_demog_addr_all$changed_address<-as.numeric(vote_poll_demog_addr_all$changed_address)
#vote_poll_demog_addr_all$changed_prec<-as.numeric(vote_poll_demog_addr_all$changed_prec)
#vote_poll_demog_addr_all$changed_poll_cat<-as.numeric(vote_poll_demog_addr_all$changed_poll_cat)
vote_poll_demog_addr_all$changed_poll_loc<-as.numeric(vote_poll_demog_addr_all$changed_poll_loc)
#vote_poll_demog_addr_all$moved_new_poll_cat<-as.numeric(vote_poll_demog_addr_all$moved_new_poll_cat)
#vote_poll_demog_addr_all$no_move_new_poll_cat<-as.numeric(vote_poll_demog_addr_all$no_move_new_poll_cat)
#vote_poll_demog_addr_all$moved_old_poll_cat<-as.numeric(vote_poll_demog_addr_all$moved_old_poll_cat)
vote_poll_demog_addr_all$moved_new_poll_loc<-as.numeric(vote_poll_demog_addr_all$moved_new_poll_loc)
vote_poll_demog_addr_all$no_move_new_poll_loc<-as.numeric(vote_poll_demog_addr_all$no_move_new_poll_loc)
vote_poll_demog_addr_all$moved_old_poll_loc<-as.numeric(vote_poll_demog_addr_all$moved_old_poll_loc)
#vote_poll_demog_addr_all$no_move_new_precinct<-as.numeric(vote_poll_demog_addr_all$no_move_new_precinct)
#vote_poll_demog_addr_all$moved_new_precinct<-as.numeric(vote_poll_demog_addr_all$moved_new_precinct)

# save data
setwd(data_dir)
write.csv(vote_poll_demog_addr_all, 'DiD_prepped_poll_vote_16to19_no_rndm_race.csv')

