#Nathaniel Flemming
# 27/9/24

# Preprocessing data for the difference in difference regressions
## uses data files made by L2_Raw_Processing scripts

library(did) #difference in difference package
library(tidyverse) #convenience
library(data.table) #read in data selectively


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


########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
# read in vote history and poll location data
## adding year indicator
hist_poll_2020<-read_add_year(data_dir, 'L2PA_votehist_VM2_20.csv',2020)
### separate into polling location info and voting history
#poll_2020<-hist_poll_2020%>%select(-c(General_2016_11_08, General_2017_11_07, General_2018_11_06, General_2019_11_05))
hist_2020<-hist_poll_2020%>%select(c(LALVOTERID, General_2016_11_08, General_2017_11_07, General_2018_11_06, General_2019_11_05))
rm(hist_poll_2020)
# read in poll location data
poll_2019<-read_add_year(data_dir, 'L2PA_votehist_VM2_19.csv',2019)
poll_2018<-read_add_year(data_dir, 'L2PA_votehist_VM2_18.csv',2018)
# bind different years of polling locations together
#poll_master<-rbind(poll_2019,poll_2018)
# remove data frames to free up memory
#rm(poll_2020, poll_2019, poll_2018)


### read in voter demographic and address data
## remove county and precinct columns since that's in the voter history/polling location files
## add year indicator
## 
# free up memory
# demog_addr_2016<-read_add_year(data_dir, 'L2PA_2016_address_in_16.csv', 2016)%>%
#     select(-c(County, Voters_FIPS, Precinct, PrecinctName))
# poll_demog_addr_16<-left_join(poll_master, demog_addr_2016, by='LALVOTERID')
# rm(demog_addr_2016)
# demog_addr_2017<-read_add_year(data_dir, 'L2PA_2017_address_in_17.csv', 2017)%>%
#   select(-c(County, Voters_FIPS, Precinct, PrecinctName))
# poll_demog_addr_17<-left_join(poll_master, demog_addr_2017, by='LALVOTERID')
# rm(demog_addr_2017)
#demog_addr_2018<-read_add_year(data_dir, 'L2PA_2018_address_in_18.csv', 2018)%>%
demog_addr_2018<-read.csv(paste0(data_dir,'\\L2PA_2018_address_in_18.csv'))%>%
  select(-c(X, County, Voters_FIPS, Precinct, location_category, pub_loc, pub_just,
            other, relig_loc, school, multiple, justice_loc, library, relig_school))
#poll_demog_addr<-left_join(poll_master, demog_addr_2018, by=c('LALVOTERID', 'year'))
#rm(demog_addr_2018)
#demog_addr_2019<-read_add_year(data_dir, 'L2PA_2019_address_in_19.csv', 2019)
demog_addr_2019<-read.csv(paste0(data_dir,'\\L2PA_2019_address_in_19.csv'))%>%
  select(-c(X, County, Voters_FIPS, Precinct,location_category, pub_loc, pub_just,
            other, relig_loc, school, multiple, justice_loc, library, relig_school))
#poll_demog_addr<-left_join(poll_demog_addr, demog_addr_2019, by=c('LALVOTERID', 'year'))
#rm(demog_addr_2019)

#### Merge poll location data with voter demographic data on L2 voterid
demog_addr_2018<-left_join(demog_addr_2018, poll_2018, by='LALVOTERID')
demog_addr_2019<-left_join(demog_addr_2019, poll_2019, by='LALVOTERID')
# remove data frames to free up memory
rm(poll_2018, poll_2019)

#### rbind years together
## make column names match (check that income variables are equivalent)
demog_addr_2018<-demog_addr_2018%>%
  rename(pred.whi=pred.whi_2018, pred.bla=pred.bla_2018, pred.his=pred.his_2018, 
         pred.asi=pred.asi_2018, pred.oth=pred.oth_2018)
demog_addr_2019<-demog_addr_2019%>%
  rename(pred.whi=pred.whi_2019, pred.bla=pred.bla_2019, pred.his=pred.his_2019, 
         pred.asi=pred.asi_2019, pred.oth=pred.oth_2019)
poll_demog_addr_all<-rbind(demog_addr_2018, demog_addr_2019)
# remove data frames to free up memory
rm(demog_addr_2018, demog_addr_2019)

#### Merge in voting history on L2 voterid
vote_poll_demog_addr_all<-left_join(poll_demog_addr_all, hist_2020, by='LALVOTERID')
# remove data frames to free up memory
rm(hist_2020, poll_demog_addr_all)

# Convert vote to binary
vote_poll_demog_addr_all <- binarize_vote(vote_poll_demog_addr_all, 'General_2016_11_08', 'Y')
vote_poll_demog_addr_all <- binarize_vote(vote_poll_demog_addr_all, 'General_2017_11_07', 'Y')
vote_poll_demog_addr_all <- binarize_vote(vote_poll_demog_addr_all, 'General_2018_11_06', 'Y')
vote_poll_demog_addr_all <- binarize_vote(vote_poll_demog_addr_all, 'General_2019_11_05', 'Y')

# Create treatment indicators (takes a very long time)
vote_poll_demog_addr_all<-vote_poll_demog_addr_all%>%
  group_by(LALVOTERID)%>%
  mutate(
    ## set to TRUE when precinct doesn't match previous
    #changed_prec = Precinct != lag(Precinct),
    ## set to TRUE when address doesn't match previous
    changed_address = Residence_Addresses_AddressLine != lag(Residence_Addresses_AddressLine),
    ## set to TRUE when polling location category doesn't match previous
    changed_poll_cat = location_category != lag(location_category))%>%
    ## Once treated, all subsequent values = treated
    ### need extra step to avoid NAs in 2017
    #changed_prec = ifelse(is.na(lag(changed_prec)), changed_prec, 
    #                      ifelse(lag(changed_prec), T, changed_prec)),
    #changed_address = ifelse(is.na(lag(changed_address)), changed_address, 
    #                         ifelse(lag(changed_address), T, changed_address)),
    #changed_poll_cat = ifelse(is.na(lag(changed_poll_cat)), changed_poll_cat, 
    #                      ifelse(lag(changed_poll_cat), T, changed_poll_cat)))%>%
  group_by(year)%>%
  mutate(
    ## set missing to FALSE (didn't change as far as we know)
    #changed_prec = ifelse(is.na(changed_prec), F, changed_prec),
    changed_address = ifelse(is.na(changed_address), F, changed_address),
    changed_poll_cat = ifelse(is.na(changed_poll_cat), F, changed_poll_cat))%>%
  ungroup()%>%
  # Create treatment indicators based on first indicators
  group_by(LALVOTERID)%>%
  mutate(
    # moved & precinct changed
    #moved_new_precinct = (changed_address==TRUE & changed_prec==TRUE),
    # didn't move, but precinct changed (should that be possible? double check when precincts change)
    #no_move_new_precinct = (changed_address==FALSE & changed_prec==TRUE),
    #moved and poll location category changed
    moved_new_poll= (changed_address==TRUE & changed_poll_cat==TRUE),
    #didn't move but poll location category changed
    no_move_new_poll = (changed_address==FALSE & changed_poll_cat==TRUE),
    #moved, but  poll location category unchanged
    moved_old_poll= (changed_address==TRUE & changed_poll_cat==FALSE))%>%
  ungroup()
# recreate location dummies for use as treatment variables for location effects

# drop variables not in DiD Models
vote_poll_demog_addr_all<-select(vote_poll_demog_addr_all, c(LALVOTERID, year,
                                   General_2016_11_08,General_2017_11_07,
                                   General_2018_11_06,General_2019_11_05,
                                   changed_address,
                                   #changed_prec,
                                   changed_poll_cat,
                                   #moved_new_precinct,
                                   #no_move_new_precinct,
                                   moved_new_poll,
                                   no_move_new_poll,
                                   moved_old_poll,
                                   location_category,
                                   Voters_Gender,Voters_Age,
                                   CommercialData_EstimatedHHIncomeAmount,
                                   Residence_Families_HHCount,
                                   known_religious,CommercialData_LikelyUnion,
                                   CommercialData_OccupationIndustry))

# Modify variables to fit DiD package requirements
## ID
vote_poll_demog_addr_all$VOTERID<-str_sub(vote_poll_demog_addr_all$LALVOTERID, 
                                          6, nchar(vote_poll_demog_addr_all$LALVOTERID))
vote_poll_demog_addr_all$VOTERID<-as.numeric(vote_poll_demog_addr_all$VOTERID)
# set years to 0 and 1
#model_data$year<-model_data$year+2017
## treatment
vote_poll_demog_addr_all$changed_address<-as.numeric(vote_poll_demog_addr_all$changed_address)
#vote_poll_demog_addr_all$changed_prec<-as.numeric(vote_poll_demog_addr_all$changed_prec)
vote_poll_demog_addr_all$changed_poll_cat<-as.numeric(vote_poll_demog_addr_all$changed_poll_cat)
vote_poll_demog_addr_all$moved_new_poll<-as.numeric(vote_poll_demog_addr_all$moved_new_poll)
vote_poll_demog_addr_all$no_move_new_poll<-as.numeric(vote_poll_demog_addr_all$no_move_new_poll)
vote_poll_demog_addr_all$moved_old_poll<-as.numeric(vote_poll_demog_addr_all$moved_old_poll)
#vote_poll_demog_addr_all$no_move_new_precinct<-as.numeric(vote_poll_demog_addr_all$no_move_new_precinct)
#vote_poll_demog_addr_all$moved_new_precinct<-as.numeric(vote_poll_demog_addr_all$moved_new_precinct)

# save data
setwd(data_dir)
write.csv(vote_poll_demog_addr_all, 'DiD_prepped_poll_vote_18to19.csv')

