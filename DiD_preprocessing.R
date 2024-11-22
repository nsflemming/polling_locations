#Nathaniel Flemming
# 27/9/24
#20/11/24

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
poll_master<-rbind(poll_2019,poll_2018)
# remove data frames to free up memory
rm(poll_2020, poll_2019, poll_2018)


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
demog_addr_2018<-read_add_year(data_dir, 'L2PA_2018_address_in_18.csv', 2018)%>%
  select(-c(County, Voters_FIPS, Precinct, PrecinctName))
#poll_demog_addr<-left_join(poll_master, demog_addr_2018, by=c('LALVOTERID', 'year'))
#rm(demog_addr_2018)
demog_addr_2019<-read_add_year(data_dir, 'L2PA_2019_address_in_19.csv', 2019)
#poll_demog_addr<-left_join(poll_demog_addr, demog_addr_2019, by=c('LALVOTERID', 'year'))
#rm(demog_addr_2019)

#remove poll location data frame
#rm(poll_master)

## rbind years together
### make column names match (check that income variables are equivalent)
# poll_demog_addr_16<-poll_demog_addr_16%>%
#   rename(CommercialData_EstimatedHHIncomeAmount=CommercialData_EstimatedIncomeAmount,
#          pred.whi=pred.whi_2016, pred.bla=pred.bla_2016, pred.his=pred.his_2016, 
#          pred.asi=pred.asi_2016, pred.oth=pred.oth_2016)
# poll_demog_addr_17<-poll_demog_addr_17%>%
#   rename(CommercialData_EstimatedHHIncomeAmount=CommercialData_EstimatedIncomeAmount,
#          pred.whi=pred.whi_2017, pred.bla=pred.bla_2017, pred.his=pred.his_2017, 
#          pred.asi=pred.asi_2017, pred.oth=pred.oth_2017,
#          pred_race=pred_race.y,
#          pred_black=pred_black.y)%>%
#   select(-c(pred.whi_2018, pred.bla_2018, pred.his_2018, pred.asi_2018, 
#             pred.oth_2018, pred_race.x, pred_black.x))
poll_demog_addr_18<-poll_demog_addr_18%>%
  rename(pred.whi=pred.whi_2018, pred.bla=pred.bla_2018, pred.his=pred.his_2018, 
         pred.asi=pred.asi_2018, pred.oth=pred.oth_2018)
poll_demog_addr_19<-poll_demog_addr_19%>%
  rename(pred.whi=pred.whi_2019, pred.bla=pred.bla_2019, pred.his=pred.his_2019, 
         pred.asi=pred.asi_2019, pred.oth=pred.oth_2019)
## rbind
poll_demog_addr_all<-rbind(poll_demog_addr_18,poll_demog_addr_19)

# Merge in polling locations
merged_data<-left_join(poll_demog_addr_all, poll_master, by=c('LALVOTERID','year'))

# Merge in voting history
merged_data<-left_join(poll_demog_addr_all, hist_2020, by='LALVOTERID')

# Convert vote to binary
merged_data <- binarize_vote(merged_data, 'General_2016_11_08', 'Y')
merged_data <- binarize_vote(merged_data, 'General_2017_11_07', 'Y')
merged_data <- binarize_vote(merged_data, 'General_2018_11_06', 'Y')
merged_data <- binarize_vote(merged_data, 'General_2019_11_05', 'Y')

# Create treatment indicators
merged_data<-merged_data%>%
  group_by(LALVOTERID)%>%
  mutate(
    ## set to TRUE when precinct doesn't match previous
    changed_prec = Precinct != lag(Precinct),
    ## set to TRUE when address doesn't match previous
    changed_address = Residence_Addresses_AddressLine != lag(Residence_Addresses_AddressLine),
    ## Once treated, all subsequent values = treated
    ### need extra step to avoid NAs in 2017
    changed_prec = ifelse(is.na(lag(changed_prec)), changed_prec, 
                          ifelse(lag(changed_prec), T, changed_prec)),
    changed_address = ifelse(is.na(lag(changed_address)), changed_address, 
                             ifelse(lag(changed_address), T, changed_address)))%>%
  group_by(year)%>%
  mutate(
    ## set missing to FALSE
    changed_prec = ifelse(is.na(changed_prec), F, changed_prec),
    changed_address = ifelse(is.na(changed_address), F, changed_address))%>%
  ungroup()%>%
  # Create treatment indicators based on first indicators
  group_by(LALVOTERID)%>%
  mutate(
    # moved & precinct changed
    moved_new_precinct = (changed_address==TRUE & changed_prec==TRUE),
    # didn't move, but precinct changed (should that be possible? double check when precincts change)
    no_move_new_precinct = (changed_address==FALSE & changed_prec==TRUE))%>%
  ungroup()
# 'school' and other location dummies work as treatment variables for location effects


# drop variables not in DiD Models
merged_data<-select(merged_data, c(LALVOTERID, year,
                                   General_2016_11_08,General_2017_11_07,
                                   General_2018_11_06,General_2019_11_05,
                                   changed_address,changed_prec,moved_new_precinct,
                                   no_move_new_precinct,
                                   location_category,
                                   Voters_Gender,Voters_Age,
                                   CommercialData_EstimatedHHIncomeAmount,
                                   Residence_Families_HHCount,
                                   known_religious,CommercialData_LikelyUnion,
                                   CommercialData_OccupationIndustry))

# save data
write.csv(merged_data, 'DiD_prepped_loc_vote_16to18.csv')







#merged_data<-read.csv('DiD_prepped_loc_vote_17_18.csv')
merged_data <- merged_data[order(merged_data$LALVOTERID),]

test_data<-merged_data[1:1000,]


### merge demographic and address data with voting history and polling location data

# combine 2017 and 2018 addresses with 2019 (2020 file) vote data
## trim 2019 data to just voter data
#data_2020_trim<-data_2020%>%
#  select(LALVOTERID, General_2016_11_08, General_2017_11_07, General_2018_11_06, 
#         General_2019_11_05)
# free up memory
#rm(data_2020)

## find duplicates
# duplicates17 <- data_2017 %>%
#   group_by(LALVOTERID) %>%
#   filter(n() > 1) %>%
#   ungroup()





# Convert vote to binary
merged_data <- binarize_vote(merged_data, 'General_2016_11_08', 'Y')
merged_data <- binarize_vote(merged_data, 'General_2017_11_07', 'Y')
merged_data <- binarize_vote(merged_data, 'General_2018_11_06', 'Y')
merged_data <- binarize_vote(merged_data, 'General_2019_11_05', 'Y')

#merged_data<-read.csv('DiD_prepped_loc_vote_17_18.csv')
merged_data <- merged_data[order(merged_data$LALVOTERID),]

test_data<-merged_data[1:1000,]
#test_data2<-test_data[5:6,]
#unique(test_data2$Residence_Addresses_AddressLine)

# Create treatment indicators
merged_data<-merged_data%>%
  group_by(LALVOTERID)%>%
  mutate(
    ## set to TRUE when precinct doesn't match previous
    changed_prec = Precinct != lag(Precinct),
    ## set to TRUE when address doesn't match previous
    changed_address = Residence_Addresses_AddressLine != lag(Residence_Addresses_AddressLine),
    ## Once treated, all subsequent values = treated
    ### need extra step to avoid NAs in 2017
    changed_prec = ifelse(is.na(lag(changed_prec)), changed_prec, 
                          ifelse(lag(changed_prec), T, changed_prec)),
    changed_address = ifelse(is.na(lag(changed_address)), changed_address, 
                             ifelse(lag(changed_address), T, changed_address)))%>%
  group_by(year)%>%
  mutate(
    ## set missing to FALSE
    changed_prec = ifelse(is.na(changed_prec), F, changed_prec),
    changed_address = ifelse(is.na(changed_address), F, changed_address))%>%
  ungroup()%>%
  # Create treatment indicators based on first indicators
  group_by(LALVOTERID)%>%
  mutate(
    # moved & precinct changed
    moved_new_precinct = (changed_address==TRUE & changed_prec==TRUE),
    # didn't move, but precinct changed (should that be possible? double check when precincts change)
    no_move_new_precinct = (changed_address==FALSE & changed_prec==TRUE))%>%
  ungroup()
# 'school' and other location dummies work as treatment variables for location effects


# drop variables not in DiD Models
merged_data<-select(merged_data, c(LALVOTERID, year,
                                   General_2016_11_08,General_2017_11_07,
                                   General_2018_11_06,General_2019_11_05,
                                   changed_address,changed_prec,moved_new_precinct,
                                   no_move_new_precinct,
                                   poll_location,
                                   Voters_Gender,Voters_Age,
                                   CommercialData_EstimatedHHIncomeAmount,
                                   Residence_Families_HHCount,
                                   known_religious,CommercialData_LikelyUnion,
                                   CommercialData_OccupationIndustry))

# save data
write.csv(merged_data, 'DiD_prepped_loc_vote_16to18.csv')


