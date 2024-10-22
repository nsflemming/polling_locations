#Nathaniel Flemming
# 27/9/24
#19/10/24

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


########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
# read in data
setwd(data_dir)
data_2020<-read.csv('L2PA_votehist_VM2_20.csv')
#data_2019<-read.csv('L2PA_full_VM2_19.csv')

# combine 2017 and 2018 addresses with 2019 (2020 file) vote data
## trim 2019 data to just voter data
data_2020_trim<-data_2020%>%
  select(LALVOTERID, General_2016_11_08, General_2017_11_07, General_2018_11_06, 
         General_2019_11_05)
# free up memory
rm(data_2020)

## find duplicates
# duplicates17 <- data_2017 %>%
#   group_by(LALVOTERID) %>%
#   filter(n() > 1) %>%
#   ungroup()

### read in voter data years
data_2018<-read.csv('L2PA_2018_address_in_18.csv')
data_2017<-read.csv('L2PA_2017_address_in_17.csv')
data_2016<-read.csv('L2PA_2016_address_in_16.csv')
## add year indicator
data_2016$year<-2016
data_2017$year<-2017
data_2018$year<-2018

## merge data each year of locations with voting indicator
merged_20_16<-left_join(data_2020_trim, data_2016, by='LALVOTERID')
merged_20_17<-left_join(data_2020_trim, data_2017, by='LALVOTERID')
merged_20_18<-left_join(data_2020_trim, data_2018, by='LALVOTERID')

# free up memory
rm(data_2016)
rm(data_2017)
rm(data_2018)

## rbind years together
### make column names match (check that income variables are equivalent)
merged_20_16<-merged_20_16%>%
  rename(CommercialData_EstimatedHHIncomeAmount=CommercialData_EstimatedIncomeAmount,
         pred.whi=pred.whi_2016, pred.bla=pred.bla_2016, pred.his=pred.his_2016, 
         pred.asi=pred.asi_2016, pred.oth=pred.oth_2016)
merged_20_17<-merged_20_17%>%
  rename(CommercialData_EstimatedHHIncomeAmount=CommercialData_EstimatedIncomeAmount,
         pred.whi=pred.whi_2017, pred.bla=pred.bla_2017, pred.his=pred.his_2017, 
         pred.asi=pred.asi_2017, pred.oth=pred.oth_2017,
         pred_race=pred_race.y,
         pred_black=pred_black.y)%>%
  select(-c(pred.whi_2018, pred.bla_2018, pred.his_2018, pred.asi_2018, 
            pred.oth_2018, pred_race.x, pred_black.x))
merged_20_18<-merged_20_18%>%
  rename(pred.whi=pred.whi_2018, pred.bla=pred.bla_2018, pred.his=pred.his_2018, 
         pred.asi=pred.asi_2018, pred.oth=pred.oth_2018)
#bind
merged_data<-rbind(merged_20_16,merged_20_17)
merged_data<-rbind(merged_data,merged_20_18)


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
                                   Voters_Gender,Voters_Age,
                                   CommercialData_EstimatedHHIncomeAmount,
                                   Residence_Families_HHCount,
                                   known_religious,CommercialData_LikelyUnion,
                                   CommercialData_OccupationIndustry))

# save data
write.csv(merged_data, 'DiD_prepped_loc_vote_16to18.csv')


