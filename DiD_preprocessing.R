#Nathaniel Flemming
# 27/9/24

# Preprocessing data for the difference in difference regressions

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
data_2019<-read.csv('L2PA_full_VM2_19.csv')
data_2020<-read.csv('L2PA_full_VM2_20.csv')

# copy 2019 L2 voter file address and location category to L2 2020 file, matching on voter ID
## leaving out covariates that could change for now, not certain if/how relevant
data_2019_trim<-data_2019%>%
  select(LALVOTERID, Residence_Addresses_AddressLine, County, Precinct, PrecinctName,
         location_category)
# free up memory
rm(data_2019)

## merge 2019 and 2020 data
#### warning of multiple matches, voter ids should be unique
#### 2020 voter file has rows with identical voter ids
merged_data<-left_join(data_2020, data_2019_trim, by='LALVOTERID')
# rename variables to indicate year
merged_data<-merged_data%>%
  rename(Residence_Addresses_AddressLine2020=Residence_Addresses_AddressLine.x,
         Residence_Addresses_AddressLine2019=Residence_Addresses_AddressLine.y,
         County2020=County.x,
         County2019=County.y,
         Precinct2020=Precinct.x,
         Precinct2019=Precinct.y,
         PrecinctName2020=PrecinctName.x,
         PrecinctName2019=PrecinctName.y,
         location_category2020=location_category.x,
         location_category2019=location_category.y)


# Convert vote to binary
merged_data <- binarize_vote(merged_data, 'General_2018_11_06', 'Y')
merged_data <- binarize_vote(merged_data, 'General_2019_11_05', 'Y')

#test_data<-sample_n(merged_data, 10000)
