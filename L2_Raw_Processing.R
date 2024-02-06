#Nathaniel Flemming
#6/2/24

library(tidyverse)
library(data.table) #read in L2 data selectively

get_L2_data <- function(L2_dir, filename, vars){
  setwd(L2_dir)
  L2<-fread(filename, sep='\t', header=T, select = vars)
  return(L2)
}



#################################################################

# set directories
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
              'Religions_Description', 'MaritalStatus_Description',
              'CommercialData_Education', 'CommercialData_HHComposition',
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
# Combine L2 data
L2demog<-left_join(L2demog, L2votehist, by = 'LALVOTERID')














##############################