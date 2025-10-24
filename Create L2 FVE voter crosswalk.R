#Nathaniel Flemming


################# Libraries
library(tidyverse)
library(data.table) #read in data selectively
library(stringr) #string manipulation

################# Functions
### pad precinct code to specified length (bugged?)
pad_code<-function(df, code_var,county_var, counties, code_length){
  df[code_var][df[county_var]%in%counties]<-
    str_pad(df[code_var][df[county_var]%in%counties], code_length, pad = "0")
  return(df)
}


################## Main
### set directories
data_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data'
### set year
year='2017'

### read in L2 voterfile which has  L2 and government voter ids
VFL2<-read.csv(paste0(data_dir,'\\L2PA_poll_loc_VM2_',str_sub(year,start=-2),'.csv'))%>%
  select(all_of(c('LALVOTERID','Voters_StateVoterID')))
#save
write.csv(VFL2,paste0(data_dir,'\\L2_StateID_crosswalk_',str_sub(year,start=-2),'.csv'))

