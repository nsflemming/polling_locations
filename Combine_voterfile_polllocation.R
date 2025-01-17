#Nathaniel Flemming
#7/11/24

## Combine processed voter file from PA_voterfile_processing with poll location file


################# Libraries
library(tidyverse)
library(data.table) #read in data selectively
library(stringr) #string manipulation

################# Functions
### pad precinct code to specified length (bugged)
pad_code<-function(df, code_var,county_var, counties, code_length){
  df[code_var][df[county_var]%in%counties]<-
    str_pad(df[code_var][df[county_var]%in%counties], code_length, pad = "0")
  return(df)
}


################## Main
### set directories
data_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data'
### set year
year='2018'

### read in processed voterfile
VF<-read.csv(paste0(data_dir,'\\FVE_',year,'.csv'))

### read in poll location file
poll<-read.csv(paste0(data_dir,'\\poll_struct_key_govsource',substring(year,3,4),'.csv'))

### merge files on county name and precinct code
## modify VF codes to match patterns of polling location codes
### Clinton, Franklin, Juniata... counties pad to length 2 with 0s on left
counties2pad<-c('SULLIVAN','ADAMS','ARMSTRONG','BRADFORD','CAMERON','HUNTINGDON',
                'NORTHUMBERLAND','SOMERSET','SUSQUEHANNA','WYOMING')
VF$PrecCode[VF$County%in%counties2pad]<-str_pad(VF$PrecCode[VF$County%in%counties2pad]
                                                 , 2, pad = "0")
### Clinton, Franklin, Juniata... counties pad to length 3 with 0s on left
counties3pad<-c('CHESTER','CLINTON','FRANKLIN', 'JUNIATA', 'PERRY','SNYDER','TIOGA','PIKE',
                'SCHUYLKILL','CLEARFIELD','WAYNE','CRAWFORD')
VF$PrecCode[VF$County%in%counties3pad]<-str_pad(VF$PrecCode[VF$County%in%counties3pad]
                                                           , 3, pad = "0")
### Montour county pad to length 4 with 0s on left
counties4pad<-c('MONTOUR','FULTON','POTTER','BEDFORD','BEAVER','BUTLER', 'ELK','JEFFERSON',
                'LAWRENCE','MIFFLIN','COLUMBIA','PHILADELPHIA','MERCER','WASHINGTON',
                'LYCOMING','WARREN')
VF$PrecCode[VF$County%in%counties4pad]<-str_pad(VF$PrecCode[VF$County%in%counties4pad]
                                                , 4, pad = "0")
### Greene, McKean county pad to length 5 with 0s on left
counties5pad<-c('GREENE','MCKEAN','ERIE','BUCKS','INDIANA','FAYETTE','WESTMORELAND')
VF$PrecCode[VF$County%in%counties5pad]<-str_pad(VF$PrecCode[VF$County%in%counties5pad]
                                                           , 5, pad = "0")
### Union county pad to length 6 with 0s on left
counties6pad<-c('UNION','NORTHAMPTON','LUZERNE','DAUPHIN')
VF$PrecCode[VF$County%in%counties6pad]<-str_pad(VF$PrecCode[VF$County%in%counties6pad]
                                                , 6, pad = "0")
### LEBANON, Remove trailing whitespace
whitespace<-c('LEBANON')
VF$PrecCode[VF$County%in%whitespace]<-str_trim(VF$PrecCode[VF$County%in%whitespace], side = "right")

### BUTLER, pad both to length 4
poll$PrecinctCode[poll$CountyName=='BUTLER']<-str_pad(poll$PrecinctCode[poll$CountyName=='BUTLER']
                                                , 4, pad = "0")
### Rename McKean to MCKEAN
poll$CountyName[poll$CountyName=='McKEAN']<-'MCKEAN'

# Merge dataframes
VF_location<-left_join(VF, poll, c('County'='CountyName', 'PrecCode'='PrecinctCode'))

## check for missingness
sum(is.na(VF_location$PrecinctName))

# drop unneeded columns
VF_location<-VF_location%>%
  select(c(VOTERID,County,PrecCode,PrecinctName,Description,location_category))

#save to csv
write.csv(VF_location,paste0(data_dir,'\\FVE_',year,'_polllocation.csv'))

