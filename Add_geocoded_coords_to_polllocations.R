#Nathaniel Flemming
#16/1/25

## Combine geocoded file from Automatic poll location geocoding with poll location file


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
year='2016'

### read in geocoded file
geo<-read.csv(paste0(data_dir,'\\gov_poll_places geocoded\\geocoderesult_',year,'_poll_locations_10000.csv'), 
              header = F)

### read in poll location file
poll<-read.csv(paste0(data_dir,'\\poll_struct_key_govsource',substring(year,3,4),'.csv'))

### remove commas from addresses (2019 addresses have weird zipcodes too)
geo$address<-str_remove_all(geo$V2, pattern = "\\,")
geo$address<-str_remove_all(geo$address, pattern = "\\.0") #handle weird numeric zipcodes
geo$address<-str_replace_all(geo$address, "[:space:]{2}", ' ')
poll$address<-str_remove_all(poll$address, pattern = "\\,")

### remove duplicate addresses (precincts using same polling location)
address_longlat<-geo%>%
  select(c(address, V6))%>%
  distinct()

### Merge coordinates into location file
poll_longlat<-left_join(poll, address_longlat, by='address')%>%
  select(c(CountyName, PrecinctCode, PrecinctName, Description, address,
           location_category, V6))%>%
  rename(long_lat = V6)

#split longitude and latitude into separate columns
poll_longlat<-poll_longlat%>%
  mutate(poll_longitude=str_extract(long_lat, '.*(?=\\,)'),
         poll_latitude=str_extract(long_lat, '(?<=\\,).*'))

### read in L2 voterfile for residence point locations
VFL2<-read.csv(paste0(data_dir,'\\L2PA_',year,'_voter_locs_',str_sub(year,start=-2),'.csv'))%>%
  select(c(LALVOTERID, Residence_Addresses_Latitude, Residence_Addresses_Longitude,
           County,Precinct))
### read in state voterfile for precinct and precinct codes (usually zipped)
VFState<-read.csv(paste0(data_dir,'\\FVE_csvs\\FVE_',year,'.csv'))%>%
  select(VOTERID, PrecCode)
### read in id crosswalk
crosswalk<-read.csv(paste0(data_dir,'\\L2_StateID_crosswalk_',str_sub(year, start=-2),'.csv'))
### Merge L2 and State voter files using crosswalk
VF<-left_join(VFL2, crosswalk, by='LALVOTERID')
VF<-left_join(VF, VFState, by=c('Voters_StateVoterID'='VOTERID'))

#### modify VF codes to match patterns of polling location codes
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
VF$PrecCode[VF$County%in%whitespace]<-str_trim(VF$PrecCode[VF$County%in%whitespace], 
                                               side = "right")
### BUTLER, pad both to length 4
poll_longlat$PrecinctCode[poll_longlat$CountyName=='BUTLER']<-
  str_pad(poll_longlat$PrecinctCode[poll_longlat$CountyName=='BUTLER']                                                      , 4, pad = "0")
### Rename McKean to MCKEAN
poll_longlat$CountyName[poll_longlat$CountyName=='McKEAN']<-'MCKEAN'

### Merge voterid locations and poll locations
voter_poll_longlats<-left_join(VF,poll_longlat, by=c('County'='CountyName',
                                                     'PrecCode'='PrecinctCode'))
# number of missing polling location coordinates
sum(is.na(voter_poll_longlats$poll_longitude))

## remove missing for arcgis, Convert to strings?
voter_poll_longlats$Residence_Addresses_Latitude<-as.character(voter_poll_longlats$Residence_Addresses_Latitude)
voter_poll_longlats$Residence_Addresses_Longitude<-as.character(voter_poll_longlats$Residence_Addresses_Longitude)
voter_poll_longlats$poll_longitude<-as.character(voter_poll_longlats$poll_longitude)
voter_poll_longlats$poll_latitude<-as.character(voter_poll_longlats$poll_latitude)
voter_poll_longlats<-voter_poll_longlats%>%
  filter(complete.cases(.))
#save
write.csv(voter_poll_longlats, paste0(data_dir,'\\voterloc_pollocation',year,'_geocoded','.csv'))


