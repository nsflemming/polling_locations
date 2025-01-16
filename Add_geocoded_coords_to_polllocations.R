#Nathaniel Flemming
#16/1/25

## Combine geocoded file from Automatic poll location geocoding with poll location file


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

### read in geocoded file
geo<-read.csv(paste0(data_dir,'\\gov_poll_places geocoded\\geocoderesult_',year,'_poll_locations_10000.csv'), 
              header = F)

### read in poll location file
poll<-read.csv(paste0(data_dir,'\\poll_struct_key_govsource',substring(year,3,4),'.csv'))

### remove commas from addresses
geo$address<-str_remove_all(geo$V2, pattern = "\\,")
geo$address<-str_replace_all(geo$address, "[:space:]{2}", ' ')
poll$address<-str_remove_all(poll$address, pattern = "\\,")

### Merge coordinates into location file
test<-left_join(poll, geo, by='address')
