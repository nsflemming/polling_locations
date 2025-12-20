# Nathaniel Flemming 
## Adding manually coded locations that were missing

####################### libraries
library(dplyr)
library(stringr) #string manipulation
library(ggplot2) #plotting
library(readxl)

####################### Main
#set directories
data_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data'
poll_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data/gov_poll_places'
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
struct_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data/Structures'
#set year
years<-c('17','18','19')

# Read in manually coded locations
## Combine manually coded location dataframes into one master list
## Prioritize 2018 coding over others (coded by Lee Ann)
manual<-data.frame()
for(i in years){
  temp<-read_excel(paste0(data_dir,'/poll_manually_coded_missing_underlying',i,'.xlsx'))%>%
    mutate(year = i)%>%
    select(all_of(c('CountyName','PrecinctCode','PrecinctName','Description',
                    'address','manual_coded_location_category','year')))
  manual<-rbind(manual,temp)data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC
}
manual<-manual%>%
  # Remove extra spaces around commas
  mutate(address = str_replace(address, "\\s,", ","))%>%
  # # Create address without zip code since theyre too inconsistent in quality for matching
  mutate(address_stripped=str_extract(address,".*\\,\\sPA(?=\\s.*)"),
         address_stripped=ifelse(is.na(address_stripped),address,address_stripped))%>%
  distinct()%>%
  # Remove leading zeros from precinct codes
  mutate(PrecinctCode_new=str_remove(PrecinctCode, "^0+"),
         PrecinctCode_new=ifelse(is.na(PrecinctCode_new),PrecinctCode,PrecinctCode_new))%>%
  ## Prioritize 2018 coding over others (coded by Lee Ann)
  group_by(PrecinctName,Description,address)%>%
  mutate(duplicate = n()>1)%>%
  mutate(manual_coded_location_category = ifelse(duplicate == T, 
                                                 nth(manual_coded_location_category, 2, order_by = year),
                                                 manual_coded_location_category))%>%
  ungroup()%>%
  select(-c('year','duplicate','address'))%>%
  distinct()%>%
  rename(location_category=manual_coded_location_category)


## Remove the automatically coded locations since those are already in the data
# (unnecessary now)
# manual<-manual%>%
#   select(all_of(c('CountyName','PrecinctCode','PrecinctName','Description',
#                   'address','manual_coded_location_category')))%>%
#   rename(location_category = manual_coded_location_category)%>%
#   mutate(PrecinctCode = as.character(PrecinctCode))


# Read in automatically coded location data and merge in manually coded data
### Overwrite catholic school as just religious school
## auto-coded data
for(i in years){
  assign('poll_struct_key_cath_govsource', read.csv(paste0(data_dir,'/poll_struct_key_cath_govsource_underlying',i,'.csv')))
  ## Merge in manual coded data
  poll_struct_key_cath_manual_govsource<-poll_struct_key_cath_govsource%>%
    select(all_of(c('CountyName','PrecinctCode','PrecinctName','Description',
                    'address','location_category')))%>%
    # remove trailing spaces
    mutate(address = trimws(as.character(address)),
           PrecinctName = trimws(as.character(PrecinctName)))%>%
    # Remove extra spaces around commas
    mutate(address = str_replace_all(address, "\\s,", ","))%>%
    # Create address without zipcode since it's too inconsistent for matching
    mutate(address_stripped=str_extract(address,".*\\,\\sPA(?=\\s.*)"),
           address_stripped=ifelse(is.na(address_stripped),address,address_stripped))%>%
    # Remove leading zeros from precinct codes
    mutate(PrecinctCode_new=str_remove(PrecinctCode, "^0+"),
           PrecinctCode_new=ifelse(is.na(PrecinctCode_new),PrecinctCode,PrecinctCode_new))
  poll_struct_key_cath_manual_govsource<-rows_patch(
    poll_struct_key_cath_manual_govsource,
    manual,
    by = c('PrecinctName','Description','address_stripped'),
    unmatched = 'ignore'
  )%>%
    select(-c('address_stripped','PrecinctCode_new'))%>%
  ### Overwrite catholic school as just religious school
  mutate(location_category = ifelse(location_category=='catholic school','religious school',
                                    location_category))
  
  missing<-poll_struct_key_cath_manual_govsource[is.na(poll_struct_key_cath_manual_govsource$location_category),]
  
  print(nrow(missing))
  # Save
  write.csv(poll_struct_key_cath_manual_govsource,paste0(data_dir,'/poll_struct_key_cath_manual_govsource_underlying',i,'.csv'))
}
##### 4 Locations in 2017 need to be fixed manually#############
