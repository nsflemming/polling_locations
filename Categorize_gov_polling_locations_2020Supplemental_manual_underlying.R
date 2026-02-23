# Nathaniel Flemming 
## Using manually coded locations in one year to fill in missing location categories in another year

####################### libraries
library(dplyr)
library(stringr) #string manipulation
library(ggplot2) #plotting
library(readxl)
library(Dict) # dictionaries

####################### Main
#set directories
data_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data'
poll_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data/gov_poll_places'
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
#set manually coded year(s)
manual_years<-c('17','18','19')
# set target year
target_year<-c('20')

#Read in target year
target_year_data<-read.csv(paste0(data_dir,'/poll_struct_key_cath_govsource_underlying',
                                  target_year,'.csv'))

# Read in manually coded locations
## Combine manually coded location dataframes into one master list
## Prioritize 2018 coding over others (coded by Lee Ann)
manual<-data.frame()
for(year in manual_years){
  temp<-read_excel(paste0(data_dir,'/poll_manually_coded_missing_underlying',year,'.xlsx'))%>%
    mutate(year = year)%>%
    select(all_of(c('CountyName','PrecinctCode','PrecinctName','Description',
                    'address','manual_coded_location_category','year')))
  manual<-rbind(manual,temp)
}

manual<-manual%>%
  # Remove extra spaces around commas
  mutate(address = str_replace(address, "\\s,", ","))%>%
  # # Create address without zip code since they're too inconsistent in quality for matching
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

## Merge in manually coded data
target_year_data<-target_year_data%>%
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
target_year_data<-rows_patch(
  target_year_data,
    manual,
    by = c('PrecinctName','Description','address_stripped'),
    unmatched = 'ignore')%>%
    select(-c('address_stripped','PrecinctCode_new'))%>%
    ### Overwrite catholic school as just religious school
    mutate(location_category = ifelse(location_category=='catholic school','religious school',
                                      location_category))

# Save
write.csv(target_year_data,paste0(data_dir,'/poll_struct_key_cath_manual_govsource_underlying',target_year,'.csv'))
