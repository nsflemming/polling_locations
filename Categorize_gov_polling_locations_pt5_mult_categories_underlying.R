# Nathaniel Flemming 
## Recategorizing locations with multiple categories matches
## and aligning categories that are spelled differently

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
#set year
years<-c('17','18','19')

i=years[1]
test<-read.csv(paste0(data_dir,'/poll_struct_key_cath_manual_govsource_underlying',i,'.csv'))

multiple_categories<-test%>%
  group_by(X,location_category)%>%
  summarize(num_locs=n())%>%
  ungroup()%>%
  filter(str_detect(location_category,".*/.*"))%>%
  select(-X)%>%
  group_by(location_category)%>%
  summarize(num_locs=n())%>%
  ungroup()%>%
  distinct()


single_categories<-test%>%
  group_by(X,location_category)%>%
  ungroup()%>%
  filter(!str_detect(location_category,".*/.*"))

# Recategorize multiples
multiple_categories<-multiple_categories%>%
  # approximately in order of frequency
  mutate(case_when(
    location_category == 'government/police' ~ "public justice",
    location_category == 'school/religious' ~ "religious school",
    location_category == 'school/sport' ~ "school",
    location_category == 'senior_center/other' ~ "senior/public center",
    location_category == 'public_center/government' ~ "senior/public center",
    location_category == 'government/sport' ~ "government",
    location_category == 'religious/union' ~ "religious",
    location_category == 'government/union' ~ "government",
    location_category == 'firestation/union' ~ "fire station",

    location_category == 'school/library' ~ "school",
    
  ))

# Fix spelling errors


