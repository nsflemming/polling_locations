# Nathaniel Flemming 
## Recategorizing locations with multiple categories matches
## and aligning categories that are spelled differently

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
#set year
years<-c('17','18','19')

for(year in years){
poll_data<-read.csv(paste0(data_dir,'/poll_struct_key_cath_manual_govsource_underlying',year,'.csv'))

# Isolate multiples to view
multiple_categories<-poll_data%>%
  group_by(X,location_category)%>%
  summarize(num_locs=n())%>%
  ungroup()%>%
  filter(str_detect(location_category,".*/.*"))%>%
  select(-X)%>%
  group_by(location_category)%>%
  summarize(num_locs=n())%>%
  ungroup()%>%
  distinct()

# single_categories<-poll_data%>%
#   group_by(X,location_category)%>%
#   ungroup()%>%
#   filter(!str_detect(location_category,".*/.*"))

# Recategorize multiples
poll_data<-poll_data%>%
  mutate(
    # Recode
    location_category = recode(
    location_category,
    'airport' = 'airport',
    'ambulance service' = 'medical',
    'apartment' = 'apartment',
    'art center' = 'art center',
    'association' = 'association',
    'assocation' = 'association',
    'business' = 'business',
    'club' = 'club',
    'community association' = 'association',
    'community center' = 'community center',
    'courthouse' = 'courthouse',
    'event space' = 'event space',
    'fire station' = 'fire station',
    'firestation' = 'fire station',
    'government' = 'government',
    'health center' = 'medical',
    'hotel' = 'hotel',
    'insufficient info' = 'insufficient info',
    'library' = 'library',
    'medical' = 'medical',
    'military' = 'military',
    'mobile home park' = 'mobile home park',
    'Monument' = 'monument',
    'multiple' = 'other', # shouldn't exist
    'museum' = 'museum',
    'school' = 'school',
    'nursing home' = 'nursing home',
    'other' = 'other',
    'park' = 'park',
    'police station' = 'police station',
    'post office' = 'post office',
    'private residence' = 'private residence',
    'public center' = 'public center',
    'public_center' = 'public center',
    'recreation facility' = 'recreation facility',
    'religious' = 'religious',
    'religious school' = 'religious school',
    'restaurant' = 'restaurant',
    'retirement community' = 'retirement community',
    'senior center' = 'senior center',
    'senior_center' = 'senior center',
    'sport' = 'sport',
    'sports association' = 'sports association',
    'stadium' = 'stadium',
    'union' = 'union',
    'veteran' = 'veteran',
    # multiples
    'government/police' = "government/justice",
    'government/military' = "government/military",
    'school/religious' = "religious school",
    'school/religious/union' = "religious school",
    'school/religious/sport' = "religious school",
    'school/sport' = "school",
    'school/union' = "school",
    'school/sport/union' = "school",
    'sport/school' = "school",
    'school/library' = "school",
    'school/government' = "school",
    'school/community center' = "school",
    'school/public_center' = "school",
    'school/votech' = "school",
    'art center/school' = "school",
    'religious/senior_center' = "senior center",
    'public_center/government' = "public center",
    'public_center/sport' = "public center",
    'public_center/union' = "public center",
    'religious/public_center' = "public center",
    'government/sport' = "government",
    'government/sport/other' = "government",
    'government/park' = "government",
    'government/union' = "government",
    'government/association' = "government",
    'religious/union' = "religious",
    'religious/sport' = "religious",
    'religious/association' = "religious",
    'religious/courthouse' = "religious",
    'religious/library' = "religious",
    'religious/sport/union' = "religious",
    'religious/government/union' = "religious",
    'apartment/public housing' = "public/subsidized housing",
    'apartment/subsidized housing' = "public/subsidized housing",
    'apartment/union' = "apartment",
    'religious/apartment' = "apartment",
    'public_center/apartment' = "apartment",
    'apartment/public center' = "apartment",
    'apartment/association' = "apartment",
    'club/apartment' = "apartment",
    'firestation/union' = "fire station",
    'firestation/government/union' = "fire station",
    'firestation/library' = "fire station",
    'school/firestation' = "fire station",
    'club/sport' = "club",
    'club/association' = "club",
    'club/union' = "club",
    'government/club' = "club",
    'private residence/insufficient info' = "insufficient info",
    'insufficient info/private residence' = "insufficient info",
    'park/insufficient info' = "insufficient info",
    'park/community center' = "community center",
    'association/community center' = "community center",
    'community center/association' = "community center",
    'art center/business' = 'art center',
    'park/public center' = "park",
    'religious/business' = "business",
    'courthouse/sport' = "courthouse",
    'courthouse/union' = "courthouse",
    'association/sport' = "association",
    'firestation/association' = "association",
    'association/event space' = 'association',
    'government/library' = "library",
    'library/union' = "library",
    'public_center/veteran' = "veteran",
    'sport/union' = 'sport',
    
    
    'firestation/government' = "government/firestation",
    'association/union' = "association/union",
    'religious/government' = "religious/government",
    'retirement community/nursing home' = "retirement community/nursing home",
    'public_center/senior_center' = "public center/senior center",
    
    'school/other' = "school",
    'school/senior_center/other' = "school",
    'senior_center/other' = "senior center",
    'government/senior_center/other' = "senior center",
    'public_center/senior_center/other' = "senior center",
    'public_center/other' = 'public center',
    'religious/other' = "religious",
    'apartment/other' = "apartment",
    'club/other' = "club",
    'association/other' = "association",
    'sport/other' = "sport",
    'union/other' = "union",
    .default = 'other'
    )
  )
# Save
write.csv(poll_data,paste0(data_dir,'/poll_struct_key_cath_manual_multcat_govsource_underlying',year,'.csv'))
}

# multiple_categories<-multiple_categories%>%
#   # approximately in order of frequency and dominant category
#   mutate(location_category = case_when(
#     location_category == 'government/police' ~ "government/justice",
#     location_category == 'school/religious' ~ "religious school",
#     location_category == 'school/religious/union' ~ "religious school",
#     location_category == 'school/religious/sport' ~ "religious school",
#     location_category == 'school/sport' ~ "school",
#     location_category == 'school/sport/union' ~ "school",
#     location_category == 'sport/school' ~ "school",
#     location_category == 'school/union' ~ "school",
#     location_category == 'school/library' ~ "school",
#     location_category == 'school/government' ~ "school",
#     location_category == 'school/community center' ~ "school",
#     location_category == 'school/public_center' ~ "school",
#     location_category == 'school/votech' ~ "school",
#     location_category == 'art center/school' ~ "school",
#     location_category == 'religious/senior_center' ~ "senior center",
#     location_category == 'public_center/government' ~ "public center",
#     location_category == 'public_center/sport' ~ "public center",
#     location_category == 'public_center/union' ~ "public center",
#     location_category == 'religious/public_center' ~ "public center",
#     location_category == 'government/sport' ~ "government",
#     location_category == 'government/sport/other' ~ "government",
#     location_category == 'government/park' ~ "government",
#     location_category == 'government/union' ~ "government",
#     location_category == 'government/association' ~ "government",
#     location_category == 'senior center/government' ~ "government",
#     location_category == 'religious/union' ~ "religious",
#     location_category == 'religious/sport' ~ "religious",
#     location_category == 'religious/association' ~ "religious",
#     location_category == 'religious/courthouse' ~ "religious",
#     location_category == 'religious/library' ~ "religious",
#     location_category == 'religious/sport/union' ~ "religious",
#     location_category == 'religious/government/union' ~ "religious",
#     location_category == 'apartment/public housing' ~ "public/subsidized housing",
#     location_category == 'apartment/subsidized housing' ~ "public/subsidized housing",
#     location_category == 'apartment/union' ~ "apartment",
#     location_category == 'religious/apartment' ~ "apartment",
#     location_category == 'public_center/apartment' ~ "apartment",
#     location_category == 'apartment/public center' ~ "apartment",
#     location_category == 'apartment/association' ~ "apartment",
#     location_category == 'club/apartment' ~ "apartment",
#     location_category == 'school/apartment' ~ "apartment",
#     location_category == 'school/apartment/union' ~ "apartment",
#     location_category == 'firestation/union' ~ "fire station",
#     location_category == 'firestation/government/union' ~ "fire station",
#     location_category == 'firestation/library' ~ "fire station",
#     location_category == 'school/firestation' ~ "fire station",
#     location_category == 'club/sport' ~ "club",
#     location_category == 'club/association' ~ "club",
#     location_category == 'club/union' ~ "club",
#     location_category == 'government/club' ~ "club",
#     location_category == 'private residence/insufficient info' ~ "insufficient info",
#     location_category == 'insufficient info/private residence' ~ "insufficient info",
#     location_category == 'park/insufficient info' ~ "insufficient info",
#     location_category == 'park/community center' ~ "community center",
#     location_category == 'association/community center' ~ "community center",
#     location_category == 'community center/association' ~ "community center",
#     location_category == 'art center/business' ~ 'art center',
#     location_category == 'park/public center' ~ "park",
#     location_category == 'religious/business' ~ "business",
#     location_category == 'courthouse/sport' ~ "courthouse",
#     location_category == 'courthouse/union' ~ "courthouse",
#     location_category == 'association/sport' ~ "association",
#     location_category == 'firestation/association' ~ "association",
#     location_category == 'association/event space' ~ 'association',
#     location_category == 'government/library' ~ "library",
#     location_category == 'library/union' ~ "library",
#     location_category == 'public_center/veteran' ~ "veteran",
#     location_category == 'sport/union' ~ 'sport',
# 
#     # Retained multiple categories and tough cases
#     location_category == 'firestation/government' ~ "government/firestation",
#     #location_category == 'association/union' ~ "association/union", #tough case
#     #location_category == 'religious/government' ~ "religious/government", #tough case
#     #location_category == 'religious/firestation' ~ "religious/firestation", #tough case
#     #location_category == 'religious/public center' ~ "other", #non-profit
#     location_category == 'retirement community/nursing home' ~ "retirement community/nursing home",
#     location_category == 'public_center/senior_center' ~ "public_center/senior_center",
# 
#     # Overwiting Others
#     location_category == 'school/other' ~ "school",
#     location_category == 'school/senior_center/other' ~ "school",
#     location_category == 'senior_center/other' ~ "senior center",
#     location_category == 'government/senior_center/other' ~ "senior center",
#     location_category == 'public_center/senior_center/other' ~ "senior center",
#     location_category == 'public_center/other' ~ 'public_center',
#     location_category == 'religious/other' ~ "religious",
#     location_category == 'apartment/other' ~ "apartment",
#     location_category == 'club/other' ~ "club",
#     location_category == 'association/other' ~ "association",
#     location_category == 'sport/other' ~ "sport",
#     location_category == 'union/other' ~ "union",
#     #
#     .default = location_category
#   ))
# 
# # Isolate multiples to view
# multiple_categories<-multiple_categories%>%
#   group_by(location_category)%>%
#   filter(str_detect(location_category,".*/.*"))%>%
#   distinct()


