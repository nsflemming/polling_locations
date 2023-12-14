#Nathaniel Flemming 17 9 23
### categorizing polling locations using structure data

library(dplyr)
library(stringr) #string manipulation

#set directory
setwd('C:/Users/natha/Desktop/Polling Places/data')

## string for abbreviating street names
rep_str <- c(' STREET'=' ST', ' ROAD'=' RD', ' AVENUE'=' AVE', ' DRIVE'=' DR', 
             ' BOULEVARD'=' BLVD',
             ' NORTH'=' N', ' SOUTH'=' S', ' EAST'=' E', ' WEST'=' W')

#define a functions
## processing structure data
process_struct_data<-function(df, street_var, city_var, state_var, zip_var, structure_name){
  df<-df %>%
    # JUST PA
    filter(df[[state_var]]=='PA')
  ### construct and trim to addresses and indicator
  df$address<-toupper(paste0(df[[street_var]],', ',df[[city_var]],
                             ', ',df[[state_var]],' ',substr(df[[zip_var]],1,5)))
  df$address<-lapply(df$address, toString)
  df$location_category=structure_name
  df<-df%>%
    select(address, location_category)
  df <- as.data.frame(lapply(df, unlist))
}
## processing national map data (keep fcode and ftype)
process_nat_map_data<-function(df, street_var, city_var, state_var, zip_var, structure_name){
  ### construct and trim to addresses and indicator
  df$address<-toupper(paste0(df[[street_var]],', ',df[[city_var]],
                             ', ',df[[state_var]],' ',substr(df[[zip_var]],1,5)))
  df$address<-lapply(df$address, toString)
  df$location_category=structure_name
  df<-df%>%
    select(address, location_category, fcode, ftype)
  df <- as.data.frame(lapply(df, unlist))
}

##read in polling locations
poll<-read.csv('polllocation2018.csv')
###remove old categorization
poll<-subset(poll, select=-c(location_category))
### abbreviate street names
poll$address<-str_replace_all(poll$address, rep_str)


########################################################read in and process data
#reset directory
setwd('C:/Users/natha/Desktop/Polling Places/data/Structures')

## places of worship
worship<-read.csv('All_Places_Of_Worship_HIFLD.csv')
### construct and trim to addresses and indicator
worship<-process_struct_data(worship, 'STREET', 'CITY', 'STATE', 'ZIP', 'religious')
### abbreviate street names
worship$address<-str_replace_all(worship$address, rep_str)


## Education buildings
education1<-read.csv('Educational_Structures_NGDA_2023/Colleges_Universities_0.csv')
education2<-read.csv('Educational_Structures_NGDA_2023/Schools_2.csv')
education3<-read.csv('Educational_Structures_NGDA_2023/Technical_Trade_Schools_1.csv')
### combine educations buildings
education = rbind(education1, education2, education3)
rm(education1, education2, education3)
### construct and trim to addresses and indicator
education<-process_struct_data(education, 'ADDRESS', 'CITY', 'STATE', 'ZIPCODE', 'school')
### abbreviate street names
education$address<-str_replace_all(education$address, rep_str)
### private schools
education_priv<-read.csv('Private_Schools_HIFLD.csv')
### construct and trim to addresses and indicator
education_priv<-process_struct_data(education_priv, 'ADDRESS', 'CITY', 'STATE', 'ZIP', 'school')
### abbreviate street names
education_priv$address<-str_replace_all(education_priv$address, rep_str)
### rbind public and private schools, remove duplicates
education = rbind(education, education_priv)
education <-
  education %>%
  distinct()

## fire stations
firestations<-read.csv('Fire_Stations_HIFLD.csv')
### construct and trim to addresses and indicator
firestations<-process_struct_data(firestations, 'ADDRESS', 'CITY', 'STATE', 'ZIPCODE', 'public')
### abbreviate street names
firestations$address<-str_replace_all(firestations$address, rep_str)

## law enforcement
policestations<-read.csv('Law_Enforcement_Structures_NGDA_2023/Police_Stations_0.csv')
### construct and trim to addresses and indicator
policestations<-process_struct_data(policestations, 'ADDRESS', 'CITY', 'STATE', 'ZIPCODE', 'justice')
### abbreviate street names
policestations$address<-str_replace_all(policestations$address, rep_str)

## libraries
libraries<-read.csv('Libraries_HIFLD_19_4_23.csv')
### construct and trim to addresses and indicator
libraries<-process_struct_data(libraries, 'Address', 'City', 'State', 'ZipCode', 'library')
### abbreviate street names
libraries$address<-str_replace_all(libraries$address, rep_str)

################# county specific structure data
allegheny<-read.csv('AlleghenyCounty_publicbldgs2023.csv')
### construct and trim to addresses and indicator
allegheny$STATE<-'PA'
allegheny<-process_struct_data(allegheny, 'address', 'city', 'STATE', 'zipcode', 'allegh_pub')
### abbreviate street names
allegheny$address<-str_replace_all(allegheny$address, rep_str)

################ national map data
nat_map<-read.csv('nat_map_PA.csv')
### ftype to structure type
###     fcode to structure (more specific category, w/in type)
# 730: education
##    02: school
##    03: school, elementary
##    04: school, middle
##    05: school, high
##    06: school, college/university
##    07: school, technical/trade
# 740: Emergency response and law enforcement
##    01: ambulance service
##    26: fire station/EMS
##    34: Law enforcement
##    36: prison
# 780: mail and shipping
##    06: post office
# 800: health and medicine
##    12: Hospital/medical center
# 820: Public attractions and landmark buildings
##    07: Cabin
##    08: Campground
##    10: Cemetery
##    18: Historic site/point of interest
##    34: National Symbol/Monument
##    40: Picnic area
##    47: Trailhead
##    48: Visitor/information center
# 830: government and military
##    04: US Capitol
##    06: State Capitol
##    08: US Supreme court
##    10: State Supreme Court
##    11: Court House
##    23: headquarters
##    33: ranger station
##    42: white house
##    44: City/town hall

#create address variable
nat_map2<-process_nat_map_data(nat_map, 'address', 'city', 'state', 'zipcode', 'placeholder')
### abbreviate street names
nat_map2$address<-str_replace_all(nat_map2$address, rep_str)
#add ftype and fcode to data frame
nat_map2$fcode<-nat_map$fcode
nat_map2$ftype<-nat_map$ftype
## code buildings
location.type <- c(school=730)
location.code <- c(public=74026, justice=74034, postoffice=78006,
                   justice=83011, rangerstation=83033, public=83044)
nat_map2$location_category <- names(location.type)[match(nat_map2$ftype, location.type)]
nat_map2$location_category <- names(location.code)[match(nat_map2$fcode, location.code)]
## drop ftype and fcode
nat_map2<-subset(nat_map2, select = -c(ftype,fcode))

########################################### merge poll locations and structures
## one building can be multiple categories
### mark whether building is a category
### since issue with duplication one building is on multiple lists
structures<-rbind(worship, education, firestations, policestations, allegheny, 
                  nat_map2, libraries)
structures<-structures%>%
  mutate(religious=as.numeric(str_detect(location_category, 'religious')),
         school=as.numeric(str_detect(location_category, 'school')),
         public=as.numeric(str_detect(location_category, 'public')),
         justice=as.numeric(str_detect(location_category, 'justice')),
         allegh_pub=as.numeric(str_detect(location_category, 'allegh_pub')),
         postoffice=as.numeric(str_detect(location_category, 'postoffice')),
         rangerstation=as.numeric(str_detect(location_category, 'rangerstation')),
         library=as.numeric(str_detect(location_category, 'library')))
#remove uncategorized structures
structures<-structures[complete.cases(structures),]

############# merge
polltest<-left_join(poll, structures, 'address', relationship='many-to-many')
#remove duplicates
polltest<-unique(polltest)
## save a copy of structure categorization matrix
write.csv(polltest, 'structure_matrix.csv')
#location category count instead of location category to see what's multiple coded
polltest<-polltest%>%
  group_by(address, precinct_id)%>%
  mutate(location_count = sum(across(c(religious,school,public,
                                           justice,postoffice,
                                           rangerstation,allegh_pub,library))))
#change locations with two categories to 'multiple' to check missingness
polltest$location_category[polltest$location_count>1]<-'multiple'
#remove individual categories and location count
polltest<-subset(polltest, select=-c(religious,school,public,
                                     justice,postoffice,
                                     rangerstation,allegh_pub,library))
#remove duplicates 
polltest<-unique(polltest)
##(still have extra rows for some reason)

#calc missingness by checking which addresses are in structure list
1-(sum(poll$address%in%structures$address)/9235) #60.9% missing
####### save to csv
#set directory
setwd('C:/Users/natha/Desktop/Polling Places/data')
write.csv(polltest, 'polllocation_and_structure.csv')




