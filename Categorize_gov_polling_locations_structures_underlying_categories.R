#Nathaniel Flemming 15 3 24
### categorizing polling locations using structure data
### polling location lists provided by state government

library(dplyr)
library(stringr) #string manipulation


############################################# functions

## create address from address components
make_address <- function(data, num, direction, street, street_type, city,
                         state, postcode){
  data[['address']]<-paste0(data[[num]], ' ',data[[direction]], ' ',data[[street]], 
                            ' ',data[[street_type]], ', ',data[[city]], ', ', 
                            data[[state]],' ', data[[postcode]])
  #replace any douple spaces created by a missing direction with a single space
  data[['address']]<-str_replace_all(data[['address']], '  ', ' ')
  return(data)
}

## read in location data
get_poll <- function(dir, filename, num, direction, street, street_type, city,
                     state, postcode){
  setwd(dir)
  data <- read.csv(filename)
  data<-make_address(data, num, direction, street, street_type, city, state, postcode)
  return(data)
}

## processing structure data, get just PA, create address, add tag with structure type
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

## get and process structure data fully
get_process_struct_data<-function(dir, df_name, street_var='ADDRESS', city_var='CITY', 
                                  state_var='STATE', zip_var='ZIPCODE', structure_name,
                                  rep_str=c(' STREET'=' ST', ' ROAD'=' RD', 
                                            ' AVENUE'=' AVE', ' DRIVE'=' DR', 
                                            ' BOULEVARD'=' BLVD',' NORTH'=' N', 
                                            ' SOUTH'=' S', ' EAST'=' E', ' WEST'=' W')){
  setwd(dir)
  data<-read.csv(df_name)
  ### construct and trim to addresses and indicator
  #data<-process_struct_data(data, street_var, city_var, state_var, zip_var, 'public')
  data<-data %>%
    # JUST PA
    filter(data[[state_var]]=='PA')
  ### construct and trim to addresses and indicator
  data[['address']]<-toupper(paste0(data[[street_var]],', ',data[[city_var]],
                             ', ',data[[state_var]],' ',substr(data[[zip_var]],1,5)))
  data[['address']]<-lapply(data[['address']], toString)
  data[['location_category']]=structure_name
  data<-data%>%
    select(address, location_category)
  data <- as.data.frame(lapply(data, unlist))
  return(data)
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

## combine education dataframes
get_educ_data <- function(dir, filenames){
  setwd(dir)
  i=1
  for(file in filenames){
    assign(paste0('educ',i),read.csv(file))
    i=i+1
  }
  ### combine educations buildings
  education = rbind(educ1, educ2, educ3)
  return(education)
}



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
##    34: Law enforcement #
##    36: prison
# 780: mail and shipping
##    06: post office #
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
##    11: Court House #
##    23: headquarters
##    33: ranger station #
##    42: white house
##    44: City/town hall





#############################################

#set directories
struct_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data/Structures'
poll_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data/gov_poll_places'
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"

## string for abbreviating street names
rep_str <- c(' STREET'=' ST', ' ROAD'=' RD', ' AVENUE'=' AVE', ' DRIVE'=' DR', 
             ' BOULEVARD'=' BLVD',
             ' NORTH'=' N', ' SOUTH'=' S', ' EAST'=' E', ' WEST'=' W')

# get poll location data and process
filenames<-c('Polling Place List 20180514.csv','Polling Place List 20190513.csv',
             'Polling Place List 20201102.csv'#,'Polling Place List 20211101.csv',
             #'Polling Place List 20220506.csv','Polling Place List 20231106.csv'
             )
## Read in and process multiple location files
for(file in filenames){
    assign(paste0('poll_loc',substr(file,20,23)),
           get_poll(poll_dir, file, num='HouseNum', direction='PrefixDirection',
                    street='Street', street_type = 'StreetType',city='City',
                    state="State", postcode='PostalCode'))
}

##### Structure data
#reset directory
setwd(struct_dir)

## places of worship
worship<-get_process_struct_data(struct_dir, 'All_Places_Of_Worship_HIFLD.csv',
                                 street_var = 'STREET', city_var='CITY',
                                 state_var='STATE', zip_var = "ZIP", 
                                 structure_name = 'religious')


## Catholic Schools
cath_school<-read.csv(paste0(struct_dir,'/MSA_PA_Catholic_Schools_1_18_2025.csv'))
### Process address into standard format
#### remove united states and phone number
cath_school$address<-str_extract(cath_school$Address, ".*(?=\\,\\sUnited)")
#### shorten longer zip codes
cath_school$zip<-str_extract(cath_school$address, "([:digit:]+[:punct:][:digit:]+|[:digit:]{5})$")
cath_school$zip<-str_sub(cath_school$zip, end=5)
#### replace zip codes in addresses
cath_school$address<-str_extract(cath_school$address, ".*(?=\\s[:digit:]+)")
cath_school$address<-paste0(cath_school$address, ' ', cath_school$zip)
#### capitalize
cath_school$address<-toupper(cath_school$address)
#### location category
cath_school$location_category<-'catholic_school'
#### Trim
cath_school<-select(cath_school, c('address', 'location_category'))
### abbreviate street names
cath_school$address<-str_replace_all(cath_school$address, rep_str)

## Education buildings
educ_files<-c('Educational_Structures_NGDA_2023/Colleges_Universities_0.csv',
              'Educational_Structures_NGDA_2023/Schools_2.csv',
              'Educational_Structures_NGDA_2023/Technical_Trade_Schools_1.csv')
education<-get_educ_data(struct_dir, educ_files)
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
education <- education %>%
  distinct()

## fire stations
firestations<-get_process_struct_data(struct_dir, 'Fire_Stations_HIFLD.csv',
                                 structure_name = 'firestation')

## law enforcement
policestations<-get_process_struct_data(struct_dir, 
                                        'Law_Enforcement_Structures_NGDA_2023/Police_Stations_0.csv',
                                      structure_name = 'policestation')

## libraries
libraries<-get_process_struct_data(struct_dir, 'Libraries_HIFLD_19_4_23.csv',
                                   street_var = 'Address', city_var = 'City',
                                   state_var = 'State', zip_var = 'ZipCode',
                                        structure_name = 'library')

################# county specific structure data
#allegheny<-read.csv('AlleghenyCounty_publicbldgs2023.csv')
### construct and trim to addresses and indicator
#allegheny$STATE<-'PA'
#allegheny<-process_struct_data(allegheny, 'address', 'city', 'STATE', 'zipcode', 'allegh_pub')
### abbreviate street names
#allegheny$address<-str_replace_all(allegheny$address, rep_str)

################ national map data
nat_map<-read.csv('nat_map_PA.csv')
#create address variable
nat_map2<-process_nat_map_data(nat_map, 'address', 'city', 'state', 'zipcode', 'placeholder')
### abbreviate street names
nat_map2$address<-str_replace_all(nat_map2$address, rep_str)
#add ftype and fcode to data frame
nat_map2$fcode<-nat_map$fcode
nat_map2$ftype<-nat_map$ftype
## code buildings
location.type <- c(school=730)
### NOTE COMBINING PUBLIC BUILDINGS INTO GOVERNMENT CATEGORY
location.code <- c(firestation=74026, policestation=74034, government=78006,
                   courthouse=83011, other=83033, government=83044) #78006: postoffice, 83044:townhall, 83011:rangerstation
nat_map2$location_category <- names(location.type)[match(nat_map2$ftype, location.type)]
nat_map2$location_category <- names(location.code)[match(nat_map2$fcode, location.code)]
## drop ftype and fcode
nat_map2<-subset(nat_map2, select = -c(ftype,fcode))

###################### merge poll locations and structures
## one building can be multiple categories
### mark whether building is a category
### since issue with duplication one building is on multiple lists
structures<-rbind(worship, cath_school, education, firestations, policestations, 
                  nat_map2, libraries)
structures<-structures%>%

  mutate(religious=as.numeric(str_detect(location_category, 'religious')),
         catholic_school=as.numeric(str_detect(location_category, 'catholic_school')),
         school=as.numeric(str_detect(location_category, 'school')),
         government=as.numeric(str_detect(location_category, 'government')),
         firestation=as.numeric(str_detect(location_category, 'firestation')),
         other=as.numeric(str_detect(location_category, 'other')),
         government=as.numeric(str_detect(location_category, 'government')),
         policestation=as.numeric(str_detect(location_category, 'policestation')),
         courthouse=as.numeric(str_detect(location_category, 'courthouse')),
         library=as.numeric(str_detect(location_category, 'library')))
#remove uncategorized structures?
structures<-structures[complete.cases(structures),]
## save a copy of structure categorization matrix
#write.csv(structures, 'structure_matrix.csv')

############# merge
### put dataframes into a list
poll_dfs<-list('2018'=poll_loc2018, '2019'=poll_loc2019,
               '2020'=poll_loc2020#, '2021'=poll_loc2021, 
               #'2022'=poll_loc2022, '2023'=poll_loc2023
               )
# join poll locations to structures matrix on address
for(i in seq_along(poll_dfs)){
  #Join
  temp<-left_join(poll_dfs[[i]], structures, by=c('address'), relationship='many-to-many')
  #remove duplicates
  temp<-unique(temp)
  #Rename dataframe
  assign(paste0('poll_struct',names(poll_dfs)[i]),temp)
}
### put dataframes into a list again
poll_struct_dfs<-list('2018'=poll_struct2018, '2019'=poll_struct2019,
               '2020'=poll_struct2020#, '2021'=poll_struct2021, 
               #'2022'=poll_struct2022, '2023'=poll_struct2023
               )

#location category count instead of location category to see what's multiple coded
for(i in seq_along(poll_struct_dfs)){
  temp<-poll_struct_dfs[[i]]%>%
    group_by(address, PrecinctCode)%>%
    mutate(location_count = sum(across(c(religious,catholic_school,school,
                                         government,firestation,other,
                                         policestation,courthouse,
                                         library))))%>%
    ungroup()
  ##### Create location categories
  temp$location_category[temp$religious==1]<-'religious'
  temp$location_category[temp$catholic_school==1]<-'catholic_school'
  temp$location_category[temp$school==1]<-'school'
  temp$location_category[temp$government==1]<-'government'
  temp$location_category[temp$firestation==1]<-'firestation'
  temp$location_category[temp$other==1]<-'other'
  #temp$location_category[temp$rangerstation==1]<-'rangerstation'
  #temp$location_category[temp$postoffice==1]<-'postoffice'
  temp$location_category[temp$policestation==1]<-'policestation'
  temp$location_category[temp$courthouse==1]<-'courthouse'
  temp$location_category[temp$location_count>1]<-'multiple'
  # ###  Create police stations/townhalls and religious schools flags
  # temp<-temp%>%
  #   # Group rows by polling location
  #   group_by(address, PrecinctCode)%>%
  #   # Sum flags for categories
  #   mutate(justice = sum(across(c(policestation,courthouse))),
  #          public_justice = sum(across(c(citytownhall,justice))),
  #          religious_school = sum(across(c(religious,school,catholic_school))))%>%
  #   ungroup()
  # ## Add to category variable
  # temp$location_category[temp$public_justice>1]<-'public_justice'
  # temp$location_category[temp$religious_school>1]<-'religious_school'
  # ## Add catholic schools back in
  # temp$location_category[temp$catholic_school>0]<-'catholic_school'
  # 
  # #remove individual categories and location count
  # temp<-subset(temp, select=-c(religious,catholic_school,school,
  #                              citytownhall,firestation,rangerstation,
  #                              postoffice,policestation,courthouse,
  #                              library))
  #remove duplicates 
  temp<-unique(temp)
  ##(still have extra rows?)
  ####### save to csv
  #set directory
  setwd('C:/Users/natha/Desktop/Polling Places DiD/data')
  write.csv(temp, paste0('poll_struct_govsource_underlying',names(poll_struct_dfs)[i],'.csv'))
}

#calc missingness by checking which addresses are in structure list
#1-(sum(poll$address%in%structures$address)/9235) #60.9% missing











