# Nathaniel Flemming 21 3 24
## coding missing polling locations with keywords

library(dplyr)
library(stringr) #string manipulation
library(ggplot2) #plotting

############################## define functions
### read in location data
get_poll <- function(dir, filename, num, direction, street, street_type, city){
  setwd(dir)
  data <- read.csv(filename)
  return(data)
}

# Function to create vector of places that contain a category word in their name
pollnamegrepl<-function(words, df, name_var){
  # create initial index
  index<-(grepl(words[1], df[[name_var]]))
  for(i in 2:length(words)){
    temp<-(grepl(words[i], df[[name_var]]))
    # 'merge' in each successive index
    index<-index|temp
  }
  return(index)
}

#remove numbers from the beginning of location names (why need this again?)
rm_nums<-function(df, name_var){
  nums<-(str_detect(df[[name_var]], "^[:digit:][:digit:]\\/[:digit:][:digit:]"))
  df[[name_var]][nums]<-substr(df[[name_var]][nums],7,100)
  #df[[name_var]]<-str_extract(df[[name_var]], "(?<=[:digit:][:space:]).*")
  #nums2<-str_detect(df[[name_var]], "[:punct:][:digit:]+[:space:][:alpha:]")
  #print(df[[name_var]][nums2])
  return(df)
}


###################### keywords
#school words
schl_wrds<-c('SCHOOL','ELEMENTARY','ELEM','ACADEMY', 'ACCADEMY', 'UNIVERSITY',
             'SCH ',' SCH ','ELE SCH ','COLLEGE',' COLLEGE ', 'COMMUNITY COLLEGE', 
             'COMMUNITY COLL', 
             'JR HIGH','JR. HIGH', 
             'SR. HIGH', 'SR HIGH', 'HIGH SCH', 'MIDDLE SCH',
             'EDUCATION','GRADUATE',
             'SHOOL','CHILD DEVELOPMENT','DAYCARE')

#religion related words
relig_wrds<-c('CHURCH','MEETINGHOUSE','MEETING HOUSE','SYNAGOGUE','PARISH','TEMPLE',
              'CHAPEL',' CH ', 'CATHEDRAL', 'CONGREGATION', 'MOSQUE', 'CHRISTIAN',
              'LUTHERAN','METHODIST','SALVATION ARMY', 'MINISTRY', 
              'KNIGHTS OF COLUMBUS' , 'YMCA', 'Y.M.C.A','Y M C A', 'JCC',
              'MASONIC','INCARNATION', 'OF GOD', 'PRAYER', 'GOSPEL', 'JEWISH',
              'WORSHIP', 'APOSTOLIC', 'MINISTRIES', 'OUR LADY', 'HOLY TRINITY', 
              'MUSLIM','ISLAMIC',
              'HOLY','GOD[:punct:]S','GREEK ORTHODOX','MISSIONARY','CATHOLIC','PRESBYTERIAN',
              'BAPTIST','UCC','CONVENT','SHALOM','SHOLOM','FRIENDS MEETING',
              'FRIENDS MTG','TABERNACLE','BIBLE','APOSTLES','FELLOWSHIP','SAINT',
              'UNITARIAN')

#public center words
cent_wrds<-c('REC CENTER','REC CTR','REC CENT','RECREATION CENT','RECREATION CTR',
             'RECREATION BUILDING', 'RECREATION HALL',
             'COMMUNITY CENTER', 'COMM CENTER', 'COM CENTER', 'COM CTR', 'COMM CTR',
             'COMMUNITY CTR',
             'COMMUNITY BLDG', 'COMMUNITY BUILDING', 'COMMUNITY HALL',
             'CIVIC CENTER')


#Fire dept words
fire_wrds<-c('FIRE','V.F.D.','VFD','V F D', 'V F C','VFC','V.F.C.','ENGINE HOUSE',
             'HOSE','ENGINE CO')


#government building words
gov_wrds<-c(' MUNI ',' MUNIC ', 'MUNICIPAL', ' BORO ','BOROUGH','TOWNSHIP','TWP',
            'CITY HALL','TOWN HALL', 'COUNCIL', 'MUN BLDG', 'CTY OFFICE BLDG',
            'CIVIC BUILDING','CIVIC BLDG',
            'COUNTY SERVICE','GOVERN SERV CENTER', 'PUBLIC WORKS', 'PUBLIC SAFETY',
            'HUMAN SERVICES', 'EMERGENCY SERVICE','COMMUNITY ACT',
            'GOVERNMENT','COURT HOUSE', 'COURT', 'COURTHOUSE', 'DISTRICT JUDGE',
            'ARMORY', 'ADMINSTRATION', 'ADMINISTRATIVE', 'AUTHORITY',
            'PARAMEDIC', 'AMBULANCE','EMS STATION',
            'REGIONAL','COMMISSION',
            'CONVENTION CENTER','MUSEUM','AIRPORT','TRAIN STATION','TERMINAL')

## Combine government and fire

#club word

#apartment words
apt_wrds<-c(' APT ', ' APTS ','APARTMENT','TOWERS','TOWER','PRIVATE RESIDENCE',
            'CONDO','HIGH RISE','HIGHRISE','HI-RISE')

#library word

#Veterans association words
vet_wrds<-c('VFW', 'V F W', 'AMERICAN LEGION', 'AMER LEGION', 'VETERANS')

#Senior center words
sen_wrds<-c('SENIOR CENTER', 'SENIOR CITIZEN[:punct:]S CENTER', 'SENIOR CITIZENS',
            'SENIOR CITIZEN', 'SENIOR CENTER', 'SR CENTER', 'SR CITZ', 'SR CITIZEN', 
            'RETIREMENT CENTER', 'RETIREMENT COMM', 'RET. COM.', 'RETIREMENT HOME',
            'BRITH SHOLOM')

#Association words
assc_wrds<-c('GRANGE', '4[:punct:]H', '4H','ORDER OF', 'ASSOC', 'D.A.R.', 
             'CHAMBER OF COMMERCE', 'ELKS LODGE', 'MOOSE LODGE', 'LION[:punct:]S', 
             'LIONS', 'FRATERNAL','COMMUNITY ASS','V.F.W.','SCOTTISH RITE',
             'HISTORICAL SOCIETY','HIST. SOCIETY','LADIES AUXILLARY',
             'UNITED WAY')

#Sports words
sport_wrds<-c('GYM', 'GOLF COURSE', 'ATHLETIC','FIELD HOUSE', 'ARENA', 'BASEBALL', 
              'ICE', 'ROLLER RINK', 'SPORTS', 'POOL', 
              'SWIMMING POOL')

#Military words
milit_wrds<-c('NATIONAL GUARD', 'MARINE CORP')

#Union words
union_wrds<-c('UNION', 'TEAMSTERS','UAW','U.E.','LOCAL')

#Other words
othr_wrds<-c('SENIOR', 'YOUTH','CHILD','OLDER ADULT','EARLY CHILDHOOD',
             'AGING','FOOD BANK')


##################################################
#set directories
cat_dir <- 'C:/Users/natha/Desktop/Polling Places/data'
poll_dir <- 'C:/Users/natha/Desktop/Polling Places/data/gov_poll_places'
plot_dir <- "C:/Users/natha/Desktop/Polling Places/plots"
# get poll location data and process
filenames<-c('poll_struct_gov2018.csv', 'poll_struct_gov2019.csv',
             'poll_struct_gov2020.csv', 'poll_struct_gov2021.csv',
             'poll_struct_gov2022.csv', 'poll_struct_gov2023.csv')
for(file in filenames){
  assign(paste0('poll_loc',substr(file,16,19)),
         # clean location names, removing some leading numbers (why need this again?)
         (rm_nums(
         # read in data
         (get_poll(cat_dir, file, num='HouseNum', direction='PrefixDirection',
                  street='Street', street_type = 'StreetType',city='City')),
         'Description')))
}

poll_dfs<-list('18'=poll_loc2018,'19'=poll_loc2019,'20'=poll_loc2020,'21'=poll_loc2021,
            '22'=poll_loc2022,'23'=poll_loc2023)

############### create keyword indices
for (i in 1:length(poll_dfs)){
##vector of school polling locations
schl_indx<-as.numeric(pollnamegrepl(schl_wrds, poll_dfs[[i]], 'Description'))

#vector of religious polling locations
relig_indx<-pollnamegrepl(relig_wrds, poll_dfs[[i]], 'Description')
#Abbreviations for saint and church
abbr_indx<-str_detect(poll_dfs[[i]]$Description, "(^ST[:space:])|(^ST\\.[:space:]*)|([:space:]CH$)")
relig_indx<-relig_indx|abbr_indx
relig_indx<-as.numeric(relig_indx)

# Vector of public centers
cent_indx<-as.numeric(pollnamegrepl(cent_wrds, poll_dfs[[i]], 'Description'))

# vector of fire stations
fire_indx<-as.numeric(pollnamegrepl(fire_wrds, poll_dfs[[i]], 'Description'))

# Vector of government buildings
gov_indx<-pollnamegrepl(gov_wrds, poll_dfs[[i]], 'Description')
#Abbreviation for borough
boro_indx<-str_detect(poll_dfs[[i]]$Description, "(^BORO[:space:])")
gov_indx<-gov_indx|boro_indx
gov_indx<-as.numeric(gov_indx)

# Combine vectors of government and fire
gov_fire_indx<-as.numeric(gov_indx|fire_indx)

#vector of club polling locations
club_indx<-as.numeric(grepl('CLUB', poll_dfs[[i]]$Description))

#Vector of apartments
apt_indx<-pollnamegrepl(apt_wrds, poll_dfs[[i]], 'Description')
#Abbreviation for apartment
ab_indx<-str_detect(poll_dfs[[i]]$Description, "(^APT[:space:])|([:space:]APTS$)")
apt_indx<-apt_indx|ab_indx
apt_indx<-as.numeric(apt_indx)

#vector of library polling locations
lib_indx<-as.numeric(grepl('LIBRARY', poll_dfs[[i]]$Description))

# vector of veterans associations
vet_indx<-as.numeric(pollnamegrepl(vet_wrds, poll_dfs[[i]], 'Description'))

# Vector of senior centers
sen_indx<-as.numeric(pollnamegrepl(sen_wrds, poll_dfs[[i]], 'Description'))

#vector of association polling locations
assc_indx<-as.numeric(pollnamegrepl(assc_wrds, poll_dfs[[i]], 'Description'))

#vector of sports polling locations
sport_indx<-as.numeric(pollnamegrepl(sport_wrds, poll_dfs[[i]], 'Description'))

#vector of military polling locations
milit_indx<-as.numeric(pollnamegrepl(milit_wrds, poll_dfs[[i]], 'Description'))

#vector of union polling locations
union_indx<-as.numeric(pollnamegrepl(union_wrds, poll_dfs[[i]], 'Description'))

#vector of other polling locations
othr_indx<-as.numeric(pollnamegrepl(othr_wrds, poll_dfs[[i]], 'Description'))

################### master index combining all location vectors
master_index<-as.data.frame(cbind(lib_indx, gov_fire_indx, cent_indx, relig_indx, 
                                  schl_indx, apt_indx,club_indx, 
                                  vet_indx, sen_indx,assc_indx,sport_indx,
                                  milit_indx,union_indx,othr_indx))
# create new category for locations with multiple keywords
master_index<-master_index%>%
  mutate(keyword_count = rowSums(across(c(lib_indx, gov_fire_indx, cent_indx, relig_indx, 
                                          schl_indx, apt_indx,club_indx, 
                                          vet_indx, sen_indx,assc_indx,sport_indx,
                                          milit_indx,union_indx,othr_indx)))) %>%
  mutate(mult_indx = case_when(keyword_count == 0 ~ 0, keyword_count == 1 ~ 0,
                               .default = 1))
## save copy of matrix
#setwd("C:/Users/natha/Desktop/Polling Places/data/Structures")
#write.csv(master_index, 'keyword_matrix.csv')

############## merge poll and keyword index
polltest<-cbind(poll_dfs[[i]], master_index)
#add in keyword codings conditionally
polltest<-polltest%>%
  mutate(location_category = case_when(mult_indx==1&is.na(location_category) ~ 'multiple',
                                       lib_indx==1&is.na(location_category) ~ 'library',
                                       gov_fire_indx==1&is.na(location_category) ~ 'public',
                                       cent_indx==1&is.na(location_category) ~ 'other',
                                       relig_indx==1&schl_indx==1&is.na(location_category) ~ 'religious_school',
                                       relig_indx==1&is.na(location_category) ~ 'religious',
                                       schl_indx==1&is.na(location_category) ~ 'school',
                                       apt_indx==1&is.na(location_category) ~ 'other',
                                       club_indx==1&is.na(location_category) ~ 'other',
                                       vet_indx==1&is.na(location_category) ~ 'other',
                                       sen_indx==1&is.na(location_category) ~ 'other',
                                       assc_indx==1&is.na(location_category) ~ 'other',
                                       sport_indx==1&is.na(location_category) ~ 'other',
                                       milit_indx==1&is.na(location_category) ~ 'other',
                                       union_indx==1&is.na(location_category) ~ 'other',
                                       othr_indx==1&is.na(location_category) ~ 'other',
                                       .default = location_category)) %>%
  #remove columns
  select(!c(mult_indx,lib_indx,gov_fire_indx,cent_indx,relig_indx,schl_indx,apt_indx,
            club_indx,vet_indx,sen_indx,assc_indx,sport_indx,
            milit_indx,union_indx,othr_indx))


#calc missingness by checking which addresses are in structure list
#sum(is.na(polltest$location_category))/9156 #9.4% missing

####### save to csv
#set directory
setwd('C:/Users/natha/Desktop/Polling Places/data')
write.csv(polltest, paste0('poll_struct_key_gov',names(poll_dfs[i]),'.csv'))
}

#plot categories
ggplot(data=polltest, aes(x=location_category))+
  geom_histogram(stat='count')









