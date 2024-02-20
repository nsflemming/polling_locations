# Nathaniel Flemming 27 9 23
## coding missing polling locations with keywords

library(dplyr)
library(stringr) #string manipulation
library(ggplot2) #plotting

#read in poll locations
setwd("C:/Users/natha/Desktop/Polling Places/data")
poll<-read.csv('polllocation_and_structure.csv')

############################## define function 
# Function to create vector of places that contain a category word in their name
pollnamegrepl<-function(words, index){
  index<-(grepl(words[1], poll$name))
  for(i in 2:length(words)){
    temp<-(grepl(words[i], poll$name))
    index<-index|temp
  }
  index
}

#################### pre-clean
#remove numbers from the beginning of location names
nums<-(str_detect(poll$name, "^[:digit:][:digit:]\\/[:digit:][:digit:]"))
poll$name[nums]<-substr(poll$name[nums],7,100)


###################### keywords
#school words
schl_wrds<-c('SCHOOL','ELEMENTARY','ELEM','ACADEMY', 'ACCADEMY', 'UNIVERSITY',
             'SCH ',' SCH ','ELE SCH ','COLLEGE',' COLLEGE ', 'COMMUNITY COLLEGE', 
             'COMMUNITY COLL', 
             'JR HIGH','JR. HIGH', 
             'SR. HIGH', 'SR HIGH', 'HIGH SCH', 'MIDDLE SCH',
             'EDUCATION','GRADUATE',
             'SHOOL','CHILD DEVELOPMENT','DAYCARE')
schl_indx<-c()
#vector of school polling locations
schl_indx<-as.numeric(pollnamegrepl(schl_wrds, schl_indx))


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
relig_indx<-c()
#vector of religious polling locations
relig_indx<-pollnamegrepl(relig_wrds, relig_indx)
#Abbreviations for saint and church
abbr_indx<-str_detect(poll$name, "(^ST[:space:])|(^ST\\.[:space:]*)|([:space:]CH$)")
relig_indx<-relig_indx|abbr_indx
relig_indx<-as.numeric(relig_indx)


#public center words
cent_wrds<-c('REC CENTER','REC CTR','REC CENT','RECREATION CENT','RECREATION CTR',
             'RECREATION BUILDING', 'RECREATION HALL',
             'COMMUNITY CENTER', 'COMM CENTER', 'COM CENTER', 'COM CTR', 'COMM CTR',
             'COMMUNITY CTR',
             'COMMUNITY BLDG', 'COMMUNITY BUILDING', 'COMMUNITY HALL',
             'CIVIC CENTER')
cent_indx<-c()
#vector of public center polling locations
cent_indx<-as.numeric(pollnamegrepl(cent_wrds, cent_indx))


#Fire dept words
fire_wrds<-c('FIRE','V.F.D.','VFD','V F D', 'V F C','VFC','V.F.C.','ENGINE HOUSE',
             'HOSE','ENGINE CO')
fire_indx<-c()
#vector of fire dept polling locations
fire_indx<-as.numeric(pollnamegrepl(fire_wrds, fire_indx))


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
gov_indx<-c()
#vector of government building polling locations
gov_indx<-pollnamegrepl(gov_wrds, gov_indx)
#Abbreviation for borough
boro_indx<-str_detect(poll$name, "(^BORO[:space:])")
gov_indx<-gov_indx|boro_indx
gov_indx<-as.numeric(gov_indx)

## Combine government and fire
gov_fire_indx<-as.numeric(gov_indx|fire_indx)

#club word
#vector of club polling locations
club_indx<-as.numeric(grepl('CLUB', poll$name))


#apartment words
apt_wrds<-c(' APT ', ' APTS ','APARTMENT','TOWERS','TOWER','PRIVATE RESIDENCE',
            'CONDO','HIGH RISE','HIGHRISE','HI-RISE')
apt_indx<-c()
#vector of apartment polling locations
apt_indx<-pollnamegrepl(apt_wrds, apt_indx)
#Abbreviation for apartment
ab_indx<-str_detect(poll$name, "(^APT[:space:])|([:space:]APTS$)")
apt_indx<-apt_indx|ab_indx
apt_indx<-as.numeric(apt_indx)

#library word
#vector of library polling locations
lib_indx<-as.numeric(grepl('LIBRARY', poll$name))


#Veterans association words
vet_wrds<-c('VFW', 'V F W', 'AMERICAN LEGION', 'AMER LEGION', 'VETERANS')
vet_indx<-c()
#vector of veterans association polling locations
vet_indx<-as.numeric(pollnamegrepl(vet_wrds, vet_indx))


#Senior center words
sen_wrds<-c('SENIOR CENTER', 'SENIOR CITIZEN[:punct:]S CENTER', 'SENIOR CITIZENS',
            'SENIOR CITIZEN', 'SENIOR CENTER', 'SR CENTER', 'SR CITZ', 'SR CITIZEN', 
            'RETIREMENT CENTER', 'RETIREMENT COMM', 'RET. COM.', 'RETIREMENT HOME',
            'BRITH SHOLOM')
sen_indx<-c()
#vector of senior center polling locations
sen_indx<-as.numeric(pollnamegrepl(sen_wrds, sen_indx))

#Association words
assc_wrds<-c('GRANGE', '4[:punct:]H', '4H','ORDER OF', 'ASSOC', 'D.A.R.', 
             'CHAMBER OF COMMERCE', 'ELKS LODGE', 'MOOSE LODGE', 'LION[:punct:]S', 
             'LIONS', 'FRATERNAL','COMMUNITY ASS','V.F.W.','SCOTTISH RITE',
             'HISTORICAL SOCIETY','HIST. SOCIETY','LADIES AUXILLARY',
             'UNITED WAY')
assc_indx<-c()
#vector of senior center polling locations
assc_indx<-as.numeric(pollnamegrepl(assc_wrds, assc_indx))

#Sports words
sport_wrds<-c('GYM', 'GOLF COURSE', 'ATHLETIC','FIELD HOUSE', 'ARENA', 'BASEBALL', 
             'ICE', 'ROLLER RINK', 'SPORTS', 'POOL', 
             'SWIMMING POOL')
sport_indx<-c()
#vector of senior center polling locations
sport_indx<-as.numeric(pollnamegrepl(sport_wrds, sport_indx))

#Military words
milit_wrds<-c('NATIONAL GUARD', 'MARINE CORP')
milit_indx<-c()
#vector of senior center polling locations
milit_indx<-as.numeric(pollnamegrepl(milit_wrds, milit_indx))

#Union words
union_wrds<-c('UNION', 'TEAMSTERS','UAW','U.E.','LOCAL')
union_indx<-c()
#vector of senior center polling locations
union_indx<-as.numeric(pollnamegrepl(union_wrds, union_indx))

#Other words
othr_wrds<-c('SENIOR', 'YOUTH','CHILD','OLDER ADULT','EARLY CHILDHOOD',
             'AGING','FOOD BANK')
othr_indx<-c()
#vector of senior center polling locations
othr_indx<-as.numeric(pollnamegrepl(othr_wrds, othr_indx))


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
setwd("C:/Users/natha/Desktop/Polling Places/data/Structures")
write.csv(master_index, 'keyword_matrix.csv')

############## merge poll and keyword index
polltest<-cbind(poll, master_index)
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
sum(is.na(polltest$location_category))/9156 #9.4% missing

####### save to csv
#set directory
setwd('C:/Users/natha/Desktop/Polling Places/data')
write.csv(polltest, 'polllocation_structure_and_keyword.csv')

#plot categories
ggplot(data=polltest, aes(x=location_category))+
  geom_histogram(stat='count')









