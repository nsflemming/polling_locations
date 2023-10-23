# Nathaniel Flemming 27 9 23
## coding missing polling locations with keywords

library(dplyr)
library(stringr)

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
             ' SCH ',' COLLEGE ', 'COMMUNITY COLLEGE', 'COMMUNITY COLL', 
             'JR HIGH','JR. HIGH', 
             'SR. HIGH', 'SR HIGH', 'HIGH SCH', 'MIDDLE SCH')
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
             'MUSLIM')
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
             'HOSE')
fire_indx<-c()
#vector of fire dept polling locations
fire_indx<-as.numeric(pollnamegrepl(fire_wrds, fire_indx))


#government building words
gov_wrds<-c(' MUNI ',' MUNIC ', 'MUNICIPAL', ' BORO ','BOROUGH','TOWNSHIP','TWP',
            'CITY HALL','TOWN HALL', 'COUNCIL', 'MUN BLDG',
            'GOVERNMENT','COURT HOUSE', 'COURT', 'COURTHOUSE', 'ARMORY', 
            'ADMINSTRATION', 'ADMINISTRATIVE') #memorial halls?
gov_indx<-c()
#vector of government building polling locations
gov_indx<-pollnamegrepl(gov_wrds, gov_indx)
#Abbreviation for borough
boro_indx<-str_detect(poll$name, "(^BORO[:space:])")
gov_indx<-gov_indx|boro_indx
gov_indx<-as.numeric(gov_indx)

#club word
#vector of club polling locations
club_indx<-as.numeric(grepl('CLUB', poll$name))


#apartment words
apt_wrds<-c(' APT ', ' APTS ','APARTMENT','TOWERS','TOWER','PRIVATE RESIDENCE')
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
sen_wrds<-c('SENIOR CENTER', 'SENIOR CITIZEN\'S CENTER', 'SENIOR CITIZENS',
            'SENIOR CITIZEN', 'SENIOR CENTER', 'SR CENTER', 'SR CITZ', 'SR CITIZEN', 
            'RETIREMENT CENTER', 'RETIREMENT COMM', 'RET. COM.', 'RETIREMENT HOME',
            'BRITH SHOLOM')
sen_indx<-c()
#vector of senior center polling locations
sen_indx<-as.numeric(pollnamegrepl(sen_wrds, sen_indx))


################### master index combining all location vectors
master_index<-as.data.frame(cbind(lib_indx, gov_indx, cent_indx, relig_indx, 
                                  schl_indx, apt_indx,club_indx, fire_indx, 
                                  vet_indx, sen_indx))
# create new category for locations with multiple keywords
master_index<-master_index%>%
  mutate(hold = rowSums(across(c(lib_indx, gov_indx, cent_indx, relig_indx, 
                                       schl_indx, apt_indx,club_indx, fire_indx, 
                                       vet_indx, sen_indx)))) %>%
  mutate(mult_indx = case_when(hold == 0 ~ 0, hold == 1 ~ 0,
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
                                       gov_indx==1&is.na(location_category) ~ 'citytownhall',
                                       cent_indx==1&is.na(location_category) ~ 'reccenter',
                                       relig_indx==1&is.na(location_category) ~ 'religious',
                                       schl_indx==1&is.na(location_category) ~ 'school',
                                       apt_indx==1&is.na(location_category) ~ 'apartment',
                                       club_indx==1&is.na(location_category) ~ 'club',
                                       fire_indx==1&is.na(location_category) ~ 'firestation',
                                       vet_indx==1&is.na(location_category) ~ 'veteran',
                                       sen_indx==1&is.na(location_category) ~ 'senior',
                                       .default = location_category)) %>%
  #remove columns
  select(!c(mult_indx,lib_indx,gov_indx,cent_indx,relig_indx,schl_indx,apt_indx,
            club_indx,fire_indx,vet_indx,sen_indx))


#calc missingness by checking which addresses are in structure list
sum(is.na(polltest$location_category))
1186/9262 #12.8% missing
####### save to csv
#set directory
setwd('C:/Users/natha/Desktop/Polling Places/data')
write.csv(polltest, 'polllocation_structure_and_keyword.csv')

#plot categories
ggplot(data=polltest, aes(x=location_category))+
  geom_histogram(stat='count')









