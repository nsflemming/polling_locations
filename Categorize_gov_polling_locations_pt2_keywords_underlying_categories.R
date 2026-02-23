# Nathaniel Flemming 21 3 24
## coding missing polling locations with keywords
#### Full breakdown into all categories, rather than lumping some into other


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
            'GOVERNMENT',
            'ARMORY', 'ADMINSTRATION', 'ADMINISTRATIVE', 'AUTHORITY',
            'PARAMEDIC', 'AMBULANCE','EMS STATION',
            'REGIONAL','COMMISSION',
            'CONVENTION CENTER','MUSEUM','AIRPORT','TRAIN STATION','TERMINAL')
# Courthouses
crt_wrds<-c('COURT HOUSE', 'COURT', 'COURTHOUSE', 'DISTRICT JUDGE')

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
cat_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data'
poll_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data/gov_poll_places'
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
struct_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data/Structures'
# get poll location data and process
filenames<-c(#'poll_struct_govsource_underlying2016.csv', 
  'poll_struct_govsource_underlying2017.csv'
             ,'poll_struct_govsource_underlying2018.csv', 
  'poll_struct_govsource_underlying2019.csv'
             #,'poll_struct_govsource_underlying2020.csv'#, 'poll_struct_govsource2021.csv',
             #'poll_struct_govsource2022.csv', 'poll_struct_govsource2023.csv'
             )
for(file in filenames){
  assign(paste0('poll_loc',substr(file,33,36)),
         # clean location names, removing some leading numbers (why need this again?)
         (rm_nums(
         # read in data
         (get_poll(cat_dir, file, num='HouseNum', direction='PrefixDirection',
                  street='Street', street_type = 'StreetType',city='City')),
         'Description')))
}

poll_dfs<-list(#'16'=poll_loc2016,
  '17'=poll_loc2017
               ,'18'=poll_loc2018,
               '19'=poll_loc2019
               #,'20'=poll_loc2020#,'21'=poll_loc2021,
            #'22'=poll_loc2022,'23'=poll_loc2023
            )

############### create keyword indices
for (i in 1:length(poll_dfs)){
  ##vector of school polling locations
  school<-as.numeric(pollnamegrepl(schl_wrds, poll_dfs[[i]], 'Description'))
  
  #vector of religious polling locations
  religious<-pollnamegrepl(relig_wrds, poll_dfs[[i]], 'Description')
  #Abbreviations for saint and church
  abbr_indx<-str_detect(poll_dfs[[i]]$Description, "(^ST[:space:])|(^ST\\.[:space:]*)|([:space:]CH$)")
  religious<-religious|abbr_indx
  religious<-as.numeric(religious)
  
  # Vector of public centers
  pubcen<-as.numeric(pollnamegrepl(cent_wrds, poll_dfs[[i]], 'Description'))
  
  # vector of fire stations
  firestation<-as.numeric(pollnamegrepl(fire_wrds, poll_dfs[[i]], 'Description'))
  
  # Vector of government buildings
  government<-pollnamegrepl(gov_wrds, poll_dfs[[i]], 'Description')
  #Abbreviation for borough
  boro_indx<-str_detect(poll_dfs[[i]]$Description, "(^BORO[:space:])")
  government<-government|boro_indx
  government<-as.numeric(government)
  
  #Vector of courthouses
  courthouse<-pollnamegrepl(crt_wrds, poll_dfs[[i]], 'Description')
  
  #vector of club polling locations
  club<-as.numeric(grepl('CLUB', poll_dfs[[i]]$Description))
  
  #Vector of apartments
  apt<-pollnamegrepl(apt_wrds, poll_dfs[[i]], 'Description')
  #Abbreviation for apartment
  ab_indx<-str_detect(poll_dfs[[i]]$Description, "(^APT[:space:])|([:space:]APTS$)")
  apt<-apt|ab_indx
  apt<-as.numeric(apt)
  
  #vector of library polling locations
  library<-as.numeric(grepl('LIBRARY', poll_dfs[[i]]$Description))
  
  # vector of veterans associations
  vet<-as.numeric(pollnamegrepl(vet_wrds, poll_dfs[[i]], 'Description'))
  
  # Vector of senior centers
  sencent<-as.numeric(pollnamegrepl(sen_wrds, poll_dfs[[i]], 'Description'))
  
  #vector of association polling locations
  assc<-as.numeric(pollnamegrepl(assc_wrds, poll_dfs[[i]], 'Description'))
  
  #vector of sports polling locations
  sport<-as.numeric(pollnamegrepl(sport_wrds, poll_dfs[[i]], 'Description'))
  
  #vector of military polling locations
  milit<-as.numeric(pollnamegrepl(milit_wrds, poll_dfs[[i]], 'Description'))
  
  #vector of union polling locations
  union<-as.numeric(pollnamegrepl(union_wrds, poll_dfs[[i]], 'Description'))
  
  #vector of other polling locations
  other<-as.numeric(pollnamegrepl(othr_wrds, poll_dfs[[i]], 'Description'))
  
  ################### master index combining all location vectors
  keyword_match_index<-as.data.frame(cbind(school,religious,pubcen,firestation, 
                                           government,courthouse,club,apt,library, 
                                           vet,sencent,assc,sport,milit,union,other))
  # create new category for locations with multiple keywords
  keyword_match_index<-keyword_match_index%>%
    mutate(keyword_count = rowSums(across(c(school,religious,pubcen,firestation, 
                                            government,courthouse,club,apt,library, 
                                            vet,sencent,assc,sport,milit,union,other)))) %>%
    mutate(mult_indx = case_when(keyword_count == 0 ~ 0, keyword_count == 1 ~ 0,
                                 .default = 1))
  ## save copy of matrix
  print('Not saving matrix')
  #setwd("C:/Users/natha/Desktop/Polling Places DiD/data/Structures")
  #write.csv(keyword_match_index, paste0('keyword_matrix_underlying',names(poll_dfs[i]),'.csv'))
}

############## merge structure coded and keyword index
keyword_cols<-c('school','religious','catholic_school','religious_school',
                'public_center','firestation','rangerstation','postoffice','policestation',
                'government','courthouse','public_justice','club','apartment','library',
                'veteran','senior_center','association','sport','military','union','other')
cols_to_drop<-c('mult_indx',keyword_cols,
                'pubcen','apt','vet','sencent','assc','milit',
                'keyword_count', 'location_count','X')

for (i in 1:length(poll_dfs)){
  keyword_match_index<-read.csv(paste0(struct_dir,'/keyword_matrix_underlying',
                                       names(poll_dfs[i]),'.csv'))
  #keyword_match_index$X<-seq(1:nrow(keyword_match_index))
  
  ### Join keyword index to structure data where structure data is missing

  #### Trim keyword index to just rows where structure data is missing
  keyword_match_index<-keyword_match_index%>%
    filter(X%in%poll_dfs[[i]][,'X'][is.na(poll_dfs[[i]]$location_category)])
    ## set indicator for category to 1 if structure data or keyword match a category
    ### Only need x and y for categories present in both structure and keyword data
  struct_and_keyword_index<-left_join(poll_dfs[[i]], keyword_match_index, by='X')%>%
    rowwise()%>%
    mutate(school = as.numeric(sum(school.x,school.y, na.rm=T)>0),
           religious = as.numeric(sum(religious.x,religious.y, na.rm=T)>0),
           public_center = as.numeric(sum(pubcen, na.rm=T)>0),
           firestation = as.numeric(sum(firestation.x,firestation.y, na.rm=T)>0),
           government = as.numeric(sum(government.x,government.y, na.rm=T)>0),
           courthouse = as.numeric(sum(courthouse.x,courthouse.y, na.rm=T)>0),
           club = as.numeric(sum(club, na.rm=T)>0),
           apartment = as.numeric(sum(apt, na.rm=T)>0),
           library = as.numeric(sum(library.x,library.y, na.rm=T)>0),
           veteran = as.numeric(sum(vet, na.rm=T)>0),
           senior_center = as.numeric(sum(sencent, na.rm=T)>0),
           association = as.numeric(sum(assc, na.rm=T)>0),
           sport = as.numeric(sum(sport, na.rm=T)>0),
           military = as.numeric(sum(milit, na.rm=T)>0),
           union = as.numeric(sum(union, na.rm=T)>0),
           other = as.numeric(sum(other, na.rm=T)>0))%>%
    # Remove uncombined columns
    select(!any_of(c('school.x','school.y','religious.x','religious.y',
              'firestation.x','firestation.y','government.x','government.y',
              'courthouse.x','courthouse.y','library.x','library.y')))%>%
    ungroup()
  #replace NAs in location category columns with 0s
  struct_and_keyword_index[keyword_cols][is.na(struct_and_keyword_index[keyword_cols])] <- 0
  # Add in keyword codings conditionally
  ### Replace 1's with the column name
  struct_and_keyword_index <- struct_and_keyword_index %>%
    mutate(across(all_of(keyword_cols),
                  ~ ifelse(. == 1, cur_column(), NA)))
  ### concatenate categories if a location has multiple categories
  struct_and_keyword_index <- struct_and_keyword_index%>%
    rowwise() %>%
    mutate(
      combined = paste(
        keyword_cols[!is.na(c_across(all_of(keyword_cols)))],
        collapse = "/"
      )
    ) %>%
    ungroup()%>%
    mutate(
      location_category = coalesce(location_category, combined)
    )%>%
  # struct_and_keyword_index<-struct_and_keyword_index%>%
  #   mutate(location_category = case_when(# Locations with multiple matches
  #                                         ## Assign 'multiple' first to avoid 
  #                                         ##  individual categories overwriting 
  #                                         ##  each other
  #                                        mult_indx==1&is.na(location_category) ~ 'multiple',
  #                                        # Locations with single matches
  #                                        school==1&is.na(location_category) ~ 'school',
  #                                        religious==1&is.na(location_category) ~ 'religious',
  #                                        catholic_school==1&is.na(location_category) ~ 'catholic school',
  #                                        pubcen==1&is.na(location_category) ~ 'public center',
  #                                        firestation==1&is.na(location_category) ~ 'fire station',
  #                                        rangerstation==1&is.na(location_category) ~ 'ranger station',
  #                                        postoffice==1&is.na(location_category) ~ 'post office',
  #                                        policestation==1&is.na(location_category) ~ 'police station',
  #                                        government==1&is.na(location_category) ~ 'government',
  #                                        courthouse==1&is.na(location_category) ~ 'courthouse',
  #                                        club==1&is.na(location_category) ~ 'club',
  #                                        apt==1&is.na(location_category) ~ 'apartment',
  #                                        library==1&is.na(location_category) ~ 'library',
  #                                        vet==1&is.na(location_category) ~ 'veteran',
  #                                        sencent==1&is.na(location_category) ~ 'senior center',
  #                                        assc==1&is.na(location_category) ~ 'association',
  #                                        sport==1&is.na(location_category) ~ 'sport',
  #                                        milit==1&is.na(location_category) ~ 'military',
  #                                        union==1&is.na(location_category) ~ 'union',
  #                                        other==1&is.na(location_category) ~ 'other',
  #                                        # Valid/identified multiple category locations
  #                                        religious==1&school==1 ~ 'religious school',
  #                                        government==1&policestation==1 ~ 'government/police',
  #                                        .default = location_category)) %>%
    #remove columns
    select(!any_of(c(cols_to_drop,'combined')))%>%
  #remove duplicates created by having multiple matching categories
    distinct()%>%
  #Change blank to missing
  mutate(location_category = ifelse(location_category=="",NA,location_category)
  )
  #calc missingness
  #print(paste0('Missingness w/ keywords added in: ',sum(is.na(struct_and_keyword_index$location_category))/nrow(struct_and_keyword_index)))
  #print('Not Saving')
  ####### save to csv
  #set directory
  setwd('C:/Users/natha/Desktop/Polling Places DiD/data')
  write.csv(struct_and_keyword_index, paste0('poll_struct_key_govsource_underlying',names(poll_dfs[i]),'.csv'))
}
