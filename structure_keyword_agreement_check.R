#Nathaniel Flemming
# 11/10/23

# check whether keyword codings agree with authoritative structure data
library(tidyverse)

#read in data
setwd("C:/Users/natha/Desktop/Polling Places/data/Structures")
gov_struct_matrix<-read.csv('structure_matrix.csv') #pure structure matrix
keyword_matrix <- read.csv('keyword_matrix.csv') #pure keyword matrix
setwd("C:/Users/natha/Desktop/Polling Places/data")
poll<-read.csv('polllocation_and_structure.csv') #poll + structure condensed

############### merge poll and keyword index so keyword matrix is reassociated with addresses
poll_coded<-cbind(poll, keyword_matrix)
#remove X columns and unneeded columns
poll_coded<-subset(poll_coded, select = -c(X, X.1, polling_place_id, source_notes))
#add in keyword codings conditionally
poll_coded<-poll_coded%>%
  mutate(keyword_category = case_when(mult_indx==1 ~ 'multiple',
                                       lib_indx==1 ~ 'library',
                                       gov_indx==1 ~ 'citytownhall',
                                       cent_indx==1 ~ 'reccenter',
                                       relig_indx==1 ~ 'religious',
                                       schl_indx==1 ~ 'school',
                                       apt_indx==1 ~ 'apartment',
                                       club_indx==1 ~ 'club',
                                       fire_indx==1 ~ 'firestation',
                                       vet_indx==1~ 'veteran',
                                       sen_indx==1 ~ 'senior',
                                       .default = location_category)) %>%
  #remove columns
  select(!c(mult_indx,lib_indx,gov_indx,cent_indx,relig_indx,schl_indx,apt_indx,
            club_indx,fire_indx,vet_indx,sen_indx))

############## Examine number of times location and keyword category match
## Tally matches
sum(poll_coded$location_category==poll_coded$keyword_category, na.rm=T)
#2935
#  Tally disagreements
sum(poll_coded$location_category!=poll_coded$keyword_category, na.rm=T)
#667

##### subset out disagreements
disagree <- poll_coded[poll_coded$location_category!=poll_coded$keyword_category,]
disagree <- disagree[complete.cases(disagree),]

##### 









