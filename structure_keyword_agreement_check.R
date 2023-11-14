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
                                       gov_fire_indx==1 ~ 'public',
                                       cent_indx==1 ~ 'reccenter',
                                       relig_indx==1 ~ 'religious',
                                       schl_indx==1 ~ 'school',
                                       apt_indx==1 ~ 'apartment',
                                       club_indx==1 ~ 'club',
                                       vet_indx==1 ~ 'veteran',
                                       sen_indx==1 ~ 'senior',
                                       assc_indx==1 ~ 'association',
                                       sport_indx==1 ~ 'sport',
                                       milit_indx==1 ~ 'military',
                                       union_indx==1 ~ 'union',
                                       othr_indx==1 ~ 'other',
                                       .default = NA)) %>%
  #remove columns
  select(!c(mult_indx,lib_indx,gov_fire_indx,cent_indx,relig_indx,schl_indx,apt_indx,
            club_indx,vet_indx,sen_indx,assc_indx,sport_indx,
            milit_indx,union_indx,othr_indx, X))

############## Examine number of times location and keyword category match
## Tally matches
sum(poll_coded$location_category==poll_coded$keyword_category, na.rm=T)
#2760
#  Tally disagreements
sum(poll_coded$location_category!=poll_coded$keyword_category, na.rm=T)
#729

##### subset out disagreements
disagree <- poll_coded[poll_coded$location_category!=poll_coded$keyword_category,]
disagree <- disagree[complete.cases(disagree),]

##### merge two codings and create histogram?
disagree$compound = paste0(disagree$location_category, ' & ', disagree$keyword_category)
temp <- data.frame(unique(disagree$compound))

## make order agnostic?
disagree<-disagree%>%
  mutate(compound = case_when(
    compound == "multiple & public" ~ "public & multiple",
    compound == "religious & multiple" ~ "multiple & religious",
    compound == "multiple & library" ~ "library & multiple",
    compound == "school & multiple" ~ "multiple & school",
    compound == "public & library" ~ "library & public",
    compound == "school & public" ~ "public & school",
    .default = compound
  ))
#plot compound categories
ggplot(data=disagree, aes(x=compound))+
  geom_histogram(stat='count')+
  scale_x_discrete(guide = guide_axis(angle = 45))
#counts for categories
summary(as.factor(disagree$compound))









