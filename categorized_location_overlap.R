#Nathaniel Flemming
#11/10/23

# examining the overlap between different categories

library(tidyverse)
library(scales) #plotting

#read in data
setwd("C:/Users/natha/Desktop/Polling Places/data/Structures")
gov_struct_matrix<-read.csv('structure_matrix.csv')
keyword_matrix <- read.csv('keyword_matrix.csv')

#############################################government structure overlap
# add location count column
gov_struct_matrix<-gov_struct_matrix%>%
  group_by(address, precinct_id)%>%
  mutate(location_count = sum(across(c(religious,school,firestation,
                                       policestation,postoffice,courthouse,
                                       rangerstation,citytownhall)))) %>%
  select(!c(source_notes,polling_place_id)) %>%
  ungroup()
# subset out locations with multiple categories
mult_cats<-gov_struct_matrix[gov_struct_matrix$location_count>1,]
## remove missing
mult_cats<-mult_cats[complete.cases(mult_cats),]
## school overlaps
# get list of IDs of all locations that have been classed as schools
school_id<-mult_cats[mult_cats$school==1,]
# use list to select rows 
school_overlap<-mult_cats[mult_cats$X %in% school_id$X,]
# remove schools rows
school_overlap<-school_overlap[school_overlap$school!=1,]
#make histogram of non-school categories
ggplot(data=school_overlap, aes(x=location_category))+
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title='Overlap with Schools (Structure Data)', y='Percentage', x='Location Category')+
  scale_y_continuous(labels=percent)
# get counts of overlaps
school_overlap_sum<-school_overlap%>%
  mutate(police_sum=sum(policestation),
         gov_sum=sum(citytownhall),
         post_sum=sum(postoffice),
         relig_sum=sum(religious),
         court_sum=sum(courthouse),
         rang_sum=sum(rangerstation),
         lib_sum=sum(library))%>%
  select(c(location_type,police_sum,gov_sum,post_sum,relig_sum,court_sum,
           rang_sum,lib_sum))%>%
  pivot_longer(cols=!location_type,
               names_to='structure',
               values_to = 'number')
school_overlap_sum<-unique(school_overlap_sum)
#remove zero count categories
school_overlap_sum<-school_overlap_sum[school_overlap_sum$number>0,]

############################################# Keyword structure overlap
#read in poll locations
setwd("C:/Users/natha/Desktop/Polling Places/data/Structures")
poll<-read.csv('polllocation_and_structure.csv')
############## merge poll and keyword index
polltest<-cbind(poll, keyword_matrix)
#remove empty columns
polltest<-subset(polltest, select=-c(source_notes,polling_place_id, library))
#remove structure location codings
polltest$location_category=NA
#add in keyword codings conditionally
polltest<-polltest%>%
  mutate(location_category = case_when(mult_indx==1 ~ 'multiple',
                                       lib_indx==1 ~ 'library',
                                       gov_indx==1 ~ 'citytownhall',
                                       cent_indx==1 ~ 'reccenter',
                                       relig_indx==1 ~ 'religious',
                                       schl_indx==1 ~ 'school',
                                       apt_indx==1 ~ 'apartment',
                                       club_indx==1 ~ 'club',
                                       fire_indx==1 ~ 'firestation',
                                       vet_indx==1 ~ 'veteran',
                                       sen_indx==1 ~ 'senior',
                                       .default = location_category))

# subset out locations with multiple categories that are schools
mult_cats2<-polltest[polltest$mult_indx==1 & polltest$schl_indx==1,]
## remove missing
mult_cats2<-mult_cats2[complete.cases(mult_cats2),]
## school overlaps counts
cats_sum<-mult_cats2%>%
  mutate(lib_sum=sum(lib_indx),
         gov_sum=sum(gov_indx),
         cent_sum=sum(cent_indx),
         relig_sum=sum(relig_indx),
         schl_sum=sum(schl_indx),
         apt_sum=sum(apt_indx),
         club_sum=sum(club_indx),
         fire_sum=sum(fire_indx),
         vet_sum=sum(vet_indx),
         sen_sum=sum(sen_indx))%>%
  select(c(location_category,lib_sum,gov_sum,cent_sum,relig_sum,apt_sum,club_sum,fire_sum,vet_sum,
           sen_sum))%>%
  pivot_longer(cols=!location_category,
               names_to='structure',
               values_to = 'number')
cats_sum<-unique(cats_sum)
#remove zero count categories
cats_sum<-cats_sum[cats_sum$number>0,]
#make histogram of non-school categories
ggplot(data=cats_sum, aes(x=structure))+
  geom_bar(aes(y = (number)/sum(number)), stat='identity') +
  labs(title='Overlap with Schools (Structure and Keyword)', y='Percentage', x='Location Category')+
  scale_y_continuous(labels=percent)






