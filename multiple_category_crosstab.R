#Nathaniel Flemming
# 11/24/23

# check whether keyword codings agree with authoritative structure data
library(tidyverse)
library(purrr) # for summing rows

#################### read in data
setwd("C:/Users/natha/Desktop/Polling Places/data/Structures")
gov_struct_matrix<-read.csv('structure_matrix.csv') #pure structure matrix
keyword_matrix <- read.csv('keyword_matrix.csv') #pure keyword matrix
setwd("C:/Users/natha/Desktop/Polling Places/data")
poll<-read.csv('polllocation_and_structure.csv') #poll + structure condensed

################
#location category count instead of location category to see what's multiple coded
gov_struct_matrix<-gov_struct_matrix%>%
  group_by(address, precinct_id)%>%
  mutate(location_count = sum(across(c(religious,school,public,
                                       justice,postoffice,
                                       rangerstation,allegh_pub,library)))) %>%
  ungroup()%>%
  select(!c(polling_place_id, source_notes))
#change locations with two categories to 'multiple' to check missingness
gov_struct_matrix$location_category[gov_struct_matrix$location_count>1]<-'multiple'
# subset out multiple category locations
gov_struct_mult = gov_struct_matrix[gov_struct_matrix$location_count>1,]
## remove missing
gov_struct_mult<-gov_struct_mult[complete.cases(gov_struct_mult),]
## distribution of multiple location count variable
summary(gov_struct_mult$location_count)
ggplot(data=gov_struct_mult, aes(x=location_count))+
  geom_histogram(bins=4)
sum(gov_struct_mult$location_count==5)/5 # 1 5-category locations
sum(gov_struct_mult$location_count==4)/4 # 11 4-category locations
sum(gov_struct_mult$location_count==3)/3 # 56 3-category locations
sum(gov_struct_mult$location_count==2)/2 # 344 2-category locations

##########  two category locations crosstab
two_category<-gov_struct_mult[gov_struct_mult$location_count==2,]
## create two category columns for each row
### replace multiple coding with row coding
two_category<-two_category%>%
  #group_by(address, precinct_id)%>%
  mutate(category_1 = case_when(religious==1 ~ 'religious',
                                school==1 ~ 'school',
                                public==1 ~ 'public',
                                allegh_pub==1 ~ 'allegh_pub',
                                postoffice==1 ~ 'postoffice',
                                justice==1 ~ 'justice',
                                rangerstation==1 ~ 'rangerstation',
                                library==1 ~ 'library',
                                .default = location_category))


output<-data.frame()
for (x in seq(from=1, to=nrow(two_category)-1, by=2)){
  y=x+1 #don't know why this is needed
  temp <- two_category[x:y,]
  temp$category_2 <- temp$category_1[2]
  output<-rbind(output,(temp[1,]))
}

################ Create crosstab
two_cat_crosstab<-table(output$category_1, output$category_2)
two_cat_crosstab

##########  three category locations crosstab
three_category<-gov_struct_mult[gov_struct_mult$location_count==3,]
## create two category columns for each row
### replace multiple coding with row coding
three_category<-three_category%>%
  #group_by(address, precinct_id)%>%
  mutate(category_1 = case_when(religious==1 ~ 'religious',
                                school==1 ~ 'school',
                                public==1 ~ 'public',
                                allegh_pub==1 ~ 'allegh_pub',
                                postoffice==1 ~ 'postoffice',
                                justice==1 ~ 'justice',
                                rangerstation==1 ~ 'rangerstation',
                                library==1 ~ 'library',
                                .default = location_category))


output_3cat<-data.frame()
for (x in seq(from=1, to=nrow(three_category)-1, by=3)){
  y=x+2 #don't know why this is needed
  temp <- three_category[x:y,]
  temp$category_2 <- temp$category_1[2]
  temp$category_3 <- temp$category_1[3]
  output_3cat<-rbind(output_3cat,(temp[1,]))
}

################ Create crosstab
mytable <- table(output_3cat$category_1, output_3cat$category_2,
                            output_3cat$category_3)
three_cat_crosstab<-ftable(mytable)
three_cat_crosstab


