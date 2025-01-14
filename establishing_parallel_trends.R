#Nathaniel Flemming
# 24/9/24
# 23/10/24

# Checking assumption for DiD, using data created by DiD_preprocessing script
#   (using extra years of voting data)
library(tidyverse) #convenience
library(data.table) #read in data selectively
library(stringr) #id string manipulation


########## Functions
### split data by treatment variable, calculate turnout for each group
calc_turnout_treated_control<-function(data,treatment_var){
  #create treatment and control data frames
  data<-select(data,all_of(c('LALVOTERID','General_2016_11_08','General_2017_11_07',
                             'General_2018_11_06','General_2019_11_05',
                             treatment_var)))
  pd_t<-data[data[treatment_var]==1,]
  pd_c<-data[data[treatment_var]==0,]
  # calculate turnout
  ##treatment
  pd_t<-pivot_longer(data=pd_t,cols=c('General_2016_11_08','General_2017_11_07',
                                      'General_2018_11_06','General_2019_11_05'),
                     names_to='election', values_to='voted')
  pd_t<-pd_t%>%
    group_by(election)%>%
    summarize(turnout=sum(voted)/n())%>%
    mutate(time_period=case_when(election=='General_2016_11_08'~2016,
                                 election=='General_2017_11_07'~2017,
                                 election=='General_2018_11_06'~2018,
                                 election=='General_2019_11_05'~2019))
  # pd_t$turnout[pd_t$time_period==-2]<-sum(pd_t$General_2016_11_08, na.rm = T)/nrow(pd_t)
  # pd_t$turnout[pd_t$time_period==-1]<-sum(pd_t$General_2017_11_07, na.rm = T)/nrow(pd_t)
  # pd_t$turnout[pd_t$time_period==0]<-sum(pd_t$General_2018_11_06, na.rm = T)/nrow(pd_t)
  # pd_t$turnout[pd_t$time_period==1]<-sum(pd_t$General_2019_11_05, na.rm = T)/nrow(pd_t)
  ##control
  pd_c<-pivot_longer(data=pd_c,cols=c('General_2016_11_08','General_2017_11_07',
                                      'General_2018_11_06','General_2019_11_05'),
                     names_to='election', values_to='voted')
  pd_c<-pd_c%>%
    group_by(election)%>%
    summarize(turnout=sum(voted)/n())%>%
    mutate(time_period=case_when(election=='General_2016_11_08'~2016,
                                 election=='General_2017_11_07'~2017,
                                 election=='General_2018_11_06'~2018,
                                 election=='General_2019_11_05'~2019))
  # pd_c$turnout<-NA
  # pd_c$turnout[pd_c$time_period==-2]<-sum(pd_c$General_2016_11_08, na.rm = T)/nrow(pd_c)
  # pd_c$turnout[pd_c$time_period==-1]<-sum(pd_c$General_2017_11_07, na.rm = T)/nrow(pd_c)
  # pd_c$turnout[pd_c$time_period==0]<-sum(pd_c$General_2018_11_06, na.rm = T)/nrow(pd_c)
  # pd_c$turnout[pd_c$time_period==1]<-sum(pd_c$General_2019_11_05, na.rm = T)/nrow(pd_c)
  # select variables
  #pd_t<-select(pd_t, all_of(c('time_period','turnout')))
  #pd_c<-select(pd_c, all_of(c('time_period','turnout')))
  # remove duplicates
  #pd_t<-distinct(pd_t)
  #pd_c<-distinct(pd_c)
  # remove missing data
  #pd_t<-pd_t[complete.cases(pd_t),]
  #pd_c<-pd_c[complete.cases(pd_c),]
  #mark and recombine groups
  pd_t[treatment_var]<-T
  pd_c[treatment_var]<-F
  pd<-rbind(pd_t,pd_c)
  # factor treatment variables for plotting?
  #pd[treatment_var]<-factor(pd[treatment_var])
  return(pd)
}


########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
# read in data
setwd(data_dir)
model_data<-read.csv('DiD_prepped_poll_vote_18to19.csv')
#model_data<-read.csv('DiD_prepped_loc_vote_16to18.csv')

## ID
model_data$VOTERID<-str_sub(model_data$LALVOTERID, 6, nchar(model_data$LALVOTERID))
model_data$VOTERID<-as.numeric(model_data$VOTERID)
# create time period variable where 2018 = 0
model_data$time_period<-model_data$year-2018
## treatment indicators
### create indicators for ever being treated
model_data<-model_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  mutate(
    ever_changed_poll_loc=(sum(changed_poll_loc)>0),
    ever_moved_new_poll_loc=(sum(moved_new_poll_loc)>0),
    ever_no_move_new_poll_loc=(sum(no_move_new_poll_loc)>0),
    ever_moved_old_poll_loc=(sum(moved_old_poll_loc)>0))%>% 
  #create single variable that indicates if someone voted in a given year
  group_by(year)%>%
  mutate(voted = ((year==2018 & General_2018_11_06==1)|(year==2019 & General_2019_11_05==1)))%>%
  ungroup()

###### Split location category into years
model_data$location_category_2018<-model_data$location_category
model_data$location_category_2018[model_data$year==2019]<-NA
model_data$location_category_2019<-model_data$location_category
model_data$location_category_2019[model_data$year==2018]<-NA

################ subset for change in poll category/location test
### people who changed polling location for whatever reason
ecp_model_data<-model_data%>%
  filter(ever_changed_poll_loc==T)
### people who changed polling location by moving and people who didn't change location
no_chng_or_mv_model_data<-model_data%>%
  filter(((ever_moved_new_poll_loc==T)|(ever_changed_poll_loc==F))&(ever_no_move_new_poll_loc==F))
### people who changed polling location by moving and people who didn't change location
no_chng_or_no_mv_model_data<-model_data%>%
  filter(((ever_no_move_new_poll_loc==T)|(ever_changed_poll_loc==F))&(ever_moved_new_poll_loc==F))
########################### subsets for general knowledge tests 
### people who changed polling location to a well known building 
ecp_wellknown_model_data<-model_data%>%
  # only people who have changed polling location w/o moving
  filter(ever_changed_poll_loc==T)%>%
  group_by(LALVOTERID)%>%
  mutate(
    # new poll location is well known
    new_poll_wellknown = sum((((location_category=='library')|
                                 (location_category=='public')|
                                 (location_category=='public_justice'))&
                                (year==2019)),na.rm=T)>0
  )%>%
  ungroup()
### people who changed polling location to a well known building w/o moving 
no_mv_wellknown_model_data<-model_data%>%
  # only people who have changed polling location w/o moving
  filter(ever_no_move_new_poll_loc==T)%>%
  group_by(LALVOTERID)%>%
  mutate(
    # new poll location is well known
    new_poll_wellknown = sum((((location_category=='library')|
                                 (location_category=='public')|
                                 (location_category=='public_justice'))&
                                (year==2019)),na.rm=T)>0
  )%>%
  ungroup()
### people who changed polling location to a well known building by moving 
mv_wellknown_model_data<-model_data%>%
  # only people who have changed polling location w/o moving
  filter(ever_moved_new_poll_loc==T)%>%
  group_by(LALVOTERID)%>%
  mutate(
    # new poll location is well known
    new_poll_wellknown = sum((((location_category=='library')|
                                 (location_category=='public')|
                                 (location_category=='public_justice'))&
                                (year==2019)),na.rm=T)>0
  )%>%
  ungroup()
### people who changed polling station and their new station is a religious building
ecp_relig_model_data<-model_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only people who changed polling location to a/another religious building
  filter(location_category_2019%in%c('religious',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is religious
    relig_new_poll_relig = sum(((known_religious==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station and their new station is a school
ecp_school_model_data<-model_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only people who changed polling location to a/another school
  filter(location_category_2019%in%c('school',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter household has children
    parent_new_poll_school = sum(((has_child==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station and their new station is a public building
ecp_public_model_data<-model_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only people who changed polling location to a/another public building
  filter(((location_category_2019%in%c('public',NA))|(location_category_2019%in%c('public_justice',NA))))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is a civil servant
    govemp_new_poll_public = sum(((CommercialData_OccupationIndustry=='Civil Servant')
                                  &(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### Parents who changed polling station
voters_parents_model_data<-model_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only parents
  filter(has_child==T)%>%
  group_by(LALVOTERID)%>%
  mutate(
    # Whether new polling location is a school
    parent_new_poll_school = sum(((location_category=='school')&(year==2019)),
                                 na.rm=T)>0
  )%>%
  ungroup()

### religious people who changed polling station
voters_relig_model_data<-model_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only religious people
  filter(known_religious==T)%>%
  group_by(LALVOTERID)%>%
  mutate(
    # Whether new polling location is religious
    relig_new_poll_relig = sum(((location_category=='religious')&(year==2019)),
                               na.rm=T)>0
  )%>%
  ungroup()

### civil servants who changed polling station
voters_govemp_model_data<-model_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only parents
  filter(CommercialData_OccupationIndustry=='Civil Servant')%>%
  group_by(LALVOTERID)%>%
  mutate(
    # Whether new polling location is a public building
    govemp_new_poll_public = sum(((location_category%in%c('public','public_justice'))
                                  &(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

## calculate turnout for treated and untreated groups
plot_data<-calc_turnout_treated_control(voters_govemp_two_data, 'govemp_new_poll_public')
## Plot Parallel trends
ggplot(plot_data,aes(x=time_period,y=turnout,colour=govemp_new_poll_public)) +
  geom_point(size=2)+
  geom_line(aes(group = govemp_new_poll_public), linetype = 2, linewidth=0.75)+
  geom_vline(xintercept=2018, alpha=0.25)+
  ylab("Turnout") +
  xlab("Year") +
  ggtitle("Parallel Trends Plot: \nPublic Employees Who Changed Polling Location") +
  scale_x_continuous(breaks=seq(2016,2019,1))+
  #convert turnout to percentage
  scale_y_continuous(labels = scales::percent)+
  scale_color_discrete(name="Changed Poll Location to a \nPublic Building",
                        breaks=c(F, T),
                        labels=c("No", "Yes"))+
  theme_minimal()+
  theme(plot.title = element_text(size=22),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15),
        legend.title = element_text(size=18),
        legend.text =element_text(size=15))



