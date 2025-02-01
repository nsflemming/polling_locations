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
    summarize(turnout=sum(voted, na.rm=T)/n())%>%
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
    summarize(turnout=sum(voted, na.rm=T)/n())%>%
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
#model_data<-read.csv('DiD_prepped_poll_vote_18to19.csv')
#model_data<-read.csv('DiD_prepped_loc_vote_16to18.csv')
model_data<-read.csv('DiD_prepped_poll_vote_16to19_no_rndm_race.csv')

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
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  group_by(LALVOTERID)%>%
  mutate(
    # new poll location is well known
    new_poll_wellknown = sum((((location_category=='library')|
                                 (location_category=='public')|
                                 (location_category=='public_justice')
                               |(location_category=='school')
                               #|#(location_category=='relig_school')
    )&
      (year==2019)),na.rm=T)>0
  )%>%
  ungroup()
########################### subsets for specific knowledge tests 
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


### catholic people who changed polling station
voters_cath_model_data<-model_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only religious people
  filter(known_catholic==T)%>%
  group_by(LALVOTERID)%>%
  mutate(
    # Whether new polling location is catholic
    cath_new_poll_cath = sum((((location_category=='catholic_church')|
                                 (location_category=='catholic_school'))&
                                (year==2019)),
                             na.rm=T)>0
  )%>%
  ungroup()

############ Convert treatment indicators to numeric
ever_chng_loc <- c("ever_changed_poll_loc","ever_moved_new_poll_loc",
                   'ever_no_move_new_poll_loc','ever_moved_old_poll_loc')
chng_to<-c('changed_to_school','changed_to_cath','changed_to_wellknown',
           'parent_changed_to_school','cath_changed_to_cath') 
new_poll<-c('new_poll_school','parent_new_poll_school','new_poll_cath','cath_new_poll_cath',
            'new_poll_wellknown')
reason<-c('ever_moved_new_poll_loc','ever_no_move_new_poll_loc')

ecp_model_data['ever_changed_poll_loc'] <- sapply(ecp_model_data['ever_changed_poll_loc'],as.numeric)
no_chng_or_mv_model_data['ever_moved_new_poll_loc']<- 
  sapply(no_chng_or_mv_model_data['ever_moved_new_poll_loc'],as.numeric)
no_chng_or_no_mv_model_data['ever_no_move_new_poll_loc']<- 
  sapply(no_chng_or_no_mv_model_data['ever_no_move_new_poll_loc'],as.numeric)

ecp_wellknown_model_data['new_poll_wellknown']<- sapply(ecp_wellknown_model_data['new_poll_wellknown'],as.numeric)
ecp_wellknown_model_data$voted<-as.numeric(ecp_wellknown_model_data$voted)

voters_parents_model_data['parent_new_poll_school'] <- sapply(voters_parents_model_data['parent_new_poll_school'],as.numeric)
voters_parents_model_data$voted<-as.numeric(voters_parents_model_data$voted)

voters_cath_model_data['cath_new_poll_cath'] <- sapply(voters_cath_model_data['cath_new_poll_cath'],as.numeric)
voters_cath_model_data$voted<-as.numeric(voters_cath_model_data$voted)


##################### Plot trends
## calculate turnout for treated and untreated groups
plot_data<-calc_turnout_treated_control(no_chng_or_no_mv_model_data, 'ever_no_move_new_poll_loc')
#plot_data_2pre<-plot_data%>%
#  filter(time_period>2016)
## Plot Parallel trends
ggplot(plot_data,aes(x=time_period,y=turnout,colour=ever_no_move_new_poll_loc)) +
  geom_point(size=2)+
  geom_line(aes(group = ever_no_move_new_poll_loc), linetype = 2, linewidth=0.75)+
  geom_vline(xintercept=2018, alpha=0.25)+
  ylab("Turnout") +
  xlab("Year") +
  ggtitle("Parallel Trends Plot: \nVoters Who Changed Polling Location Without Moving \nvs. Voters Who Didn't Change") +
  scale_x_continuous(breaks=seq(2016,2019,1))+
  #convert turnout to percentage
  scale_y_continuous(labels = scales::percent)+
  scale_color_discrete(name="Changed Poll Location without Moving",
                        breaks=c(F, T),
                        labels=c("No", "Yes"))+
  theme_minimal()+
  theme(plot.title = element_text(size=22),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15),
        legend.title = element_text(size=18),
        legend.text =element_text(size=15))
#PT_everchange_16_19
#Parallel Trends Plot: \nVoters Who Changed Polling Location for Any Reason \nvs. Voters Who Didn't Change
#Changed Poll Location 
#PT_movedchng_16_19
#Parallel Trends Plot: \nVoters Who Changed Polling Location by Moving \nvs. Voters Who Didn't Change
#Changed Poll Location by Moving
#PT_everchange_16_19
#Parallel Trends Plot: \nVoters Who Changed Polling Location Without Moving \nvs. Voters Who Didn't Change
#Changed Poll Location without Moving
#PT_wellknown_16_19
#"Parallel Trends Plot: \nRegistered Voters Who Changed Polling Location"
#"Changed Poll Location to a \nWell-known Building"
#PT_school_16_19
#Parallel Trends Plot: \nParents Who Changed Polling Location to a School \nvs. a Non-School
#Changed Poll Location to a \nSchool
#PT_Catholic_16_19
#Parallel Trends Plot: \nCatholics Who Changed Polling Location to a \n Catholic Church vs. Another Buildingl
#Changed Poll Location to a \nCatholic Church


######################### Pre-testing
library(did)

#### Event study
#reformat data for multi periods
gvar<-'new_poll_wellknown'
mini<-ecp_wellknown_model_data%>%
  select(c('General_2016_11_08','General_2017_11_07','General_2018_11_06',
           'General_2019_11_05',
           'voted',gvar,'LALVOTERID','VOTERID','year'
           #covariates
           ,'Voters_Gender'
           ,'Voters_Age','Parties_Description','pred_race',
           'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
           'known_religious','CommercialData_LikelyUnion','CommercialData_OccupationGroup',
           'years_reg'))%>%
  mutate(new_poll_wellknown=ifelse(new_poll_wellknown==1,2019,0))%>%
  rowwise() %>%
  slice(rep(1, 2)) %>%
  group_by(LALVOTERID) %>%
  mutate(year = seq(2016, by = 1, length.out = n()),
         voted = ((year==2016 & General_2016_11_08==1)|(year==2017 & General_2017_11_07==1)|
                    (year==2018 & General_2018_11_06==1)|(year==2019 & General_2019_11_05==1)))

did.att.gt <- att_gt(yname = "voted",
                     tname = "year",
                     idname = "VOTERID",
                     gname = gvar,
                     xformla = ~Voters_Gender+Voters_Age+Parties_Description+pred_race+
                       CommercialData_EstimatedHHIncomeAmount+Residence_Families_HHCount+
                       known_religious+CommercialData_LikelyUnion+CommercialData_OccupationGroup+
                       years_reg,
                     data = mini
)
summary(did.att.gt)
# plot them
ggdid(did.att.gt)

###### Conditional parallel trends (didn't wait for it to finish)
cdp <- conditional_did_pretest(yname = "voted",
                               tname = "year",
                               idname = "VOTERID",
                               gname = gvar,
                               xformla = ~Voters_Gender+Voters_Age+Parties_Description+pred_race+
                                 CommercialData_EstimatedHHIncomeAmount+Residence_Families_HHCount+
                                 known_religious+CommercialData_LikelyUnion+CommercialData_OccupationGroup+
                                 years_reg, 
                               data = mini)
