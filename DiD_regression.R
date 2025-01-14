#Nathaniel Flemming
# 24/9/24

# Difference in difference regressions, using data created by DiD_preprocessing script

library(did) #difference in difference package, 3+ periods
library(DRDID) #difference in difference package, 2 periods
library(tidyverse) #convenience
library(data.table) #read in data selectively
library(stringr) #id string manipulation

#### Example code
# Load data in long format that comes in the DRDID package
# data(nsw_long)
# # Form the Lalonde sample with CPS comparison group
# eval_lalonde_cps <- subset(nsw_long, nsw_long$treated == 0 | nsw_long$sample == 2)
# drdidex <- drdid(yname = "re",
#              tname = "year", 
#              idname = "id", 
#              dname = "experimental", 
#              xformla= ~ age + educ + black + married + nodegree + hisp + re74, 
#              data = eval_lalonde_cps, 
#              panel = TRUE) 
# summary(drdidex)
########## Functions
###

### plot did result
plot_did<-function(data, did_model, dvar, plot_title, group1='Control', group2='Parallel Trend',
                   group3='Treated'){
  ## set up data frame
  plot_data <- data.frame(matrix(ncol = 5, nrow = 6))
  x <- c("group","year", "turnout","linetype",'se')
  colnames(plot_data) <- x
  plot_data$group<-rep(c('control','simulated','treated'),2)
  plot_data$year<-rep(c(2018,2019),3)
  plot_data$linetype<-rep(c('a','b','a'),2)
  plot_data$se<-NA
  ## add in turnout values
  ###control
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='control')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==F)])
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='control')]<-
    mean(data$voted[(data$year==2019) & (data[dvar]==F)])
  ###treated
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='treated')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==T)])
  ###simulated
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='simulated')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==T)])
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='simulated')]<-
    ##2019  control value
    (plot_data$turnout[(plot_data$year==2019)&(plot_data$group=='control')]-
        ##diff between 2018 treatment and control
        (plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='control')]-
        plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='treated')]))
  ###treated
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='treated')]<-
    plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='simulated')]+did_model$ATT
  ###standard error
  plot_data$se[(plot_data$year==2019) & (plot_data$group=='treated')]<-did_model$se
  ## plot
  ggplot(data=plot_data)+
    geom_point(aes(x=year, y=turnout, color=group),size=2.5)+
    geom_line(aes(x=year, y=turnout, color=group, linetype = linetype),linewidth=1.5)+
    geom_errorbar(aes(x=year, ymin=turnout-se, ymax=turnout+se, color=group), 
                  width=0.075)+
    scale_x_continuous(breaks=seq(2018, 2019, 1))+
    #convert turnout to percentage
    scale_y_continuous(labels = scales::percent)+
    theme_minimal()+
    theme(plot.title = element_text(size=22),
          axis.title = element_text(size=20),
          axis.text = element_text(size=15),
          legend.title = element_text(size=18),
          legend.text =element_text(size=15))+
    #change text
    labs(title=plot_title,
         colour = "Group",
         x='Year', y='Probability of Voting')+
    scale_color_hue(labels = c(group1, group2, group3))+
    #remove linetype legend
    guides(linetype='none')
}

### blank plot of did result
blank_plot_did<-function(data, did_model, dvar, plot_title, group1='Control', group2='Parallel Trend',
                   group3='Treated'){
  ## set up data frame
  plot_data <- data.frame(matrix(ncol = 5, nrow = 6))
  x <- c("group","year", "turnout","linetype",'se')
  colnames(plot_data) <- x
  plot_data$group<-rep(c('control','simulated','treated'),2)
  plot_data$year<-rep(c(2018,2019),3)
  plot_data$linetype<-rep(c('a','b','a'),2)
  plot_data$se<-NA
  ## add in turnout values
  ###control
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='control')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==F)])
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='control')]<-
    mean(data$voted[(data$year==2019) & (data[dvar]==F)])
  ###treated
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='treated')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==T)])
  ###simulated
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='simulated')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==T)])
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='simulated')]<-
    ##2019  control value
    (plot_data$turnout[(plot_data$year==2019)&(plot_data$group=='control')]-
       ##diff between 2018 treatment and control
       plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='control')]-
             plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='treated')])
  ###treated
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='treated')]<-
    plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='simulated')]+did_model$ATT
  ###standard error
  plot_data$se[(plot_data$year==2019) & (plot_data$group=='treated')]<-did_model$se
  ## plot
  ggplot(data=plot_data, aes(x=year, y=turnout, color=group))+
    scale_x_continuous(breaks=seq(2018, 2019, 1))+
    #convert turnout to percentage
    scale_y_continuous(labels = scales::percent)+
    theme_minimal()+
    theme(plot.title = element_text(size=22),
          axis.title = element_text(size=20),
          axis.text = element_text(size=15),
          legend.title = element_text(size=18),
          legend.text =element_text(size=15))+
    #change text
    labs(title=plot_title,
         colour = "Group",
         x='Year', y='Probability of Voting')+
    scale_color_hue(labels = c(group1, group2, group3))+
    #remove linetype legend
    guides(linetype='none')
}


########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
# read in two-time period data
setwd(data_dir)
two_data<-read.csv('DiD_prepped_poll_vote_18to19.csv')

# create a time period version of year
two_data$time_period<-two_data$year-2018

### create indicators for ever being treated
#two_data<-two_data%>%
two_data<-two_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  mutate(#ever_changed_precinct=sum(changed_prec), #300,838 cases (2%)
         #ever_no_move_new_precinct=sum(no_move_new_precinct), #367,478 cases (3%)
         #ever_moved_new_precinct=sum(moved_new_precinct),#66,640 cases (0.5%)
         ever_changed_poll_cat=(sum(changed_poll_cat)>0),
         ever_changed_poll_loc=(sum(changed_poll_loc)>0),
         ever_moved_new_poll_cat=(sum(moved_new_poll_cat)>0),
         ever_no_move_new_poll_cat=(sum(no_move_new_poll_cat)>0),
         ever_moved_old_poll_cat=(sum(moved_old_poll_cat)>0),
         ever_moved_new_poll_loc=(sum(moved_new_poll_loc)>0),
         ever_no_move_new_poll_loc=(sum(no_move_new_poll_loc)>0),
         ever_moved_old_poll_loc=(sum(moved_old_poll_loc)>0))%>% 
  #create single variable that indicates if someone voted in a given year
  group_by(year)%>%
  mutate(voted = ((year==2018 & General_2018_11_06==1)|(year==2019 & General_2019_11_05==1)))%>%
  ungroup()

###### Split location category into years
two_data$location_category_2018<-two_data$location_category
two_data$location_category_2018[two_data$year==2019]<-NA
two_data$location_category_2019<-two_data$location_category
two_data$location_category_2019[two_data$year==2018]<-NA

############ testing data
## order by voterid
# two_data <- two_data[order(two_data$VOTERID),]
# mini_data<-two_data[1:500000,]
# 
# rigged_data<-two_data[1:1000000,c('VOTERID','voted','General_2019_11_05','ever_changed_precinct','year')]
#changed_prec<- rbinom(500000,0:1,.015)
#changed_prec<-rep(changed_prec,each=2)
#rigged_data$ever_changed_precinct<-changed_prec
#prob_vote<-runif(n=1000000,min=0,max=1)
#normal_probs <- pmax(pmin(rnorm(1000000, mean = 0.3771259, sd = 0.4846669), 1), 0)
#rigged_data$General_2019_11_05<-round(rigged_data$ever_changed_precinct*0.002+normal_probs, digits=0)
#rigged_data$General_2019_11_05<-round(prob_vote, digits=0)
# out0 <- drdid(yname = "voted", #outcome
#               dname = "ever_changed_precinct", #treatment group 
#               idname = "VOTERID", #respondent
#               tname = "year",
#               data = rigged_data)
# out0

################ subset for change in poll category/location test
### people who changed polling location for whatever reason
ecp_two_data<-two_data%>%
  filter(ever_changed_poll_loc==T)

### people who changed polling location by moving and people who didn't change location
no_chng_or_mv_two_data<-two_data%>%
  filter(((ever_moved_new_poll_loc==T)|(ever_changed_poll_loc==F))&(ever_no_move_new_poll_loc==F))

### people who changed polling location by moving and people who didn't change location
no_chng_or_no_mv_two_data<-two_data%>%
  filter(((ever_no_move_new_poll_loc==T)|(ever_changed_poll_loc==F))&(ever_moved_new_poll_loc==F))


########################### subsets for general knowledge tests 
### people who changed polling location to a well known building 
ecp_wellknown_two_data<-two_data%>%
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
no_mv_wellknown_two_data<-two_data%>%
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
mv_wellknown_two_data<-two_data%>%
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

########################### subsets for specific knowledge tests 
### people who changed polling station and their new station is a religious building
ecp_relig_two_data<-two_data%>%
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

### people who changed polling station w/o moving and their new station is a religious building
no_mv_relig_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_no_move_new_poll_loc==T)%>%
  # only people who changed polling location to a/another religious building
  filter(location_category_2019%in%c('religious',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is religious
    relig_new_poll_relig = sum(((known_religious==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station by moving and their new station is a religious building
mv_relig_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_moved_new_poll_loc==T)%>%
  # only people who changed polling location to a/another religious building
  filter(location_category_2019%in%c('religious',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is religious
    relig_new_poll_relig = sum(((known_religious==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### religious people who changed polling station
voters_relig_two_data<-two_data%>%
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


### people who changed polling station and their new station is a school
ecp_school_two_data<-two_data%>%
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

### people who changed polling station w/o moving and their new station is a school
no_mv_school_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_no_move_new_poll_loc==T)%>%
  # only people who changed polling location to a/another school
  filter(location_category_2019%in%c('school',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter household has children
    parent_new_poll_school = sum(((has_child==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station by moving and their new station is a school
mv_school_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_moved_new_poll_loc==T)%>%
  # only people who changed polling location to a/another school
  filter(location_category_2019%in%c('school',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter household has children
    parent_new_poll_school = sum(((has_child==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### Parents who changed polling station
voters_parents_two_data<-two_data%>%
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

### people who changed polling station and their new station is a public building
ecp_public_two_data<-two_data%>%
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

### people who changed polling station w/o moving and their new station is a public building
no_mv_public_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_no_move_new_poll_loc==T)%>%
  # only people who changed polling location to a/another school
  filter(location_category_2019%in%c('public','public_justice',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is a civil servant
    govemp_new_poll_public = sum(((CommercialData_OccupationIndustry=='Civil Servant')
                                  &(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station by moving and their new station is a school
mv_public_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_moved_new_poll_loc==T)%>%
  # only people who changed polling location to a/another school
  filter(location_category_2019%in%c('public','public_justice',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is a civil servant
    govemp_new_poll_public = sum(((CommercialData_OccupationIndustry=='Civil Servant')
                                  &(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### civil servants who changed polling station
voters_govemp_two_data<-two_data%>%
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

###################################### DiD ###########
# ######## Estimating Group-Time Average Treatment Effect, 2-periods
#covariates, must be time invariant
### setting covariates to value at first time point to make them invariant (just for testing purposes)
### Reminder to probably not include covariates that could be post-treatment, i.e. affected by changing precinct
# two_data<-two_data%>%
#   group_by(VOTERID)%>%
#   mutate(Voters_Gender = Voters_Gender[1])

ever_chng_loc <- c("ever_changed_poll_loc","ever_moved_new_poll_loc",
              'ever_no_move_new_poll_loc','ever_moved_old_poll_loc')
ever_chng_cat<-c('ever_changed_poll_cat','ever_moved_new_poll_cat',
              'ever_no_move_new_poll_cat','ever_moved_old_poll_cat')
chng_to<-c('changed_to_relig','changed_to_school','changed_to_public',
           'changed_to_wellknown',
           'relig_changed_to_relig','parent_changed_to_school','govemp_changed_to_public') 
new_poll<-c('new_poll_relig','relig_new_poll_relig','new_poll_school','parent_new_poll_school',
           'new_poll_public','govemp_new_poll_public','new_poll_wellknown')

############ Convert treatment indicators to numeric
two_data['ever_changed_poll_loc'] <- sapply(two_data['ever_changed_poll_loc'],as.numeric)
reason<-c('ever_moved_new_poll_loc','ever_no_move_new_poll_loc')
ecp_two_data[reason] <- sapply(ecp_two_data[reason],as.numeric)
no_chng_or_mv_two_data['ever_moved_new_poll_loc']<- 
  sapply(no_chng_or_mv_two_data['ever_moved_new_poll_loc'],as.numeric)
no_chng_or_no_mv_two_data['ever_no_move_new_poll_loc']<- 
  sapply(no_chng_or_no_mv_two_data['ever_no_move_new_poll_loc'],as.numeric)

ecp_wellknown_two_data['new_poll_wellknown']<- sapply(ecp_wellknown_two_data['new_poll_wellknown'],as.numeric)
no_mv_wellknown_two_data['new_poll_wellknown']<- sapply(no_mv_wellknown_two_data['new_poll_wellknown'],as.numeric)
mv_wellknown_two_data['new_poll_wellknown']<- sapply(mv_wellknown_two_data['new_poll_wellknown'],as.numeric)

ecp_school_two_data['parent_new_poll_school'] <- sapply(ecp_school_two_data['parent_new_poll_school'],as.numeric)
no_mv_school_two_data['parent_new_poll_school'] <- sapply(no_mv_school_two_data['parent_new_poll_school'],as.numeric)
mv_school_two_data['parent_new_poll_school'] <- sapply(mv_school_two_data['parent_new_poll_school'],as.numeric)
voters_parents_two_data['parent_new_poll_school'] <- sapply(voters_parents_two_data['parent_new_poll_school'],as.numeric)

ecp_relig_two_data['relig_new_poll_relig'] <- sapply(ecp_relig_two_data['relig_new_poll_relig'],as.numeric)
no_mv_relig_two_data['relig_new_poll_relig'] <- sapply(no_mv_relig_two_data['relig_new_poll_relig'],as.numeric)
mv_relig_two_data['relig_new_poll_relig'] <- sapply(mv_relig_two_data['relig_new_poll_relig'],as.numeric)
voters_relig_two_data['relig_new_poll_relig'] <- sapply(voters_relig_two_data['relig_new_poll_relig'],as.numeric)

ecp_public_two_data['govemp_new_poll_public'] <- sapply(ecp_public_two_data['govemp_new_poll_public'],as.numeric)
no_mv_public_two_data['govemp_new_poll_public'] <- sapply(no_mv_public_two_data['govemp_new_poll_public'],as.numeric)
mv_public_two_data['govemp_new_poll_public'] <- sapply(mv_public_two_data['govemp_new_poll_public'],as.numeric)
voters_govemp_two_data['govemp_new_poll_public'] <- sapply(voters_govemp_two_data['govemp_new_poll_public'],as.numeric)

#mini<-ecp_two_data[order(ecp_two_data$LALVOTERID),]
#mini<-mini[1:1000,]

for(col in reason){
  print(col)
  out0 <- drdid(yname = "voted", #outcome
                dname = col, #treatment group 
                idname = "VOTERID", #respondent
                tname = "year",
                #xformla = ~Voters_Gender, 
                data = ecp_two_data)
  out0
  # save model summary
  setwd(results_dir)
  sink(paste0(col,"_ecp_subset_DRDiD_model_summary.txt"))
  print(out0)
  sink()
}

use_data=voters_govemp_two_data
dvar<-'govemp_new_poll_public'
out0 <- drdid(yname = "voted", #outcome
              dname = dvar, #treatment group
              idname = "VOTERID", #respondent
              tname = "year",
              xformla = ~1,
              data = use_data)
out0
## save model summary
# setwd(results_dir)
# sink(paste0(dvar,"_no_chng_or_no_mv_DRDiD_model_summary.txt"))
# print(out0)
# sink()

# plot results
title = 'ATET of Changing Polling Location to a \nPublic Building vs. Non-Public Building \nfor the Public Employees'
groups = c('Non-Public Building', 'Parallel Trend', 'Public Building')
plot_did(data=use_data, did_model=out0, dvar=dvar, 
         plot_title=title,
         group1=groups[1], group2=groups[2],group3=groups[3])
# blank plot
blank_plot_did(data=use_data, did_model=out0, dvar=dvar, 
               plot_title=title,
               group1=groups[1], group2=groups[2],group3=groups[3])

