#Nathaniel Flemming
# 26/8/24

# Difference in difference regressions, using data created by DiD_preprocessing script
# Propensity score matching done manually

#library(did) #difference in difference package, 3+ periods
#library(DRDID) #difference in difference package, 2 periods
library(tidyverse) #convenience
library(data.table) #read in data selectively
library(stringr) #id string manipulation


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
#two_data<-read.csv('DiD_prepped_poll_vote_18to19.csv')
model_data<-read.csv('DiD_prepped_poll_vote_16to19_no_rndm_race.csv')


## Recode extraneous parties to 'other'
model_data$Parties_Description <- fct_collapse(model_data$Parties_Description, 
                                               Other = c('American', 'American Independent','Anarchist','Bull Moose',
                                                         'Christian','Communist','Conservative','Constitution',
                                                         'Constitutional','Consumer','Federalist','Free Choice',
                                                         'Freedom','Green','Independence','Independent Democrat',
                                                         'Independent Republican','Labor','Liberal',
                                                         'Libertarian','Natural Law','Non-Partisan','Patriot',
                                                         'Peace and Freedom','Populist','Progressive','Prohibition','Rainbow',
                                                         'Reform','Registered Independent','Right to Life',
                                                         'Social Democrat','Socialist','Socialist Labor',
                                                         'Taxpayers','Unknown','Whig'))
model_data$Parties_Description <- relevel(model_data$Parties_Description, ref = "Democratic")

## create vector of location categories
#categories<-c('pub_loc','pub_just','other','relig_loc','school','multiple',
#              'justice_loc','library','relig_school')
## Create vector of location category labels
loc_labels_NAsettoOther<-c('Other','Justice Location','Library','Multiple Categories',
                           'Public Location','Public/Justice Location','Religious Location',
                           'Religious School','School')
loc_labels_OthersettoNA<-c('Multiple Categories','Justice Location','Library',
                           'Public Location','Public/Justice Location','Religious Location',
                           'Religious School','School')
## Create dictionary of location labels
loc_dict<-c('pub_loc'='Public Location','pub_just'='Public and Justice Location',
            'other'='Other','relig_loc'='Religious Location','school'='School',
            'multiple'='Multiple Categories', 'justice_loc'='Justice Location',
            'library'='Library', 'relig_school'='Religious School',
            'catholic_school'='Catholic School','catholic_church'='Catholic Church',
            'cath_loc'='Catholic Location')
## Create dictionary of variable labels
var_dict<-c('Voters_Gender'='Gender', 'Voters_Age'='Age',
            'CommercialData_EstimatedHHIncomeAmount'='Estimated HH Income',
            'Residence_Families_HHCount'='HH Resident Count',
            'known_religious'='Known Religious',
            'CommercialData_LikelyUnion'='Likely Union Member', 
            'CommercialData_OccupationIndustry'='Occupation Industry',
            'CommercialData_OccupationIndustry'='Occupation Group',
            'has_child'='Has Child(ren)','known_gov_emp'='Known Government Employee',
            'Parties_Description'='Political Party','pred_race'='Predicted Race',
            'Shape_Length'='Distance to Polling Station','known_catholic'='Known Catholic')

## Calculate years registered based on dependent variable year
model_data$years_reg<-2018-as.numeric(model_data$year_reg)

## Common set of covariates
common_covars <-c(
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'pred_race',
  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
  'known_religious','CommercialData_LikelyUnion', 
  'CommercialData_OccupationGroup',
  'Shape_Length',
  'years_reg'
)
## Create simplified location categories variable (subsume catholic into religious)
# model_data$location_category_simpl<-model_data$location_category
# model_data$location_category_simpl[model_data$location_category_simpl=='catholic_church']<-'religious'
# model_data$location_category_simpl[model_data$location_category_simpl=='catholic_school']<-'religious_school'

## set how 'other category is treated
#other_cond='default'
#other_cond='OtherSettoNA'
other_cond='NASettoOther'
loc_labels=loc_labels_NAsettoOther
#loc_labels=loc_labels_OthersettoNA
## recode missing or other
model_data$location_category[is.na(model_data$location_category)]<-'other'
#model_data$location_category[model_data$location_category=='other']<-NA

## Factorize variables
#location categories
model_data$location_category<-as.factor(model_data$location_category)
model_data$location_category<-relevel(model_data$location_category, ref='other')
#model_data$location_category<-relevel(model_data$location_category, ref='multiple')

#race
model_data$pred_race <- as.factor(model_data$pred_race)
model_data$pred_race <- relevel(model_data$pred_race, ref = "pred.whi")
#political party
model_data$Parties_Description <- as.factor(model_data$Parties_Description)
model_data$Parties_Description <- relevel(model_data$Parties_Description, ref = "Democratic")
#religious
model_data$known_religious<-as.factor(model_data$known_religious)
#catholic
model_data$known_catholic<-as.factor(model_data$known_catholic)
#child present
model_data$has_child<-as.factor(model_data$has_child)
#government employee
#model_data$known_gov_emp <- ifelse(model_data$CommercialData_OccupationIndustry=="Civil Servant",TRUE,FALSE)
#model_data$known_gov_emp<-as.factor(model_data$known_gov_emp)
#union member
model_data$CommercialData_LikelyUnion<-as.factor(model_data$CommercialData_LikelyUnion)
#occupational group
model_data$CommercialData_OccupationGroup<-as.factor(model_data$CommercialData_OccupationGroup)
model_data$CommercialData_OccupationGroup<-relevel(model_data$CommercialData_OccupationGroup, ref='Blue Collar')

### create indicators for ever being treated
#two_data<-two_data%>%
two_data<-model_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  mutate(ever_changed_poll_loc=(sum(changed_poll_loc)>0),
         ever_moved_new_poll_loc=(sum(moved_new_poll_loc)>0),
         ever_no_move_new_poll_loc=(sum(no_move_new_poll_loc)>0),
         ever_moved_old_poll_loc=(sum(moved_old_poll_loc)>0))%>% 
  #create single variable that indicates if someone voted in a given year
  group_by(year)%>%
  mutate(voted = ((year==2018 & General_2018_11_06==1)|(year==2019 & General_2019_11_05==1)),
         ## Calculate years registered by 2018
         years_reg = 2018-as.numeric(year_reg))%>%
  ungroup()%>%
  #remove duplicate voters (not sure where they came from)
  distinct(LALVOTERID, year, .keep_all = T)
#set missing voted to 0?
#two_data$voted[is.na(two_data$voted)]<-F

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


### catholic people who changed polling station
voters_cath_two_data<-two_data%>%
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
chng_to<-c('changed_to_school','changed_to_cath','changed_to_wellknown',
           'parent_changed_to_school','cath_changed_to_cath') 
new_poll<-c('new_poll_school','parent_new_poll_school','new_poll_cath','cath_new_poll_cath',
            'new_poll_wellknown')

############ Convert treatment indicators to numeric
two_data['ever_changed_poll_loc'] <- sapply(two_data['ever_changed_poll_loc'],as.numeric)
# reason<-c('ever_moved_new_poll_loc','ever_no_move_new_poll_loc')
# ecp_two_data[reason] <- sapply(ecp_two_data[reason],as.numeric)
no_chng_or_mv_two_data['ever_moved_new_poll_loc']<-
  sapply(no_chng_or_mv_two_data['ever_moved_new_poll_loc'],as.numeric)
no_chng_or_no_mv_two_data['ever_no_move_new_poll_loc']<-
  sapply(no_chng_or_no_mv_two_data['ever_no_move_new_poll_loc'],as.numeric)

ecp_wellknown_two_data['new_poll_wellknown']<- sapply(ecp_wellknown_two_data['new_poll_wellknown'],as.numeric)
ecp_wellknown_two_data$voted<-as.numeric(ecp_wellknown_two_data$voted)

voters_parents_two_data['parent_new_poll_school'] <- sapply(voters_parents_two_data['parent_new_poll_school'],as.numeric)
voters_parents_two_data$voted<-as.numeric(voters_parents_two_data$voted)

voters_cath_two_data['cath_new_poll_cath'] <- sapply(voters_cath_two_data['cath_new_poll_cath'],as.numeric)
voters_cath_two_data$voted<-as.numeric(voters_cath_two_data$voted)

#mini<-ecp_two_data[order(ecp_two_data$LALVOTERID),]
#mini<-mini[1:1000,]

# for(col in reason){
#   print(col)
#   out0 <- drdid(yname = "voted", #outcome
#                 dname = col, #treatment group 
#                 idname = "VOTERID", #respondent
#                 tname = "year",
#                 #xformla = ~Voters_Gender, 
#                 data = ecp_two_data)
#   out0
#   # save model summary
#   setwd(results_dir)
#   sink(paste0(col,"_ecp_subset_DRDiD_model_summary.txt"))
#   print(out0)
#   sink()
# }

#### DiD Regression
dvar<-'ever_no_move_new_poll_loc'
# remove missing to prevent different length Y1 and Y0
mini<-no_chng_or_no_mv_two_data%>%
  select(all_of(c('voted',dvar,'LALVOTERID','VOTERID','year'
                  ,'Voters_Gender'
                  ,'Voters_Age','Parties_Description','pred_race',
                  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
                  'known_religious','CommercialData_LikelyUnion','CommercialData_OccupationGroup',
                  'years_reg'
  )))%>%
  filter(complete.cases(.))%>%
  #filter out voters who aren't in both 2018 and 2019?
  group_by(LALVOTERID)%>%
  filter(length(year)==2)%>%
  ungroup()

# fix covariates at 2018 values
## Filter data for year 2018
mini_2018 <- mini %>%
  filter(year == 2018) %>%
  select(LALVOTERID, Voters_Gender, Voters_Age, Parties_Description, pred_race, 
         CommercialData_EstimatedHHIncomeAmount, Residence_Families_HHCount, 
         known_religious, CommercialData_LikelyUnion, CommercialData_OccupationGroup, years_reg)
## Join back to the original dataset
mini <- mini %>%
  left_join(mini_2018, by = "LALVOTERID", suffix = c("", "_2018")) %>%
  mutate(
    Voters_Gender = Voters_Gender_2018,
    Voters_Age = Voters_Age_2018,
    Parties_Description = Parties_Description_2018,
    pred_race = pred_race_2018,
    CommercialData_EstimatedHHIncomeAmount = CommercialData_EstimatedHHIncomeAmount_2018,
    Residence_Families_HHCount = Residence_Families_HHCount_2018,
    known_religious = known_religious_2018,
    CommercialData_LikelyUnion = CommercialData_LikelyUnion_2018,
    CommercialData_OccupationGroup = CommercialData_OccupationGroup_2018,
    years_reg = years_reg_2018
  ) %>%
  select(-ends_with("_2018"))  # Remove extra columns




##### Package DID
# out1 <- drdid(yname = "voted", #outcome
#               dname = dvar, #treatment group
#               idname = "VOTERID", #respondent
#               tname = "year",
#               xformla = ~Voters_Gender+Voters_Age+Parties_Description+pred_race+
#                 CommercialData_EstimatedHHIncomeAmount+Residence_Families_HHCount+
#                 known_religious+CommercialData_LikelyUnion+CommercialData_OccupationGroup+
#                 years_reg,
#               data = mini)
# out1
# ## save model summary
# setwd(results_dir)
# sink(paste0(dvar,'_DRDiD_matched_on_covars_',other_cond,'model_summary.txt'))
# print(out1)
# sink()

# plot results
title = 'ATET of Changing Polling Location to a \nCatholic Church vs. Another Building for Catholics'
groups = c('Non-Catholic Church', 'Parallel Trend', 'Catholic Church')
plot_did(data=mini, did_model=out1, dvar=dvar, 
         plot_title=title,
         group1=groups[1], group2=groups[2],group3=groups[3])
# blank plot
# blank_plot_did(data=use_data, did_model=out0, dvar=dvar, 
#                plot_title=title,
#                group1=groups[1], group2=groups[2],group3=groups[3])


######################### Multiperiod
library(did)

#### Event study
#reformat data for multi periods (1 row per year from 2016 to 2019)
gvar<-'cath_new_poll_cath'
mini<-voters_cath_two_data%>%
  select(c('General_2016_11_08','General_2017_11_07','General_2018_11_06',
           'General_2019_11_05',
           'voted',gvar,'LALVOTERID','VOTERID','year'
           #covariates
           ,'Voters_Gender'
           ,'Voters_Age','Parties_Description','pred_race',
           'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
           #'known_religious',
           'CommercialData_LikelyUnion','CommercialData_OccupationGroup',
           'years_reg'))%>%
  mutate(cath_new_poll_cath=ifelse(cath_new_poll_cath==1,2019,0))%>%
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
                       #known_religious+
                       CommercialData_LikelyUnion+CommercialData_OccupationGroup+
                       years_reg,
                     data = mini
)
summary(did.att.gt)
## save model summary
setwd(results_dir)
sink(paste0(gvar,'_mpdid_matched_on_covars_',other_cond,'model_summary.txt'))
print(did.att.gt)
sink()
# plot them
ggdid(did.att.gt)
