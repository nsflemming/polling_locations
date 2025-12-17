#Nathaniel Flemming
# 15/9/25

# Difference in difference regressions, using data created by DiD_preprocessing script
# Propensity score matching done manually

#library(did) #difference in difference package, 3+ periods
#library(DRDID) #difference in difference package, 2 periods
library(tidyverse) #convenience
library(data.table) #read in data selectively
library(stringr) #id string manipulation
#library(psa) #all in on propensity score analysis package (no version for my version of R?)
library(MatchIt) #matching for DiD
#library(PSAgraphics)
library(marginaleffects) #estimates effects with robust standard errors
library(cobalt)# Check balance

########## Functions
###



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
model_data$years_reg<-2017-as.numeric(model_data$year_reg)

## Common set of covariates
common_covars <-c(
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'pred_race',
  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
  'known_religious','CommercialData_LikelyUnion', 
  #'CommercialData_OccupationGroup',
  #''CommercialData_OccupationIndustry',
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
#union member
model_data$CommercialData_LikelyUnion<-as.factor(model_data$CommercialData_LikelyUnion)
#occupational group (Not a variable in 2017 data, so have to remove if using that year)
model_data$CommercialData_OccupationGroup<-as.factor(model_data$CommercialData_OccupationGroup)
model_data$CommercialData_OccupationGroup<-relevel(model_data$CommercialData_OccupationGroup, ref='Blue Collar')
#occupational Industry (substitute for occupation group)
model_data$CommercialData_OccupationIndustry<-as.factor(model_data$CommercialData_OccupationIndustry)
model_data$CommercialData_OccupationIndustry<-relevel(model_data$CommercialData_OccupationIndustry, ref='Unknown')


### Create treatment indicators
two_data<-model_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  mutate(ever_changed_poll_loc=(sum(changed_poll_loc)>0),
    ever_moved_new_poll_loc=(sum(moved_new_poll_loc)>0),
    ever_no_move_new_poll_loc=(sum(no_move_new_poll_loc)>0)
    #ever_moved_old_poll_loc=(sum(moved_old_poll_loc)>0)
  )%>% 
  #create single variable that indicates if someone voted in a given year
  group_by(year)%>%
  #mutate(voted = ((year==2018 & General_2018_11_06==1)|(year==2019 & General_2019_11_05==1)),
  mutate(voted = ((year==2017 & General_2017_11_07==1)|(year==2019 & General_2019_11_05==1)),
         ## Calculate years registered by 2017
         years_reg = 2017-as.numeric(year_reg))%>%
  ungroup()%>%
  #remove duplicate voters (not sure where they came from)
  distinct(LALVOTERID, year, .keep_all = T)
#set missing voted to 0?
#two_data$voted[is.na(two_data$voted)]<-F

###### Split location category into years
two_data$location_category_2017<-two_data$location_category
two_data$location_category_2017[two_data$year==2019]<-NA
two_data$location_category_2019<-two_data$location_category
two_data$location_category_2019[two_data$year==2017]<-NA


######## Generate propensity scores
### new poll location without moving
dvar<-'ever_no_move_new_poll_loc'
### people who changed polling location without moving and people who didn't change location
no_chng_or_no_mv_two_data<-two_data%>%
  filter(((ever_no_move_new_poll_loc==T)|(ever_changed_poll_loc==F))&(ever_moved_new_poll_loc==F))

# remove missing to prevent different length Y1 and Y0
## Test with subsample
no_chng_or_no_mv_two_data<-no_chng_or_no_mv_two_data%>%
  select(all_of(c('voted',dvar,'LALVOTERID','VOTERID','year','County'
                  ,'Voters_Gender'
                  ,'Voters_Age','Parties_Description','pred_race',
                  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
                  'known_religious','CommercialData_LikelyUnion',
                  'CommercialData_OccupationIndustry',
                  'years_reg'
  )))%>%
  filter(complete.cases(.))%>%
  #filter out voters who aren't in both 2017 and 2019?
  group_by(LALVOTERID)%>%
  filter(length(year)==2)%>%
  ungroup()

# Logistic Regression based scores
## Change in polling location without moving
ps_formula <- parent_new_poll_school ~  Voters_Gender + Voters_Age + Parties_Description+
  pred_race+CommercialData_EstimatedHHIncomeAmount+Residence_Families_HHCount+
  known_religious+CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg

################## Test on parents/schools
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

voters_parents_two_data['parent_new_poll_school'] <- sapply(voters_parents_two_data['parent_new_poll_school'],as.numeric)
voters_parents_two_data$voted<-as.numeric(voters_parents_two_data$voted)
### Subsample for testing
# mini_parent<-voters_parents_two_data%>%
#   group_by(parent_new_poll_school)%>%
#   slice_sample(n=1000000)%>%
#   ungroup()

# fix covariates at 2017 values
## Filter data for year 2017
voters_parents_two_data_2017 <- voters_parents_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'years_reg')))
## Join back to the original dataset
voters_parents_two_data <- voters_parents_two_data %>%
  left_join(voters_parents_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  mutate(
    Voters_Gender = Voters_Gender_2017,
    Voters_Age = Voters_Age_2017,
    Parties_Description = Parties_Description_2017,
    pred_race = pred_race_2017,
    CommercialData_EstimatedHHIncomeAmount = CommercialData_EstimatedHHIncomeAmount_2017,
    Residence_Families_HHCount = Residence_Families_HHCount_2017,
    known_religious = known_religious_2017,
    CommercialData_LikelyUnion = CommercialData_LikelyUnion_2017,
    #CommercialData_OccupationGroup = CommercialData_OccupationGroup_2017,
    years_reg = years_reg_2017
  )%>%
  select(-ends_with("_2017"))%>%  # Remove extra columns
  # Remove voters who have missing data
  filter(complete.cases(.))

## Default logistic regression calculated propensity score matching
### no replacement, one control to one treated
m.logit<-matchit(ps_formula,data=voters_parents_two_data, replace=F)
summary(m.logit)
# Check balance (default balance is poor (probably small sample size))
love.plot(m.logit, drop.distance = TRUE)
## Convert match object into a dataset
logit.match<-match.data(m.logit)
## t-test comparing outcome
t.test(voted ~ parent_new_poll_school, data = logit.match)


#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
parent.fit1 <- glm(voted ~ treat,
                   data = logit.match,
                   family = binomial(link = 'logit'),
                   weights = weights)

