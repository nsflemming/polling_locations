#Nathaniel Flemming
# 6/7/26

# Difference in difference regressions, using data created by DiD_preprocessing script
# Propensity score matching done manually
# Running seperate regressions for Allegheny (Pittsburgh) and Philadelphia vs the Rest of the state

library(tidyverse) #convenience
library(data.table) #read in data selectively
library(stringr) #id string manipulation
library(MatchIt) #matching for DiD
library(marginaleffects) #estimates effects with robust standard errors
library(cobalt)# Check balance
library(sandwich) # clustered standard errors
library(lmtest) # clustered standard errors

######### Functions
### Not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

### TWFE Run and save
Run_TWFE_and_save_output<-function(formula, data, path, filename){
  base.fit1 <- glm(
    formula=formula,
    data = data,
    family = binomial(link = 'logit'))
  summary(base.fit1)
  ## Clustered standard errors
  cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
  summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
  print(summary_clustered)
  chars <- capture.output(print(summary_clustered))
  writeLines(chars, con = file(paste0(path,filename)))
}


########################################################### Main
# set directories ####
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
#####
# read in two-time period data and process####
setwd(data_dir)
model_data<-read.csv('DiD_prepped_poll_vote_16to19_no_rndm_race.csv')

## Recode extraneous parties to 'other' ####
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
#####

## Create vector of location category labels #####
loc_labels_NAsettoOther<-c('Other','Justice Location','Library','Multiple Categories',
                           'Public Location','Public/Justice Location','Religious Location',
                           'Religious School','School')

#####
## Create dictionary of location labels #####
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
#####

## Create simplified location categories variable 
###(subsume catholic into religious) ####
model_data<-model_data%>%
  mutate(location_category_simpl = case_when(
    location_category=='apartment' ~ 'apartment building',
    location_category=='public center' ~ 'community center',
    location_category=='senior center' ~ 'community center',
    location_category=='public center/senior center' ~ 'community center',
    location_category=='art center' ~ 'community center',
    location_category=='post office' ~ 'government',
    location_category=='government' ~ 'government',
    location_category=='government/police' ~ 'government/justice',
    location_category=='courthouse' ~ 'justice',
    location_category=='police station' ~ 'justice',
    location_category=='religious school' ~ 'religious',
    location_category=='catholic school' ~ 'religious',
    # #Military buildings on its own
    #location_category=='military' ~ 'military',
    # #Grouping military with veteran association buildings
    location_category=='military' ~ 'military/veteran',
    location_category=='veteran' ~ 'military/veteran',
    ## Folding in gov/milit too since it's a small category with a large effect
    location_category=='government/military' ~ 'military/veteran',
    location_category=='association' ~ 'association/club/sport/union',
    location_category=='club' ~ 'association/club/sport/union',
    location_category=='sport' ~ 'association/club/sport/union',
    location_category=='union' ~ 'association/club/sport/union',
    location_category=='association/union' ~ 'association/club/sport/union',
    location_category=='sports association' ~ 'association/club/sport/union',
    ## Should have been recoded to association earlier...
    location_category=='event space' ~ 'association/club/sport/union',
    location_category=='restaurant' ~ 'business',
    location_category=='nursing home' ~ 'retirement community/nursing home',
    location_category=='retirement community' ~ 'retirement community/nursing home',
    # #Grouping most smaller categories we haven't theorized about together into 'other'
    # location_category=='military' ~ 'other',
    # location_category=='veteran' ~ 'other',
    # location_category=='association' ~ 'other',
    # location_category=='club' ~ 'other',
    # location_category=='sport' ~ 'other',
    # location_category=='union' ~ 'other',
    location_category=='insufficient info' ~ 'other',
    location_category=='stadium' ~ 'other',
    location_category=='museum' ~ 'other',
    location_category=='hotel' ~ 'other',
    location_category=='monument' ~ 'other',
    location_category=='recreation facility' ~ 'other',
    location_category=='airport' ~ 'other',
    location_category=='mobile home park' ~ 'other',
    location_category=='private residence' ~ 'other',
    # Should have been recategorized to other earlier...
    location_category=='religious/government' ~ 'other',
    .default = location_category
  ))

#####

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

## set how 'other category is treated (All NA should be coded now we've done it manually)
other_cond='NASettoOther'
loc_labels=loc_labels_NAsettoOther
## recode missing to other
model_data$location_category[is.na(model_data$location_category)]<-'other'
model_data$location_category_simpl[is.na(model_data$location_category_simpl)]<-'other'

## Factorize variables ####
#location categories
model_data$location_category<-as.factor(model_data$location_category)
model_data$location_category<-relevel(model_data$location_category, ref='other')
model_data$location_category_simpl<-as.factor(model_data$location_category_simpl)
model_data$location_category_simpl<-relevel(model_data$location_category_simpl, ref='other')
#race
model_data$pred_race <- as.factor(model_data$pred_race)
model_data$pred_race <- relevel(model_data$pred_race, ref = "pred.whi")
#political party
model_data$Parties_Description <- as.factor(model_data$Parties_Description)
model_data$Parties_Description <- relevel(model_data$Parties_Description, ref = "Democratic")
#religious
model_data$known_religious<-as.factor(model_data$known_religious)
#child present
model_data$has_child<-as.factor(model_data$has_child)
#union member
model_data$CommercialData_LikelyUnion<-as.factor(model_data$CommercialData_LikelyUnion)
#occupational group (Not a variable in 2017 data, so have to remove if using that year)
#model_data$CommercialData_OccupationGroup<-as.factor(model_data$CommercialData_OccupationGroup)
#model_data$CommercialData_OccupationGroup<-relevel(model_data$CommercialData_OccupationGroup, ref='Blue Collar')
#occupational Industry (substitute for occupation group)
model_data$CommercialData_OccupationIndustry<-as.factor(model_data$CommercialData_OccupationIndustry)
model_data$CommercialData_OccupationIndustry<-relevel(model_data$CommercialData_OccupationIndustry, ref='Unknown')
#####

### Create dependent variable 'voted' ####
two_data<-model_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  # mutate(ever_changed_poll_loc=(sum(changed_poll_loc)>0),
  #        ever_moved_new_poll_loc=(sum(moved_new_poll_loc)>0),
  #        ever_no_move_new_poll_loc=(sum(no_move_new_poll_loc)>0)
  # )%>% 
  #create single variable that indicates if someone voted in a given year
  group_by(year)%>%
  mutate(voted = ((year==2016 & General_2016_11_08==1)
                  |(year==2017 & General_2017_11_07==1)
                  |(year==2018 & General_2018_11_06==1)
                  |(year==2019 & General_2019_11_05==1)))%>%
  ungroup()%>%
  ## Calculate years registered by 2017
  mutate(years_reg = 2017-as.numeric(year_reg))%>%
  ungroup()%>%
  #remove duplicate records (not sure where they came from)
  distinct(LALVOTERID, year, .keep_all = T)

# ####

### Set location category being tested
#treatment_location='school'

######## Generate propensity scores ####
######## Matching should only be done on pre-treatment observations
### General Tests of Effects of Changing Location

#### People who changed location vs. people who didn't
## Philly
#### Set subpopulation of interest
subpopulation<-'Philadelphia'
#Matching regression formula####
ps_formula <- new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+Shape_Length
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
  filter(County %in% c('PHILADELPHIA'))%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # Whether new polling location in 2019 or not
    new_poll_treated = ifelse(
      any((changed_poll_loc>0)&(year==2019)),T,F)
  )%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length',
                  'new_poll_treated',
                  'voted'
  )))

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
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
    CommercialData_OccupationIndustry = CommercialData_OccupationIndustry_2017,
    years_reg = years_reg_2017
  )%>%
  select(-ends_with("_2017"))%>%  # Remove extra columns
  # Remove voters who have missing data
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
# filter data so only voters present in 2017 and 2019 are included
## otherwise end up with treated voters matched to incomplete controls
pre_data <- change_location_two_data%>%
  group_by(LALVOTERID)%>%
  filter(any(year==2017) & any(year==2019))%>%
  ungroup()%>%
  filter(year==2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
### T-test to examine baseline voting frequency ####
test_year=2019
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_new_location_vs_not_matched_t_test_",as.character(test_year),".txt")))
t_test_data<-change_location_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_new_location_vs_not_all_obs_t_test_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_new_location_vs_not_twfe_no_covars_cse_17_19_6_30_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_new_location_vs_not_twfe_covars_cse_17_19_6_30_26.txt")))
############
# #####

#### 2019 or 2018 if they didn't vote in 2018 ####
# Flag voters treated in 2018 or 2019
change_location_two_data<-two_data%>%
  filter(County %in% c('PHILADELPHIA'))%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  group_by(LALVOTERID)%>%
  mutate(
    new_poll_treated = 
      # Whether new polling location in 2019 or not
      (any((changed_poll_loc>0)&(year==2019)))
    |
      # Whether new polling location in 2018, but didn't vote in 2018 and then didn't change in 2019
      any((
      (changed_poll_loc>0)&(year==2018)&(General_2018_11_06==0)
    )& any(
      (year==2019)&(changed_poll_loc==0)
    ))
  )%>%
  select(all_of(c('LALVOTERID','year','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length',
                  'new_poll_treated',
                  'voted'
  )))%>%
  ungroup()

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
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
    CommercialData_OccupationIndustry = CommercialData_OccupationIndustry_2017,
    years_reg = years_reg_2017
  )%>%
  select(-ends_with("_2017"))%>%  # Remove extra columns
  # Remove voters who have missing data
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
pre_data <- change_location_two_data%>%
  group_by(LALVOTERID)%>%
  filter(any(year==2017) & any(year==2019))%>%
  ungroup()%>%
  filter(year==2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
### T-test to examine baseline voting frequency ####
test_year=2019
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_new_location_vs_not_matched_t_test_",as.character(test_year),".txt")))
t_test_data<-change_location_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_new_location_vs_not_all_obs_t_test_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
# base.fit1 <- glm(voted ~ new_poll_treated*factor(year),
#                  data = matched_panel,
#                  family = binomial(link = 'logit'))
# summary(base.fit1)
# ## Clustered standard errors
# cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
# summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
# print(summary_clustered)
# chars <- capture.output(print(summary_clustered))
# writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
#                                     subpopulation,"_new_location_vs_not_twfe_no_covars_cse_17_1819_6_30_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_new_location_vs_not_twfe_covars_cse_17_1819_6_30_26.txt")))
############
# #####


## Big Cities
subpopulation<-'Big_Cities'
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
  filter(County %in% c('ALLEGHENY','PHILADELPHIA'))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # Whether new polling location in 2019 or not
    new_poll_treated = ifelse(
      any((changed_poll_loc>0)&(year==2019)),T,F)
  )%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'new_poll_treated',
                  'voted'
  )))

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
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
    CommercialData_OccupationIndustry = CommercialData_OccupationIndustry_2017,
    years_reg = years_reg_2017
  )%>%
  select(-ends_with("_2017"))%>%  # Remove extra columns
  # Remove voters who have missing data
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
# Filter to pre-treatment period only
pre_data <- change_location_two_data%>%
  filter(year == 2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
### T-test to examine baseline voting frequency ####
test_year=2019
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "Big_cities_new_location_vs_not_matched_t_test_",as.character(test_year),".txt")))
t_test_data<-change_location_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "Big_cities_new_location_vs_not_all_obs_t_test_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "Big_cities_new_location_vs_not_twfe_covars_cse_17_19_6_16_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg+County,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "Big_cities_new_location_vs_not_twfe_covars_cse_17_19_6_16_26.txt")))
############
# #####

## Everywhere Else
subpopulation<-'Rest_of_PA'
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
  filter(County %!in% c('ALLEGHENY','PHILADELPHIA'))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # Whether new polling location in 2019 or not
    new_poll_treated = ifelse(
      any((changed_poll_loc>0)&(year==2019)),T,F)
  )%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'new_poll_treated',
                  'voted'
  )))

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
# Filter to pre-treatment period only
pre_data <- change_location_two_data%>%
  filter(year == 2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
### T-test to examine baseline voting frequency ####
test_year=2017
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "Rest_of_PA_new_location_vs_not_matched_t_test_",as.character(test_year),".txt")))
t_test_data<-change_location_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "Rest_of_PA_new_location_vs_not_all_obs_t_test_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "Rest_of_PA_new_location_vs_not_twfe_covars_cse_17_19_6_16_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg+County,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "Rest_of_PA_new_location_vs_not_twfe_covars_cse_17_19_6_16_26.txt")))
############
# #####


#### People who moved vs. people who didn't change ####
## Philly
subpopulation<-'Philadelphia'
#Matching regression formula####
ps_formula <- moved_new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+Shape_Length
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
  filter(County %in% c('PHILADELPHIA'))%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  group_by(LALVOTERID)%>%
  # remove voters who changed location without moving
  filter(!any((no_move_new_poll_loc==1)&(year==2019)))%>%
  mutate(
    # Whether new polling location in 2019 or not
    moved_new_poll_treated = ifelse(
      any((moved_new_poll_loc==1)&(year==2019)),T,F)
  )%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length',
                  'moved_new_poll_treated',
                  'voted'
  )))

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
# filter data so only voters present in 2017 and 2019 are included
## otherwise end up with treated voters matched to incomplete controls
pre_data <- change_location_two_data%>%
  group_by(LALVOTERID)%>%
  filter(any(year==2017) & any(year==2019))%>%
  ungroup()%>%
  filter(year==2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
### T-test to examine baseline voting frequency ####
test_year=2019
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ moved_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ moved_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_moved_new_location_vs_not_matched_t_test_",as.character(test_year),".txt")))
t_test_data<-change_location_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ moved_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ moved_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_moved_new_location_vs_not_all_obs_t_test_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ moved_new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_moved_new_location_vs_not_twfe_no_covars_cse_17_19_6_30_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ moved_new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg+Shape_Length,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_moved_new_location_vs_not_twfe_covars_cse_17_19_6_30_26.txt")))
############
# #####

#### 2019 or 2018 if they didn't vote in 2018 ####
# Flag voters treated in 2018 or 2019 separately because it makes filtering easier?
change_location_two_data_19<-two_data%>%
  filter(County %in% c('PHILADELPHIA'))%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  group_by(LALVOTERID)%>%
  # remove voters who changed location without moving
  filter(
    # In 2019
    !any((no_move_new_poll_loc==1)&(year==2019)))%>%
  mutate(
    # Whether new polling location in 2019 or not
    moved_new_poll_treated = 
      any((moved_new_poll_loc==1)&(year==2019))
  )%>%
  ungroup()
# Voters treated in 2018
## Ignore voters with 2019 treatment indicator when flagging 2018 voters for treatment
##  ...to avoid overwriting
change_location_two_data_18_treated<-two_data%>%
  filter(County %in% c('PHILADELPHIA'))%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  # Hasn't been identified as treated in 2019
  filter(LALVOTERID%!in%change_location_two_data_19$LALVOTERID['moved_new_poll_treated'==T])%>%
  group_by(LALVOTERID)%>%
  # remove voters who changed location without moving
  filter(
    # In 2018 or 2019
    !any((no_move_new_poll_loc==1)&(year==2018)),
    !any((no_move_new_poll_loc==1)&(year==2019)))%>%
  mutate(
    # Whether new polling location in 2018 or not and didn't vote
    moved_new_poll_treated = 
      any((moved_new_poll_loc==1)&(year==2018)&(General_2018_11_06==0))
  )%>%
  filter(any(moved_new_poll_treated==T))%>%
  ungroup()
# Merge 2018 and 2019 data
## Replace the 2018 treated voters in the 2019 data frame
### Avoid rbind to prevent duplication since 2018 treated voters are untreated in the 2019 frame
change_location_two_data<-rows_update(change_location_two_data_19,change_location_two_data_18_treated,
                                      by=c('LALVOTERID','year'))%>%
  select(all_of(c('LALVOTERID','year','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length',
                  'moved_new_poll_treated',
                  'voted'
  )))

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
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
    CommercialData_OccupationIndustry = CommercialData_OccupationIndustry_2017,
    years_reg = years_reg_2017
  )%>%
  select(-ends_with("_2017"))%>%  # Remove extra columns
  # Remove voters who have missing data
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
pre_data <- change_location_two_data%>%
  group_by(LALVOTERID)%>%
  filter(any(year==2017) & any(year==2019))%>%
  ungroup()%>%
  filter(year==2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
### T-test to examine baseline voting frequency ####
test_year=2019
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ moved_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ moved_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_moved_new_location_vs_not_matched_t_test_incl2018_",as.character(test_year),".txt")))
t_test_data<-change_location_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ moved_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ moved_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"moved_new_location_vs_not_all_obs_t_test_incl2018_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
# base.fit1 <- glm(voted ~ moved_new_poll_treated*factor(year),
#                  data = matched_panel,
#                  family = binomial(link = 'logit'))
# summary(base.fit1)
# ## Clustered standard errors
# cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
# summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
# print(summary_clustered)
# chars <- capture.output(print(summary_clustered))
# writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
#                                     subpopulation,"_moved_new_location_vs_not_twfe_no_covars_cse_17_1819_6_30_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ moved_new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_moved_new_location_vs_not_twfe_covars_cse_17_1819_6_30_26.txt")))
############
# #####

#Matching regression formula####
ps_formula <- moved_new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+County+Shape_Length
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
  filter(County %in% c('ALLEGHENY','PHILADELPHIA'))%>%
  group_by(LALVOTERID)%>%
  # remove voters who changed location without moving
  filter(!any((no_move_new_poll_loc==1)&(year==2019)))%>%
  mutate(
    # Whether new polling location in 2019 or not
    moved_new_poll_treated = ifelse(
      any((moved_new_poll_loc==1)&(year==2019)),T,F)
  )%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length',
                  'moved_new_poll_treated',
                  'voted'
  )))

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
# Filter to pre-treatment period only
pre_data <- change_location_two_data%>%
  filter(year == 2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
#love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
### T-test to examine baseline voting frequency ####
t_test_data<-matched_panel%>%
  filter(year==2017)
t.test(data=t_test_data, voted ~ moved_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ moved_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "Big_Cities_moved_new_location_vs_not_matched_t_test_17.txt")))
t_test_data<-change_location_two_data%>%
  filter(year==2017)
t.test(data=t_test_data, voted ~ moved_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ moved_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "Big_Cities_moved_new_location_vs_not_all_obs_t_test_17.txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ moved_new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "Big_Cities_moved_new_location_vs_not_twfe_no_covars_cse_17_19_6_7_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ moved_new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg+County,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "Big_Cities_moved_new_location_vs_not_twfe_covars_cse_17_19_6_7_26.txt")))
############
#####
#####

#### People who changed without moving vs. People who didn't change ####
## Philly
subpopulation<-'Philadelphia'
#Matching regression formula####
ps_formula <- no_move_new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
  filter(County %in% c('PHILADELPHIA'))%>%
  group_by(LALVOTERID)%>%
  # remove voters who changed location by moving
  filter(!any((moved_new_poll_loc==1)&(year==2019)))%>%
  mutate(
    # Whether new polling location in 2019 or not
    no_move_new_poll_treated = 
      any((no_move_new_poll_loc==1)&(year==2019))
  )%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length',
                  'no_move_new_poll_treated',
                  'voted'
  )))

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
# filter data so only voters present in 2017 and 2019 are included
## otherwise end up with treated voters matched to incomplete controls
pre_data <- change_location_two_data%>%
  group_by(LALVOTERID)%>%
  filter(any(year==2017) & any(year==2019))%>%
  ungroup()%>%
  filter(year==2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
### T-test to examine baseline voting frequency ####
test_year=2019
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ no_move_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ no_move_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_no_move_new_location_vs_not_matched_t_test_",as.character(test_year),".txt")))
t_test_data<-change_location_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ no_move_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ no_move_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_no_move_new_location_vs_not_all_obs_t_test_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ no_move_new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_no_move_new_location_vs_not_twfe_no_covars_cse_17_19_6_30_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ no_move_new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg+Shape_Length,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_no_move_new_location_vs_not_twfe_covars_cse_17_19_6_30_26.txt")))
############
# #####

#### 2019 or 2018 if they didn't vote in 2018 ####
# Flag voters treated in 2018 or 2019 separately because it makes filtering easier?
change_location_two_data_19<-two_data%>%
  filter(County %in% c('PHILADELPHIA'))%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  group_by(LALVOTERID)%>%
  # remove voters who changed location by moving
  filter(
    # In 2019
    !any((moved_new_poll_loc==1)&(year==2019)))%>%
  mutate(
    # Whether new polling location in 2019 or not
    no_move_new_poll_treated = 
      any((no_move_new_poll_loc==1)&(year==2019))
  )%>%
  ungroup()
# Voters treated in 2018
## Ignore voters with 2019 treatment indicator when flagging 2018 voters for treatment
##  ...to avoid overwriting
change_location_two_data_18_treated<-two_data%>%
  filter(County %in% c('PHILADELPHIA'))%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  # Hasn't been identified as treated in 2019
  filter(LALVOTERID%!in%change_location_two_data_19$LALVOTERID['moved_new_poll_treated'==T])%>%
  group_by(LALVOTERID)%>%
  # remove voters who changed location by moving
  filter(
    # In 2018 or 2019
    !any((moved_new_poll_loc==1)&(year==2018)),
    !any((moved_new_poll_loc==1)&(year==2019)))%>%
  mutate(
    # Whether new polling location in 2018 or not and didn't vote
    no_move_new_poll_treated = 
      any((no_move_new_poll_loc==1)&(year==2018)&(General_2018_11_06==0))
  )%>%
  filter(any(no_move_new_poll_treated==T))%>%
  ungroup()
# Merge 2018 and 2019 data
## Replace the 2018 treated voters in the 2019 data frame
### Avoid rbind to prevent duplication since 2018 treated voters are untreated in the 2019 frame
change_location_two_data<-rows_update(change_location_two_data_19,change_location_two_data_18_treated,
                                      by=c('LALVOTERID','year'))%>%
  select(all_of(c('LALVOTERID','year','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length',
                  'no_move_new_poll_treated',
                  'voted'
  )))

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg','Shape_Length')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
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
    CommercialData_OccupationIndustry = CommercialData_OccupationIndustry_2017,
    years_reg = years_reg_2017
  )%>%
  select(-ends_with("_2017"))%>%  # Remove extra columns
  # Remove voters who have missing data
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
pre_data <- change_location_two_data%>%
  group_by(LALVOTERID)%>%
  # Only keep voters with complete set of pre and post treatment years
  filter(any(year==2017) & any(year==2019))%>%
  ungroup()%>%
  filter(year==2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
### T-test to examine baseline voting frequency ####
test_year=2019
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ no_move_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ no_move_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_no_move_new_location_vs_not_matched_t_test_incl2018_",as.character(test_year),".txt")))
t_test_data<-change_location_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ no_move_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ no_move_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_no_move_new_location_vs_not_all_obs_t_test_incl2018_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ no_move_new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_no_move_new_location_vs_not_twfe_no_covars_cse_17_1819_6_30_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ no_move_new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_no_move_new_location_vs_not_twfe_covars_cse_17_1819_6_30_26.txt")))
############
# #####


##Big Cities
subpopulation<-'Big_Cities'
#Matching regression formula####
ps_formula <- no_move_new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+County
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
  filter(County %in% c('ALLEGHENY','PHILADELPHIA'))%>%
  group_by(LALVOTERID)%>%
  # remove voters who changed location by moving
  filter(!any((moved_new_poll_loc==1)&(year==2019)))%>%
  mutate(
    # Whether new polling location in 2019 or not
    no_move_new_poll_treated = ifelse(
      any((no_move_new_poll_loc==1)&(year==2019)),T,F)
  )%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'no_move_new_poll_treated',
                  'voted'
  )))

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
# Filter to pre-treatment period only
pre_data <- change_location_two_data%>%
  filter(year == 2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
#love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
### T-test to examine baseline voting frequency ####
t_test_data<-matched_panel%>%
  filter(year==2017)
t.test(data=t_test_data, voted ~ no_move_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ no_move_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "Big_Cities_no_move_new_location_vs_not_matched_t_test_17.txt")))
t_test_data<-change_location_two_data%>%
  filter(year==2017)
t.test(data=t_test_data, voted ~ no_move_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ no_move_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "Big_Cities_no_move_new_location_vs_not_all_obs_t_test_17.txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ no_move_new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "Big_Cities_no_move_new_location_vs_not_twfe_no_covars_cse_17_19_6_16_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ no_move_new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg+County,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "Big_Cities_no_move_new_location_vs_not_twfe_covars_cse_17_19_6_16_26.txt")))
############
# ####

## Everywhere Else
subpopulation<-'Rest_of_PA'
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
  filter(County %!in% c('ALLEGHENY','PHILADELPHIA'))%>%
  group_by(LALVOTERID)%>%
  # remove voters who changed location by moving
  filter(!any((moved_new_poll_loc==1)&(year==2019)))%>%
  mutate(
    # Whether new polling location in 2019 or not
    no_move_new_poll_treated = ifelse(
      any((no_move_new_poll_loc==1)&(year==2019)),T,F)
  )%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'no_move_new_poll_treated',
                  'voted'
  )))

# Convert treatment and outcome variable to numeric for matching function?
change_location_two_data$voted<-as.numeric(change_location_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
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
    CommercialData_OccupationIndustry = CommercialData_OccupationIndustry_2017,
    years_reg = years_reg_2017
  )%>%
  select(-ends_with("_2017"))%>%  # Remove extra columns
  # Remove voters who have missing data
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)

## Default nearest neighbor calculated propensity score matching ####
# Filter to pre-treatment period only
pre_data <- change_location_two_data%>%
  filter(year == 2017)
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = pre_data,
  replace = FALSE
)
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- change_location_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')%>%
  # Remove any matched pairs missing an observation in one year
  group_by(subclass)%>%
  filter(n()==4)
#####
### T-test to examine baseline voting frequency ####
test_year=2019
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ no_move_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ no_move_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_no_move_new_location_vs_not_matched_t_test_",as.character(test_year),".txt")))
t_test_data<-change_location_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ no_move_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ no_move_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_no_move_new_location_vs_not_all_obs_t_test_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ no_move_new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_no_move_new_location_vs_not_twfe_no_covars_cse_17_19_6_16_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ no_move_new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg+County,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_no_move_new_location_vs_not_twfe_covars_cse_17_19_6_16_26.txt")))
############
# ####


### Parents who changed polling station to chosen category vs. not
treatment_location='school'
## Philly
subpopulation<-'Philadelphia'
## Matching formula
ps_formula <- parent_new_poll_treated ~  Voters_Gender + Voters_Age + Parties_Description+
  pred_race+CommercialData_EstimatedHHIncomeAmount+Residence_Families_HHCount+
  known_religious+CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+Shape_Length 
#### Just 2019 ####
voters_parents_two_data<-two_data%>%
  # only parents
  filter(County %in% c('PHILADELPHIA'),
         has_child==T)%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  ## any() means if any row in the group fulfills the condition all rows are kept
  filter(any(no_move_new_poll_loc==T & year==2019))%>%
  mutate(
    # Whether new polling location in 2019 is a school
    parent_new_poll_treated = ifelse(
      #((location_category_simpl==treatment_location)&(year==2019)),T,F)
      any((location_category_simpl==treatment_location)&(year==2019)),T,F)
  )%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg', 'Shape_Length',
                  'parent_new_poll_treated',
                  'voted'
  )))
voters_parents_two_data$voted<-as.numeric(voters_parents_two_data$voted)

# fix covariates at 201X (first time period) values
## Filter data for year 2017
voters_parents_two_data_2017 <- voters_parents_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','parent_new_poll_treated',
                  'Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg', 'Shape_Length')))%>%
  filter(complete.cases(.))
## Join back to the original data frame
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
    CommercialData_OccupationIndustry = CommercialData_OccupationIndustry_2017,
    years_reg = years_reg_2017,
    Shape_Length = Shape_Length_2017
  )%>%
  select(-ends_with("_2017"))%>%  # Remove extra columns
  # Remove voters who have missing data
  filter(complete.cases(.))

#####
## Default nearest neighbor calculated propensity score matching ####
# Use pre-treatment period only
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = voters_parents_two_data_2017,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- voters_parents_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')%>%
  # Remove any matched pairs missing an observation in one year
  group_by(subclass)%>%
  filter(n()==4)
# Remove 2017 dataframe
rm(voters_parents_two_data_2017)

### T-test to examine baseline voting frequency ####
test_year=2019
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ parent_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ parent_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_parent_school_vs_not_school_matched_t_test_",as.character(test_year),".txt")))
t_test_data<-voters_parents_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ parent_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ parent_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_parent_school_vs_not_school_all_obs_t_test_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ parent_new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_parent_school_vs_not_school_twfe_no_covars_cse_17_19_6_16_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ parent_new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg+Shape_Length,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_parent_school_vs_not_school_twfe_covars_cse_17_19_6_16_26.txt")))
############


### Elderly (age 65+) who changed polling station to chosen category vs. not
treatment_location='retirement community/nursing home'
## Philly
subpopulation<-'Philadelphia'
## Matching formula
ps_formula <- elder_new_poll_treated ~  Voters_Gender + Voters_Age + Parties_Description+
  pred_race+CommercialData_EstimatedHHIncomeAmount+Residence_Families_HHCount+
  known_religious+CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+Shape_Length 
#### Just 2019 ####
voters_elderly_two_data<-two_data%>%
  # only parents
  filter(County %in% c('PHILADELPHIA'),
         Voters_Age>=65)%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  ## any() means if any row in the group fulfills the condition all rows are kept
  filter(any(no_move_new_poll_loc==T & year==2019))%>%
  mutate(
    # Whether new polling location in 2019 is location of interest
    elder_new_poll_treated = ifelse(
      any((location_category_simpl==treatment_location)&(year==2019)),T,F)
  )%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg', 'Shape_Length',
                  'elder_new_poll_treated',
                  'voted'
  )))
voters_elderly_two_data$voted<-as.numeric(voters_elderly_two_data$voted)

# fix covariates at 201X (first time period) values
## Filter data for year 2017
voters_elderly_two_data_2017 <- voters_elderly_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','elder_new_poll_treated',
                  'Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg', 'Shape_Length')))%>%
  filter(complete.cases(.))
## Join back to the original data frame
voters_elderly_two_data <- voters_elderly_two_data %>%
  left_join(voters_elderly_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
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
    CommercialData_OccupationIndustry = CommercialData_OccupationIndustry_2017,
    years_reg = years_reg_2017,
    Shape_Length = Shape_Length_2017
  )%>%
  select(-ends_with("_2017"))%>%  # Remove extra columns
  # Remove voters who have missing data
  filter(complete.cases(.))

#####
## Default nearest neighbor calculated propensity score matching ####
# Use pre-treatment period only
# Run matching (1:1 nearest neighbor propensity score matching, no replacement)
match_out <- matchit(
  ps_formula,
  data = voters_elderly_two_data_2017,
  replace = FALSE
)
## Love plot for balance (stars for standardized mean differences (continuous variables are standardized automatically))
love.plot(match_out, drop.distance = TRUE, stars = 'std') 
## alternative plot
plot(summary(match_out, interactions = F),var.order = "unmatched")
# Extract matched data and filter full data (pre and post) to matched units only
matched_ids<-match.data(match_out)$LALVOTERID
matched_panel <- voters_elderly_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')%>%
  # Remove any matched pairs missing an observation in one year
  group_by(subclass)%>%
  filter(n()==4)
# Remove 2017 dataframe
rm(voters_parents_two_data_2017)

### T-test to examine baseline voting frequency ####
test_year=2019
t_test_data<-matched_panel%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ elder_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ elder_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_elderly_retirement_vs_not_retirement_matched_t_test_",as.character(test_year),".txt")))
t_test_data<-voters_elderly_two_data%>%
  filter(year==test_year)
t.test(data=t_test_data, voted ~ elder_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ elder_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    subpopulation,"_elderly_retirement_vs_not_retirement_all_obs_t_test_",as.character(test_year),".txt")))
# ####
############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ elder_new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_elderly_retirement_vs_not_retirement_twfe_no_covars_cse_17_19_6_16_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ elder_new_poll_treated*factor(year)+Voters_Gender + Voters_Age + 
                   Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
                   Residence_Families_HHCount+known_religious+
                   CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
                   years_reg+Shape_Length,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    subpopulation,"_elderly_retirement_vs_not_retirement_twfe_covars_cse_17_19_6_16_26.txt")))
############


