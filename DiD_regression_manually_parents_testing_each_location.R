#Nathaniel Flemming
# 3/21/26

# Difference in difference regressions, using data created by DiD_preprocessing script
# Propensity score matching done manually

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
loc_labels_OthersettoNA<-c('Multiple Categories','Justice Location','Library',
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

### Create simplified location categories variable  ####
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

######## Generate propensity scores ####
######## Matching should only be done on pre-treatment observations
#Matching regression formula####
ps_formula <- new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+County

### Set location category being tested
treatment_location='school'


### General Tests of Effects of Changing Location
#### People who changed location vs. people who didn't ####
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
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
t_test_data<-matched_panel%>%
  filter(year==2017)
t.test(data=t_test_data, voted ~ new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "new_location_vs_not_matched_t_test_17.txt")))
t_test_data<-change_location_two_data%>%
  filter(year==2017)
t.test(data=t_test_data, voted ~ new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "new_location_vs_not_all_obs_t_test_17.txt")))
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
                                    "new_location_vs_not_twfe_covars_cse_17_19_6_7_26.txt")))
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
                                    "new_location_vs_not_twfe_covars_cse_17_19_6_7_26.txt")))
############
# #####

#### Just 2018 to 2019 ####
change_location_two_data<-two_data%>%
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

# Create fixed covariates with 2018 (pre-treatment) values
## Filter data for year 2018
change_location_two_data_2018 <- change_location_two_data %>%
  filter(year == 2018) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2018, by = "LALVOTERID", suffix = c("", "_2018"))%>%
  filter(complete.cases(.))
# Remove 2018 dataframe
rm(change_location_two_data_2018)


## Default nearest neighbor calculated propensity score matching ####
# Filter to pre-treatment period only
pre_data <- change_location_two_data%>%
  filter(year == 2018)
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
         year!=2017)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
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
                                    "new_location_vs_not_twfe_no_covars_cse_18_19_6_7_26.txt")))
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
                                    "new_location_vs_not_twfe_covars_cse_18_19_6_7_26.txt")))
############
#####

#### People who moved vs. people who didn't change ####
#Matching regression formula####
ps_formula <- moved_new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+County
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
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
                  'years_reg',
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
t.test(data=t_test_data, voted ~ moved_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ moved_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "moved_new_location_vs_not_matched_t_test_17.txt")))
t_test_data<-change_location_two_data%>%
  filter(year==2017)
t.test(data=t_test_data, voted ~ moved_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ moved_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "moved_new_location_vs_not_all_obs_t_test_17.txt")))
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
                                    "moved_new_location_vs_not_twfe_no_covars_cse_17_19_6_7_26.txt")))
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
                                    "moved_new_location_vs_not_twfe_covars_cse_17_19_6_7_26.txt")))
############
#####
#####

#### People who changed without moving vs. People who didn't change ####
#Matching regression formula####
ps_formula <- no_move_new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+County
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
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
                                    "no_move_new_location_vs_not_matched_t_test_17.txt")))
t_test_data<-change_location_two_data%>%
  filter(year==2017)
t.test(data=t_test_data, voted ~ no_move_new_poll_treated)
chars <- capture.output(print(t.test(data=t_test_data, voted ~ no_move_new_poll_treated)))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/T tests/",
                                    "no_move_new_location_vs_not_all_obs_t_test_17.txt")))
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
                                    "no_move_new_location_vs_not_twfe_no_covars_cse_17_19_6_7_26.txt")))
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
                                    "no_move_new_location_vs_not_twfe_covars_cse_17_19_6_7_26.txt")))
############
# ####

#### Just 2018 to 2019 ####
change_location_two_data<-two_data%>%
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

# Create fixed covariates with 2018 (pre-treatment) values
## Filter data for year 2018
change_location_two_data_2018 <- change_location_two_data %>%
  filter(year == 2018) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2018, by = "LALVOTERID", suffix = c("", "_2018"))%>%
  filter(complete.cases(.))
# Remove 2018 dataframe
rm(change_location_two_data_2018)


## Default nearest neighbor calculated propensity score matching ####
# Filter to pre-treatment period only
pre_data <- change_location_two_data%>%
  filter(year == 2018)
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
         year!=2017)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
#####
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
                                    "no_move_new_location_vs_not_twfe_no_covars_cse_18_19_6_7_26.txt")))
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
                                    "no_move_new_location_vs_not_twfe_covars_cse_18_19_6_7_26.txt")))
############
#####

#### People who changed without moving vs. People who changed by moving ####
#Matching regression formula####
ps_formula <- no_move_new_poll_vs_moved_new_poll ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+County
#### Just 2017 to 2019 ####
change_location_two_data<-two_data%>%
  group_by(LALVOTERID)%>%
  # remove voters who didn't change location
  filter(!any((changed_poll_loc==0)&(year==2019)))%>%
  mutate(
    # Whether new polling location in 2019 without moving or not
    no_move_new_poll_vs_moved_new_poll = ifelse(
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
                  'no_move_new_poll_vs_moved_new_poll',
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

############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ no_move_new_poll_vs_moved_new_poll*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "no_move_new_location_vs_moved_new_location_twfe_no_covars_cse_17_19_6_7_26.txt")))
#binomial model w/ covariates
base.fit2 <- glm(voted ~ no_move_new_poll_vs_moved_new_poll*factor(year)+Voters_Gender + Voters_Age + 
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
                                    "no_move_new_location_vs_moved_new_location_twfe_covars_cse_17_19_6_7_26.txt")))
############
#####



### Parents who changed polling station to chosen category vs. not
#### Just 2019 ####
voters_parents_two_data<-two_data%>%
  # only parents
  filter(has_child==T)%>%
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
                  'years_reg',
                  'no_move_new_poll_loc','parent_new_poll_treated',
                  'General_2017_11_07','General_2018_11_06','General_2019_11_05',
                  'voted'
  )))
#####

#### Just 2019, excluding parents switching from school to school ####
voters_parents_two_data<-two_data%>%
  # only parents
  filter(has_child==T)%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  ## any() means if any row in the group fulfills the condition all rows are kept
  filter(any(no_move_new_poll_loc==T & year==2019))%>%
  mutate(
    # Whether new polling location in 2019 is a school
    parent_new_poll_treated = any((location_category_simpl==treatment_location)
                                  &(year==2019)),
    # Exclude people who switched from a school to a school
    parent_new_poll_treated = any(ifelse(
      (parent_new_poll_treated==T)
      &((location_category_simpl==treatment_location)&(year==2017)),
        F,parent_new_poll_treated)
    )
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
                  'no_move_new_poll_loc','parent_new_poll_treated',
                  'General_2017_11_07','General_2018_11_06','General_2019_11_05',
                  'voted'
  )))
#####


####

#### 2019 or 2018 if they didn't vote in 2018 #####
# Get 2018 cases
voters_parents_two_data_2018<-two_data%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  # only parents
  filter(has_child==T)%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  ## People who changed in 2018, but didn't vote in 2018
  filter(any((no_move_new_poll_loc==1) & (year==2018) & (General_2018_11_06==0)),
         ##  And didn't change poll location in 2019
         any((year==2019) & (no_move_new_poll_loc==0) & (moved_new_poll_loc==0)))%>%
  ungroup()%>%
  mutate(
    # Whether got a new polling location in 2018 and it was a school or not 
    parent_new_poll_treated = ifelse(
      ((location_category_simpl==treatment_location)&(year==2018)),T,F)
  )

# Get 2019 cases
voters_parents_two_data_2019<-two_data%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  # only parents
  filter(has_child==T)%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  filter(any(no_move_new_poll_loc==1 & year==2019))%>%
  ungroup()%>%
  # Whether got a new polling location in 2019 and it was the category of interest or not
  mutate(parent_new_poll_treated = ifelse(
    ((location_category_simpl==treatment_location)&(year==2019)),T,F))

#Combine 2018 and 2019 cases
voters_parents_two_data<-rbind(voters_parents_two_data_2018, voters_parents_two_data_2019)%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'no_move_new_poll_loc','parent_new_poll_treated',
                  'General_2017_11_07','General_2018_11_06','General_2019_11_05',
                  'voted'
  )))%>%  
  # set 2017 treatment indicator to 1 if 1 in 2018 or 2019 
  group_by(LALVOTERID)%>%
  mutate(parent_new_poll_treated = any(parent_new_poll_treated==T))%>%
  ungroup()
rm(voters_parents_two_data_2018, voters_parents_two_data_2019)
#####

# Convert treatment and outcome variable to numeric for matching function?
#voters_parents_two_data['parent_new_poll_treated'] <- sapply(voters_parents_two_data['parent_new_poll_treated'],as.numeric)
voters_parents_two_data$voted<-as.numeric(voters_parents_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
voters_parents_two_data_2017 <- voters_parents_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg')))
## Join back to the original dataset
voters_parents_two_data <- voters_parents_two_data %>%
  left_join(voters_parents_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  # mutate(
  #   Voters_Gender = Voters_Gender_2017,
  #   Voters_Age = Voters_Age_2017,
  #   Parties_Description = Parties_Description_2017,
  #   pred_race = pred_race_2017,
  #   CommercialData_EstimatedHHIncomeAmount = CommercialData_EstimatedHHIncomeAmount_2017,
  #   Residence_Families_HHCount = Residence_Families_HHCount_2017,
  #   known_religious = known_religious_2017,
  #   CommercialData_LikelyUnion = CommercialData_LikelyUnion_2017,
  #   #CommercialData_OccupationGroup = CommercialData_OccupationGroup_2017,
  #   CommercialData_OccupationIndustry = CommercialData_OccupationIndustry_2017,
  #   years_reg = years_reg_2017
  # )%>%
  # Remove voters who have missing data
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(voters_parents_two_data_2017)

#Matching regression formula
ps_formula <- parent_new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg
## Can't match on County because of perfect separation issues 
##    (only treated or untreated people in some counties)


## Default nearest neighbor calculated propensity score matching
# Filter to pre-treatment period only
pre_data <- voters_parents_two_data%>%
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
matched_panel <- voters_parents_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')

## t-test comparing outcome in post-treatment period
#t.test(voted ~ parent_new_poll_treated, data = matched_panel[matched_panel$year==2019,])

############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit1 <- glm(voted ~ parent_new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit1)
# chars <- capture.output(print(summary(base.fit1)))
# writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
#                                     "parent_",treatment_location,"_twfe_17_19_4_6_26.txt")))
## Clustered standard errors
cluster_se <- vcovCL(base.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "parent_",treatment_location,"_twfe_cse_17_19_4_6_26.txt")))
############


######## Test each location's effect, no interaction with person characteristics
unique(two_data$location_category_simpl)

#Matching regression formula####
ps_formula <- new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+County
#####

### Set location category being tested
treatment_location='government/justice'

### People of interest who changed polling station to chosen category vs. not
#### Just 2019 ####
voters_two_data<-two_data%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  ## any() means if any row in the group fulfills the condition all rows are kept
  filter(any(no_move_new_poll_loc==T & year==2019))%>%
  mutate(
    # Whether new polling location in 2019 is a location of interest
    new_poll_treated = ifelse(
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
                  'years_reg',
                  'no_move_new_poll_loc','new_poll_treated',
                  'voted'
  )))
#####


####

#### 2019 or 2018 if they didn't vote in 2018 #####
# Get 2018 cases
voters_two_data_2018<-two_data%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  ## People who changed in 2018, but didn't vote in 2018
  filter(any((no_move_new_poll_loc==1) & (year==2018) & (General_2018_11_06==0)),
         ##  And didn't change poll location in 2019
         any((year==2019) & (no_move_new_poll_loc==0) & (moved_new_poll_loc==0)))%>%
  ungroup()%>%
  mutate(
    # Whether got a new polling location in 2018 and it was a school or not 
    new_poll_treated = ifelse(
      ((location_category_simpl==treatment_location)&(year==2018)),T,F)
  )

# Get 2019 cases
voters_two_data_2019<-two_data%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  filter(any(no_move_new_poll_loc==1 & year==2019))%>%
  ungroup()%>%
  # Whether got a new polling location in 2019 and it was the category of interest or not
  mutate(new_poll_treated = ifelse(
    ((location_category_simpl==treatment_location)&(year==2019)),T,F))

#Combine 2018 and 2019 cases
voters_two_data<-rbind(voters_two_data_2018, voters_two_data_2019)%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'no_move_new_poll_loc','new_poll_treated',
                  'voted'
  )))%>%  
  # set 2017 treatment indicator to 1 if 1 in 2018 or 2019 
  group_by(LALVOTERID)%>%
  mutate(new_poll_treated = any(new_poll_treated==T))%>%
  ungroup()
rm(voters_two_data_2018, voters_two_data_2019)
#####

# Convert treatment and outcome variable to numeric for matching function?
voters_two_data$voted<-as.numeric(voters_two_data$voted)

# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
voters_two_data_2017 <- voters_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg')))
## Join back to the original dataset
voters_two_data <- voters_two_data %>%
  left_join(voters_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(voters_two_data_2017)


## Default nearest neighbor calculated propensity score matching
# Filter to pre-treatment period only
pre_data <- voters_two_data%>%
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
matched_panel <- voters_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract cluster/pair ids for robust errors later
## Matched data object
match_out_data<-match.data(match_out)
## voter id and clusterid
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
# Add back into model data
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')


############# Run two way fixed effects ##########
#binomial model w/o covariates (covariates not necessarily needed if balance is good enough)
base.fit2 <- glm(voted ~ new_poll_treated*factor(year),
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit2)
# chars <- capture.output(print(summary(base.fit1)))
# writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
#                                     treatment_location,"_twfe_17_19_4_6_26.txt")))
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    treatment_location,"_twfe_cse_17_19_4_30_26.txt")))
############

######## Reverse treatment to changing location category from
########    a school/location of interest
######## Comparing parents to parents #####

### Set location category being tested
treatment_location='school'

#### Just 2019 #####
voters_parents_two_data<-two_data%>%
  # only parents
  filter(has_child==T)%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  filter(any(no_move_new_poll_loc==1 & year==2019))%>%
  mutate(
    # Whether old polling location in 2017 was a school/location category of interest...
    #   ...and new polling location in 2019 isn't a school/location category of interest
    parent_new_poll_treated = (
      any((location_category_simpl!=treatment_location)&(year==2019))
      & any((location_category_simpl==treatment_location)&(year==2017)))
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
                  'no_move_new_poll_loc','parent_new_poll_treated',
                  'General_2017_11_07','General_2018_11_06','General_2019_11_05',
                  'voted'
  )))
#####

#### 2019 or 2018 if they didn't vote in 2018 #####
# Get 2018 cases
voters_parents_two_data_2018<-two_data%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  # only parents
  filter(has_child==T)%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  ## People who changed in 2018, but didn't vote in 2018
  filter(any((no_move_new_poll_loc==1) & (year==2018) & (General_2018_11_06==0)),
         ##  And didn't change poll location in 2019
         any((year==2019) & (no_move_new_poll_loc==0) & (moved_new_poll_loc==0)))%>%
  mutate(
    # Whether old polling location in 2017 was a school/location category of interest...
    #   ...and new polling location in 2019 isn't a school/location category of interest
    parent_new_poll_treated = ifelse(
      any((location_category_simpl!=treatment_location)&(year==2018))
      & any((location_category_simpl==treatment_location)&(year==2017)),T,F)
  )%>%
  ungroup()

# Get 2019 cases
voters_parents_two_data_2019<-two_data%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  # only parents
  filter(has_child==T)%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  filter(any(no_move_new_poll_loc==1 & year==2019))%>%
  mutate(    
    # Whether old polling location in 2017 was a school/location category of interest...
    #   ...and new polling location in 2019 isn't a school/location category of interest
    parent_new_poll_treated = (
      any((location_category_simpl!=treatment_location)&(year==2019))
      & any((location_category_simpl==treatment_location)&(year==2017)))
  )%>%
  ungroup()

#Combine 2018 and 2019 cases
voters_parents_two_data<-rbind(voters_parents_two_data_2018, voters_parents_two_data_2019)%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'no_move_new_poll_loc','parent_new_poll_treated',
                  #'General_2017_11_07','General_2018_11_06','General_2019_11_05',
                  'voted'
  )))%>%  
  # set 2017 treatment indicator to 1 if 1 in 2018 or 2019 
  group_by(LALVOTERID)%>%
  mutate(parent_new_poll_treated = any(parent_new_poll_treated==T))%>%
  ungroup()
rm(voters_parents_two_data_2018, voters_parents_two_data_2019)
#####

# Convert treatment and outcome variable to numeric?
#voters_parents_two_data['parent_new_poll_treated'] <- sapply(voters_parents_two_data['parent_new_poll_treated'],as.numeric)
voters_parents_two_data$voted<-as.numeric(voters_parents_two_data$voted)
# Remove missing data
voters_parents_two_data<-voters_parents_two_data%>%
  filter(complete.cases(.))

## Default nearest neighbor calculated propensity score matching
# Filter to pre-treatment period only
pre_data <- voters_parents_two_data%>%
  filter(year == 2017)
## Look for perfect separation
### Cross tabs of outcome (voting) and variables
for (col in names(pre_data)) {
  if (col != "parent_new_poll_treated") {
    #concatenate and print column name with --- and line breaks
    cat("\n---", col, "---\n")
    # print crosstab
    print(table(pre_data$parent_new_poll_treated, pre_data[[col]]))
  }
}
### Drop any categories/levels with perfect separation?
###    And bin numeric variables to prevent perfect separation ####
# Define breaks
age_breaks    <- c(seq(18,99,10))
reg_breaks    <- c(seq(1,76,5))
income_breaks <- seq(6000, 260000, 10000)

# Compute midpoints for each set of breaks
midpoints <- function(breaks) {
  ## add each bin to the next and divide by two
  (breaks[-length(breaks)] + breaks[-1]) / 2
}
age_mids    <- midpoints(age_breaks)
reg_mids    <- midpoints(reg_breaks)
income_mids <- midpoints(income_breaks)

pre_data<-pre_data%>%
  # bin numerics, but keep numeric and give them the middle value of the bin
  ## findInterval bins, but turns the bin number into a numeric instead of factor
  mutate(Voters_Age = age_mids[findInterval(Voters_Age, age_breaks)],
         # Separate 0 years registered from additional years, by starting at 1
         years_reg  = ifelse(years_reg==0,0,reg_mids[findInterval(years_reg, reg_breaks)]),
         CommercialData_EstimatedHHIncomeAmount  = income_mids[
             findInterval(CommercialData_EstimatedHHIncomeAmount, income_breaks)]
         )%>%
  # remove rows with separating levels
  filter(CommercialData_OccupationIndustry%!in%c('Civil Servant','Creative Arts',
                                                 'Maintenance Services',
                                                 'Scientific'))
#print(table(test$parent_new_poll_treated, test[['years_reg']]))
#####
# Run matching (1:1 nearest neighbor propensity score matching, no replacement) ####
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
matched_panel <- voters_parents_two_data%>%
  filter(LALVOTERID %in% matched_ids,
         year!=2018)
# Extract and re-add cluster/pair ids for robust errors
match_out_data<-match.data(match_out)
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
############

############# Run two way fixed effects ##########
#binomial model w/ county fixed effects since couldn't match on county due to separation
reverse.fit1 <- glm(voted ~ parent_new_poll_treated*factor(year)+County,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(reverse.fit1)
## Clustered standard errors
cluster_se <- vcovCL(reverse.fit1, cluster = ~ subclass)
summary_clustered <- coeftest(reverse.fit1, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "parent_leave_",treatment_location,"_twfe_cse_17_1819_4_6_26.txt")))
############


######## Reverse treatment to changing location category from
########    a school/location of interest 
######### treatment = being a parent, 'activated' by change in location ####
### Set location category being tested
treatment_location='school'

#### Just 2019 #####
voters_parents_two_data<-two_data%>%
  filter(year!=2018)%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                'Parties_Description', 
                'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                'Residence_Families_HHCount','known_religious', 
                'CommercialData_LikelyUnion', 
                #'CommercialData_OccupationGroup',
                'CommercialData_OccupationIndustry',
                'years_reg',
                'no_move_new_poll_loc',
                'voted','location_category_simpl','has_child'
                )))%>%
  # Remove missing data
  filter(complete.cases(.))%>%
  # Create treatment variable
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  # ... and who's old polling location in 2017 was a school/location category of interest...
  #   ...and new polling location in 2019 isn't a school/location category of interest
  filter(any(no_move_new_poll_loc==1 & year==2019),
         (any((location_category_simpl!=treatment_location)&(year==2019))
         & any((location_category_simpl==treatment_location)&(year==2017))))%>%
  # Being a parent (in 2017? 2017 and 2019?) as the treatment
  mutate(parent_treatment = any((has_child==T) & (year==2017)))%>%
  ungroup()
#####

#### 2019 or 2018 if they didn't vote in 2018 #####
# Get 2018 cases
voters_parents_two_data_2018<-two_data%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'no_move_new_poll_loc','moved_new_poll_loc','General_2018_11_06',
                  'voted','location_category_simpl','has_child'
  )))%>%
  # Remove missing data
  filter(complete.cases(.))%>%
  # Create treatment variable
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  ## People who changed in 2018, but didn't vote in 2018
  filter(any((no_move_new_poll_loc==1) & (year==2018) & (General_2018_11_06==0)),
         ##  And didn't change poll location in 2019
         any((year==2019) & (no_move_new_poll_loc==0) & (moved_new_poll_loc==0)))%>%
  # only people who have changed polling location without moving after 201X
  # ... and who's old polling location in 2018 was a school/location category of interest...
  #   ...and new polling location in 2019 isn't a school/location category of interest
  filter((any((location_category_simpl!=treatment_location)&(year==2018))
          & any((location_category_simpl==treatment_location)&(year==2017))))%>%
  # Being a parent (in 2017? 2017 and 2019?) as the treatment
  mutate(parent_treatment = any((has_child==T) & (year==2017)))%>%
  ungroup()%>%
  select(-c('moved_new_poll_loc','General_2018_11_06'))

# Get 2019 cases
voters_parents_two_data_2019<-two_data%>%
  # Registered to vote in 2019
  filter(!is.na(General_2019_11_05))%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'no_move_new_poll_loc',
                  'voted','location_category_simpl','has_child'
  )))%>%
  # Remove missing data
  filter(complete.cases(.))%>%
  group_by(LALVOTERID)%>%
  # only people who have changed polling location without moving after 201X
  # ... and who's old polling location in 2017 was a school/location category of interest...
  #   ...and new polling location in 2019 isn't a school/location category of interest
  filter(any(no_move_new_poll_loc==1 & year==2019),
         (any((location_category_simpl!=treatment_location)&(year==2019))
          & any((location_category_simpl==treatment_location)&(year==2017))))%>%
  # Being a parent (in 2017? 2017 and 2019?) as the treatment
  mutate(parent_treatment = any((has_child==T) & (year==2017)))%>%
  ungroup()

#Combine 2018 and 2019 cases
voters_parents_two_data<-rbind(voters_parents_two_data_2018, voters_parents_two_data_2019)%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'no_move_new_poll_loc','parent_treatment',
                  #'General_2017_11_07','General_2018_11_06','General_2019_11_05',
                  'voted'
  )))%>%  
  # set 2017 treatment indicator to 1 if 1 in 2018 or 2019 
  group_by(LALVOTERID)%>%
  mutate(parent_treatment = any(parent_treatment==T))%>%
  ungroup()
rm(voters_parents_two_data_2018, voters_parents_two_data_2019)
#####

# Convert outcome variable to numeric
voters_parents_two_data$voted<-as.numeric(voters_parents_two_data$voted)

## Default nearest neighbor calculated propensity score matching
# Filter to pre-treatment period only
pre_data <- voters_parents_two_data%>%
  filter(year == 2017)

#####
# Run matching (1:1 nearest neighbor propensity score matching, no replacement) ####
match_out <- matchit(
  #Matching regression formula
  parent_treatment ~  Voters_Gender + Voters_Age + 
    Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
    Residence_Families_HHCount+known_religious+
    CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
    years_reg+County,
  data = pre_data,
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
# Extract and re-add cluster/pair ids for robust errors
match_out_data<-match.data(match_out)
match_out_data<-select(match_out_data, all_of(c('LALVOTERID','subclass')))
matched_panel<-left_join(matched_panel,match_out_data,by='LALVOTERID')
############

############# Run two way fixed effects ##########
#binomial model
reverse.fit2 <- glm(voted ~ parent_treatment*factor(year),
                    data = matched_panel,
                    family = binomial(link = 'logit'))
summary(reverse.fit2)
## Clustered standard errors
cluster_se <- vcovCL(reverse.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(reverse.fit2, vcov = cluster_se)
print(summary_clustered)
chars <- capture.output(print(summary_clustered))
writeLines(chars, con = file(paste0("C:/Users/natha/Desktop/Polling Places DiD/second_submission_diff_in_diffs/TWFE model tests/",
                                    "leave_",treatment_location,"_parent_treatment_twfe_cse_17_1819_4_16_26.txt")))
############

















#binomial model w/ only naturally fixed covariates
## check if any are naturally static (besides ones that should be/we know are)
check_static<-matched_panel%>%
  group_by(LALVOTERID)%>%
  mutate(Parties_Description=sum(first(Parties_Description)!=last(Parties_Description)),
            pred_race=sum(first(pred_race)!=last(pred_race)),
            Residence_Families_HHCount=sum(first(Residence_Families_HHCount)!=last(Residence_Families_HHCount)),
            known_religious=sum(first(known_religious)!=last(known_religious)),
            CommercialData_LikelyUnion=sum(first(CommercialData_LikelyUnion)!=last(CommercialData_LikelyUnion)),
            CommercialData_OccupationIndustry=sum(first(CommercialData_OccupationIndustry)!=last(CommercialData_OccupationIndustry)))%>%
  ungroup()%>%
  summarize(Parties_Description=sum(Parties_Description),
            pred_race=sum(pred_race),
            Residence_Families_HHCount=sum(Residence_Families_HHCount),
            known_religious=sum(known_religious),
            CommercialData_LikelyUnion=sum(CommercialData_LikelyUnion),
            CommercialData_OccupationIndustry=sum(CommercialData_OccupationIndustry),)

base.fit2 <- glm(voted~parent_new_poll_treated*year + Voters_Gender + 
                   pred_race_2017+County,
                 data = matched_panel,
                 family = binomial(link = 'logit'))

summary(base.fit2)
## Clustered standard errors
cluster_se <- vcovCL(base.fit2, cluster = ~ subclass)
summary_clustered <- coeftest(base.fit2, vcov = cluster_se)
print(summary_clustered)
# avg_comparisons(base.fit1,
#                 variables = "parent_new_poll_treated",
#                 vcov = ~subclass,
#                 newdata = subset(parent_new_poll_treated == 1))

#binomial model w/ all covariates artificially fixed at pre-treatment values
base.fit3 <- glm(glm_formula_fake,
                 data = matched_panel,
                 family = binomial(link = 'logit'))
summary(base.fit3)
avg_comparisons(base.fit3,
                variables = "parent_new_poll_treated",
                vcov = ~subclass,
                newdata = subset(parent_new_poll_treated == 1))
