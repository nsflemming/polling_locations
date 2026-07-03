#Nathaniel Flemming
# 6/14/26

# Compare the demographics of groups that had their poll location change to those that
#  didn't, changed b/c they moved, etc.

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
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots/second_submission_demog_comparison"
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

######## Matching should only be done on pre-treatment observations
#Matching regression formula####
ps_formula <- new_poll_treated ~  Voters_Gender + Voters_Age + 
  Parties_Description+pred_race+CommercialData_EstimatedHHIncomeAmount+
  Residence_Families_HHCount+known_religious+
  CommercialData_LikelyUnion+CommercialData_OccupationIndustry+
  years_reg+County

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


# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  'CommercialData_OccupationIndustry',
                  'years_reg')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)


### Compare distributions for each matching variable ####
# County ####
change_location_two_data%>%
  group_by(year,County,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(County,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_county_comparison_plot.png'),width=10,height=6)

# Gender ####
change_location_two_data%>%
  group_by(year,Voters_Gender,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(Voters_Gender,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_gender_comparison_plot.png'),width=10,height=6)

# Age ####
change_location_two_data%>%
  group_by(year,Voters_Age,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(Voters_Age,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_Voters_Age_comparison_plot.png'),width=10,height=6)

# Party  ####
change_location_two_data%>%
  group_by(year,Parties_Description ,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(Parties_Description ,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_Parties_Description _comparison_plot.png'),width=10,height=6)

# Race  ####
change_location_two_data%>%
  group_by(year,pred_race ,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(pred_race ,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_pred_race_comparison_plot.png'),width=10,height=6)

# Income  ####
change_location_two_data%>%
  mutate(income_bin=cut_interval(CommercialData_EstimatedHHIncomeAmount, n = 50))%>%
  group_by(year,income_bin,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(income_bin,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_CommercialData_EstimatedHHIncomeAmount _comparison_plot.png'),width=10,height=6)

# Household count  ####
change_location_two_data%>%
  group_by(year,Residence_Families_HHCount,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(Residence_Families_HHCount,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_Residence_Families_HHCount_comparison_plot.png'),width=10,height=6)

# Religious  ####
change_location_two_data%>%
  group_by(year,known_religious,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(known_religious,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_known_religious_comparison_plot.png'),width=10,height=6)

# Union membership  ####
change_location_two_data%>%
  group_by(year,CommercialData_LikelyUnion,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(CommercialData_LikelyUnion,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_CommercialData_LikelyUnion_comparison_plot.png'),width=10,height=6)

# Occupation industry  ####
change_location_two_data%>%
  group_by(year,CommercialData_OccupationIndustry,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(CommercialData_OccupationIndustry,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_CommercialData_OccupationIndustry_comparison_plot.png'),width=10,height=6)

# Years registered  ####
change_location_two_data%>%
  group_by(year,years_reg,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(years_reg,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_years_reg_comparison_plot.png'),width=10,height=6)


### Compare unbalanced distributions after matching
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
# County ####
matched_panel%>%
  group_by(year,County,new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(County,proportion),y=proportion, fill=new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/new_poll_treated_county_matched_comparison_plot.png'),width=10,height=6)


#### People who changed location without moving vs. people who didn't change ####
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


# Create fixed covariates with 2017 (pre-treatment) values
## Filter data for year 2017
change_location_two_data_2017 <- change_location_two_data %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID','County','Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  'CommercialData_OccupationIndustry',
                  'years_reg')))
## Join back to the original dataset
change_location_two_data <- change_location_two_data %>%
  left_join(change_location_two_data_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  filter(complete.cases(.))
# Remove 2017 dataframe
rm(change_location_two_data_2017)


### Compare distributions for each matching variable ####
# County ####
change_location_two_data%>%
  group_by(year,County,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(County,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_county_comparison_plot.png'),width=10,height=6)

# Gender ####
change_location_two_data%>%
  group_by(year,Voters_Gender,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(Voters_Gender,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_gender_comparison_plot.png'),width=10,height=6)

# Age ####
change_location_two_data%>%
  group_by(year,Voters_Age,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(Voters_Age,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_Voters_Age_comparison_plot.png'),width=10,height=6)

# Party  ####
change_location_two_data%>%
  group_by(year,Parties_Description ,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(Parties_Description ,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_Parties_Description _comparison_plot.png'),width=10,height=6)

# Race  ####
change_location_two_data%>%
  group_by(year,pred_race ,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(pred_race ,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_pred_race_comparison_plot.png'),width=10,height=6)

# Income  ####
change_location_two_data%>%
  mutate(income_bin=cut_interval(CommercialData_EstimatedHHIncomeAmount, n = 50))%>%
  group_by(year,income_bin,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(income_bin,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_CommercialData_EstimatedHHIncomeAmount _comparison_plot.png'),width=10,height=6)

# Household count  ####
change_location_two_data%>%
  group_by(year,Residence_Families_HHCount,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(Residence_Families_HHCount,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_Residence_Families_HHCount_comparison_plot.png'),width=10,height=6)

# Religious  ####
change_location_two_data%>%
  group_by(year,known_religious,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(known_religious,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_known_religious_comparison_plot.png'),width=10,height=6)

# Union membership  ####
change_location_two_data%>%
  group_by(year,CommercialData_LikelyUnion,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(CommercialData_LikelyUnion,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_CommercialData_LikelyUnion_comparison_plot.png'),width=10,height=6)

# Occupation industry  ####
change_location_two_data%>%
  group_by(year,CommercialData_OccupationIndustry,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(CommercialData_OccupationIndustry,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_CommercialData_OccupationIndustry_comparison_plot.png'),width=10,height=6)

# Years registered  ####
change_location_two_data%>%
  group_by(year,years_reg,no_move_new_poll_treated)%>%
  summarize(n=n())%>%
  ungroup()%>%
  group_by(year,no_move_new_poll_treated)%>%
  mutate(total=sum(n),
         proportion=n/total)%>%
  ggplot(aes(x=reorder(years_reg,proportion),y=proportion, fill=no_move_new_poll_treated,group=year))+
  geom_bar(stat = "identity", position = "dodge")+
  facet_wrap(year ~ no_move_new_poll_treated,nrow=3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'none')
ggsave(filename=paste0(plot_dir,'/no_move_new_poll_treated_years_reg_comparison_plot.png'),width=10,height=6)



