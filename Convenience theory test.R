#Nathaniel Flemming
# 28/12/24

# LogisticRegressions for 2017 and 2019, using data created by DiD_preprocessing
## retaining voters only present for one year


library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet
library(stringr) #string manipulation
library(ggplot2) # plotting
library(gridExtra) # plot multiple graphs together
library(margins) # get average marginal effects
library(ggeffects) #plot predicted probabilities
library(dplyr)
library(clubSandwich) # Cluster robust standard errors (clustered standard errors)

########## Functions ####

## Binarize vote variables ####
binarize_vote <- function(data, vote_var, yes_vote_value){
  data[[vote_var]] <- ifelse(data[[vote_var]]==yes_vote_value,1,0)
  return(data)
}
#####
## Logistic regression ####
log_reg <- function(data, dep_var, ind_vars){
  ind_vars_coll <- paste(ind_vars, collapse = '+')
  print(ind_vars_coll)
  formula = paste0(dep_var,'~',ind_vars_coll)
  m_base <- glm(data=data, 
                formula,
                family = "binomial")
  return(m_base)
}
#####
## Logistic regression + average marginal effects ####
log_reg_plus_margins <- function(data, dep_var, ind_vars, var_of_interest, save_dir, model_name){
  ind_vars_coll <- paste(ind_vars, collapse = '+')
  #print(ind_vars_coll)
  formula = paste0(dep_var,'~',ind_vars_coll)
  m_base <- glm(data=data, 
                formula,
                family = "binomial")
  # calculate Average Marginal Effect
  base_margins<-margins(m_base, variables=var_of_interest)
  #save marginal effects
  setwd(save_dir)
  sink(file=paste0("MarginsSummary_",model_name,".txt"))
  print(summary(base_margins))
  sink()
  # return model
  return(m_base)
}
#####
## Save model result summary ####
write_summ <- function(results_dir, model_name, model){
  setwd(results_dir)
  sink(file=paste0("RegressionSummary_", model_name,".txt"))
  print(summary(model))
  sink()
}
#####
## Logistic regressions for a list of interactions #####
log_reg_inter<-function(df,dep_var='General_2018_11_06', interaction_terms, 
                        ind_vars=c('Voters_Gender', 'Voters_Age',
                                   'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
                                   'known_religious','CommercialData_LikelyUnion', 
                                   'CommercialData_OccupationIndustry'),
                        results_dir){
  for(interaction in interaction_terms){
    print(interaction)
    ind_vars_list<-c(interaction,ind_vars)
    # model
    model<-log_reg(df, dep_var, ind_vars_list)
    summary(model)
    ## get first variable in interaction term
    first_term<-str_extract(interaction, ".*(?=\\*)")
    second_term<-str_extract(interaction, "(?<=\\*).*")
    ## predict
    model_pred<-predict_response(model, terms=c(first_term,second_term), 
                                 margin='marginalmeans')
    #save results
    write_summ(results_dir, paste0(first_term,'x',second_term,
                                   substr(dep_var,9,12),'_summary'), model)
  }
}
#####
## Logistic regressions for a list of interactions plus plotting predicted probabilities ####
log_reg_inter_plus_plot<-function(df,dep_var='General_2018_11_06', interaction_terms, 
                                  ind_vars=c('Voters_Gender', 'Voters_Age',
                                             'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
                                             'known_religious','CommercialData_LikelyUnion', 
                                             'CommercialData_OccupationIndustry'),
                                  turnout,
                                  x_label, x_axis_labels = c('FALSE','TRUE'), loc_dict, var_dict,
                                  vjust_value=0, hjust_value=0, image_name,
                                  results_dir, image_dir){
  for(interaction in interaction_terms){
    print(interaction)
    ind_vars_list<-c(interaction,ind_vars)
    # model
    model<-log_reg(df, dep_var, ind_vars_list)
    summary(model)
    # calculate predicted probabilities
    ## get first variable in interaction term
    first_term<-str_extract(interaction, ".*(?=\\*)")
    second_term<-str_extract(interaction, "(?<=\\*).*")
    ## predict
    model_pred<-predict_response(model, terms=c(first_term,second_term), 
                                 margin='marginalmeans')
    #save results
    write_summ(results_dir, paste0(first_term,'x',second_term,
                                   substr(dep_var,9,12),'_summary','_',other_cond), model)
    # Generate plot details
    plot_title=paste0(substr(dep_var,9,12),' Probability of Voting for ', 
                      var_dict[first_term][[1]], ' at ', loc_dict[second_term][[1]])
    legend_title = loc_dict[second_term][[1]]
    pred_prob_plot(model_data=df, dep_var=dep_var, model_pred, mean_vote=turnout, 
                   plot_title=plot_title, dodge=0.8, xlab=x_label, 
                   x_axis_labels = x_axis_labels, ylab='Predicted Probability of Voting', 
                   legend_exist = T, legend_title = legend_title, angle = 0, 
                   vjust_value = vjust_value, hjust_value=hjust_value, legend_position='right', 
                   output_dir=image_dir, image_name=image_name)
  }
}
#####
## Logistic regressions for a list of interactions + marginal effects ####
log_reg_inter_plus_margins<-function(df,dep_var='General_2018_11_06', interaction_terms, 
                                     ind_vars=c('Voters_Gender', 'Voters_Age',
                                                'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
                                                'known_religious','CommercialData_LikelyUnion', 
                                                'CommercialData_OccupationIndustry'),
                                     var_of_int, save_dir){
  for(interaction in interaction_terms){
    print(interaction)
    ind_vars_list<-c(interaction,ind_vars)
    # model
    model<-log_reg(df, dep_var, ind_vars_list)
    summary(model)
    # calculate Average Marginal Effect
    base_margins<-margins(model, variables = var_of_int)
    #save marginal effects
    setwd(save_dir)
    sink(file=paste0("AMEMarginsSummary_",paste0(substr(interaction,1,11),'x',substr(interaction,13,100)),".txt")) #make dynamic
    print(summary(base_margins))
    sink()
    #save results
    write_summ(results_dir, paste0(substr(interaction,1,11),'x',substr(interaction,13,100),'_AMEsummary'), model)
  }
}
#####
#plot predicted probabilities ####
pred_prob_plot<-function(model_data, dep_var, pred_probs_obj, dodge=0.8, mean_vote,
                         plot_title, 
                         xlab, #x_axis_labels, 
                         ylab='Predicted Probability of Voting',
                         legend_exist=TRUE, legend_title, angle=0, 
                         hjust_value=0, vjust_value=0,
                         legend_position='right', output_dir, image_name){
  g<-ggplot(pred_probs_obj, aes(x, predicted, color=group))+
    geom_point(size=3, position=position_dodge(dodge), show.legend = legend_exist)+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width=0.2, linewidth=1, position=position_dodge(dodge), 
                  show.legend = legend_exist)+
    # draw line at mean voting rate
    geom_hline(aes(yintercept = mean_vote, linetype=''), color='black')+
    #scale_linetype_manual(name = "Mean Voting Rate", values=1, guide = guide_legend(override.aes = list(color = c("black"))))+
    labs(title = plot_title,
         x = xlab,
         y = ylab) +
    scale_color_discrete(name = legend_title)+
    scale_y_continuous(labels = scales::percent)+
    #scale_x_discrete(labels= x_axis_labels)+
    theme(plot.title = element_text(size=20, face='bold'),
          axis.title = element_text(size=15),
          axis.text.y = element_text(size=17),
          axis.text.x = element_text(size=15, angle=angle,hjust = hjust_value,
                                     vjust = vjust_value),
          legend.title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.key.size = unit(1, 'cm'),
          legend.position = legend_position)
  # Save plot
  setwd(output_dir)
  ggsave(file=paste0(image_name,'.png'), device='png', width=3000, height=2000, units='px', g) #saves g
}
#####
#blank plot predicted probabilities ####
blank_pred_prob_plot<-function(model_data, dep_var, pred_probs_obj, dodge=0.8, mean_vote,
                               xmin,xmax,ymin,ymax,
                               plot_title, 
                               xlab, x_axis_labels, ylab='Predicted Probability of Voting',
                               legend_exist=TRUE, legend_title, angle=0, 
                               hjust_value=0, vjust_value=0,
                               legend_position='right', output_dir, image_name){
  g<-ggplot(pred_probs_obj, aes(x, predicted, color=group))+
    ylim(ymin,ymax)+
    labs(title = plot_title,
         x = xlab,
         y = ylab) +
    scale_color_discrete(name = legend_title)+
    scale_y_continuous(labels = scales::percent)+
    scale_x_discrete(labels= x_axis_labels)+
    theme(plot.title = element_text(size=20, face='bold'),
          axis.title = element_text(size=15),
          axis.text.y = element_text(size=17),
          axis.text.x = element_text(size=15, angle=angle,hjust = hjust_value,
                                     vjust = vjust_value),
          legend.title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.key.size = unit(1, 'cm'),
          legend.position = legend_position)
  # Save plot
  setwd(output_dir)
  ggsave(file=paste0(image_name,'.png'), device='png', width=3000, height=2000, units='px', g) #saves g
}
#####
########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/second_submission_logistic_regressions"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots/second_submission_logistic_regressions"
# read in data
model_data<-read.csv(paste0(data_dir,'/DiD_prepped_poll_vote_16to19_no_rndm_race.csv'))

### Subsample for testing
# model_data<-raw_data%>%sample_n(500000)

## Recode extraneous parties to 'other'  ####
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
## Create vector of location category labels ####
loc_labels_NAsettoOther<-c('Other','Justice Location','Library','Multiple Categories',
                           'Public Location','Public/Justice Location','Religious Location',
                           'Religious School','School')
loc_labels_OthersettoNA<-c('Multiple Categories','Justice Location','Library',
                           'Public Location','Public/Justice Location','Religious Location',
                           'Religious School','School')
#####
## Create dictionary of location labels####
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
## Create simplified location categories variable ####
###(subsume catholic into religious)
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

## Common set of covariates####
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
#####
## set how 'other category is treated (All NA should be coded now we've done it manually) ####
other_cond='NASettoOther'
loc_labels=loc_labels_NAsettoOther
## recode missing to other
model_data$location_category[is.na(model_data$location_category)]<-'other'
model_data$location_category_simpl[is.na(model_data$location_category_simpl)]<-'other'
#####
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

### Create outcome indicator (voted in general election) and years registered####
model_data<-model_data%>%
  # group by voter
  group_by(LALVOTERID, year)%>%
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
#####


### Set location category being tested
convenient_location='religious'


##### Probability of Voting, If they changed from a ____ to anything else
### Set location category being tested
convenient_location='religious'
### Set election year
electionyear=c('2018')
### Create treatment variable
convenience_data<-model_data%>%
  group_by(LALVOTERID)%>%
  # Order by year to make sure lag() works properly
  arrange(year)%>%
  # only people who have changed polling location without moving
  ## any() means if any row in the group fulfills the condition all rows are kept
  filter(any(no_move_new_poll_loc==T),
         n()>1)%>%
  mutate(
    change_from_convenient_to_non = ifelse(
      # Whether new polling location isn't a convenient location
      ((location_category_simpl!=convenient_location)
       # And old location was convenient
       &(lag(location_category_simpl)==convenient_location)),T,F)
  )%>%
  #Drop first row for each voter, since no previous year for comparison
  slice(2:n())%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'no_move_new_poll_loc','change_from_convenient_to_non',
                  'voted','has_child','location_category_simpl',common_covars
  )))

### Create formula
ind_vars_convenient_loc <-c(
  # var of interest
  'change_from_convenient_to_non',
  common_covars,'County'
)
# model
## Convert variables to factor for predicted probabilities function later
cols<-c('year','voted','change_from_convenient_to_non','County')
convenience_data[cols]<-lapply(convenience_data[cols], factor)
## Filter to selected year(s)?
convenience_data<-convenience_data%>%
  filter(year==electionyear)
m_convt<-log_reg(convenience_data, 'voted', ind_vars_convenient_loc)
summary(m_convt)

#save results
write_summ(results_dir, paste0('convenience_allvoters_school_',electionyear), m_convt)

##### Probability of Voting, If has/lacks child and they changed from a ____
#####   to a different type of polling location (convenience theory test) #####
### Selection election year (don't do multiple years b/c same people in both years)
electionyear=c('2018')
convenience_data<-model_data%>%
  group_by(LALVOTERID)%>%
  # Order by year to make sure lag() works properly
  arrange(year)%>%
  # only people who have changed polling location without moving
  ## any() means if any row in the group fulfills the condition all rows are kept
  filter(any(no_move_new_poll_loc==T),
         n()>1)%>%
  mutate(
    change_from_convenient_to_non = ifelse(
      # Whether new polling location isn't a convenient location
      ((location_category_simpl!=convenient_location)
          # And old location was convenient
          &(lag(location_category_simpl)==convenient_location)),T,F)
  )%>%
  #Drop first row for each voter, since no previous year for comparison
  slice(2:n())%>%
  ungroup()%>%
  select(all_of(c('LALVOTERID','year','County','Voters_Gender', 'Voters_Age', 
                  'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'CommercialData_OccupationIndustry',
                  'years_reg',
                  'no_move_new_poll_loc','change_from_convenient_to_non',
                  'voted','has_child','location_category_simpl',common_covars
  )))

### Subsample for testing
# test_data<-convenience_data%>%
#   sample_n(100000)
# test_data$year<-factor(test_data$year)
### Create interaction effect formula
ind_vars_convenient_loc <-c(
  # var of interest
  'has_child*change_from_convenient_to_non',
  common_covars
)
# model
## Convert variables to factor for predicted probabilities function later
cols<-c('year','voted','change_from_convenient_to_non','County')
convenience_data[cols]<-lapply(convenience_data[cols], factor)
## Filter to selected year(s)?
convenience_data<-convenience_data%>%
  filter(year==electionyear)
m_convt<-log_reg(convenience_data, 'voted', ind_vars_convenient_loc)
summary(m_convt)

#save results
write_summ(results_dir, paste0('convenience_school_',electionyear), m_convt)

## Calculate and plot predicted probabilities
convt_pred_new<-predict_response(m_convt, terms=c('has_child','change_from_convenient_to_non'), 
                                 margin='marginalmeans')
#plot predicted probabilities
mean_turnout = sum(model_data$voted==T,na.rm=T)/nrow(model_data)
pred_prob_plot(model_data=model_data, dep_var=voted, convt_pred_new, mean_vote = mean_turnout,
               plot_title = paste0(electionyear,' Probability of Voting of (Non-)Parents \nafter Being Switched from School to Non-School Locations'),
               xlab='Has a Child/Children', 
               #x_axis_labels = c('FALSE', 'TRUE'),
               legend_exist=T, legend_title ='Switched from School to Non-School', angle=0,legend_position = 'right',
               output_dir = plot_dir, 
               image_name = paste0('Pred_Prob_child_convenience_school_',electionyear))
#####

