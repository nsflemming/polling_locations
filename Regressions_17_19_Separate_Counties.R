#Nathaniel Flemming
# 6/18/24

# LogisticRegressions for 2017 and 2019, using data created by DiD_preprocessing
## retaining voters only present for one year
# Running seperate regressions for Allegheny (Pittsburgh) and Philadelphia vs the Rest of the state

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
library(lmtest) #likelihood ratio test for interaction effect significance

########## Functions ####

## Binarize vote variables
binarize_vote <- function(data, vote_var, yes_vote_value){
  data[[vote_var]] <- ifelse(data[[vote_var]]==yes_vote_value,1,0)
  return(data)
}

## Logistic regression
log_reg <- function(data, dep_var, ind_vars){
  ind_vars_coll <- paste(ind_vars, collapse = '+')
  print(ind_vars_coll)
  formula = paste0(dep_var,'~',ind_vars_coll)
  m_base <- glm(data=data, 
                formula,
                family = "binomial")
  return(m_base)
}

## Logistic regression + average marginal effects
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

## Save model result summary
write_summ <- function(results_dir, model_name, model){
  setwd(results_dir)
  sink(file=paste0("RegressionSummary_", model_name,".txt"))
  print(summary(model))
  sink()
}

## Logistic regressions for a list of interactions
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
                         hjust_value=0.5, 
                         vjust_value=0,
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
#plot CIs around interaction's marginal effects ####

interact_CI_plot<-function(model_obj, coef1_var_loc, coef2_var_loc, interact_var_loc,
                           var1_labels = c('label missing','label missing','label missing'),
                           plot_title = 'missing plot title', 
                           xlab = '', x_axis_labels = c('x=1,z=0','x=0,z=1','x=1,z=1'), 
                           ylab='Effect on Log Odds of Voting',
                           legend_exist=TRUE, legend_title='missing legend title',
                           legend_position='right'){
  ### Calculate standard error at each value of Z (retirement home voting)
  # Effect of X at z = 0 and z = 1
  # estimated variance around that effect at that value of Z (V(Δy/Δx))
  #	The t-statistic for testing whether this estimate is distinguishable from 0 
  #  is found by dividing the estimated effect Δy/Δx by the estimated standard error 
  #  of Δy/Δx and evaluating the result against the t-distribution with n-k degrees 
  #  of freedom (n observations, k regressors)
  ## Covariance of the coefficient estimates
  cov_matrix<-vcov(model_obj)
  covar = cov_matrix[interact_var_loc,coef1_var_loc]
  ## Variance of the coefficient estimates
  coef1_var = cov_matrix[coef1_var_loc,coef1_var_loc]
  coef2_var = cov_matrix[coef2_var_loc,coef2_var_loc]
  interact_var = cov_matrix[interact_var_loc,interact_var_loc]
  
  ##Compute confidence interval on marginal effect
  ### Variance at x=0, Z = 1 (e.g. not elderly, retirement home)
  coef2_std_err = sqrt(coef2_var)
  ### Variance at Z = 0 (e.g. not retirement home)
  coef1_std_err = sqrt(coef1_var)
  ### Variance at Z = 1 (e.g. elderly, retirement home)
  var_z1 = coef1_var+(1^2)*interact_var+2*1*covar
  std_errz1 = sqrt(var_z1)
  
  #Plot CI around effects
  ## Order effects
  xs<-c(1,2,3)
  ##Variable values (e.g. '<65 Yrs Old','65 Yrs Old','65 Yrs Old')
  zs<-var1_labels
  ## Effect estimates
  effects<-c(model_obj$coefficients[[coef2_var_loc]],
             model_obj$coefficients[[coef1_var_loc]],
             model_obj$coefficients[[interact_var_loc]])
  conf.low<-c(model_obj$coefficients[[coef2_var_loc]]-1.96*coef2_std_err,
              model_obj$coefficients[[coef1_var_loc]]-1.96*coef1_std_err,
              model_obj$coefficients[[interact_var_loc]]-1.96*std_errz1)
  conf.high<-c(model_obj$coefficients[[coef2_var_loc]]+1.96*coef2_std_err,
               model_obj$coefficients[[coef1_var_loc]]+1.96*coef1_std_err,
               model_obj$coefficients[[interact_var_loc]]+1.96*std_errz1)
  interact_plot<-data.frame(cbind(xs,zs,as.numeric(effects),as.numeric(conf.low),as.numeric(conf.high)))
  
  g<-ggplot(interact_plot, aes(x=as.factor(xs), y=effects, color=as.factor(zs)))+
    geom_point(size=3)+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width=0.2, linewidth=1)+
    labs(title = plot_title,
         x = xlab,
         y = ylab) +
    scale_color_discrete(name = legend_title)+
    scale_x_discrete(labels= x_axis_labels)+
    theme(plot.title = element_text(size=20, face='bold'),
          axis.title = element_text(size=15),
          axis.text.y = element_text(size=17),
          # axis.text.x = element_text(size=15, angle=angle,hjust = hjust_value,
          #                            vjust = vjust_value),
          legend.title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.key.size = unit(1, 'cm'),
          legend.position = legend_position)
  return(g)
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
### Not in function
'%!in%' <- function(x,y)!('%in%'(x,y))
########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/second_submission_logistic_regressions"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots/second_submission_logistic_regressions"
#results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/county_regression_test"
#plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots/county_regression"
# read in data
setwd(data_dir)
raw_data<-read.csv('DiD_prepped_poll_vote_16to19_no_rndm_race.csv')

model_data<-raw_data

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
## create vector of location categories
#categories<-c('pub_loc','pub_just','other','relig_loc','school','multiple',
#              'justice_loc','library','relig_school')
## Create vector of location category labels
#loc_labels_NAsettoOther<-c('Other','Justice Location','Library','Multiple Categories',
#                           'Public Location','Public/Justice Location','Religious Location',
#                           'Religious School','School')
#loc_labels_OthersettoNA<-c('Multiple Categories','Justice Location','Library',
#                           'Public Location','Public/Justice Location','Religious Location',
#                           'Religious School','School')
## Create dictionary of location labels ####
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

#################### Logistic Regression
##set dependent variable
#dep_var = 'General_2017_11_07'
#dep_var = 'General_2018_11_06'
dep_var = 'General_2019_11_05'
year=substr(dep_var,9,12)
year_num<-as.numeric(year)
### Filter to one year and calculate overall mean turnout for election for plotting
model_data<-model_data[model_data$year==year_num,]
model_data[[dep_var]][is.na(model_data[[dep_var]])]<-0
mean_turnout <- mean(model_data[[dep_var]], na.rm=T)

## Calculate years registered based on dependent variable year
model_data$years_reg<-year_num-as.numeric(model_data$year_reg)

## Common set of covariates ####
common_covars <-c(
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'pred_race',
  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
  'known_religious','CommercialData_LikelyUnion', 
  #'CommercialData_OccupationGroup',
  'CommercialData_OccupationIndustry',
  # Other
  'Shape_Length',
  'years_reg'
)
common_covars_no_age <-c(
  # Demographics
  'Voters_Gender', 'Parties_Description',
  'pred_race',
  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
  'known_religious','CommercialData_LikelyUnion', 
  #'CommercialData_OccupationGroup',
  'CommercialData_OccupationIndustry',
  # Other
  'Shape_Length',
  'years_reg'
)
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

# location category frequencies
# loc_freq<-model_data%>%
#   group_by(location_category_simpl)%>%
#   summarise(n=n())

other_cond='NASettoOther'
## recode missing location category to other
model_data$location_category_simpl[is.na(model_data$location_category_simpl)]<-'other'
model_data$location_category[is.na(model_data$location_category)]<-'other'

## Factorize variables ####
#location categories
model_data$location_category_simpl<-as.factor(model_data$location_category_simpl)
model_data$location_category<-as.factor(model_data$location_category)
#model_data$location_category_simpl<-relevel(model_data$location_category_simpl, ref='other')
#model_data$location_category<-relevel(model_data$location_category, ref='other')
model_data$location_category_simpl<-relevel(model_data$location_category_simpl, ref='other')
model_data$location_category<-relevel(model_data$location_category, ref='other')

#race
model_data$pred_race <- as.factor(model_data$pred_race)
model_data$pred_race <- relevel(model_data$pred_race, ref = "pred.whi")
#political party
model_data$Parties_Description <- as.factor(model_data$Parties_Description)
model_data$Parties_Description <- relevel(model_data$Parties_Description, ref = "Democratic")
#religious
model_data$known_religious<-as.factor(model_data$known_religious)
#catholic
#model_data$known_catholic<-as.factor(model_data$known_catholic)
#child present
model_data$has_child<-as.factor(model_data$has_child)
#government employee
#model_data$known_gov_emp <- ifelse(model_data$CommercialData_OccupationIndustry=="Civil Servant",TRUE,FALSE)
#model_data$known_gov_emp<-as.factor(model_data$known_gov_emp)
#union member
model_data$CommercialData_LikelyUnion<-as.factor(model_data$CommercialData_LikelyUnion)
#occupational group
#model_data$CommercialData_OccupationGroup<-as.factor(model_data$CommercialData_OccupationGroup)
#model_data$CommercialData_OccupationGroup<-relevel(model_data$CommercialData_OccupationGroup, ref='Blue Collar')
# Occupational industry (more complete across years)
model_data$CommercialData_OccupationIndustry<-as.factor(model_data$CommercialData_OccupationIndustry)
model_data$CommercialData_OccupationIndustry<-relevel(model_data$CommercialData_OccupationIndustry, ref='Unknown')
#####

### Philadelphia ####

##### Probability of Voting, If has/lacks child and is voting at a school
vers='parent_school'
other_cond<-'6_21_26'
#### Set subpopulation of interest
subpopulation<-'Philadelphia'
parent_data<-model_data%>%
  filter(County == 'PHILADELPHIA')
#vars
## create school dummy
parent_data$school <- parent_data$location_category=='school' 
parent_data$school <- as.factor(parent_data$school)
## interaction
ind_vars_child_schl <-c(
  # var of interest
  'has_child*school',
  common_covars
)
# model
m_schl<-log_reg(parent_data, dep_var, ind_vars_child_schl)
summary(m_schl)

#save results
write_summ(results_dir, paste0(subpopulation,'_school_',year,'_',vers,'_',other_cond), m_schl)

## Calculate and plot CIs around interaction/main effects
g<-interact_CI_plot(m_schl, 2, 3, length(m_schl$coefficients),
                    var1_labels = c('No Child','Has Child','Has Child'),
                    plot_title = paste0('Effects of Being a (Non-)Parent & \nVoting at a School in ',year), 
                    xlab = '', x_axis_labels = c('Not Parent/School','Parent/Other','Parent/School'), 
                    ylab='Effect on Log Odds of Voting',
                    legend_exist=TRUE, legend_title='Has a Child/Children',
                    legend_position='right')
g
image_name = paste0(subpopulation,'_effect_parent_school_',year,'_',vers,'_',other_cond)
ggsave(file=paste0(plot_dir,'/',image_name,'.png'), device='png', width=3000, height=2000, units='px', g)

## Calculate and plot predicted probabilities
### 'marginal means': non-focal predictors are set to their mean or averaged over levels for factors 
###   to get a weighted average for the values at which factors are held constant.
###   Generates predictions closer to the sample. Answers 'what is the expected 
###   value of the response at meaningful levels of my focal terms for an 'average' observation in my data?'E
schl_pred<-predict_response(m_schl, terms=c('has_child','school'), margin='marginalmeans',rg.limit = 122000)
#plot predicted probabilities
pred_prob_plot(model_data=parent_data, dep_var=dep_var, schl_pred, mean_vote = mean_turnout,
               plot_title = paste0(year,' Probability of Voting of (Non-)Parents at School Locations'),
               xlab='Has a Child/Children', 
               #x_axis_labels = c('FALSE', 'TRUE'),
               legend_exist=T, legend_title ='Votes at a School', angle=0,legend_position = 'right',
               output_dir = plot_dir, 
               image_name = paste0(subpopulation,'_Pred_Prob_child_school_',year,'_',vers,'_',other_cond))

##### Probability of voting for age 65+ in voting in retirement/nursing home and not
#set vers
vers='V_elderly_test'
#### Set subpopulation of interest
subpopulation<-'Philadelphia'
elderly_data<-model_data%>%
  filter(County == 'PHILADELPHIA')%>%
  # Create elderly dummy
  mutate(elderly = as.factor(Voters_Age>64))
## create retirement home dummy
elderly_data$retircomm_nurshom <- elderly_data$location_category_simpl=='retirement community/nursing home' 
elderly_data$retircomm_nurshom <- as.factor(elderly_data$retircomm_nurshom)
# Calculate mean turnout
mean_turnout <- mean(elderly_data[[dep_var]], na.rm=T)
## interaction (remove age as a covariate?)
ind_vars_elder_retir <-c(
  # var of interest
  'elderly*retircomm_nurshom',
  common_covars_no_age
)
# model
m_elder_new<-log_reg(elderly_data, dep_var, ind_vars_elder_retir)
summary(m_elder_new)

#save results
other_cond<-'no_age_covar'
write_summ(results_dir, paste0(subpopulation,'_elder_retire-nursing',year,'_',vers,'_',other_cond), m_elder_new)

g<-interact_CI_plot(m_elder_new, 2, 3, 35,
                    var1_labels = c('<65 Yrs Old','65 Yrs Old','65 Yrs Old'),
                    plot_title = paste0('Effects of Being Elderly & Voting \nat a Retirement/Nursing Home in ',year), 
                    xlab = '', x_axis_labels = c('Not Elderly/Retirement Home','Elderly/Other','Elderly/Retirement Home'), 
                    ylab='Effect on Log Odds of Voting',
                    legend_exist=TRUE, legend_title='Age Group',
                    legend_position='right')
g
image_name = paste0(subpopulation,'_effect_elderly_retire_',year,'_',vers,'_',other_cond)
ggsave(file=paste0(plot_dir,'/',image_name,'.png'), device='png', width=3000, height=2000, units='px', g)
## Calculate and plot predicted probabilities
elder_pred_new<-predict_response(m_elder_new, terms=c('elderly','retircomm_nurshom'), margin='marginalmeans'
                                 ,rg.limit = 122000
)
#plot predicted probabilities
pred_prob_plot(model_data=elderly_data, dep_var=dep_var, elder_pred_new, mean_vote = mean_turnout,
               plot_title = paste0(year,' Probability of Voting of (Non-)Elderly at \nRetirement Communities and Nursing Homes'),
               xlab='65+', 
               #x_axis_labels = c('FALSE', 'TRUE'),
               legend_exist=T, legend_title ='Votes at a Retirement Community/Nursing Home', angle=0,legend_position = 'right',
               output_dir = plot_dir, 
               image_name = paste0(subpopulation,'_Pred_Prob_elder_retire_',year,'_',vers,'_',other_cond))








#####

##### Probability of Voting, If has/lacks child and is voting at a school
vers='parent_school'
other_cond<-'6_18_26'
#### Set subpopulation of interest
subpopulation<-'Rest_of_PA'
model_data<-model_data%>%
  filter(County %!in% c('ALLEGHENY','PHILADELPHIA'))
#vars
## create school dummy
model_data$school <- model_data$location_category=='school' 
model_data$school <- as.factor(model_data$school)
## interaction
ind_vars_child_schl <-c(
  # var of interest
  'has_child*school',
  common_covars
)
# model
m_schl<-log_reg(model_data, dep_var, ind_vars_child_schl)
summary(m_schl)
# Clustered standard errors (clustered at county level)
### Can't cluster errors with this model specification b/c of the size of the covariance matrix being too large to store in memory
#### Too many observations and/or clusters I believe
# robust_se_cluster<-coef_test(m_schl, vcov = 'CR2', cluster = model_data$County,
#                              coefs=c('has_childTRUE','schoolTRUE','has_childTRUE:schoolTRUE'))

#save results
write_summ(results_dir, paste0(subpopulation,'_school_',year,'_',vers,'_',other_cond), m_schl)

## Calculate and plot CIs around interaction/main effects
g<-interact_CI_plot(m_schl, 2, 3, length(m_schl$coefficients),
                    var1_labels = c('No Child','Has Child','Has Child'),
                    plot_title = paste0('Effects of Being a (Non-)Parent & \nVoting at a School in ',year), 
                    xlab = '', x_axis_labels = c('Not Parent/School','Parent/Other','Parent/School'), 
                    ylab='Effect on Log Odds of Voting',
                    legend_exist=TRUE, legend_title='Has a Child/Children',
                    legend_position='right')
g
image_name = paste0(subpopulation,'_effect_parent_school_',year,'_',vers,'_',other_cond)
ggsave(file=paste0(plot_dir,'/',image_name,'.png'), device='png', width=3000, height=2000, units='px', g)

## Calculate and plot predicted probabilities
other_cond<-paste0(subpopulation,'_6_18_26')
### 'marginal means': non-focal predictors are set to their mean or averaged over levels for factors 
###   to get a weighted average for the values at which factors are held constant.
###   Generates predictions closer to the sample. Answers 'what is the expected 
###   value of the response at meaningful levels of my focal terms for an 'average' observation in my data?'E
schl_pred<-predict_response(m_schl, terms=c('has_child','school'), margin='marginalmeans',rg.limit = 122000)
#plot predicted probabilities
pred_prob_plot(model_data=model_data, dep_var=dep_var, schl_pred, mean_vote = mean_turnout,
               plot_title = paste0(year,' Probability of Voting of (Non-)Parents at School Locations'),
               xlab='Has a Child/Children', 
               #x_axis_labels = c('FALSE', 'TRUE'),
               legend_exist=T, legend_title ='Votes at a School', angle=0,legend_position = 'right',
               output_dir = plot_dir, 
               image_name = paste0(subpopulation,'_Pred_Prob_child_school_',year,'_',vers,'_',other_cond))



# ##### Probability of Voting, If has/lacks child and is voting at a school and it's a new polling location for them
# ## create school dummy
# model_data$school <- model_data$location_category=='school' 
# model_data$school <- as.factor(model_data$school)
# #filter to just people who got assigned a new poll location
# model_data_new_loc<-model_data%>%
#   filter(no_move_new_poll_loc==1)
# ## interaction
# ind_vars_child_schl <-c(
#   # var of interest
#   'has_child*school',
#   common_covars
# )
# # model
# m_schl_new<-log_reg(model_data_new_loc, dep_var, ind_vars_child_schl)
# summary(m_schl_new)
# #save results
# write_summ(results_dir, paste0('school_new_loc',year,'_',vers,'_',other_cond), m_schl_new)
# ## Calculate and plot predicted probabilities
# schl_pred_new<-predict_response(m_schl_new, terms=c('has_child','school'), margin='marginalmeans')
# #plot predicted probabilities
# pred_prob_plot(model_data=model_data_new_loc, dep_var=dep_var, schl_pred_new, mean_vote = mean_turnout,
#                plot_title = paste0(year,' Probability of Voting of (Non-)Parents at School Locations\nafter Being Assigned a New Poll Location'),
#                xlab='Has a Child/Children', 
#                #x_axis_labels = c('FALSE', 'TRUE'),
#                legend_exist=T, legend_title ='Votes at a School', angle=0,legend_position = 'right',
#                output_dir = plot_dir, 
#                image_name = paste0(subpopulation,'_Pred_Prob_child_school_new_loc_',year,'_',vers,'_',other_cond))



##### Probability of voting for age 65+ in voting in retirement/nursing home and not
#set vers
vers='V_elderly_test'
## create retirement home dummy
model_data$retircomm_nurshom <- model_data$location_category=='retirement community/nursing home' 
model_data$retircomm_nurshom <- as.factor(model_data$retircomm_nurshom)
# Create elderly dummy
elderly_data<-model_data%>%
  mutate(elderly = as.factor(Voters_Age>64))
# Calculate mean turnout
mean_turnout <- mean(elderly_data[[dep_var]], na.rm=T)
## interaction (remove age as a covariate?)
ind_vars_elder_retir <-c(
  # var of interest
  'elderly*retircomm_nurshom',
  common_covars_no_age
)
# model
m_elder_new<-log_reg(elderly_data, dep_var, ind_vars_elder_retir)
summary(m_elder_new)

#save results
other_cond<-'no_age_covar'
write_summ(results_dir, paste0(subpopulation,'_elder_retire-nursing',year,'_',vers,'_',other_cond), m_elder_new)

g<-interact_CI_plot(m_elder_new, 2, 3, 35,
                    var1_labels = c('<65 Yrs Old','65 Yrs Old','65 Yrs Old'),
                    plot_title = paste0('Effects of Being Elderly & Voting \nat a Retirement/Nursing Home in ',year), 
                    xlab = '', x_axis_labels = c('Not Elderly/Retirement Home','Elderly/Other','Elderly/Retirement Home'), 
                    ylab='Effect on Log Odds of Voting',
                    legend_exist=TRUE, legend_title='Age Group',
                    legend_position='right')
image_name = paste0(subpopulation,'_effect_elderly_retire_',year,'_',vers,'_',other_cond)
ggsave(file=paste0(plot_dir,'/',image_name,'.png'), device='png', width=3000, height=2000, units='px', g)
## Calculate and plot predicted probabilities
elder_pred_new<-predict_response(m_elder_new, terms=c('elderly','retircomm_nurshom'), margin='marginalmeans'
                                 ,rg.limit = 122000
)
#plot predicted probabilities
pred_prob_plot(model_data=elderly_data, dep_var=dep_var, elder_pred_new, mean_vote = mean_turnout,
               plot_title = paste0(year,' Probability of Voting of (Non-)Elderly at \nRetirement Communities and Nursing Homes'),
               xlab='65+', 
               #x_axis_labels = c('FALSE', 'TRUE'),
               legend_exist=T, legend_title ='Votes at a Retirement Community/Nursing Home', angle=0,legend_position = 'right',
               output_dir = plot_dir, 
               image_name = paste0(subpopulation,'_Pred_Prob_elder_retire_',year,'_',vers,'_',other_cond))



# Distribution of polling locations for elderly voters vs. non-elderly
temp<-elderly_data%>%
  group_by(elderly)%>%
  mutate(num_voters=n())%>%
  ungroup()%>%
  group_by(elderly, location_category_simpl)%>%
  reframe(prop_voters = n()/num_voters)%>%
  distinct()%>%
  filter(complete.cases(.))

ggplot(temp, aes(x=location_category_simpl, y=prop_voters,fill=elderly))+
  geom_col(position='dodge')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

###### random code for troubleshooting
#geocoded<-read.csv('C:/Users/natha/Desktop/Polling Places DiD/data/gov_poll_places geocoded/geocoderesult_2017_poll_locations_10000.csv')
