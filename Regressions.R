#Nathaniel Flemming
# 24/1/24

# Regressions, using data created by


library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet
library(stringr) #string manipulation
library(ggplot2) # plotting
library(gridExtra) # plot multiple graphs together
library(margins) # get average marginal effects
library(ggeffects) #plot predicted probabilities

########## Functions

## Binarize vote variables
binarize_vote <- function(data, vote_var, yes_vote_value){
  data[[vote_var]] <- ifelse(data[[vote_var]]==yes_vote_value,1,0)
  return(data)
}

## Logistic regression
log_reg <- function(data, dep_var, ind_vars){
  ind_vars_coll <- paste(ind_vars, collapse = '+')
  #print(ind_vars_coll)
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

## Logistic regressions for a list of interactions plus plotting predicted probabilities
log_reg_inter_plus_plot<-function(df,dep_var='General_2018_11_06', interaction_terms, 
                        ind_vars=c('Voters_Gender', 'Voters_Age',
                                   'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
                                   'known_religious','CommercialData_LikelyUnion', 
                                   'CommercialData_OccupationIndustry'),
                        turnout,
                        x_label, x_axis_labels = c('FALSE','TRUE'), loc_dict, var_dict,
                        vjust_value=0, hjust_value=0,
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
                                   substr(dep_var,9,12),'_summary'), model)
    # Generate plot details
    plot_title=paste0(substr(dep_var,9,12),' Probability of Voting for ', 
                      var_dict[first_term][[1]], ' at ', loc_dict[second_term][[1]])
    legend_title = loc_dict[second_term][[1]]
    image_name = paste0('Pred_Prob_',first_term,'x',loc_dict[second_term][[1]],
                        substr(dep_var,9,12))
    pred_prob_plot(model_data=df, dep_var=dep_var, model_pred, mean_vote=turnout, 
                   plot_title=plot_title, dodge=0.8, xlab=x_label, 
                   x_axis_labels = x_axis_labels, ylab='Predicted Probability of Voting', 
                   legend_exist = T, legend_title = legend_title, angle = 0, 
                   vjust_value = vjust_value, hjust_value=hjust_value, legend_position='right', 
                   output_dir=image_dir, image_name=image_name)
  }
}

## Logistic regressions for a list of interactions + marginal effects
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

#plot predicted probabilities
pred_prob_plot<-function(model_data, dep_var, pred_probs_obj, dodge=0.8, mean_vote,
                         plot_title, 
                         xlab, x_axis_labels, ylab='Predicted Probability of Voting',
                         legend_exist=TRUE, legend_title, angle=0, 
                         hjust_value=0, vjust_value=0,
                         legend_position='right', output_dir, image_name){
  g<-ggplot(pred_probs_obj, aes(x, predicted, color=group))+
    geom_point(size=3, position=position_dodge(dodge), show.legend = legend_exist)+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width=0.2, linewidth=1, position=position_dodge(dodge), 
                  show.legend = legend_exist)+
    # draw line at mean voting rate
    #geom_hline(aes(yintercept = mean_vote, linetype=''), color='black')+
    #scale_linetype_manual(name = "Mean Voting Rate", values=1, guide = guide_legend(override.aes = list(color = c("black"))))+
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


########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/ESRA Poster Regressions"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
# read in data
setwd(data_dir)
model_data<-read.csv('L2PA_full.csv')

# Convert vote to binary
model_data <- binarize_vote(model_data, 'General_2018_11_06', 'Y')
model_data <- binarize_vote(model_data, 'General_2017_11_07', 'Y')
### set reference categories
#### factorize variables and relevel
# Location category
model_data$location_category<-as.factor(model_data$location_category)
model_data$location_category <- relevel(model_data$location_category, ref = "other")
# Party
model_data$Parties_Description <- as.factor(model_data$Parties_Description)
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
# Race
model_data$pred_race <- as.factor(model_data$pred_race)
model_data$pred_race <- relevel(model_data$pred_race, ref = "pred.whi_2018")
# Factorize any other variables we're using
col_names<-c('CommercialData_LikelyUnion', 'CommercialData_OccupationGroup', 
             'CommercialData_OccupationIndustry',
             'has_child','known_religious','known_gov_emp','known_repub',
             'pub_loc','pub_just','other','relig_loc','school','multiple','justice_loc',
             'library','relig_school')
model_data[,col_names]<-lapply(model_data[,col_names],factor)
# Drop unused levels
model_data<-droplevels(model_data)


## create vector of location categories
categories<-c('pub_loc','pub_just','other','relig_loc','school','multiple',
              'justice_loc','library','relig_school')
## Create vector of location category labels
loc_labels<-c('Other','Justice Location','Library','Multiple Categories',
              'Public Location','Public/Justice Location','Religious Location',
              'Religious School','School')
## Create dictionary of location labels
loc_dict<-c('pub_loc'='Public Location','pub_just'='Public and Justice Location',
            'other'='Other','relig_loc'='Religious Location','school'='School',
            'multiple'='Multiple Categories', 'justice_loc'='Justice Location',
            'library'='Library', 'relig_school'='Religious School')
## Create dictionary of variable labels
var_dict<-c('Voters_Gender'='Gender', 'Voters_Age'='Age',
            'CommercialData_EstimatedHHIncomeAmount'='Estimated HH Income',
            'Residence_Families_HHCount'='HH Resident Count',
            'known_religious'='Known Religious',
            'CommercialData_LikelyUnion'='Likely Union Member', 
            'CommercialData_OccupationIndustry'='Occupation Industry',
            'CommercialData_OccupationIndustry'='Occupation Group',
            'has_child'='Has Child(ren)','known_gov_emp'='Known Government Employee',
            'Parties_Description'='Political Party','pred_race'='Predicted Race')

#################### Logistic Regression
## Common set of covariates
common_covars <-c(
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'pred_race',
  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
  'known_religious','CommercialData_LikelyUnion', 
  'CommercialData_OccupationGroup'
)

##set dependent variable
dep_var = 'General_2018_11_06'
year=substr(dep_var,9,12)
### calculate turnout for election
turnout <- mean(model_data[[dep_var]], na.rm=T)

##### Probability of Voting, Location category as predictor
ind_vars_loc<-c('location_category',common_covars)
# locations model
#m_base<-log_reg_plus_margins(model_data, 'General_2018_11_06', ind_vars_loc, 
#                             'location_category', results_dir, 'base')
m_base<-log_reg(model_data, dep_var, ind_vars_loc)
summary(m_base)
#save results
#write_summ(results_dir, paste0('base_',year), m_base)
# calculate and plot predicted probabilities using Marginal Effect at the Means
#   see ggeffects documentation for details, but numerical variables are set to the mean
#   and effects for categorical variables are calculated  as a weighted average
#   over the factor levels. Should come closer to sample 'average observation' 
base_pred<-predict_response(m_base, terms='location_category', margin='marginalmeans',
                            rg.limit = 12000)
#plot predicted probabilities
pred_prob_plot(model_data=model_data, dep_var=dep_var, base_pred, mean_vote=turnout,
               plot_title = paste0(year,' Probability of Voting at Each Category of Location'),
               dodge=0, xlab='Location Category', x_axis_labels = loc_labels,
               legend_exist=F,legend_title =NULL, angle=45, hjust_value=1, 
               vjust_value=1,
               legend_position = 'right', output_dir = plot_dir, 
               image_name = paste0('Pred_Prob_Location_categories_',year))

## AME for base model (takes too long to run)
#ame_pred<-predict_response(m_base, terms='location_category', margin='empirical')

##### Probability of Voting, If has/lacks child and is voting at a school
#vars
ind_vars_child_schl <-c(
  # var of interest
  'has_child*school',
  common_covars
)
# model
m_schl<-log_reg(model_data, 'General_2018_11_06', ind_vars_child_schl)
summary(m_schl)
#save results
#write_summ(results_dir, 'school_2018', m_schl)
## Calculate and plot predicted probabilities
schl_pred<-predict_response(m_schl, terms=c('has_child','school'), margin='marginalmeans')
#plot predicted probabilities
pred_prob_plot(model_data=model_data, dep_var=dep_var, schl_pred, mean_vote = turnout,
               plot_title = paste0(year,' Probability of Voting of (Non-)Parents at School Locations'),
               xlab='Has a Child/Children', x_axis_labels = c('FALSE', 'TRUE'),
               legend_exist=T, legend_title ='Votes at a School', angle=0,legend_position = 'right',
               output_dir = plot_dir, image_name = paste0('Pred_Prob_child_school_',year))


##### Probability of Voting, if gov employee and is voting at gov building
#vars
ind_vars_gov_emp <-c(
  # var of interest
  'known_gov_emp*pub_loc',
  common_covars[! common_covars %in% c('CommercialData_OccupationIndustry')]
)
# model
m_gov<-log_reg(model_data, 'General_2018_11_06', ind_vars_gov_emp)
summary(m_gov)
#save results
#write_summ(results_dir, 'gov_employees_2018', m_gov)
## Calculate and plot predicted probabilities
gov_pred<-predict_response(m_gov, terms=c('known_gov_emp','pub_loc'), margin='marginalmeans')
#plot predicted probabilities
pred_prob_plot(model_data=model_data, dep_var=dep_var, gov_pred, mean_vote = turnout,
               plot_title = paste0(year,' Probability of Voting of (Non-)Government Employees at Public Locations'),
               xlab='Is a Government Employee', x_axis_labels = c('FALSE', 'TRUE'),
               legend_exist=T, legend_title ='Votes at a Public Building', 
               angle=0,legend_position = 'right', output_dir = plot_dir, 
               image_name = paste0('Pred_Prob_govemp_pub_',year))


######### Probability of Voting, if known_republican at building type
## create interaction terms
category<-c('relig_loc')
interactions<-paste('Parties_Description*',category, sep='')
## remove parties variable
repub_covars<-common_covars[! common_covars %in% c('Parties_Description')]
## run models and plot results
log_reg_inter_plus_plot(model_data, dep_var='General_2018_11_06', 
                        interaction_terms=interactions, ind_vars = repub_covars,
                        turnout = turnout,
                        x_label='Party', x_axis_labels=c('Democrat','Other','Republican'),
                        hjust_value=0.5,loc_dict = loc_dict, var_dict=var_dict,
                        results_dir = results_dir, image_dir=plot_dir)


### Probability of Voting, if known_religious at building type
## create interaction terms
category<-c('relig_loc')
interactions<-paste('known_religious*',category, sep='')
## remove religion variable
relig_covars<-common_covars[! common_covars %in% c('known_religious')]
## run models and plot results
log_reg_inter_plus_plot(model_data,dep_var='General_2018_11_06', 
                        interaction_terms=interactions, ind_vars = relig_covars,
                        turnout = turnout,
                        x_label='Known Religious',x_axis_labels=c('FALSE','TRUE'),
                        loc_dict = loc_dict, var_dict=var_dict,
                        results_dir= results_dir, image_dir=plot_dir)




