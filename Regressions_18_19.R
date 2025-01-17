#Nathaniel Flemming
# 28/12/24

# LogisticRegressions for 2018 and 2019, using data created by DiD_preprocessing
## retaining voters only present for one year


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
                        substr(dep_var,9,12),'_V3')
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

#blank plot predicted probabilities
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

########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/ESRA Poster Regressions"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
# read in data
setwd(data_dir)
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
common_covars <-c(
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'pred_race',
  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
  'known_religious','CommercialData_LikelyUnion', 
  'CommercialData_OccupationGroup'
)
## Factor variables
#location categories
model_data$location_category<-as.factor(model_data$location_category)
model_data$location_category<-relevel(model_data$location_category, ref='other')
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
#government employee
model_data$known_gov_emp <- ifelse(model_data$CommercialData_OccupationIndustry=="Civil Servant",TRUE,FALSE)
model_data$known_gov_emp<-as.factor(model_data$known_gov_emp)
#union member
model_data$CommercialData_LikelyUnion<-as.factor(model_data$CommercialData_LikelyUnion)
#occupational group
model_data$CommercialData_OccupationGroup<-as.factor(model_data$CommercialData_OccupationGroup)
model_data$CommercialData_OccupationGroup<-relevel(model_data$CommercialData_OccupationGroup, ref='Blue Collar')

##set dependent variable
#dep_var = 'General_2018_11_06'
dep_var = 'General_2019_11_05'
year=substr(dep_var,9,12)
### calculate overall mean turnout for election for plotting
data_2019<-model_data[model_data$year==2019,]
data_2019$General_2019_11_05[is.na(data_2019$General_2019_11_05)]<-0
mean_turnout <- mean(data_2019$General_2019_11_05, na.rm=T)
#data removing missing race imputations
### Create race indicator for most probable race
# most_prob_race<-function(data, race_vars){
#   # remove rows with all missing race predictions
#   data <- data[complete.cases(data),]
#   # create column taking value of most probable race
#   data <- mutate(data, pred_race = names(data[,race_vars])
#                  [max.col(data[,race_vars])])
#   return(data)
# }
# mini<-most_prob_race(model_data,c("pred.whi","pred.bla","pred.his","pred.asi",
#                                   "pred.oth"))

##### Probability of Voting, Location category as predictor
ind_vars_loc<-c('location_category',common_covars)
# locations model
m_base<-log_reg(model_data, dep_var, ind_vars_loc)
summary(m_base)
#save results
#write_summ(results_dir, paste0('base_',year,'_V3'), m_base)
# calculate and plot predicted probabilities using Marginal Effect at the Means
#   see ggeffects documentation for details, but numerical variables are set to the mean
#   and effects for categorical variables are calculated  as a weighted average
#   over the factor levels. Should come closer to sample 'average observation' 
base_pred<-predict_response(m_base, terms='location_category', margin='marginalmeans',
                            rg.limit = 12000)
 #plot predicted probabilities
pred_prob_plot(model_data=model_data, dep_var=dep_var, base_pred, mean_vote=mean_turnout,
               plot_title = paste0(year,' Probability of Voting at Each Category of Location'),
               dodge=0, xlab='Location Category', x_axis_labels = loc_labels,
               legend_exist=F,legend_title =NULL, angle=45, hjust_value=1, 
               vjust_value=1,
               legend_position = 'right', output_dir = plot_dir, 
               image_name = paste0('Pred_Prob_Location_categories_',year,'_V3'))
# create blank plot
blank_pred_prob_plot(model_data=model_data, dep_var=dep_var, base_pred, mean_vote=mean_turnout,
                     ymin=31.5,ymax=37,
                     plot_title = paste0(year,' Probability of Voting at Each Category of Location'),
                     dodge=0, xlab='Location Category', x_axis_labels = loc_labels,
                     legend_exist=F,legend_title =NULL, angle=45, hjust_value=1, 
                     vjust_value=1,
                     legend_position = 'right', output_dir = plot_dir, 
                     image_name = paste0('Pred_Prob_Location_categories_',year,'_V3_blank'))

##### Probability of Voting, If has/lacks child and is voting at a school
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
#save results
#write_summ(results_dir, paste0('school_',year,'_V3'), m_schl)
## Calculate and plot predicted probabilities
schl_pred<-predict_response(m_schl, terms=c('has_child','school'), margin='marginalmeans')
#plot predicted probabilities
pred_prob_plot(model_data=model_data, dep_var=dep_var, schl_pred, mean_vote = mean_turnout,
               plot_title = paste0(year,' Probability of Voting of (Non-)Parents at School Locations'),
               xlab='Has a Child/Children', x_axis_labels = c('FALSE', 'TRUE'),
               legend_exist=T, legend_title ='Votes at a School', angle=0,legend_position = 'right',
               output_dir = plot_dir, 
               image_name = paste0('Pred_Prob_child_school_',year,'_V3'))
# create blank plot
blank_pred_prob_plot(model_data=model_data, dep_var=dep_var, schl_pred, mean_vote=mean_turnout,
                     ymin=31,ymax=34.75,
                     plot_title = paste0(year,' Probability of Voting of (Non-)Parents at School Locations'),
                     xlab='Has a Child/Children', x_axis_labels = c('FALSE', 'TRUE'),
                     legend_exist=T,legend_title ='Votes at a School', angle=0, 
                     legend_position = 'right', output_dir = plot_dir, 
                     image_name = paste0('Pred_Prob_child_school_',year,'_V3_blank'))


##### Probability of Voting, if gov employee and is voting at gov building
#vars
## Create Location dummy variables
model_data$pub_loc<-model_data$location_category=='public'
model_data$pub_loc <- as.factor(model_data$pub_loc)
## interaction
ind_vars_gov_emp <-c(
  # var of interest
  'known_gov_emp*pub_loc',
  common_covars[! common_covars %in% c('CommercialData_OccupationIndustry')]
)
# model
m_gov<-log_reg(model_data, dep_var, ind_vars_gov_emp)
summary(m_gov)
#save results
#write_summ(results_dir, paste0('gov_employees_',year,'_V3'), m_gov)
## Calculate and plot predicted probabilities
gov_pred<-predict_response(m_gov, terms=c('known_gov_emp','pub_loc'), margin='marginalmeans')
#plot predicted probabilities
pred_prob_plot(model_data=model_data, dep_var=dep_var, gov_pred, mean_vote = mean_turnout,
               plot_title = paste0(year,' Probability of Voting of (Non-)Government Employees at Public Locations'),
               xlab='Is a Government Employee', x_axis_labels = c('FALSE', 'TRUE'),
               legend_exist=T, legend_title ='Votes at a Public Building', 
               angle=0,legend_position = 'right', output_dir = plot_dir, 
               image_name = paste0('Pred_Prob_govemp_pub_',year,'_V3'))

### Probability of Voting, if known_religious at building type
##religious location dummy var
model_data$relig_loc <- (model_data$location_category=='religious'|model_data$location_category=='religious_school')
model_data$relig_loc <- as.factor(model_data$relig_loc)
## create interaction terms
category<-c('relig_loc')
interactions<-paste('known_religious*',category, sep='')
## remove religion variable
relig_covars<-common_covars[! common_covars %in% c('known_religious')]
## run models and plot results
log_reg_inter_plus_plot(model_data,dep_var=dep_var, 
                        interaction_terms=interactions, ind_vars = relig_covars,
                        turnout = turnout,
                        x_label='Known Religious',x_axis_labels=c('FALSE','TRUE'),
                        loc_dict = loc_dict, var_dict=var_dict,
                        results_dir= results_dir, image_dir=plot_dir)



