#Nathaniel Flemming
# 24/1/24

# Regressions

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

## Logistic regression + marginal effects
log_reg_plus_margins <- function(data, dep_var, ind_vars, var_of_int, save_dir, model_name){
  ind_vars_coll <- paste(ind_vars, collapse = '+')
  #print(ind_vars_coll)
  formula = paste0(dep_var,'~',ind_vars_coll)
  m_base <- glm(data=data, 
                formula,
                family = "binomial")
  # calculate Average Marginal Effect
  base_margins<-margins(m_base, variables=var_of_int)
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
    #save results
    write_summ(results_dir, paste0(substr(interaction,1,11),'x',substr(interaction,13,100),'_summary'), model)
  }
}

## Logistic regressions for a list of interactions plus plotting predicted probabilities
log_reg_inter_plus_plot<-function(df,dep_var='General_2018_11_06', interaction_terms, 
                        ind_vars=c('Voters_Gender', 'Voters_Age',
                                   'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
                                   'known_religious','CommercialData_LikelyUnion', 
                                   'CommercialData_OccupationIndustry'),
                        results_dir, image_dir){
  for(interaction in interaction_terms){
    print(interaction)
    ind_vars_list<-c(interaction,ind_vars)
    # model
    model<-log_reg(df, dep_var, ind_vars_list)
    summary(model)
    # calculate predicted probabilities
    model_pred<-predict_response(model, terms=c(substr(interaction,1,11), substr(interaction,13,100)), 
                                 margin='marginalmeans')
    #save results
    write_summ(results_dir, paste0(substr(interaction,1,11),'x',substr(interaction,13,100),'_summary'), model)
    # Generate plot details
    plot_title=paste0('Probability of Turning Out for', substr(interaction,1,11),'x',
                      substr(interaction,13,100))
    xlab = 'Known Republican'
    legend_title = substr(interaction,13,100)
    output_dir = image_dir
    image_name = paste0('Pred_Prob_',substr(interaction,1,11),'x',substr(interaction,13,100))
    pred_prob_plot(model_pred, dodge=0.8, plot_title, xlab, ylab='Predicted Probability of Turning Out',
                   legend_title, angle=0, legend_position='right', output_dir, image_name)
  }
}

## Logistic regressions for a list of interactions + marginal effects
log_reg_inter_plus_margins<-function(df,dep_var='General_2018_11_06', interaction_terms, 
                        ind_vars=c('Voters_Gender', 'Voters_Age',
                                   'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
                                   'known_religious','CommercialData_LikelyUnion', 
                                   'CommercialData_OccupationIndustry')
                        , var_of_int, save_dir){
  for(interaction in interaction_terms){
    print(interaction)
    ind_vars_list<-c(interaction,ind_vars)
    # model
    model<-log_reg(model_data, dep_var, ind_vars_list)
    summary(model)
    # calculate Average Marginal Effect
    base_margins<-margins(model, variables = var_of_int)
    #save marginal effects
    setwd(save_dir)
    sink(file=paste0("MarginsSummary_",paste0(substr(interaction,1,11),'x',substr(interaction,13,100)),".txt"))
    print(summary(base_margins))
    sink()
    #save results
    write_summ(results_dir, paste0(substr(interaction,1,11),'x',substr(interaction,13,100),'_summary'), model)
  }
}

#plot predicted probabilities
pred_prob_plot<-function(pred_probs_obj, dodge=0.8, plot_title, xlab, ylab='Predicted Probability of Turning Out',
                         legend_title, angle=0, legend_position='right', output_dir, image_name){
  g<-ggplot(pred_probs_obj, aes(x, predicted, color=group))+
    geom_point(size=3, position=position_dodge(dodge))+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width=0.2, linewidth=1, position=position_dodge(dodge))+
    labs(title = plot_title,
         x = xlab,
         y = ylab) +
    scale_color_discrete(name = legend_title)+
    scale_y_continuous(labels = scales::percent)+
    theme(plot.title = element_text(size=20),
          axis.title = element_text(size=15),
          axis.text.y = element_text(size=15),
          axis.text.x = element_text(size=12, angle=angle,),
          legend.title = element_text(size=20),
          legend.text = element_text(size=15),
          legend.key.size = unit(1, 'cm'),
          legend.position = legend_position)
  # Save plot
  setwd(output_dir)
  ggsave(file=paste0(image_name,'.png'), device='png', width=3000, height=2000, units='px', g) #saves g
}


# Make and plot predictions for interaction models for specific value of indp vars
boxplot_preds <- function(model, plotdata, var1, var2, num_ran_vars, xlab, ylab, color_lab, 
                          dir, plot_name){
  #calculate predicted probabilities
  preds<-predict(model, newdata=plotdata, type='response', se.fit=T)
  pred_probs<-preds$fit
  pred_errs<-preds$se.fit
  # Plot predictions
  ## create plot title based on random variable values
  var_settings = c()
  for(i in 1:num_ran_vars){
    var_settings=c(var_settings, paste0(colnames(plotdata)[i+2],': ',plotdata[[i+2]][1]))
  }
  plot_title=paste0(var_settings, collapse='\n')
  # plot
  pred_plot<-ggplot(plotdata, aes(x = plotdata[[var1]], y = pred_probs, 
                                  color = factor(plotdata[[var2]]))) +
    geom_boxplot() +
    geom_errorbar(aes(ymin=pred_probs-1.96*pred_errs,
                      ymax=pred_probs+1.96*pred_errs),
                  width=0.2, position=position_dodge(0.8))+
    labs(x = xlab, y = ylab, fill = color_lab, 
         title = plot_title) +
    scale_color_discrete(name = color_lab)+
    theme(plot.title = element_text(size=10),
          axis.title = element_text(size = 10),
          legend.position = "none")
  setwd(dir)
  return(pred_plot)
}

# Make grid of multiple boxplots 
#   of predictions for interaction models for specific value of indp vars
grid_boxplot <- function(model, data, var1, var2, num_ran_vars, xlab, ylab, color_lab, 
                         plot_dir, plot_name){
  num_rows=nrow(data)
  num_plots = num_rows/4
  start <- seq(from = 1, to = num_rows-3, by = 4)
  end <- seq(from = 4, to = num_rows, by = 4)
  plots_list<-list()
  for(i in 1:num_plots){
    p<-boxplot_preds(model=model, plotdata=data[start[i]:end[i],], var1 = var1, 
                     var2=var2, xlab = xlab, ylab = ylab, color_lab = color_lab,
                     num_ran_vars = num_ran_vars, dir=plot_dir, plot_name=plot_name)
    plots_list[[i]]<-p
  }
  setwd(plot_dir)
  ncols<-ceiling(sqrt(num_plots)) # can add 1 extra plot for legend
  nrows<-ceiling((num_plots)/ncols)
  grid.arrange(grobs=plots_list, ncol=ncols, nrow=nrows)
  g <- arrangeGrob(grobs=plots_list, ncol=ncols, nrow=nrows) #generates g for saving
  ggsave(file=plot_name, device='png', width=2000, height=2000, units='px', g) #saves g
  #ggsave(plot_name, device='png', width=2000, height=2000, units='px')
}




########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places/plots"
# read in data
setwd(data_dir)
model_data<-read.csv('L2PA_full.csv')

# Convert vote to binary
model_data <- binarize_vote(model_data, 'General_2018_11_06', 'Y')
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
col_names<-c('CommercialData_LikelyUnion','CommercialData_OccupationIndustry',
             'has_child','known_religious','known_gov_emp','known_repub',
             'pub_loc','pub_just','other','relig_loc','school','multiple','justice_loc',
             'library','relig_school')
model_data[,col_names]<-lapply(model_data[,col_names],factor)
# Drop unused levels
model_data<-droplevels(model_data)

## subset data for initial testing
#mini_data <- model_data[sample(nrow(model_data), 100000),]

## create vector of location categories
categories<-c('pub_loc','pub_just','other','relig_loc','school','multiple',
              'justice_loc','library','relig_school ')

#################### Logistic Regression
## Common set of covariates
common_covars <-c(
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
  'known_religious','CommercialData_LikelyUnion', 
  'CommercialData_OccupationIndustry'
)

##### Probability of turning out, Location category as predictor
ind_vars_loc<-c('location_category',common_covars)
# locations model
#m_base<-log_reg_plus_margins(model_data, 'General_2018_11_06', ind_vars_loc, 
#                             'location_category', results_dir, 'base')
m_base<-log_reg(model_data, 'General_2018_11_06', ind_vars_loc)
summary(m_base)
#save results
write_summ(results_dir, 'base', m_base)
# calculate and plot predicted probabilities using Marginal Effect at the Means
#   see ggeffects documentation for details, but numerical variables are set to the mean
#   and effects for categorical variables are calculated  as a weighted average
#   over the factor levels. Should come closer to sample 'average observation' 
base_pred<-predict_response(m_base, terms='location_category', margin='marginalmeans')
#plot predicted probabilities
pred_prob_plot(base_pred, plot_title = 'Probability of Turning Out at Each Category of Location',
               dodge=0, xlab='Location Category',legend_title =NULL, angle=45,legend_position = 'none',
               output_dir = plot_dir, image_name = 'Pred_Prob_Location_categories')


##### Probability of turning out, If has/lacks child and is voting at a school
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
write_summ(results_dir, 'school', m_schl)
## Calculate and plot predicted probabilities
schl_pred<-predict_response(m_schl, terms=c('has_child','school'), margin='marginalmeans')
#plot predicted probabilities
pred_prob_plot(schl_pred, plot_title = 'Probability of Turning Out of (Non-)Parents (Not) Voting at Schools',
               xlab='Has a Child/Children', legend_title ='Votes at a School', angle=0,legend_position = 'right',
               output_dir = plot_dir, image_name = 'Pred_Prob_child_school')


##### Probability of turning out, if gov employee and is voting at gov building
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
write_summ(results_dir, 'gov_employees', m_gov)
## Calculate and plot predicted probabilities
## Calculate and plot predicted probabilities
gov_pred<-predict_response(m_gov, terms=c('known_gov_emp','pub_loc'), margin='marginalmeans')
#plot predicted probabilities
pred_prob_plot(gov_pred, plot_title = 'Probability of Turning Out of (Non-)Government Employees (Not) Voting at Public Buildings',
               xlab='Is a Government Employee', legend_title ='Votes at a Public Building', angle=0,legend_position = 'right',
               output_dir = plot_dir, image_name = 'Pred_Prob_govemp_pub')


######### Probability of turning out, if known_republican at each building type
## create interaction terms
interactions<-paste('known_repub*',categories, sep='')
## remove parties variable
repub_covars<-common_covars[! common_covars %in% c('Parties_Description')]
## run models
log_reg_inter_plus_plot(model_data, interaction_terms=interactions, ind_vars = repub_covars,
              results_dir= results_dir, image_dir=plot_dir)

## Calculate and plot predicted probabilities
# Create dataframe for prediction
new_data <- expand.grid(known_repub = c(TRUE,FALSE),
                        relig_loc = c(TRUE,FALSE),
                        Voters_Gender=levels(as.factor(model_data$Voters_Gender)),
                        #Parties_Description=levels(as.factor(model_data$Parties_Description)),
                        #pred_race=levels(as.factor(model_data$pred_race)),
                        Voters_Age=mean(model_data$Voters_Age, na.rm=T), 
                        CommercialData_Education=mean(model_data$CommercialData_Education, na.rm=T),
)
preds<-predict(m_repub, newdata=new_data, type='response', se.fit=T)
pred_probs<-preds$fit
pred_errs<-preds$se.fit
#plot predictions
grid_boxplot(m_repub, new_data, 'known_repub', 'relig_loc', 2, xlab='Is a Republican', 
             ylab = 'Probability of Turning Out',color_lab = 'Votes at a Religious Building',
             plot_dir=plot_dir, plot_name='repub_relig_pred_plot.png')

### Probability of turning out, if known_religious and is voting at religious building
#vars
ind_vars_relig <-c(
  # var of interest
  'known_religious*relig_loc',
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'CommercialData_Education','CommercialData_EstimatedHHIncomeAmount',
  # Race
  'pred_race'
)
# model
m_relig<-log_reg(model_data, 'General_2018_11_06', ind_vars_relig)
summary(m_relig)
#save results
write_summ(results_dir, 'relig', m_relig)

### Probability of turning out, if black and voting at justice system building
#vars
ind_vars_blk_just <-c(
  # var of interest
  'pred_black*justice_loc',
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'CommercialData_Education','CommercialData_EstimatedHHIncomeAmount'
)
# model
m_blk<-log_reg(model_data, 'General_2018_11_06', ind_vars_blk_just)
summary(m_blk)
#save results
write_summ(results_dir, 'black', m_blk)




#####################
# Create dataframe for prediction
new_data <- expand.grid(has_child = c(TRUE,FALSE),
                        school = c(TRUE,FALSE),
                        Voters_Gender=levels(as.factor(model_data$Voters_Gender)),
                        Parties_Description=levels(as.factor(model_data$Parties_Description)),
                        #pred_race=levels(as.factor(model_data$pred_race)),
                        Voters_Age=mean(model_data$Voters_Age, na.rm=T), 
                        CommercialData_EstimatedHHIncomeAmount = mean(model_data$CommercialData_EstimatedHHIncomeAmount, na.rm=T),
                        Residence_Families_HHCount=mean(model_data$Residence_Families_HHCount, na.rm=T),
                        known_religious=c(TRUE,FALSE),
                        CommercialData_LikelyUnion =levels(as.factor(model_data$CommercialData_LikelyUnion)),
                        CommercialData_OccupationIndustry=levels(as.factor(model_data$CommercialData_OccupationIndustry))
)
preds<-predict(m_schl, newdata=new_data, type='response', se.fit=T)
pred_probs<-preds$fit
pred_errs<-preds$se.fit
#plot predictions
p1<-boxplot_preds(model=m_schl, plotdata=new_data[1:4,], var1 = 'has_child', 
                  var2='school', xlab = 'Has a child/children',
                  num_ran_vars=2,
                  ylab = 'Probability of Turning Out',color_lab = 'Votes at a School',
                  dir=plot_dir, plot_name='child_schl_pred_plot.png')
p2<-boxplot_preds(model=m_schl, plotdata=new_data[5:8,], var1 = 'has_child', 
                  var2='school', xlab = 'Has a child/children', 
                  num_ran_vars=2,
                  ylab = 'Probability of Turning Out',color_lab = 'Votes at a School',
                  dir=plot_dir, plot_name='child_schl_pred_plot.png')
grid.arrange(p1,p2,nrow=1)
grid_boxplot(m_schl, new_data, 'has_child', 'school', 2, xlab='Has a child/children', 
             ylab = 'Probability of Turning Out',color_lab = 'Votes at a School',
             plot_dir=plot_dir, plot_name='child_schl_pred_plot.png')

