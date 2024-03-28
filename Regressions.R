#Nathaniel Flemming
# 24/1/24

# Regressions

library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet
library(stringr) #string manipulation
library(ggplot2) # plotting
library(gridExtra) # plot multiple graphs together

########## Functions

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
                                   'CommercialData_Education','Residence_Families_HHCount',
                                   'known_religious','CommercialData_LikelyUnion', 
                                   'CommercialData_OccupationIndustry',
                                   'MilitaryStatus_Description')){
  for(interaction in interaction_terms){
    print(interaction)
    ind_vars_list<-c(interaction,ind_vars)
    # model
    model<-log_reg(model_data, dep_var, ind_vars_list)
    summary(model)
    #save results
    write_summ(results_dir, paste0(substr(interaction,1,11),'x',substr(interaction,13,100),'_summary'), model)
  }
}
# Make and plot predictions
boxplot_preds <- function(model, plotdata, var1, var2, num_ran_vars, xlab, ylab, color_lab, 
                          dir, plot_name){
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
         title=plot_title) +
    scale_color_discrete(name = color_lab)+
    theme(plot.title = element_text(size=10),
          axis.title = element_text(size = 10),
          legend.position = "none")
  setwd(dir)
  return(pred_plot)
}

# Make grid of multiple boxplots
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
# Drop unused levels
model_data<-droplevels(model_data)

## subset data for initial testing
mini_data <- model_data[sample(nrow(model_data), 100000),]

## create vector of location categories
categories<-c('pub_loc','pub_just','other','relig_loc','school','multiple',
              'justice_loc','library','relig_school ')

#################### Logistic Regression
## Common set of covariates
common_covars <-c(
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'CommercialData_Education','Residence_Families_HHCount',
  'known_religious','CommercialData_LikelyUnion', 
  'CommercialData_OccupationIndustry'
)

#### Probability of turning out, location category as predictor
ind_vars_loc<-c('location_category',common_covars)
# locations model
m_base<-log_reg(model_data, 'General_2018_11_06', ind_vars_loc)
summary(m_base)
#save results
write_summ(results_dir, 'base', m_base)
## Calculate and plot predicted probabilities
plotdata<-with(model_data, data.frame(location_category=c('justice','library',
                                                          'multiple','public',
                                                          'public_justice','religious',
                                                          'religious_school','school'),
                                      Voters_Gender='M',
                                      Voters_Age=mean(Voters_Age, na.rm=T),
                                      Parties_Description='Republican',
                                      CommercialData_Education=mean(CommercialData_Education, na.rm=T)
                                      ))

### Probability of turning out, if has/lacks child and is voting at a school
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
# Create dataframe for prediction
new_data <- expand.grid(has_child = c(TRUE,FALSE),
                        school = c(TRUE,FALSE),
                        Voters_Gender=levels(as.factor(model_data$Voters_Gender)),
                        Parties_Description=levels(as.factor(model_data$Parties_Description)),
                        #pred_race=levels(as.factor(model_data$pred_race)),
                        Voters_Age=mean(model_data$Voters_Age, na.rm=T), 
                        CommercialData_Education=mean(model_data$CommercialData_Education, na.rm=T),
                        CommercialData_EstimatedHHIncomeAmount = mean(model_data$CommercialData_EstimatedHHIncomeAmount, na.rm=T)
                        )
preds<-predict(m_schl, newdata=new_data, type='response', se.fit=T)
pred_probs<-preds$fit
pred_errs<-preds$se.fit
#plot predictions
p1<-boxplot_preds(model=m_schl, plotdata=new_data[1:4,], var1 = 'has_child', 
                              var2='school', xlab = 'Has a child/children',
                              num_ran_vars=2,
                              ylab = 'Probability of Turningout',color_lab = 'Votes at a School',
                              dir=plot_dir, plot_name='child_schl_pred_plot.png')
p2<-boxplot_preds(model=m_schl, plotdata=new_data[5:8,], var1 = 'has_child', 
                  var2='school', xlab = 'Has a child/children', 
                  num_ran_vars=2,
                  ylab = 'Probability of Turningout',color_lab = 'Votes at a School',
                  dir=plot_dir, plot_name='child_schl_pred_plot.png')
grid.arrange(p1,p2,nrow=1)
grid_boxplot(m_schl, new_data, 'has_child', 'school', 2, xlab='Has a child/children', 
             ylab = 'Probability of Turningout',color_lab = 'Votes at a School',
             plot_dir=plot_dir, plot_name='child_schl_pred_plot.png')



### Probability of turning out, if gov employee and is voting at gov building
#vars
ind_vars_gov_emp <-c(
  # var of interest
  'known_gov_emp*pub_loc',
  common_covars
)
# model
m_gov<-log_reg(model_data, 'General_2018_11_06', ind_vars_gov_emp)
summary(m_gov)
#save results
write_summ(results_dir, 'gov_employees', m_gov)
## Calculate and plot predicted probabilities
# Create dataframe for prediction
new_data <- expand.grid(known_gov_emp = c(TRUE,FALSE),
                        pub_loc = c(TRUE,FALSE),
                        Voters_Gender=levels(as.factor(model_data$Voters_Gender)),
                        Parties_Description=levels(as.factor(model_data$Parties_Description)),
                        #pred_race=levels(as.factor(model_data$pred_race)),
                        Voters_Age=mean(model_data$Voters_Age, na.rm=T), 
                        CommercialData_Education=mean(model_data$CommercialData_Education, na.rm=T),
                        CommercialData_EstimatedHHIncomeAmount = mean(model_data$CommercialData_EstimatedHHIncomeAmount, na.rm=T)
)
preds<-predict(m_gov, newdata=new_data, type='response', se.fit=T)
pred_probs<-preds$fit
pred_errs<-preds$se.fit
#plot predictions
grid_boxplot(m_gov, new_data, 'known_gov_emp', 'pub_loc', 2, xlab='Is a Government Employee', 
             ylab = 'Probability of Turningout',color_lab = 'Votes at a Public Building',
             plot_dir=plot_dir, plot_name='gov_pub_pred_plot.png')


######### Probability of turning out, if known_republican at each building type
## create interaction terms
interactions<-paste('known_repub*',categories, sep='')
## remove parties variable
repub_covars<-common_covars[-3]
## run models
log_reg_inter(model_data, interaction_terms=interactions, ind_vars = repub_covars)

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
             ylab = 'Probability of Turningout',color_lab = 'Votes at a Religious Building',
             plot_dir=plot_dir, plot_name='repub_relig_pred_plot.png')




### Probability of turning out, if known_republican at different buildings

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




