#Nathaniel Flemming
# 24/1/24

# Regressions

library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet
library(stringr) #string manipulation
library(ggplot2) # plotting

########## Functions

## Binarize vote variables
binarize_vote <- function(data, vote_var, yes_vote_value){
  data[[vote_var]] <- ifelse(data[[vote_var]]==yes_vote_value,1,0)
  return(data)
}


## Logistic regression
log_reg <- function(data, dep_var, ind_vars){
  ind_vars_coll <- paste(ind_vars, collapse = '+')
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

## subset data for initial testing
mini_data <- model_data[sample(nrow(model_data), 100000),]


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
                           'Populist','Progressive','Prohibition','Rainbow',
                           'Reform','Registered Independent','Right to Life',
                           'Social Democrat','Socialist','Socialist Labor',
                           'Taxpayers','Unknown','Whig'))
model_data$Parties_Description <- relevel(model_data$Parties_Description, ref = "Democratic")
# Race
model_data$pred_race <- as.factor(model_data$pred_race)
model_data$pred_race <- relevel(model_data$pred_race, ref = "pred.whi_2018")

### Logistic Regression
#### Probability of turning out, location category as predictor
## Sets of variables
ind_vars_base <-c(
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'CommercialData_Education','CommercialData_EstimatedHHIncomeAmount',
  # Race
  'pred_race',
  # Polling place type
  'location_category'
)
# Base model
m_base<-log_reg(model_data, 'General_2018_11_06', ind_vars_base)
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
                                      CommercialData_Education=mean(CommercialData_Education, na.rm=T),
                                      CommercialData_EstimatedHHIncomeAmount = mean(CommercialData_EstimatedHHIncomeAmount, na.rm=T),
                                      pred_race='pred.whi_2018'))
preds<-predict(m_schl, plotdata, type='response',se.fit=TRUE)
predf <- preds$fit # predicted
lower <- preds$fit - (1.96*preds$se.fit) # lower bounds
upper <- preds$fit + (1.96*preds$se.fit) # upper bounds
plot(1:8, predf, type="l", ylab="Predicted Probability to Vote", xlab="location", bty="n")
lines(18:90, lower, lty=2)
lines(18:90, upper, lty=2)



### Probability of turning out, if has/lacks child and is voting at a school
#vars
ind_vars_child_schl <-c(
  # var of interest
  'has_child*school',
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'CommercialData_Education','CommercialData_EstimatedHHIncomeAmount',
  # Race
  'pred_race'
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
                        Voters_Gender='M', Voters_Age=mean(model_data$Voters_Age, na.rm = T),
                        Parties_Description='Republican',
                        CommercialData_Education=mean(model_data$CommercialData_Education, na.rm=T),
                        CommercialData_EstimatedHHIncomeAmount = mean(model_data$CommercialData_EstimatedHHIncomeAmount, na.rm=T),
                        pred_race='pred.whi_2018')
# Make predictions
preds<-predict(m_schl, newdata=new_data, type='response', se.fit=T)
pred_probs<-preds$fit
pred_errs<-preds$se.fit
# Plot predictions
ggplot(new_data, aes(x = has_child, y = pred_probs, color = factor(school))) +
  geom_boxplot() +
  geom_errorbar(aes(ymin=pred_probs-1.96*pred_errs,
                    ymax=pred_probs+1.96*pred_errs),
                width=0.2, position=position_dodge(0.8))+
  labs(x = "Has a child/children", y = "Predicted Probability of Turning Out", color = "Votes at a School") +
  scale_color_discrete(name = "Votes at a School") +
  theme_minimal()



### Probability of turning out, if gov employee and is voting at gov building
#vars
ind_vars_gov_emp <-c(
  # var of interest
  'known_gov_emp*pub_loc',
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'CommercialData_Education','CommercialData_EstimatedHHIncomeAmount',
  # Race
  'pred_race'
)
# model
m_gov<-log_reg(model_data, 'General_2018_11_06', ind_vars_gov_emp)
summary(m_gov)
#save results
write_summ(results_dir, 'gov_employees', m_gov)

### Probability of turning out, if known_republican and is voting at religious building
#vars
ind_vars_repub <-c(
  # var of interest
  'known_repub*relig_loc',
  # Demographics
  'Voters_Gender', 'Voters_Age',
  'CommercialData_Education','CommercialData_EstimatedHHIncomeAmount',
  # Race
  'pred_race'
)
# model
m_repub<-log_reg(model_data, 'General_2018_11_06', ind_vars_repub)
summary(m_repub)
#save results
write_summ(results_dir, 'repub_relig', m_repub)

### Probability of turning out, if known_republican and is voting at a public building
#vars
ind_vars_repub2 <-c(
  # var of interest
  'known_repub*pub_loc',
  # Demographics
  'Voters_Gender', 'Voters_Age',
  'CommercialData_Education','CommercialData_EstimatedHHIncomeAmount',
  # Race
  'pred_race'
)
# model
m_repub2<-log_reg(model_data, 'General_2018_11_06', ind_vars_repub2)
summary(m_repub2)
#save results
write_summ(results_dir, 'repub_pub', m_repub2)

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




