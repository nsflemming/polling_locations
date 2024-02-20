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

#### Regressions
### set reference categories
model_data$location_category <- relevel(model_data$location_category, ref = "apartment")
model_data$Parties_Description <- relevel(model_data$Parties_Description, ref = "Democratic")
model_data$pred_race <- relevel(model_data$pred_race, ref = "pred.whi_2018")
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

### Logistic Regression
#### Probability of turning out, location category as predictor
##### Base model
# model
m_base<-log_reg(model_data, 'General_2018_11_06', ind_vars_base)
summary(m_base)

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




