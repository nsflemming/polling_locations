#Nathaniel Flemming
# 24/1/24

# Regressions

library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet
library(stringr) #string manipulation

########## Functions

## Binarize vote variables
binarize_vote <- function(data, vote_var, yes_vote_value){
  data[[vote_var]] <- ifelse(data[[vote_var]]==yes_vote_value,1,0)
  return(data)
}

## convert dollars to numeric
dollar_to_num <- function(data, inc_var){
  #remove $
  data[[inc_var]] = str_extract(data[[inc_var]], "(?<=\\$)(.*)")
  # convert to numeric
  data[[inc_var]] = as.numeric(data[[inc_var]])
  return(data)
}

## Convert education to ordinal
educ_to_ord <- function(data, educ_var, mapping){
  data[[educ_var]] <- mapping[as.character(data[[educ_var]])]
  #map blank/now missing to 0?
  #data[[educ_var]][is.na(data[[educ_var]])] <- 0
  return(data)
}


## Logistic regression
log_reg <- function(data, dep_var, ind_vars){
  ind_vars_coll <- paste(ind_vars, collapse = '+')
  formula = paste0(dep_var,'~',ind_vars_coll)
  m_base <- glm(data=data, 
                formula,
                family = "binomial")
  return(summary(m_base))
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
# Convert income to numeric
model_data<-dollar_to_num(model_data, 'CommercialData_EstimatedHHIncomeAmount')
# Convert education to ordinal
## level to numeric map
educ_map <- c("Less than HS Diploma - Ex Like"=1,
              "Less than HS Diploma - Likely"=1,
              'HS Diploma - Extremely Likely'=2, "HS Diploma - Likely"=2,
              "Some College -Extremely Likely"=3, "Some College - Likely"=3, 
              "Vocational Technical Degree - Extremely Likely"=4,
              "Bach Degree - Extremely Likely"=5, "Bach Degree - Likely"=5,
              "Grad Degree - Extremely Likely"=6, "Grad Degree - Likely"=6
)
model_data<-educ_to_ord(model_data, 'CommercialData_Education', educ_map)
# Convert location category to factor
model_data$location_category <- as.factor(model_data$location_category)
## subset data to usable size for initial testing
mini_data <- model_data[sample(nrow(model_data), 100000),]

#### Regressions
## Sets of variables
ind_vars_base <-c(
  # Demographics
  'CommercialData_Education','CommercialData_EstimatedHHIncomeAmount',
  # Race
  'EthnicGroups_EthnicGroup1Desc',
  # Polling place type
  'location_category'
)

### Logistic Regression
#### Probability of turning out, location category as predictor
##### Base model
### set reference category
model_data$location_category <- relevel(model_data$location_category, ref = "public")
# model
log_reg(model_data, 'General_2018_11_06', ind_vars_base)

### Probability of turning out, if has/lacks child and is voting at a school

### Probability of turning out, if gov employee and is voting at gov building
### Probability of turning out, if religious and is voting at religious building
