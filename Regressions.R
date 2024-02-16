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
  return(m_base)
}
  



########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places/plots"
# read in data
setwd(data_dir)
model_data<-read.csv('L2PA_full.csv')
load('PA_2016_race.Rdata')
race16<-df.pred
load('PA_2017_race.Rdata')
race17<-df.pred
load('PA_2018_race.Rdata')
race18<-df.pred
rm(df.pred)
## merge in race estimates
model_data<-left_join(model_data, race16, by='LALVOTERID')
model_data<-left_join(model_data, race17, by='LALVOTERID')
model_data<-left_join(model_data, race18, by='LALVOTERID')

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
## Create factor variables
# Convert location category to factor
model_data$location_category <- as.factor(model_data$location_category)
# Convert parties to factor
model_data$Parties_Description <- as.factor(model_data$Parties_Description)
## Create Binary Variables
# Convert household composition to child yes/no
model_data$has_child <- str_detect(model_data$CommercialData_HHComposition, 'Children|children')
# Convert religious description to know religious yes/no, set blank to no
model_data$known_religious <- model_data$Religions_Description!=""
# Convert gender to religious M/F, set blank to missing
model_data$Voters_Gender <- ifelse(model_data$Voters_Gender=="",NA,model_data$Voters_Gender)
## Create Location dummy variables
model_data$school <- model_data$location_category=='school' 
model_data$relig_loc <- model_data$location_category=='religious' 
model_data$justice_loc <- model_data$location_category=='justice' 
# Create black dummy variable
## most probably race category
model_data_sub<-model_data[,c('pred.whi_2018', 'pred.bla_2016', 'pred.his_2016',
                              'pred.asi_2018', 'pred.oth_2018')]
model_data <- mutate(model_data, pred_race = names(model_data_sub)[max.col(model_data_sub)])
model_data$pred_black<-ifelse(model_data$pred_race=='pred.bla_2018', TRUE, FALSE)
model_data$pred_race<-as.factor(model_data$pred_race)
## subset data to usable size for initial testing
mini_data <- model_data[sample(nrow(model_data), 100000),]

#### Regressions
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
### set reference categories
model_data$location_category <- relevel(model_data$location_category, ref = "apartment")
model_data$Parties_Description <- relevel(model_data$Parties_Description, ref = "Democratic")
model_data$pred_race <- relevel(model_data$pred_race, ref = "pred.whi_2018")
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

### Probability of turning out, if gov employee and is voting at gov building
#vars
ind_vars_gov_emp <-c(
  # var of interest
  'has_child*school',
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'CommercialData_Education','CommercialData_EstimatedHHIncomeAmount',
  # Race
  'pred_race'
)
# model
m_gov<-log_reg(model_data, 'General_2018_11_06', ind_vars_gov_emp)
summary(m_gov)

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
