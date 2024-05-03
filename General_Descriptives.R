#Nathaniel Flemming
# 25/4/24

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
col_names<-c('CommercialData_LikelyUnion','CommercialData_OccupationIndustry',
             'has_child','known_religious','known_gov_emp','known_repub',
             'pub_loc','pub_just','other','relig_loc','school','multiple','justice_loc',
             'library','relig_school')
model_data[,col_names]<-lapply(model_data[,col_names],factor)
model_data$CommercialData_OccupationGroup<-factor(model_data$CommercialData_OccupationGroup)
# Drop unused levels
model_data<-droplevels(model_data)

## subset data for initial testing
#mini_data <- model_data[sample(nrow(model_data), 100000),]

## create vector of location categories
categories<-c('pub_loc','pub_just','other','relig_loc','school','multiple',
              'justice_loc','library','relig_school ')


############################ Crosstab of voting frequency at location categories
votetable<-table(model_data$General_2018_11_06, model_data$location_category)
prop.table(votetable, 2)
