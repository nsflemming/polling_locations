#Nathaniel Flemming
# 24/9/24
# 17/10/24

# Difference in difference regressions, using data created by DiD_preprocessing script

#library(did) #difference in difference package, 3+ periods
library(DRDID) #difference in difference package, 2 periods
library(tidyverse) #convenience
library(data.table) #read in data selectively
library(stringr) #id string manipulation


########## Functions



########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
# read in data
setwd(data_dir)
model_data<-read.csv('DiD_prepped_loc_vote_17_18.csv')

# subset data to only people with entries in both years
# test_data<-model_data%>%
#   group_by(LALVOTERID)%>%
#   filter(n() > 1) %>%
#   ungroup()

# Modify variables to fit DiD package requirements
## treatment
model_data$changed_poll_loc<-as.numeric(model_data$changed_poll_loc)
## ID
model_data$VOTERID<-str_sub(model_data$LALVOTERID, 6, nchar(model_data$LALVOTERID))
model_data$VOTERID<-as.numeric(model_data$VOTERID)
# set years to 0 and 1
model_data$year<-model_data$year-2017

model_data<-model_data%>%
   group_by(LALVOTERID)%>%
   mutate(ever_changed_poll_loc=sum(changed_poll_loc),
          ever_no_move_new_precinct=sum(no_move_new_precinct),
          ever_moved_new_precinct=sum(moved_new_precinct))%>%
   ungroup()

mini_data<-test_data[1:100000,]



##### DiD
## compare change in outcome over time b/t treatment & control groups
## Unit fixed effects will account for time-invariant differences (within unit variation).
## Time fixed effects will account for time-variant differences that are
##    constant across units (within time variation).
## Two-way fixed effect (unit and time FE) = DiD.

## compare change in probability of voting for people who changed precinct and people who didn't

###### Fixed effects regression framework, 2 periods, glm
model<-glm(General_2018_11_06~ever_no_move_new_precinct*year+Voters_Gender+Voters_Age
           +CommercialData_EstimatedHHIncomeAmount+Residence_Families_HHCount
           +known_religious+CommercialData_LikelyUnion+CommercialData_OccupationIndustry,
           data = model_data, family = 'binomial')
summary(model)




######## Estimating Group-Time Average Treatment Effects
out0 <- drdid(yname = "General_2018_11_06", #outcome
              dname = "ever_no_move_new_precinct", #treatment
              idname = "VOTERID", #respondent
              tname = "year",
              xformla = ~Voters_Gender,
              data = mini_data)

out0 #
#ggdid(out0)


######## Establish parallel trends


