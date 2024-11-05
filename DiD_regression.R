#Nathaniel Flemming
# 24/9/24
# 24/10/24

# Difference in difference regressions, using data created by DiD_preprocessing script

library(did) #difference in difference package, 3+ periods
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
model_data<-read.csv('DiD_prepped_loc_vote_16to18.csv')

# subset data to only people with entries in both years
# test_data<-model_data%>%
#   group_by(LALVOTERID)%>%
#   filter(n() > 1) %>%
#   ungroup()

# Modify variables to fit DiD package requirements
## ID
model_data$VOTERID<-str_sub(model_data$LALVOTERID, 6, nchar(model_data$LALVOTERID))
model_data$VOTERID<-as.numeric(model_data$VOTERID)
# set years to 0 and 1
#model_data$year<-model_data$year+2017
## treatment
model_data$changed_prec<-as.numeric(model_data$changed_prec)
model_data$no_move_new_precinct<-as.numeric(model_data$no_move_new_precinct)
model_data$moved_new_precinct<-as.numeric(model_data$moved_new_precinct)

#2 period data set (2017 and 18)
two_data<-model_data%>%
  filter(complete.cases(.))%>%
  filter(year!=2016)%>%
  group_by(LALVOTERID)%>%
  filter(n()>1)%>%
  ungroup()

#3 period data set
three_data<-model_data%>%
  filter(complete.cases(.))%>%
  group_by(LALVOTERID)%>%
  filter(n()>2)%>%
  ungroup()


# testing data sets
mini_data<-two_data[1:500000,]

two_data<-two_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  mutate(ever_changed_precinct=sum(changed_prec),
         ever_no_move_new_precinct=sum(no_move_new_precinct),
         ever_moved_new_precinct=sum(moved_new_precinct))

%>%
  # group into voter-years
  # set first treated year to year if treated is true
  group_by(year)%>%
  mutate(first_treated=ifelse(no_move_new_precinct==1, year, 0))%>%
  ungroup()

three_data<-three_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  mutate(ever_changed_precinct=sum(changed_prec),
         ever_no_move_new_precinct=sum(no_move_new_precinct),
         ever_moved_new_precinct=sum(moved_new_precinct))%>%
  # group into voter-years
  # set first treated year to year if treated is true
  group_by(year)%>%
  mutate(first_treated=ifelse(no_move_new_precinct==1, year, 0))%>%
  ungroup()%>%
  # set first treated year to minimum year that's not 0 for all voter's rows
  ## if all zero, keep 0
  group_by(LALVOTERID)%>%
  mutate(first_treated=ifelse(length(first_treated[first_treated>0])>0,
           min(first_treated[first_treated>0]), 0))%>%
  # Add outcome variable of voting in each year
  # group by voter-year
  group_by(LALVOTERID, year)%>%
  mutate(voted=case_when(
    ((General_2016_11_08==1) & (year==2016)) ~ 1,
    ((General_2017_11_07==1) & (year==2017)) ~ 1,
    ((General_2018_11_06==1) & (year==2018)) ~ 1,
    .default = 0 # Default value if none of previous conditional holds
  ))%>%
  ungroup()

#convert to integer to match year?
#test$first_treated<-as.integer(test$first_treated)
# test with fake outcome variable (works)
#test$outcome<-rnorm(nrow(test))

test<-mini_data%>%
  ungroup()%>%
  select(c(VOTERID,year,first_treated,voted,Voters_Gender,
           Voters_Age))

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
           data = two_data, family = 'binomial')
summary(model)




# ######## Estimating Group-Time Average Treatment Effect, 2-periods
# out0 <- drdid(yname = "General_2018_11_06", #outcome
#               dname = "ever_no_move_new_precinct", #treatment
#               idname = "VOTERID", #respondent
#               tname = "year",
#               xformla = ~Voters_Gender,
#               data = mini_data)
# 
# out0 #
#ggdid(out0)

# Estimating Group-Time Average Treatment Effects,3+ periods
## issue with outcome variable
set.seed(1234) # set seed important for bootstrap standard errors
out0 <- att_gt(yname = "voted",
               gname = "first_treated",
               idname = "VOTERID",
               tname = "year",
               xformla = ~Voters_Gender + Voters_Age,
               data = test_data)

out0 #
ggdid(out0)

############# example code
# Estimating Group-Time Average Treatment Effects
### no pretreatment periods for some reason
set.seed(1234) # set seed important for bootstrap standard errors
ex0 <- att_gt(yname = "nationalism_s",
               gname = "first_treat",
               idname = "respondent",
               tname = "year",
               xformla = ~age + education,
               data = d)
ex0 #2018 -2018 = group 3 first exposure
ggdid(ex0)
