#Nathaniel Flemming
# 24/9/24

# Difference in difference regressions, using data created by DiD_preprocessing script

library(did) #difference in difference package, 3+ periods
library(DRDID) #difference in difference package, 2 periods
library(tidyverse) #convenience
library(data.table) #read in data selectively
library(stringr) #id string manipulation

# Load data in long format that comes in the DRDID package
data(nsw_long)
# Form the Lalonde sample with CPS comparison group
eval_lalonde_cps <- subset(nsw_long, nsw_long$treated == 0 | nsw_long$sample == 2)
drdidex <- drdid(yname = "re",
             tname = "year", 
             idname = "id", 
             dname = "experimental", 
             xformla= ~ age + educ + black + married + nodegree + hisp + re74, 
             data = eval_lalonde_cps, 
             panel = TRUE) 
summary(drdidex)
########## Functions



########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
# read in data
setwd(data_dir)
model_data<-read.csv('DiD_prepped_loc_vote_18to19.csv')

# subset data to only people with entries in both years
# test_data<-model_data%>%
#   group_by(LALVOTERID)%>%
#   filter(n() > 1) %>%
#   ungroup()

# #2 period data set (2017 and 18)
# two_data<-model_data%>%
#   filter(complete.cases(.))%>%
#   filter(year!=2016)%>%
#   group_by(LALVOTERID)%>%
#   filter(n()>1)%>%
#   ungroup()
two_data<-model_data%>%
  filter(complete.cases(.))%>% #remove rows with missing data
  group_by(LALVOTERID)%>% 
  filter(n()>1)%>% # remove voters with only one year
  ungroup()
# create a time period version of year
two_data$time_period<-two_data$year-2018


#3 period data set
# three_data<-model_data%>%
#   filter(complete.cases(.))%>%
#   group_by(LALVOTERID)%>%
#   filter(n()>2)%>%
#   ungroup()


### create indicators for ever being treated
#two_data<-two_data%>%
two_data<-two_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  mutate(ever_changed_precinct=sum(changed_prec), #300,838 cases (2%)
         ever_no_move_new_precinct=sum(no_move_new_precinct), #367,478 cases (3%)
         ever_moved_new_precinct=sum(moved_new_precinct))%>% #66,640 cases (0.5%)
  #create single variable that indicates if someone voted in a given year
  group_by(year)%>%
  mutate(voted = ((year==2018 & General_2018_11_06==1)|(year==2019 & General_2019_11_05==1)))%>%
  #ungroup()
  # group into voter-years
  # create first year treated variable
  # group_by(year)%>%
  # # create year changed precinct variable
  mutate(yr_changed_prec=ifelse(changed_prec==1, year, 0),
         yr_no_move_new_precinct=ifelse(no_move_new_precinct==1, year, 0),
         yr_moved_new_precinct=ifelse(moved_new_precinct==1, year, 0))%>%
  # ungroup back to voter
  ungroup()
  # group_by(LALVOTERID)%>%
  # # create first year treated by lowest year for which treatment indicator is 1, else 0
  # mutate(FT_changed_prec = ifelse(1%in%changed_prec, min(year[changed_prec==1]),0),
  #        FT_no_move_new_precinct = ifelse(1%in%no_move_new_precinct, min(year[no_move_new_precinct==1]),0),
  #        FT_moved_new_precinct = ifelse(1%in%moved_new_precinct, min(year[moved_new_precinct==1]),0))

# three_data<-three_data%>%
#   # group by voter
#   group_by(LALVOTERID)%>%
#   mutate(ever_changed_precinct=sum(changed_prec),
#          ever_no_move_new_precinct=sum(no_move_new_precinct),
#          ever_moved_new_precinct=sum(moved_new_precinct))%>%
#   # group into voter-years
#   # set first treated year to year if treated is true
#   group_by(year)%>%
#   mutate(first_treated=ifelse(no_move_new_precinct==1, year, 0))%>%
#   ungroup()%>%
#   # set first treated year to minimum year that's not 0 for all voter's rows
#   ## if all zero, keep 0
#   group_by(LALVOTERID)%>%
#   mutate(first_treated=ifelse(length(first_treated[first_treated>0])>0,
#            min(first_treated[first_treated>0]), 0))%>%
#   # Add outcome variable of voting in each year
#   # group by voter-year
#   group_by(LALVOTERID, year)%>%
#   mutate(voted=case_when(
#     ((General_2016_11_08==1) & (year==2016)) ~ 1,
#     ((General_2017_11_07==1) & (year==2017)) ~ 1,
#     ((General_2018_11_06==1) & (year==2018)) ~ 1,
#     ((General_2019_11_05==1) & (year==2019)) ~ 1,
#     .default = 0 # Default value if none of previous conditional holds
#   ))%>%
#   ungroup()

############ testing data
## order by voterid
two_data <- two_data[order(two_data$VOTERID),]
mini_data<-two_data[1:500000,]

rigged_data<-two_data[1:1000000,c('VOTERID','voted','General_2019_11_05','ever_changed_precinct','year')]
#changed_prec<- rbinom(500000,0:1,.015)
#changed_prec<-rep(changed_prec,each=2)
#rigged_data$ever_changed_precinct<-changed_prec
#prob_vote<-runif(n=1000000,min=0,max=1)
#normal_probs <- pmax(pmin(rnorm(1000000, mean = 0.3771259, sd = 0.4846669), 1), 0)
#rigged_data$General_2019_11_05<-round(rigged_data$ever_changed_precinct*0.002+normal_probs, digits=0)
#rigged_data$General_2019_11_05<-round(prob_vote, digits=0)
out0 <- drdid(yname = "voted", #outcome
              dname = "ever_changed_precinct", #treatment group 
              idname = "VOTERID", #respondent
              tname = "year",
              data = rigged_data)
out0

#convert to integer to match year?
#test$first_treated<-as.integer(test$first_treated)
# test with fake outcome variable (works)
#test$outcome<-rnorm(nrow(test))
# 
# test<-mini_data%>%
#   ungroup()%>%
#   select(c(VOTERID,year,first_treated,voted,Voters_Gender,
#            Voters_Age))

##### DiD
## compare change in outcome over time b/t treatment & control groups
## Unit fixed effects will account for time-invariant differences (within unit variation).
## Time fixed effects will account for time-variant differences that are
##    constant across units (within time variation).
## Two-way fixed effect (unit and time FE) = DiD.

## compare change in probability of voting for people who changed precinct and people who didn't

###### Fixed effects regression framework, 2 periods, glm
### NOTE: some covariates switch year to year probably shouldn't (like gender), likely error
## Manual calculation
# c_pre<-mean(two_data$General_2018_11_06[(two_data$ever_changed_precinct==0 & two_data$time_period==0)])
# c_post<-mean(two_data$General_2019_11_05[(two_data$ever_changed_precinct==0 & two_data$time_period==1)])
# t_pre<-mean(two_data$General_2018_11_06[two_data$ever_changed_precinct==1 & two_data$time_period==0])
# t_post<-mean(two_data$General_2019_11_05[two_data$ever_changed_precinct==1 & two_data$time_period==1])
# 
# difference_treated = t_post - t_pre #diff in trtment grp
# difference_control = c_post - c_pre #diff in control grp
# difference_in_differences = difference_treated  - difference_control
# difference_in_differences #0.002115474

## TWFE glm
mini_model<-glm(voted~ever_changed_precinct*year,
           data = two_data, family = 'binomial')
summary(mini_model)
# save model summary
setwd(results_dir)
sink("move_changed_prec_2WFE_mini_model_summary.txt")
print(summary(mini_model))
sink()

## TWFE glm, control variables added incorrectly
# model<-glm(voted~ever_changed_precinct*time_period #interaction of being and treatment group and pre/post
#            +Voters_Gender
#            +Residence_Families_HHCount
#            +known_religious,
#            data = two_data, family = 'binomial')
# summary(model)
# # save model summary
# setwd(results_dir)
# sink("changed_prec_2WFE_model_summary.txt")
# print(summary(model))
# sink()

# ######## Estimating Group-Time Average Treatment Effect, 2-periods
#covariates, must be time invariant
### setting covariates to value at first time point to make them invariant (just for testing purposes)
### Reminder to probably not include covariates that could be post-treatment, i.e. affected by changing precinct
two_data<-two_data%>%
  group_by(VOTERID)%>%
  mutate(Voters_Gender = Voters_Gender[1])

out0 <- drdid(yname = "voted", #outcome
              dname = "ever_moved_new_precinct", #treatment group 
              idname = "VOTERID", #respondent
              tname = "year",
              #xformla = ~Voters_Gender, 
              data = two_data)
out0
# save model summary
setwd(results_dir)
sink("moved_new_precinct_DRDiD_model_summary.txt")
print(out0)
sink()

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
