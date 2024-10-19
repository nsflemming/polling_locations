#Nathaniel Flemming
# 24/9/24
# 17/10/24

# Checking assumption for DiD, using data created by DiD_preprocessing script

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

# Modify variables to fit DiD package requirements
## ID
model_data$VOTERID<-str_sub(model_data$LALVOTERID, 6, nchar(model_data$LALVOTERID))
model_data$VOTERID<-as.numeric(model_data$VOTERID)
# set years to 0 and 1
model_data$year<-model_data$year-2017
## treatment indicators
model_data<-model_data%>%
  group_by(LALVOTERID)%>%
  mutate(ever_changed_poll_loc=sum(changed_poll_loc),
         ever_no_move_new_precinct=sum(no_move_new_precinct),
         ever_moved_new_precinct=sum(moved_new_precinct))%>%
  ungroup()

#Trim data
##### proof of concept, currently uses wrong election years
plot_data<-model_data%>%
  select(c(year, General_2018_11_06,General_2019_11_05,ever_no_move_new_precinct))
# split data by group
pd_t<-plot_data[plot_data$ever_no_move_new_precinct==1,]
pd_c<-plot_data[plot_data$ever_no_move_new_precinct==0,]

pd_t$turnout[pd_t$year==0]<-sum(pd_t$General_2018_11_06, na.rm = T)/nrow(pd_t)
pd_t$turnout[pd_t$year==1]<-sum(pd_t$General_2019_11_05, na.rm = T)/nrow(pd_t)
pd_t<-pd_t%>%
  select(c(year,turnout,ever_no_move_new_precinct))%>%
  distinct()

pd_c$turnout[pd_c$year==0]<-sum(pd_c$General_2018_11_06, na.rm = T)/nrow(pd_c)
pd_c$turnout[pd_c$year==1]<-sum(pd_c$General_2019_11_05, na.rm = T)/nrow(pd_c)
pd_c<-pd_c%>%
  select(c(year,turnout,ever_no_move_new_precinct))%>%
  distinct()
pd_c<-pd_c[complete.cases(pd_c),]

#recombine groups
pd<-rbind(pd_t,pd_c)

ggplot(pd,aes(x=year,y=turnout,colour=ever_no_move_new_precinct)) +
  geom_point()+
  geom_line(aes(group = ever_no_move_new_precinct))+
  ylab("% Turnout") +
  xlab("Year") +
  ggtitle("Parallel Trends?") +
  theme_minimal()+
  theme(legend.position="none")





