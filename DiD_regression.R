#Nathaniel Flemming
# 24/9/24

# Difference in difference regressions

library(did) #difference in difference package
library(tidyverse) #convenience
library(data.table) #read in data selectively


########## Functions
## Binarize vote variables
binarize_vote <- function(data, vote_var, yes_vote_value){
  data[[vote_var]] <- ifelse(data[[vote_var]]==yes_vote_value,1,0)
  return(data)
}


########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
# read in data
setwd(data_dir)
model_data<-read.csv('L2PA_full.csv')

### variable modifications
# Convert vote to binary
model_data <- binarize_vote(model_data, 'General_2018_11_06', 'Y')
model_data <- binarize_vote(model_data, 'General_2017_11_07', 'Y')

test_data<-sample_n(model_data, 10000)


######## Estimating Group-Time Average Treatment Effects
out0 <- att_gt(yname = "nationalism_s",
               gname = "first_treat",
               idname = "respondent",
               tname = "year",
               xformla = ~age + education,
               data = d)
out0 #2018 -2018 = group 3 first exposure
ggdid(out0)


######## Establish parallel trends


