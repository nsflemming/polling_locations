#Nathaniel Flemming
#9/26/24

# Create a data file with multiple years of voting, voter data and poll locations for
#   DiD analyses

#### Libraries

library(tidyverse)
library(data.table) #read in L2 data selectively
library(stringr) #string manipulation
library(fuzzyjoin) #fuzzy match precinct names
library(stringdist) #fuzzy match precinct names


####### Functions
# Read in poll location data
get_poll_data <- function(poll_dir, filename, vars){
  setwd(poll_dir)
  poll<-read.csv(filename)
  ## remove unneeded columns
  poll <- subset(poll, select = vars)
  return(poll)
}



########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"

# read in data
setwd(data_dir)
## Processed L2 data with 2018 locations
L2_data<-read.csv('L2PA_full.csv')
# Read in 2019 location/category data
poll <- get_poll_data(data_dir, 'poll_struct_key_govsource19.csv', 
                      c('CountyName', 'PrecinctName', 'location_category'))




