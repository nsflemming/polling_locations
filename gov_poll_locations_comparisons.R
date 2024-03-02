#Nathaniel Flemming
# 29/2/24

library(tidyverse)
library(data.table) #read in data selectively
library(openxlsx) #save crosstabs as excel sheet
library(stringr) #string manipulation
library(ggplot2) # plotting
library(gridExtra) # plot multiple graphs together

######### Functions
### create address from address components
make_address <- function(data, num, direction, street, street_type, city){
  data[['Address']]<-paste(data[[num]],data[[direction]],data[[street]], 
                           data[[street_type]], data[[city]], sep=' ')
  return(data)
}

### read in location data
get_poll <- function(dir, filename, num, direction, street, street_type, city){
  setwd(dir)
  data <- read.csv(filename)
  data<-make_address(data, num, direction, street, street_type, city)
  return(data)
}

### How many addresses kept/dropped year to year
addr_kept_dropped<-function(addrs_set1, addrs_set2){
  num_addr_kept<-sum(addrs_set1 %in% addrs_set2)
  kept<-data.frame(num_addr_kept)
  num_addr_dropped <- length(addrs_set1)-num_addr_kept
  dropped <- data.frame(num_addr_dropped)
  df<-cbind(kept,dropped)
  return(df)
}

### How many addresses added year to year
addr_added<-function(addrs_set1, addrs_set2){
  num_addr_add<-sum(!(addrs_set2 %in% addrs_set1))
  return(data.frame(num_addr_add))
}

### How many precinct's changed polling place
prec_chng<-function(data1,data2){
  vars<-c('CountyName', 'PrecinctCode','PrecinctName','Description','Address')
  temp1<-data1[vars]
  temp2<-data2[vars]
  #match precincts year to year by code and name
  output<-left_join(temp1,temp2, by=c('PrecinctCode','PrecinctName'))
  output[['LocationChanged']]<-output[['Address.x']]!=output[['Address.y']]
  return(output)
}

### How many precincts changed polling place
prec_chng_list<-function(df_list){
  output<-data.frame()
  for(i in 1:(length(df_list)-1)){
    df1<-df_list[[i]]
    df2<-df_list[[i+1]]
    vars<-c('CountyName', 'PrecinctCode','PrecinctName','Description','Address')
    temp1<-df1[vars]
    temp2<-df2[vars]
    #match precincts year to year by code and name
    matched<-left_join(temp1,temp2, by=c('PrecinctCode','PrecinctName'))
    matched[['LocationChanged']]<-matched[['Address.x']]!=matched[['Address.y']]
    matched[['Years']]<-paste(toString(names(df_list)[i]),
                                                  toString(names(df_list)[i+1]),
                                                  sep = '_')
    output<-rbind(output,matched)
  }
  return(output)
}

### How many precinct's changed polling place by county
#### variable names hardcoded because of dplyr
county_prec_chng<-function(data){
  output<-data %>%
    group_by(CountyName.x)%>%
    mutate(NumLocChanges = sum(LocationChanged, na.rm=T))%>%
    select(CountyName.x, NumLocChanges) %>%
    distinct()%>%
    ungroup()
  return(output)
}

### How many precinct's changed polling place by county for multiple year pairs
#### variable names hardcoded because of dplyr
county_prec_chng_list<-function(data){
  output<-data %>%
    group_by(CountyName.x, Years)%>%
    mutate(NumLocChanges = sum(LocationChanged, na.rm=T))%>%
    select(Years, CountyName.x, NumLocChanges) %>%
    distinct()%>%
    ungroup()
  return(output)
}


### Compare two sets of addresses
comp_addrs<-function(data1, data2, addr_var){
  #get dataframe year
  year1<-substr(deparse(substitute(data1)),9,12)
  year2<-substr(deparse(substitute(data2)),9,12)
  years<-paste(year1,year2,sep='_')
  #compare addresses
  ## sets of unique addresses
  addrs_set1<-unique(data1[[addr_var]])
  addrs_set2<-unique(data2[[addr_var]])
  ## number of kept and dropped addresses
  kept_dropped<-addr_kept_dropped(addrs_set1, addrs_set2)
  ## number of added addresses
  added<-addr_added(addrs_set1, addrs_set2)
  # bind and return
  output<-cbind(data.frame(years),kept_dropped, added)
  output[['total_change']]<-output[['num_addr_dropped']]+output[['num_addr_add']]
  output[['net_change']]<-output[['num_addr_add']]-output[['num_addr_dropped']]
  return(output)
} 

### Compare two sets of addresses grouping by county
comp_addrs_county<-function(data1, data2, addr_var){
  #get dataframe year
  year1<-substr(deparse(substitute(data1)),9,12)
  year2<-substr(deparse(substitute(data2)),9,12)
  years<-paste(year1,year2,sep='_')
  #compare addresses
  output<-data.frame()
  for(county in unique(data1[['CountyName']])){
    CountyName<-county
    addrs_set1<-unique(data1[[addr_var]][data1[['CountyName']]==county])
    addrs_set2<-unique(data2[[addr_var]][data1[['CountyName']]==county])
    kept_dropped<-addr_kept_dropped(addrs_set1, addrs_set2)
    added<-addr_added(addrs_set1, addrs_set2)
    temp<-cbind(data.frame(years),data.frame(CountyName),kept_dropped, added)
    temp[['total_change']]<-temp[['num_addr_dropped']]+temp[['num_addr_add']]
    temp[['net_change']]<-temp[['num_addr_add']]-temp[['num_addr_dropped']]
    output=rbind(output,temp)
  }
  return(output)
} 

## compare dataframes of years in a list
comp_addrs_list<-function(df_list){
  output<-data.frame()
  for(i in 1:(length(df_list)-1)){
    df1<-df_list[[i]]
    df2<-df_list[[i+1]]
    output<-rbind(output,comp_addrs(df1, df2, 'Address'))
    output[['years']][i]<-paste(toString(names(df_list)[i]),
                                toString(names(df_list)[i+1]),sep = '_')
  }
  return(output)
}

### compare dataframes of years in a lists grouping by county
comp_addrs_list_cnty<-function(df_list){
  output<-data.frame()
  for(i in 1:(length(df_list)-1)){
    df1<-df_list[[i]]
    df2<-df_list[[i+1]]
    output<-rbind(output,comp_addrs_county(df1, df2, 'Address'))
    output[['years']][((67*i)-66):(67*i)]<-paste(toString(names(df_list)[i]),
                                toString(names(df_list)[i+1]),sep = '_')
  }
  return(output)
}


##################################################
#set directories
poll_dir <- 'C:/Users/natha/Desktop/Polling Places/data/gov_poll_places'
plot_dir <- "C:/Users/natha/Desktop/Polling Places/plots"
# get data and process
filenames<-c('Polling Place List 20180514.csv','Polling Place List 20190513.csv',
             'Polling Place List 20201102.csv','Polling Place List 20211101.csv',
             'Polling Place List 20220506.csv','Polling Place List 20231106.csv')
for(file in filenames){
  assign(paste0('poll_loc',substr(file,20,23)),
         get_poll(poll_dir, file, num='HouseNum', direction='PrefixDirection',
                  street='Street', street_type = 'StreetType',city='City'))
}
# compare addresses in one year to addresses in another year
### put dataframes into a list
poll_dfs<-list('2018'=poll_loc2018, '2019'=poll_loc2019,
               '2020'=poll_loc2020, '2021'=poll_loc2021, 
               '2022'=poll_loc2022, '2023'=poll_loc2023)
## loop through list
overallchng_yrtoyr<-comp_addrs_list(poll_dfs)
## compare years by county
countychng_yrtoyr<-comp_addrs_list_cnty(poll_dfs)

#match precincts year to year and see which changed their polling location
locchng_yrtoyr<-prec_chng(poll_loc2018, poll_loc2019)
locchng_yrtoyr_sum<-locchng_yrtoyr
## create separate lists before and after redistricting
poll_dfs_1819<-list('2018'=poll_loc2018, '2019'=poll_loc2019)
poll_dfs_2023<-list('2020'=poll_loc2020, '2021'=poll_loc2021, 
                    '2022'=poll_loc2022, '2023'=poll_loc2023)
locchng_yrtoyr_cnty<-prec_chng_list(poll_dfs_1819)
locchng_yrtoyr_cnty<-rbind(locchng_yrtoyr_cnty, prec_chng_list(poll_dfs_2023))

## See number of changes by county and year (variable names hardcoded because of dplyr)
precchng_yrtoyr<-county_prec_chng_list(data=locchng_yrtoyr_cnty)

# Plots 
## Number of address adds, drops, changes per year pair
ttl_chng_plot<-overallchng_yrtoyr %>%
  pivot_longer(cols=c(num_addr_dropped, num_addr_add)) %>%
  ggplot(aes(fill=name, x=years, y=value))+
  geom_col() +
  labs(title = 'Changes in Addresses Year to Year', x='Year Pairs', 
       y='Number of Addresses')+
  scale_fill_discrete(name='',labels=c('Addresses Added', 'Addresses Dropped'))+
  theme_minimal()
ttl_chng_plot
## Number of location changes 









