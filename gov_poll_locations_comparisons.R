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

### How many precinct's changed polling place for multiple year pairs
#### variable names hardcoded because of dplyr
sum_prec_chng_list<-function(data){
  output<-data %>%
    group_by(Years)%>%
    mutate(NumLocChanges = sum(LocationChanged, na.rm=T),
           PctChng = NumLocChanges/n())%>%
    select(Years, NumLocChanges, PctChng) %>%
    distinct()%>%
    ungroup()
  return(output)
}

### How many precinct's changed polling place by county
#### variable names hardcoded because of dplyr
county_prec_chng<-function(data){
  output<-data %>%
    group_by(CountyName.x)%>%
    mutate(NumLocChanges = sum(LocationChanged, na.rm=T),
           num_loc = n(),
           PctChng = NumLocChanges/num_loc)%>%
    select(CountyName.x, NumLocChanges, num_loc, PctChng) %>%
    distinct()%>%
    ungroup()
  return(output)
}

### How many precinct's changed polling place by county for multiple year pairs
#### variable names hardcoded because of dplyr
county_prec_chng_list<-function(data){
  output<-data %>%
    group_by(CountyName.x, Years)%>%
    mutate(NumLocChanges = sum(LocationChanged, na.rm=T),
           num_loc = n(),
           PctChng = NumLocChanges/num_loc)%>%
    select(Years, CountyName.x, NumLocChanges, num_loc, PctChng) %>%
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
  ## number of addresses in first and second year
  num_addr_yr1<-length(addrs_set1)
  num_addr_yr2<-length(addrs_set2)
  ## number of kept and dropped addresses
  kept_dropped<-addr_kept_dropped(addrs_set1, addrs_set2)
  ## number of added addresses
  added<-addr_added(addrs_set1, addrs_set2)
  # bind and return
  output<-cbind(data.frame(years),kept_dropped, added, data.frame(num_addr_yr1), 
                data.frame(num_addr_yr2))
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
    ## number of addresses in first and second year
    num_addr_yr1<-length(addrs_set1)
    num_addr_yr2<-length(addrs_set2)
    ## changes
    kept_dropped<-addr_kept_dropped(addrs_set1, addrs_set2)
    added<-addr_added(addrs_set1, addrs_set2)
    temp<-cbind(data.frame(years),data.frame(CountyName),kept_dropped, added,
                data.frame(num_addr_yr1), 
                data.frame(num_addr_yr2))
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

### compare dataframes of years in a list grouping by county
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

### Save plot
save_plot<-function(plot_dir, filename, plotname, width=2000, height=1000){
  setwd(plot_dir)
  ggsave(filename,width=width, height=height, units = 'px', bg = "white", plot=plotname)
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

####match precincts year to year and see which changed their polling location
## create separate lists before and after redistricting
poll_dfs_1819<-list('2018'=poll_loc2018, '2019'=poll_loc2019)
poll_dfs_2023<-list('2020'=poll_loc2020, '2021'=poll_loc2021, 
                    '2022'=poll_loc2022, '2023'=poll_loc2023)
locchng_yty_cnty<-prec_chng_list(poll_dfs_1819)
locchng_yty_cnty<-rbind(locchng_yty_cnty, prec_chng_list(poll_dfs_2023))

## See number of changes by county and year (variable names hardcoded because of dplyr)
locchng_yty_cnty_sum<-county_prec_chng_list(data=locchng_yty_cnty)
## Number of changes overall year to year
locchng_yty_sum<-sum_prec_chng_list(locchng_yty_cnty)
  

################# Plots 
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
save_plot(plot_dir, 'Changes_in_Addresses.png', ttl_chng_plot)

## Number of address adds, drops, changes per year pair, by county
cnty_chng_plot<-countychng_yrtoyr %>%
  pivot_longer(cols=c(num_addr_dropped, num_addr_add))%>%
  ggplot(aes(fill=name, x=CountyName, y=value))+
  geom_bar(position='stack',stat='identity') +
  labs(title = 'Changes in Addresses Year to Year', x='County', 
       y='Number of Addresses Changed')+
  facet_grid(rows = vars(years))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_discrete(name='',labels=c('Addresses Added', 'Addresses Dropped'))
cnty_chng_plot
save_plot(plot_dir, 'Changes_in_addresses_by_county.png', cnty_chng_plot,
          width=4000, height=2000)

## Number of location changes , year to year
loc_chng_plot<-locchng_yty_sum %>%
  ggplot(aes(x=Years, y=NumLocChanges))+
  geom_col(fill='deepskyblue3') +
  labs(title = 'Changes in Polling Locations Year to Year', x='Year Pairs', 
       y='Number of Locations')+
  theme_minimal()
loc_chng_plot
save_plot(plot_dir, 'Changes_in_location.png', loc_chng_plot)
## Percentage of location changes , year to year
loc_pctchng_plot<-locchng_yty_sum %>%
  ggplot(aes(x=Years, y=PctChng))+
  geom_col(fill='deepskyblue3') +
  labs(title = 'Changes in Polling Locations Year to Year', x='Year Pairs', 
       y='Proportion of Locations')+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()
loc_pctchng_plot
save_plot(plot_dir, 'Changes_in_location_percentage.png', loc_pctchng_plot,
          width=4000, height=2000)

## Number of location changes , year to year, by county
cnty_loc_chng_plot<-locchng_yty_cnty_sum %>%
  ggplot(aes(fill=CountyName.x, x=CountyName.x, y=NumLocChanges))+
  geom_bar(position='dodge',stat='identity') +
  labs(title = 'Changes in Polling Locations Year to Year', x='County', 
       y='Number of Locations Changed')+
  facet_grid(rows = vars(Years))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_discrete(name='County')
cnty_loc_chng_plot
save_plot(plot_dir, 'Changes_in_location_by_county.png', cnty_loc_chng_plot,
          width=4000, height=2000)

## Percentage of location changes , year to year, by county
cnty_loc_pctchng_plot<-locchng_yty_cnty_sum %>%
  ggplot(aes(fill=CountyName.x, x=CountyName.x, y=PctChng))+
  geom_bar(position='dodge',stat='identity') +
  labs(title = 'Changes in Polling Locations Year to Year', x='County', 
       y='Percentage of Locations Changed')+
  facet_grid(rows = vars(Years))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_discrete(name='County')+ 
  scale_y_continuous(labels = scales::percent)
cnty_loc_pctchng_plot
save_plot(plot_dir, 'Changes_in_location_by_county_percentage.png', cnty_loc_pctchng_plot,
          width=4000, height=2000)



