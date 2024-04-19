#Nathaniel Flemming
# 19/3/24

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

# Read in location category data
get_cat_data <- function(dir, filename, vars){
  setwd(dir)
  cat<-read.csv(filename)
  ## remove unneeded columns
  cat <- subset(cat, select = vars)
  return(cat)
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
  #dataframe to store output in 
  output<-data.frame()
  # for dataframe with a succeeding dataframe
  for(i in 1:(length(df_list)-1)){
    # dataframe and succeeding dataframe
    df1<-df_list[[i]]
    df2<-df_list[[i+1]]
    # 'concatenate' outputs of comp_addrs together
    output<-rbind(output,comp_addrs(df1, df2, 'Address'))
    # Label the years
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

### Compare polling location category across years
comp_cat<-function(df1, df2, merge_v1, merge_v2, cat_v1, cat_v2){
  temp<-left_join(df1, df2, by=c(merge_v1, merge_v2))
  temp<-temp%>%
    select(merge_v1, merge_v2, cat_v1, cat_v2)
  # Check if x matches y
  temp$LocCategoryChanged<-(temp[cat_v1]!=temp[cat_v2] | 
                              (is.na(temp[cat_v1]) & !is.na(temp[cat_v2]) | 
                              !is.na(temp[cat_v1]) & is.na(temp[cat_v2])))
  return(temp)
}

## Calculate change in number of locations per category
net_cat_chng<-function(df, cat_v1='location_category.y', 
                       cat_v2='location_category.x', cat){
  # subtract succeeding year sum from previous year sum
  output<-sum(df[cat_v1]==cat, na.rm=T)-
    sum(df[cat_v2]==cat, na.rm=T)
  return(output)
}

## Calculate change in number of locations by category 
#### group_by variable hardcoded because of dplyr
sum_cat_chng<-function(df, cat_v='location_category.x'){
  output<-df %>%
    #group by location type
    group_by(location_category.x) %>%
    # sum number of category changes
    mutate(num_changes = sum(LocCategoryChanged)) %>%
    select(c(location_category.x, num_changes))%>%
    distinct()
  return(output)
}

### Save plot
save_plot<-function(plot_dir, filename, plotname, width=2000, height=1000){
  setwd(plot_dir)
  ggsave(filename,width=width, height=height, units = 'px', bg = "white", plot=plotname)
}


##################################################
#set directories
cat_dir <- 'C:/Users/natha/Desktop/Polling Places/data'
poll_dir <- 'C:/Users/natha/Desktop/Polling Places/data/gov_poll_places'
plot_dir <- "C:/Users/natha/Desktop/Polling Places/plots"
# get poll location data and process
filenames<-c('poll_struct_key_govsource18.csv', 'poll_struct_key_govsource19.csv',
             'poll_struct_key_govsource20.csv', 'poll_struct_key_govsource21.csv',
             'poll_struct_key_govsource22.csv', 'poll_struct_key_govsource23.csv')
for(file in filenames){
  assign(paste0('poll_loc',substr(file,26,27)),
         get_poll(cat_dir, file, num='HouseNum', direction='PrefixDirection',
                  street='Street', street_type = 'StreetType',city='City'))
}

### put dataframes into a list
poll_dfs<-list('2018'=poll_loc18, '2019'=poll_loc19,
               '2020'=poll_loc20, '2021'=poll_loc21, 
               '2022'=poll_loc22, '2023'=poll_loc23)
## create separate lists before and after redistricting
poll_dfs_1819<-list('2018'=poll_loc18, '2019'=poll_loc19)
poll_dfs_2023<-list('2020'=poll_loc20, '2021'=poll_loc21, 
                    '2022'=poll_loc22, '2023'=poll_loc23)

# compare addresses in one year to addresses in another year
## loop through list
overallchng_yrtoyr<-comp_addrs_list(poll_dfs)
## compare years by county
countychng_yrtoyr<-comp_addrs_list_cnty(poll_dfs)

####match precincts year to year and see which changed their polling location
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


################## Number of location changes where the type of location changes
## flag changes in each year pair
for(i in 1:(length(poll_dfs)-1)){
  assign(paste0('poll_loccat_chng',paste(toString(names(poll_dfs)[i]),
                                         toString(names(poll_dfs)[i+1]),sep = '_')),
         comp_cat(poll_dfs[[i]], poll_dfs[[i+1]], 'PrecinctName', 'PrecinctCode',
                  'location_category.x', 'location_category.y'))
}
# put dataframes into a list
cat_chng_dfs<-list('2018_2019'=poll_loccat_chng2018_2019, '2019_2020'=poll_loccat_chng2019_2020,
               '2020_2021'=poll_loccat_chng2020_2021, '2021_2022'=poll_loccat_chng2021_2022, 
               '2022_2023'=poll_loccat_chng2022_2023)

###### Total number of type changes
##### move this into a function
ttl_type_chng<-data.frame()
for(i in 1:length(cat_chng_dfs)){
  ttl_type_chng<-rbind(ttl_type_chng, toString(names(cat_chng_dfs)[i]))
  ttl_type_chng$total_change[i]<-sum(cat_chng_dfs[[i]][5]==T, na.rm=T)
}
ttl_type_chng$total_change<-as.numeric(ttl_type_chng$total_change)
ttl_type_chng<-ttl_type_chng%>%rename(years=X.2018_2019.)
### Plot
ttl_type_chng_plot<-ttl_type_chng %>%
  ggplot(aes(x=years, y=total_change))+
  geom_col(fill='deepskyblue3') +
  labs(title = 'Changes in Polling Locations\' Categories Year to Year', x='Year Pairs', 
       y='Number of Locations')+
  theme_minimal()
ttl_type_chng_plot
save_plot(plot_dir, 'Changes_in_type_by_year.png', ttl_type_chng_plot,
          width=4000, height=2000)

##### Number of changes by category
ttl_chngs_by_cat_year<-data.frame()
for (i in 1:length(cat_chng_dfs)){
  temp<-sum_cat_chng(cat_chng_dfs[[i]])
  temp$years<-(toString(names(cat_chng_dfs)[i]))
  ttl_chngs_by_cat_year<-rbind(ttl_chngs_by_cat_year, temp)
}
## Plot
ttl_chngs_by_cat_year_plot<-ttl_chngs_by_cat_year %>%
  ggplot(aes(fill=location_category.x, x=location_category.x, y=num_changes))+
  geom_bar(position='dodge',stat='identity') +
  labs(title = 'Changes in Polling Locations\' Categories Year to Year', x='County', 
       y='Number of Locations Changed')+
  facet_grid(rows = vars(years))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_discrete(name='Category')
ttl_chngs_by_cat_year_plot
save_plot(plot_dir, 'Changes_in_type_by_cat_and_year.png', ttl_chngs_by_cat_year_plot,
          width=4000, height=2000)

##### Net change by category
# list of location categories
cat_list<-c('public_justice','public','multiple','school','religious','library',
            'justice','religious_school', 'other')
# loop through dataframes and categories
net_chng_by_cat_year<-data.frame()
for(i in 1:(length(cat_chng_dfs))){
  for(j in (1:length(cat_list))){
    # 'concatenate' outputs of sum category changes together
    net_chng_by_cat_year<-rbind(net_chng_by_cat_year, 
                            net_cat_chng(cat_chng_dfs[[i]],'location_category.y',
                                        'location_category.x',cat_list[j]))
    # Label the years
    net_chng_by_cat_year[['years']][((i-1)*length(cat_list))+j]<-paste(toString(names(cat_chng_dfs)[i]))
  }
}
# Label the sum column
net_chng_by_cat_year<-net_chng_by_cat_year%>%
  rename(net_change=X21L)
# Label the categories
net_chng_by_cat_year['category']<-rep(cat_list, length(cat_chng_dfs))
## Plot change in number of locations of each category
net_chng_by_cat_year_plot<-net_chng_by_cat_year %>%
  ggplot(aes(fill=category, x=category, y=net_change))+
  geom_bar(position='dodge',stat='identity') +
  labs(title = 'Changes in Polling Locations\' Types', x='Year Pair', 
       y='Number of Locations Changed')+
  facet_grid(rows = vars(years))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_discrete(name='Category')+
  geom_abline()
net_chng_by_cat_year_plot
save_plot(plot_dir, 'Net_changes_in_location_type.png', net_chng_by_cat_year_plot,
          width=4000, height=2000)

#### Percentage of location changes that change the type of location too
total_change<-overallchng_yrtoyr%>%select(years, total_change)
ttl_chngs_by_cat_year<-left_join(ttl_chngs_by_cat_year, total_change, by='years')
ttl_chngs_by_cat_year$perc_of_changes<-ttl_chngs_by_cat_year$num_changes/ttl_chngs_by_cat_year$total_change
## plot
perc_chngs_by_cat_year_plot<-ttl_chngs_by_cat_year %>%
  ggplot(aes(fill=location_category.x, x=location_category.x, y=perc_of_changes))+
  geom_bar(position='dodge',stat='identity') +
  labs(title = 'Changes in Polling Locations\' Types', x='Year Pair', 
       y='Percentage of Locations Changed that also Changed Category')+
  facet_grid(rows = vars(years))+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_discrete(name='Category')+
  geom_abline()
perc_chngs_by_cat_year_plot
save_plot(plot_dir, 'Percent_changes_in_location_type.png', perc_chngs_by_cat_year_plot,
          width=4000, height=2000)

#### Number of location changes that don't change category, within each category





