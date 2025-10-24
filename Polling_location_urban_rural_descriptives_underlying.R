#Nathaniel Flemming
#9/11/24

## Rural Urban distribution of polling locations
### also using voter location files created through ArcGIS placing voters in census tracts
#### Full breakdown into all categories, rather than lumping some into other


################# Libraries
library(tidyverse)
library(data.table) #read in data selectively
library(stringr) #string manipulation
library(forcats) # plot bars in order of value
library(readxl) # read excel sheets

################# Functions
### Not in
'%!in%' <- function(x,y)!('%in%'(x,y))

### Read in L2 data selectively
get_L2_data <- function(L2_dir, filename, vars){
  setwd(L2_dir)
  L2<-fread(filename, sep='\t', header=T, select = vars)
  return(L2)
}


## Create simplified location categories variable (subsume catholic into religious)
simplify_loc_cat<-function(data, orig_var, simple_var){
  data[[simple_var]]<-data[[orig_var]]
  data[[simple_var]][data[[simple_var]]=='catholic_church']<-'religious'
  data[[simple_var]][data[[simple_var]]=='catholic_school']<-'religious_school'
  return(data)
}


## recode missing location category to other
recode_NAtoOther<-function(data, loc_cat){
  data[[loc_cat]][is.na(data[[loc_cat]])]<-'MISSING'
  return(data)
}

## recode other to missing
recode_OthertoNA<-function(data, loc_cat){
  data[[loc_cat]][data[[loc_cat]]=='other']<-NA
  return(data)
}


## Factorize location categories and set reference categories
factorize_set_ref<-function(data, loc_cat, ref_cat){
  data[[loc_cat]]<-as.factor(data[[loc_cat]])
  data[[loc_cat]]<-relevel(data[[loc_cat]], ref=ref_cat)
  return(data)
}


################## Main
### set directories
data_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\'
### set year
year='2018'

## Create vector of location category labels
loc_labels_NAsettoOther<-c('Public Location','Public/Justice Location','Other',
                           'Religious Location','School','Multiple Categories',
                           'Justice Location','Library','Religious School',
                           'Catholic School','Catholic Church','Catholic Location',
                           'Fire Station','Police Station','Religious','Post Office',
                           'Court House','Government','MISSING')
# loc_labels_OthersettoNA<-c('Multiple Categories','Justice Location','Library',
#                            'Public Location','Public/Justice Location','Religious Location',
#                            'Religious School','School')
## Create dictionary of location labels
loc_dict<-c('pub_loc'='Public Location','pub_just'='Public and Justice Location',
            'other'='Other','relig_loc'='Religious Location','school'='School',
            'multiple'='Multiple Categories', 'justice_loc'='Justice Location',
            'library'='Library', 'relig_school'='Religious School',
            'catholic_school'='Catholic School','catholic_church'='Catholic Church',
            'cath_loc'='Catholic Location',
            'firestation'='Fire Station','policestation'='Police Station',
            'religious'='Religious','postoffice'='Post Office','courthouse'='Court House',
            'citytownhall'='Government','missing'='MISSING')

############# Plot proportion of polling locations in rural vs urban zip codes
### read in poll location file
poll_data17<-read.csv(paste0(data_dir,'poll_struct_key_cath_govsource_underlying17.csv'))%>%
  select(c(location_category, PostalCode, PrecinctName, PrecinctCode, address))
poll_data18<-read.csv(paste0(data_dir,'poll_struct_key_cath_govsource_underlying18.csv'))%>%
  select(c(location_category, PostalCode, PrecinctName, PrecinctCode, address))
poll_data19<-read.csv(paste0(data_dir,'poll_struct_key_cath_govsource_underlying19.csv'))%>%
  select(c(location_category, PostalCode, PrecinctName, PrecinctCode, address))%>%
  mutate(PostalCode=as.character(PostalCode))
poll_data20<-read.csv(paste0(data_dir,'poll_struct_key_cath_govsource_underlying20.csv'))%>%
  select(c(location_category, PostalCode, PrecinctName, PrecinctCode, address))%>%
  mutate(PostalCode=as.character(PostalCode))
### Add years
poll_data17$year<-2017
poll_data18$year<-2018
poll_data19$year<-2019
poll_data20$year<-2020


# ## Factorize location categories and set reference categories
# poll_data_all<-factorize_set_ref(poll_data_all,'location_category',
#                                  ref_cat = 'other')

# Read in urban rural codes
## 2010
RUCA10<-read.csv(paste0(data_dir,'RUCA Codes\\RUCA2010zipcode.csv'))%>%
  filter(STATE=='PA')%>%
  mutate(# Create postal code variable
    PostalCode = str_extract(X..ZIP_CODE..,"(?<=[:punct:])[:digit:]+"),
    # Create binary rural urban classification
         RUCA.Binary = ifelse(RUCA1>3,"Rural","Urban"),
    Rural = RUCA1>3)%>%
  select(all_of(c('PostalCode','Rural')))
#2020
RUCA20<-read.csv(paste0(data_dir,'RUCA Codes\\RUCA-codes-2020-zipcode.csv'))%>%
  filter(State=='PA')%>%
  mutate(# Create postal code variable
    PostalCode = as.character(ZIPCode),
    # Create binary rural urban classification
    RUCA.Binary = ifelse(PrimaryRUCA>3,"Rural","Urban"),
    Rural = PrimaryRUCA>3)%>%
  select(all_of(c('PostalCode','Rural')))

# Merge in Rural codes to polling locations
poll_data17<-left_join(poll_data17,RUCA10,by='PostalCode')
poll_data18<-left_join(poll_data18,RUCA10,by='PostalCode')
poll_data19<-left_join(poll_data19,RUCA10,by='PostalCode')
poll_data20<-left_join(poll_data20,RUCA20,by='PostalCode')

#Combine years of poll data
poll_data_all<-rbind(poll_data17,poll_data18,poll_data19,poll_data20)
## set how 'other category is treated
#other_cond='OthersettoNA'
other_cond='NAsettoOther'
#loc_labels=loc_labels_OthersettoNA
loc_labels=loc_labels_NAsettoOther

# Manipulate poll data into plotting data
poll_data_all_summ<-poll_data_all%>%
  group_by(year,Rural,location_category)%>%
  summarize(num_locs=n())%>%
  mutate(prop_locs=num_locs/sum(num_locs))%>%
  ungroup()

#rename location categories
# levels(poll_data_all$location_category_simpl)<-c('Other', 'Justice', 'Library',
#                                                  'Multiple', 'Public', 'Public/Justice',
#                                                  'Religious','Religious School','School')

# plot proportions by year and rurality
ggplot(poll_data_all_summ, aes(x=fct_reorder(location_category, prop_locs), y=prop_locs,
                          group=factor(Rural), fill=factor(Rural)))+
  geom_col(position='dodge')+
  labs(title='Percentage of Polling Locations in \nEach Location Category',
       x='Location Category',
       y='Percentage of Locations',
       fill='Rural')+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette='Blues')+
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size=15, angle=30,hjust = 0.9,
                                   vjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 15))+
  facet_grid(rows=vars(year))



# Plot proportion of locations that change year to year rural vs. urban
#    Do urban polling locations change more often?
## Plot 2018 to 2020 (all use the same precinct names & codes)
poll_data_18to20<-poll_data_all%>%
  filter(year>2017)
### Calculate the number of polling location addresses that change year to year
poll_data_18to20_change<-poll_data_18to20%>%
  # Combine precinct name and code to uniquely(?) identify locations
  mutate(PrecinctNameCode = paste(PrecinctName, PrecinctCode, sep=' '))%>%
  group_by(PrecinctNameCode)%>%
  # compare polling location address year to year and flag if it changes
  ## arrange by year
  arrange(year)%>%
  ## compare locations
  mutate(Location_Change = (lag(address)!= address))

### Summarize poll data into plotting data
poll_data_18to20_change_summ<-poll_data_18to20_change%>%
  group_by(year,Rural,Location_Change)%>%
  summarize(num_locs=n())%>%
  mutate(prop_locs=num_locs/sum(num_locs))%>%
  ungroup()

### plot number of locations that changed by year and rurality
ggplot(poll_data_18to20_change_summ,
       aes(x=Location_Change, y=prop_locs,
                               group=factor(Rural), fill=factor(Rural)))+
  geom_col(position='dodge')+
  labs(title='Percentage of Polling Locations which Changed from the Previous Year',
       x='Location Changed',
       y='Percentage of Locations',
       fill='Rural')+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette='Blues')+
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size=15, angle=30,hjust = 0.9,
                                   vjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 15))+
  facet_grid(rows=vars(year))


## Cruder address list comparison to plot changes from 2017 to 2018
### Calculate the number of polling location addresses that change year to year
poll_data_all$Address.Absent[poll_data_all$year==2017]<-poll_data_all$address[poll_data_all$year==2017]%!in%poll_data_all$address[poll_data_all$year==2018]
poll_data_all$Address.Absent[poll_data_all$year==2018]<-poll_data_all$address[poll_data_all$year==2018]%!in%poll_data_all$address[poll_data_all$year==2019]
poll_data_all$Address.Absent[poll_data_all$year==2019]<-poll_data_all$address[poll_data_all$year==2019]%!in%poll_data_all$address[poll_data_all$year==2020]

### Summarize poll data into plotting data
poll_data_all_change_summ<-poll_data_all%>%
  group_by(year,Rural,Address.Absent)%>%
  summarize(num_locs=n())%>%
  mutate(prop_locs=num_locs/sum(num_locs))%>%
  ungroup()

### plot number of locations that changed by year and rurality
ggplot(poll_data_all_change_summ,
       aes(x=Address.Absent, y=prop_locs,
           group=factor(Rural), fill=factor(Rural)))+
  geom_col(position='dodge')+
  labs(title='Percentage of Addresses that \nweren\'t a Location the Next Year',
       x='Location Changed',
       y='Percentage of Locations',
       fill='Rural')+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette='Blues')+
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size=15, angle=30,hjust = 0.9,
                                   vjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 15))+
  facet_grid(rows=vars(year))








