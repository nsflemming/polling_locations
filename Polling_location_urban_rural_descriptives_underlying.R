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

################# Additional
### Create voter location data file for ArcGIS
# demog_addr_2017<-read.csv(paste0(data_dir,'\\L2PA_2019_address_in_19.csv'))
# demog_addr_2017$Residence_Addresses_Latitude<-as.character(demog_addr_2017$Residence_Addresses_Latitude)
# demog_addr_2017$Residence_Addresses_Longitude<-as.character(demog_addr_2017$Residence_Addresses_Longitude)
# demog_addr_2017<-demog_addr_2017%>%
#   select(c(LALVOTERID, Residence_Addresses_AddressLine, Residence_Addresses_Latitude,
#            Residence_Addresses_Longitude, County, Precinct))
# write.csv(demog_addr_2017,'L2PA_2019_voter_locs_19.csv')

### L2 id State voter id cross walks
#L2_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\VM2_PA_2019_08_23'
#L2_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\VM2--PA--2018-08-22'
#L2_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data\\VoterMapping--PA--HEADERS--2017-08-05'
#crosswalk<-get_L2_data(L2_dir, 'VM2--PA--2018-08-22-DEMOGRAPHIC.tab', 
#                     c('LALVOTERID', 'Voters_StateVoterID'))
#write.csv(crosswalk, paste0(data_dir,'\\L2_StateID_crosswalk_18.csv'))

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
poll_data18<-read.csv(paste0(data_dir,'poll_struct_key_cath_govsource_underlying18.csv'))%>%
  select(c(location_category, PostalCode))
poll_data19<-read.csv(paste0(data_dir,'poll_struct_key_cath_govsource_underlying19.csv'))%>%
  select(c(location_category, PostalCode))%>%
  mutate(PostalCode=as.character(PostalCode))
poll_data20<-read.csv(paste0(data_dir,'poll_struct_key_cath_govsource_underlying20.csv'))%>%
  select(c(location_category, PostalCode))%>%
  mutate(PostalCode=as.character(PostalCode))
### Add years
poll_data18$year<-2018
poll_data19$year<-2019
poll_data20$year<-2020


## Create simplified location categories variable (subsume catholic into religious)
#poll_data_all<-simplify_loc_cat(poll_data_all,'location_category')
## recode missing location category to other or vice versa
poll_data_all<-recode_NAtoOther(poll_data_all,'location_category')
#poll_data<-recode_OthertoNA(poll_data,'location_category')

## Factorize location categories and set reference categories
poll_data_all<-factorize_set_ref(poll_data_all,'location_category',
                                 ref_cat = 'other')

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
poll_data18<-left_join(poll_data18,RUCA10,by='PostalCode')
poll_data19<-left_join(poll_data19,RUCA10,by='PostalCode')
poll_data20<-left_join(poll_data20,RUCA20,by='PostalCode')

#Combine years of poll data
poll_data_all<-rbind(poll_data18,poll_data19,poll_data20)
## set how 'other category is treated
#other_cond='OthersettoNA'
other_cond='NAsettoOther'
#loc_labels=loc_labels_OthersettoNA
loc_labels=loc_labels_NAsettoOther

# Manipulate poll data into plotting data
poll_data_all<-poll_data_all%>%
  group_by(year,Rural,location_category)%>%
  summarize(num_locs=n())%>%
  mutate(prop_locs=num_locs/sum(num_locs))%>%
  ungroup()

#rename location categories
# levels(poll_data_all$location_category_simpl)<-c('Other', 'Justice', 'Library',
#                                                  'Multiple', 'Public', 'Public/Justice',
#                                                  'Religious','Religious School','School')

# plot proportions by year and rurality
ggplot(poll_data_all, aes(x=fct_reorder(location_category, prop_locs), y=prop_locs,
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








############ Plot Proportion of voters in each location category
# voter poll location data
#poll_data<-read.csv(paste0(data_dir,'\\FVE_',year,'_polllocation_underlying.csv'))
poll_data18<-read.csv(paste0(data_dir,'\\FVE_2018_polllocation_underlying.csv'))%>%
  select(c(location_category))
poll_data19<-read.csv(paste0(data_dir,'\\FVE_2019_polllocation_underlying.csv'))%>%
  select(c(location_category))
poll_data20<-read.csv(paste0(data_dir,'\\FVE_2020_polllocation_underlying.csv'))%>%
  select(c(location_category))
###combine years of poll data
poll_data18$year<-2018
poll_data19$year<-2019
poll_data20$year<-2020
poll_data_all<-rbind(poll_data18,poll_data19,poll_data20)
rm(poll_data18,poll_data19,poll_data20)
# id cross walk
crosswalk<-read.csv(paste0(data_dir,'\\L2_StateID_crosswalk_',str_sub(year, start=-2),'.csv'))

## Create simplified location categories variable (subsume catholic into religious)
#poll_data<-simplify_loc_cat(poll_data,'location_category')

## set how 'other category is treated
#other_cond='OthersettoNA'
#other_cond='NAsettoOther'
#loc_labels=loc_labels_OthersettoNA
loc_labels=loc_labels_NAsettoOther
## recode missing location category to other or vice versa
poll_data_all<-recode_NAtoOther(poll_data_all,'location_category')
#poll_data<-recode_OthertoNA(poll_data,'location_category')

## Factorize location categories and set reference categories
poll_data_all<-factorize_set_ref(poll_data_all,'location_category',
                                 ref_cat = 'other')

##### Plot proportion of registered voters at each category of polling location
## manipulate poll data into plotting data
plot_data<-poll_data_all%>%
  group_by(year,location_category)%>%
  summarize(num_voters=n())%>%
  ungroup()%>%
  mutate(prop_voters=num_voters/sum(num_voters))


# plot counts
ggplot(plot_data, aes(x=fct_reorder(location_category, num_voters), y=num_voters,
                      group=factor(year), fill=factor(year)))+
  geom_col(position = 'dodge')+
  labs(title=paste0('Number of Voters Assigned to \nEach Polling Location Category'),
       x='Location Category',
       y='Percentage of Voters')+
  theme_minimal()+
  scale_fill_brewer(palette='Blues')+
  #scale_x_discrete(labels= loc_labels)+
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size=15, angle=30,hjust = 0.9,
                                   vjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 15))

# plot proportions
ggplot(plot_data, aes(x=fct_reorder(location_category, prop_voters), y=prop_voters,
                      group=factor(year), fill=factor(year)))+
  geom_col(position = 'dodge')+
  labs(title=paste0('Proportion of Voters Assigned to \nEach Polling Location Category'),
       x='Location Category',
       y='Percentage of Voters')+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_brewer(palette='Blues')+
  #scale_x_discrete(labels= loc_labels)+
  theme(plot.title = element_text(size = 20),
        axis.text.x = element_text(size=15, angle=30,hjust = 0.9,
                                   vjust = 1),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 15))

##### Plot registered voters at each category of polling location by rural urban divide
### Read in Data
# rurality data
setwd(data_dir)
rural_data<-read_excel('ruca2010revised.xlsx')
# Voter location data
voterloc_data<-read.csv(paste0(data_dir,'\\CensusTract_VoterLoc',str_sub(year,start=-2),'_SpatialJoin.csv'))
## Filter rurality data to PA
rural_data<-rural_data%>%
  filter(`Select State`=='PA')%>%
  select(c(`State-County FIPS Code`,`Select County`,
           `State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`,
           `Primary RUCA Code 2010`,`Secondary RUCA Code, 2010 (see errata)`))%>%
  rename(State_County_FIPS = `State-County FIPS Code`,
         County = `Select County`,
         State_County_Tract_FIPS=`State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`,
         Primary_RUCA=`Primary RUCA Code 2010`,
         Secondary_RUCA=`Secondary RUCA Code, 2010 (see errata)`)
# and bring county names into line with L2 and government data
rural_data$County<-str_extract(rural_data$County, '[:alpha:]*(?=[:space:])')
rural_data$County<-toupper(rural_data$County)
# set 99 (zero pop) to NA
rural_data$Primary_RUCA[rural_data$Primary_RUCA==99]<-NA
rural_data$Secondary_RUCA[rural_data$Secondary_RUCA==99]<-NA
# create urban-rural binaries
rural_data<-rural_data%>%
  #metropolitan (1-3) and non (4-10)
  mutate(metropolitan = case_when(Primary_RUCA<4 ~ 'Urban',
                                  Primary_RUCA>3 ~ 'Rural'),
         #metro and micropolitan
         metro_micro = case_when(Primary_RUCA<7 ~ 'Urban',
                                 Primary_RUCA>6 ~ 'Rural'))
### Merge rurality to voter location data on tract level FIPS codes
voterloc_data$FIPS<-as.character(voterloc_data$FIPS)
plot_data<-left_join(voterloc_data, rural_data, by=c('FIPS'='State_County_Tract_FIPS'))
## merge in state ids
plot_data<-left_join(plot_data, crosswalk, by='LALVOTERID')
### Merge location categories into plotting data
mini<-poll_data%>%
  select(c(VOTERID,location_category))
plot_data<-left_join(plot_data, mini, by=c('Voters_StateVoterID'='VOTERID'))


## summarize plotting data
plot_data_summ<-plot_data%>%
  # remove missing location category
  filter(!is.na(location_category),
         !is.na(metropolitan))%>%
  group_by(metropolitan,location_category)%>%
  summarize(num_voters=n())%>%
  mutate(prop_voters=num_voters/sum(num_voters))%>%
  ungroup()

### plot proportions by urban rural
# Plot
ggplot(plot_data_summ, aes(x=fct_reorder(location_category, prop_voters), y=prop_voters))+
  geom_col(fill='blue4')+
  labs(title=paste0('Proportion of Voters Assigned to \nEach Polling Location Category \nUrban vs. Rural ',year),
       x='Location Category',
       y='Proportion of Voters')+
  theme_minimal()+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle=45, hjust=1, size=12),
        axis.text.y = element_text(size=12),
        strip.text = element_text(size=13))+
  facet_wrap(~metropolitan, nrow=2)

