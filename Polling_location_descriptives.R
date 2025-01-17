#Nathaniel Flemming
#7/11/24

## Descriptive of polling locations using file from Combine_voterfile_polllocation
### also using voter location files created through ArcGIS placing voters in census tracts


################# Libraries
library(tidyverse)
library(data.table) #read in data selectively
library(stringr) #string manipulation
library(forcats) # plot bars in order of value
library(readxl) # read excel sheets

################# Functions
### Read in L2 data selectively
get_L2_data <- function(L2_dir, filename, vars){
  setwd(L2_dir)
  L2<-fread(filename, sep='\t', header=T, select = vars)
  return(L2)
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
data_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data'
### set year
year='2018'

### read in data
# voter poll location data
poll_data<-read.csv(paste0(data_dir,'\\FVE_',year,'_polllocation.csv'))
# id cross walk
crosswalk<-read.csv(paste0(data_dir,'\\L2_StateID_crosswalk_',str_sub(year, start=-2),'.csv'))


##### Plot proportion of registered voters at each category of polling location
## manipulate poll data into plotting data
plot_data<-poll_data%>%
  group_by(location_category)%>%
  summarize(num_voters=n())%>%
  ungroup()%>%
  mutate(prop_voters=num_voters/sum(num_voters))

# plot proportions
ggplot(plot_data, aes(x=fct_reorder(location_category, prop_voters), y=prop_voters))+
  geom_col(fill='blue4')+
  labs(title=paste0('Proportion of Voters Assigned to \nEach Polling Location Category ',year),
       x='Location Category',
       y='Proportion of Voters')+
  theme_minimal()+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

##### Plot proportion of registered voters at each category of polling location by rural urban divide
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

