#Nathaniel Flemming
# 24/12/24

# Changes in polling location and category, using data created by DiD_preprocessing script

library(tidyverse) #convenience
library(data.table) #read in data selectively
#library(usmap) #plotting on map of PA

########## Functions
## create address from address components
make_address <- function(data, num, direction, street, street_type, city,
                         state, postcode){
  #clean address components
  ## remove trailing spaces
  cols<-c(num, direction, street, street_type, city,state, postcode)
  data <- data %>%
    mutate(across(all_of(cols), trimws))
  # paste together address components
  data[['address']]<-paste0(data[[num]], ' ',data[[direction]], ' ',data[[street]], 
                            ' ',data[[street_type]], ', ',data[[city]], ', ', 
                            data[[state]],' ', data[[postcode]])
  #replace any douple spaces created by a missing direction with a single space
  data[['address']]<-str_replace_all(data[['address']], '  ', ' ')
  return(data)
}

## read in location data
get_poll <- function(dir, filename, num, direction, street, street_type, city,
                     state, postcode){
  setwd(dir)
  data <- read.csv(filename)
  data<-make_address(data, num, direction, street, street_type, city, state, postcode)
  return(data)
}

## Not in
'%!in%' <- function(x,y)!('%in%'(x,y))

########################################################### Main
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
poll_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data/gov_poll_places'

############################################ CHange by proportion of locations
## string for abbreviating street names
rep_str <- c(' STREET'=' ST', ' ROAD'=' RD', ' AVENUE'=' AVE', ' DRIVE'=' DR', 
             ' BOULEVARD'=' BLVD',
             ' NORTH'=' N', ' SOUTH'=' S', ' EAST'=' E', ' WEST'=' W')

# get poll location data and process
filenames<-c('Polling Place List 20171106.csv',
             #'Polling Place List 20180514.csv',
             'Polling Place List 20190513.csv'
             #,'Polling Place List 20201102.csv','Polling Place List 20211101.csv',
             #'Polling Place List 20220506.csv','Polling Place List 20231106.csv'
)
## Read in and process multiple location files
for(file in filenames){
  assign(paste0('poll_loc',substr(file,20,23)),
         get_poll(poll_dir, file, num='HouseNum', direction='PrefixDirection',
                  street='Street', street_type = 'StreetType',city='City',
                  state="State", postcode='PostalCode'))
}

### create plotting dataframe
plot_data<-data.frame(matrix(ncol=2, nrow=1))
colnames(plot_data)<-c('Year_Pair', 'Change')
#plot_data$Year_Pair<-c('2018/2019','2019/2020','2020/2021','2021/2022','2022/2023')
plot_data$Year_Pair<-'2017/2019'
########### UNFINISHED #####################
#two_data<-read.csv('DiD_prepped_poll_vote_18to19.csv')
#state <- map_data("state")
#PA <- subset(state, region=="pennsylvania")
#counties <- map_data("county")
#PA_county <- subset(counties, region=="pennsylvania")

# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"


######################## Changes by county
# Add year to poll location data frames
poll_loc2017$Year<-2017
poll_loc2019$Year<-2019

# Compare years of poll location data
## Combine years
poll_loc_both<-rbind(poll_loc2017,poll_loc2019)

# Find differences in poll location address by county, precinct, description
## Find the poll locations that match on all three across years
## keep only locations that don't match
poll_loc_2017_unmatched<-poll_loc_both%>%
  # Remove locations with address that matches a 2019 address
  group_by(CountyName)%>%
  filter(address%!in%unique(address[Year==2019]))%>%
  select(all_of(c('Year','CountyName','PrecinctCode','PrecinctName','Description',
                  'address')))%>%
  ungroup()
## Match locations missing a matching address to a matching description in 2019
poll_loc2019_trim<-poll_loc2019%>%
  select(all_of(c('Year','CountyName','PrecinctCode','PrecinctName','Description',
                  'address')))
poll_loc_2017_unmatched<-left_join(poll_loc_2017_unmatched,poll_loc2019_trim,
                                   by=c('CountyName','Description'))

## Separate out likely matches and unmatched locations
poll_loc_2017_poss_matched<-poll_loc_2017_unmatched%>%
  filter(complete.cases(.))
poll_loc_2017_unmatched<-poll_loc_2017_unmatched%>%
  filter(!complete.cases(.))

## Save for manual cleaning
write.csv(poll_loc_2017_unmatched,paste0(data_dir,'/Polling Place List 2017 Unmatched.csv'))
write.csv(poll_loc_2017_poss_matched,paste0(data_dir,'/Polling Place List 2017 Possible Matched.csv'))
write.csv(poll_loc2019_trim,paste0(data_dir,'/Polling Place List 2019 Reference.csv'))



  # lots of incomplete 2017 addresses that should match 2019
  summarize(NumDiffs = sum(unique(address[Year==2017])%!in%unique(address[Year==2019])),
            PercChng =sum(unique(address[Year==2017])%!in%unique(address[Year==2019]))/
              length(unique(address[Year==2017])))

## Number of poll locations by county
poll_loc_num2017<-poll_loc2017%>%
  group_by(CountyName)%>%
  summarise(num_locs=n(),
            num_unique_addresses=length(unique(address)))

# change in location as a percentage by county
county_change<-model_data%>%
  group_by(County)%>%
  summarize(per_chng_cat=(sum(ever_changed_poll_cat)/n()),
            per_chng_loc=(sum(ever_changed_poll_loc)/n()),
            num_chng_cat=(sum(ever_changed_poll_cat)/2),
            num_chng_loc=(sum(ever_changed_poll_loc)/2),
            num_voters=n()/2)
#save
#write.csv(county_change, paste0(data_dir,'\\cat_loc_chng_18_19.csv'))
county_change<-read.csv(paste0(data_dir,'\\cat_loc_chng_18_19.csv'))

## plot as bar plot
### location change
ggplot(data=county_change)+
  geom_col(aes(x=fct_reorder(County,per_chng_loc), y=per_chng_loc),fill='blue4')+
  labs(title='Percentage of Voters Whose Polling Location Changed by County',
       x='County',
       y='Percentage of Voters')+
  theme_minimal()+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle=90, hjust=1),
        axis.text.y = element_text(size = 15))+ 
  scale_y_continuous(labels = scales::percent)


### category change
ggplot(data=county_change)+
  geom_col(aes(x=fct_reorder(County,per_chng_cat), y=per_chng_cat),fill='blue4')+
  labs(title='Percentage of Voters Whose Polling Location Category Changed by County',
       x='County',
       y='Percentage of Voters')+
  theme_minimal()+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
        axis.text.y = element_text(size = 15))+ 
  scale_y_continuous(labels = scales::percent)

### number of registered voters in the data set
ggplot(data=county_change)+
  geom_col(aes(x=fct_reorder(County,num_voters), y=num_voters),fill='blue4')+
  labs(title='Number of Voters by County',
       x='County',
       y='Number of Voters')+
  theme_minimal()+
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
        axis.text.y = element_text(size = 15))

ca_map <- ggplot(data=washington, mapping=aes(x=long, y=lat, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color="black", fill="gray") + 
  geom_polygon(data=washington_county, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  ggtitle('Washington Map with Counties') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
ca_map



