#Nathaniel Flemming
# 24/12/24

# Changes in polling location and category, using data created by DiD_preprocessing script

library(tidyverse) #convenience
library(data.table) #read in data selectively
library(usmap) #plotting on map of PA

########## Functions
## create address from address components
make_address <- function(data, num, direction, street, street_type, city,
                         state, postcode){
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
filenames<-c('Polling Place List 20180514.csv','Polling Place List 20190513.csv',
             'Polling Place List 20201102.csv','Polling Place List 20211101.csv',
             'Polling Place List 20220506.csv','Polling Place List 20231106.csv'
)
## Read in and process multiple location files
for(file in filenames){
  assign(paste0('poll_loc',substr(file,20,23)),
         get_poll(poll_dir, file, num='HouseNum', direction='PrefixDirection',
                  street='Street', street_type = 'StreetType',city='City',
                  state="State", postcode='PostalCode'))
}

### create plotting dataframe
plot_data<-data.frame(matrix(ncol=2, nrow=0))
colnames(plot_data)<-c('Year_Pair', 'Change')
plot_data$Year_Pair<-c('2018/2019','2019/2020','2020/2021','2021/2022','2022/2023')
########### UNFINISHED #####################
#two_data<-read.csv('DiD_prepped_poll_vote_18to19.csv')
#state <- map_data("state")
#PA <- subset(state, region=="pennsylvania")
#counties <- map_data("county")
#PA_county <- subset(counties, region=="pennsylvania")

############################################ CHange by proportion of voters
### create indicators for ever being treated
#two_data<-two_data%>%
two_data<-two_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  mutate(#ever_changed_precinct=sum(changed_prec), #300,838 cases (2%)
    #ever_no_move_new_precinct=sum(no_move_new_precinct), #367,478 cases (3%)
    #ever_moved_new_precinct=sum(moved_new_precinct),#66,640 cases (0.5%)
    ever_changed_poll_cat=(sum(changed_poll_cat)>0),
    ever_changed_poll_loc=(sum(changed_poll_loc)>0),
    ever_moved_new_poll_cat=(sum(moved_new_poll_cat)>0),
    ever_no_move_new_poll_cat=(sum(no_move_new_poll_cat)>0),
    ever_moved_old_poll_cat=(sum(moved_old_poll_cat)>0),
    ever_moved_new_poll_loc=(sum(moved_new_poll_loc)>0),
    ever_no_move_new_poll_loc=(sum(no_move_new_poll_loc)>0),
    ever_moved_old_poll_loc=(sum(moved_old_poll_loc)>0))%>% 
  #create single variable that indicates if someone voted in a given year
  group_by(year)%>%
  mutate(voted = ((year==2018 & General_2018_11_06==1)|(year==2019 & General_2019_11_05==1)))%>%
  ungroup()
### save to csv?
#write.csv(two_data,'')


##### Changes in location and category numbers
num_chng_poll_cat = sum(two_data$ever_changed_poll_cat)/2
num_chng_poll_loc = sum(two_data$ever_changed_poll_loc)/2
num_moved_poll_cat = sum(two_data$ever_moved_new_poll_cat)/2
num_moved_poll_loc = sum(two_data$ever_moved_new_poll_loc)/2
num_no_move_poll_cat = sum(two_data$ever_no_move_new_poll_cat)/2
num_no_move_poll_loc = sum(two_data$ever_no_move_new_poll_loc)/2

##### Changes by county

# change in location as a percentage by county
county_change<-two_data%>%
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



