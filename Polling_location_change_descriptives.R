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
#plot_data$Year_Pair<-c('2018/2019','2019/2020','2020/2021','2021/2022','2022/2023')
########### UNFINISHED #####################
#two_data<-read.csv('DiD_prepped_poll_vote_18to19.csv')
#state <- map_data("state")
#PA <- subset(state, region=="pennsylvania")
#counties <- map_data("county")
#PA_county <- subset(counties, region=="pennsylvania")

############################################ CHange by proportion of voters
# set directories
data_dir <- "C:/Users/natha/Desktop/Polling Places DiD/data"
results_dir <-"C:/Users/natha/Desktop/Polling Places DiD/model_results"
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
# read in two-time period data
setwd(data_dir)
#two_data<-read.csv('DiD_prepped_poll_vote_18to19.csv')
model_data<-read.csv('DiD_prepped_poll_vote_16to19_no_rndm_race.csv')


## Recode extraneous parties to 'other'
model_data$Parties_Description <- fct_collapse(model_data$Parties_Description, 
                                               Other = c('American', 'American Independent','Anarchist','Bull Moose',
                                                         'Christian','Communist','Conservative','Constitution',
                                                         'Constitutional','Consumer','Federalist','Free Choice',
                                                         'Freedom','Green','Independence','Independent Democrat',
                                                         'Independent Republican','Labor','Liberal',
                                                         'Libertarian','Natural Law','Non-Partisan','Patriot',
                                                         'Peace and Freedom','Populist','Progressive','Prohibition','Rainbow',
                                                         'Reform','Registered Independent','Right to Life',
                                                         'Social Democrat','Socialist','Socialist Labor',
                                                         'Taxpayers','Unknown','Whig'))
model_data$Parties_Description <- relevel(model_data$Parties_Description, ref = "Democratic")

## create vector of location categories
#categories<-c('pub_loc','pub_just','other','relig_loc','school','multiple',
#              'justice_loc','library','relig_school')
## Create vector of location category labels
loc_labels_NAsettoOther<-c('Other','Justice Location','Library','Multiple Categories',
                           'Public Location','Public/Justice Location','Religious Location',
                           'Religious School','School')
loc_labels_OthersettoNA<-c('Multiple Categories','Justice Location','Library',
                           'Public Location','Public/Justice Location','Religious Location',
                           'Religious School','School')
## Create dictionary of location labels
loc_dict<-c('pub_loc'='Public Location','pub_just'='Public and Justice Location',
            'other'='Other','relig_loc'='Religious Location','school'='School',
            'multiple'='Multiple Categories', 'justice_loc'='Justice Location',
            'library'='Library', 'relig_school'='Religious School',
            'catholic_school'='Catholic School','catholic_church'='Catholic Church',
            'cath_loc'='Catholic Location')
## Create dictionary of variable labels
var_dict<-c('Voters_Gender'='Gender', 'Voters_Age'='Age',
            'CommercialData_EstimatedHHIncomeAmount'='Estimated HH Income',
            'Residence_Families_HHCount'='HH Resident Count',
            'known_religious'='Known Religious',
            'CommercialData_LikelyUnion'='Likely Union Member', 
            'CommercialData_OccupationIndustry'='Occupation Industry',
            'CommercialData_OccupationIndustry'='Occupation Group',
            'has_child'='Has Child(ren)','known_gov_emp'='Known Government Employee',
            'Parties_Description'='Political Party','pred_race'='Predicted Race',
            'Shape_Length'='Distance to Polling Station','known_catholic'='Known Catholic')

## Calculate years registered based on dependent variable year
model_data$years_reg<-2018-as.numeric(model_data$year_reg)

## Common set of covariates
common_covars <-c(
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'pred_race',
  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
  'known_religious','CommercialData_LikelyUnion', 
  'CommercialData_OccupationGroup',
  'Shape_Length',
  'years_reg'
)
## Create simplified location categories variable (subsume catholic into religious)
# model_data$location_category_simpl<-model_data$location_category
# model_data$location_category_simpl[model_data$location_category_simpl=='catholic_church']<-'religious'
# model_data$location_category_simpl[model_data$location_category_simpl=='catholic_school']<-'religious_school'

## set how 'other category is treated
#other_cond='default'
#other_cond='OtherSettoNA'
other_cond='NASettoOther'
loc_labels=loc_labels_NAsettoOther
#loc_labels=loc_labels_OthersettoNA
## recode missing or other
model_data$location_category[is.na(model_data$location_category)]<-'other'
#model_data$location_category[model_data$location_category=='other']<-NA

###### Split location category into years
model_data$location_category_2018<-model_data$location_category
model_data$location_category_2018[model_data$year==2019]<-NA
model_data$location_category_2019<-model_data$location_category
model_data$location_category_2019[model_data$year==2018]<-NA

test <- model_data[order(model_data$VOTERID),]
test18<-test%>%
  filter(year==2018)%>%
  select(LALVOTERID, VOTERID, location_category)

test19<-test%>%
  filter(year==2019)%>%
  select(LALVOTERID, VOTERID, location_category)

test_both<-left_join(test18,test19, by = "LALVOTERID", suffix = c("", "_2018"))
sum(test_both$location_category!=test_both$location_category_2018, na.rm = T)

## Factorize variables
#location categories
model_data$location_category<-as.factor(model_data$location_category)
model_data$location_category<-relevel(model_data$location_category, ref='other')
### create indicators for ever being treated
two_data<-model_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  mutate(#ever_changed_poll_cat=(sum(changed_poll_cat)>0),
    ever_changed_poll_loc=(sum(changed_poll_loc)>0),
    #ever_moved_new_poll_cat=(sum(moved_new_poll_cat)>0),
    #ever_no_move_new_poll_cat=(sum(no_move_new_poll_cat)>0),
    #ever_moved_old_poll_cat=(sum(moved_old_poll_cat)>0),
    ever_moved_new_poll_loc=(sum(moved_new_poll_loc)>0),
    ever_no_move_new_poll_loc=(sum(no_move_new_poll_loc)>0),
    ever_moved_old_poll_loc=(sum(moved_old_poll_loc)>0))%>% 
  #create single variable that indicates if someone voted in a given year
  group_by(year)%>%
  mutate(voted = ((year==2018 & General_2018_11_06==1)|(year==2019 & General_2019_11_05==1)),
         ## Calculate years registered by 2018
         years_reg = 2018-as.numeric(year_reg))%>%
  ungroup()%>%
  #remove duplicate voters (not sure where they came from)
  distinct(LALVOTERID, year, .keep_all = T)



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



