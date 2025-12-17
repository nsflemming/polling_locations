# Nathaniel Flemming 
## Adding catholic church to government polling locations categorizations

####################### libraries
library(dplyr)
library(stringr) #string manipulation
library(ggplot2) #plotting
library(stringdist) #fuzzy matching church names


###################### Functions
### fuzzy match church names to location descriptions within city and zip code 
fuzzy_match_churches<-function(churchesdf, polldf,name_var='Description', 
                               city_var='City',zip_var='PostalCode',
                               dist_threshold=1){
  # get list of cities with churches
  cities<-unique(churchesdf[[city_var]])
  # create matched data frame
  matched<-data.frame(matrix(ncol=6,nrow=0))
  x <- c('City','Church','Description','poll_zip','church_zip', 'Dist')
  colnames(matched) <- x
  # match churches to poll locations with similar names in the same city
  for(city in cities){
    # get dataframes of churches and poll locations plus zip codes w/in city
    churches<-churchesdf[churchesdf[[city_var]]==city,]
    polls<-polldf[polldf[[city_var]]==toupper(city),]
    # convert church names to all caps
    churches[[name_var]]<-toupper(churches[[name_var]])
    # remove periods
    churches[[name_var]]<-str_replace_all(churches[[name_var]], "\\.", "")
    # filter out non-religious poll locations
    polls<-polls[polls[['location_category']]=='religious',]
    ## calculate string distance b/t descriptions and names
    result<-stringdistmatrix(churches[[name_var]], polls[[name_var]], method='jw')
    ## get index of col with lowest value for each row
    closest_match<-apply(result, 1, which.min)
    # create data frame with church, poll location, and respective zip codes
    temp<-as.data.frame(cbind('City'=city,'Church'=churches[[name_var]],
                              'Description'=polls[[name_var]][closest_match],
                              'PostalCode'=polls[[zip_var]][closest_match],
                              'church_zip'=churches[[zip_var]], 
                              'Dist'=apply(result, 1, min, na.rm=T)))
    # filter out churches and poll locations with mismatched zip codes
    temp<-temp[temp[['PostalCode']]==temp[['church_zip']],]
    # filter out matches with distance above threshold 
    temp<-temp[temp[['Dist']]<dist_threshold,]
    print(city)
    # bind into larger matched data frame
    matched<-rbind(matched, temp)
  }
  return(matched)
}

####################### Main
#set directories
data_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data'
poll_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data/gov_poll_places'
plot_dir <- "C:/Users/natha/Desktop/Polling Places DiD/plots"
struct_dir <- 'C:/Users/natha/Desktop/Polling Places DiD/data/Structures'
## Catholic Churches (incompete addresses)
catholic<-read.csv(paste0(struct_dir,'\\Cath_Dir_PA_Churches_1_20_2025.csv'))
#### shorten longer zip codes
catholic$PostalCode<-str_sub(catholic$ZIP, end=5)

years<-c('17','18','19')
for(year in years){
  assign('poll_struct_key_govsource', read.csv(paste0(data_dir,'/poll_struct_key_govsource_underlying',year,'.csv')))
  #(distance at which correct matches become the norm: 0.251)
  conservative_matched_locs<-fuzzy_match_churches(catholic, poll_struct_key_govsource,
                                                  name_var='Description', 
                                                  city_var='City',zip_var='PostalCode',
                                                  dist_threshold=0.251)
  ### add catholic church location category
  temp<-select(conservative_matched_locs,c('Description','PostalCode'))
  temp$catholic_church<-1
  #convert postal code to character
  poll_struct_key_govsource$PostalCode<-as.character(poll_struct_key_govsource$PostalCode)
  # join catholic church locations to polling locations
  ## Many to many because there can be multiple churches per post code
  poll_struct_key_govsource<-left_join(poll_struct_key_govsource,temp, 
                                         by=c('Description','PostalCode'),
                                         relationship = "many-to-many")
  poll_struct_key_govsource$catholic_church[is.na(poll_struct_key_govsource$catholic_church)]<-0
  #add catholic church as a location category
  #poll_struct_key_govsource$location_category[poll_struct_key_govsource$catholic_church==1]<-'catholic_church'
  #save
  write.csv(poll_struct_key_govsource, paste0(data_dir,'/poll_struct_key_cath_govsource_underlying',year,'.csv'))
}





