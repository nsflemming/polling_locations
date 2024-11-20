#Nathaniel Flemming
#7/11/24

## Descriptive of polling locations using file from Combine_voterfile_polllocation


################# Libraries
library(tidyverse)
library(data.table) #read in data selectively
library(stringr) #string manipulation
library(forcats) # plot bars in order of value

################# Functions



################## Main
### set directories
data_dir <- 'C:\\Users\\natha\\Desktop\\Polling Places DiD\\data'
### set year
year='2020'

# read in data
poll_data<-read.csv(paste0(data_dir,'\\FVE_',year,'_polllocation.csv'))

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


