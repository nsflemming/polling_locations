#Nathaniel Flemming
# 12/6/23

# Descriptive statistics
library(tidyverse)
library(data.table) #read in data selectively

# read in data
setwd("C:/Users/natha/Desktop/Polling Places/data")
L2<-read.csv('L2_join_poll_place.csv')
L2 <- subset(L2, select = -c(CountyEthnic_LALEthnicCode_2018, 
                                       CountyEthnic_Description_2018))
## create mini dataset for troubleshooting
L2_mini <- L2[sample(nrow(L2), 1000),]


######################### geographic distribution and prevalence of missingness
## dataframe of rows with any missing data
L2_missing<-L2[!complete.cases(L2),]

#### functions
#factorize variables and fill blanks
factor_and_fill<-function(df, var, fill){
  df[[var]] <- factor(df[[var]])
  #convert blanks to 'none' or 'not specified' or missing?
  levels(df[[var]])[levels(df[[var]]) == ""] <- fill
  return(df)
}
#count missing obs by country
miss_by_county<-function(df, var, new_var){
  var <- enquo(var)
  temp<-df %>% 
    group_by(county) %>% 
    mutate(new_var = sum(is.na(!!var)))%>%
    ungroup()%>%
    select(county,new_var)%>%
    distinct()
  return(temp)
}


##### any missingness by county
L2_missing$county <- factor(L2_missing$county)
missingness<-count(L2_missing, county)
## normalize by number of registered voters in each county
missingness$reg_voter_pop<-count(L2, county)$n
missingness$prop_miss<-missingness$n/missingness$reg_voter_pop
#rename column
missingness <- missingness%>%
  rename(sum_any_na=n)

### Missingness of religious description in L2
L2_missing<-factor_and_fill(L2_missing, 'Religions_Description_2018', 
                            'Not specified')
# count missing by county
temp <- miss_by_county(L2_missing, `Religions_Description_2018`, sum_relig_na)
temp<-L2_missing %>% 
  group_by(county) %>% 
  mutate(sum_relig_na = sum(is.na(Religions_Description_2018)))%>%
  ungroup()%>%
  select(county,sum_relig_na)%>%
  distinct()
# merge into missingness dataframe and calculate proportion
missingness <- left_join(missingness, temp, by='county')
missingness$prop_miss_relig<-missingness$sum_relig_na/missingness$reg_voter_pop

### Missingness of ethnic group
L2_missing$EthnicGroups_EthnicGroup1Desc_2018 <- 
  factor(L2_missing$EthnicGroups_EthnicGroup1Desc_2018)
#convert blanks to missing
levels(L2_missing$EthnicGroups_EthnicGroup1Desc_2018)[levels(L2_missing$EthnicGroups_EthnicGroup1Desc_2018) == ""] <- 
  NA
# count missing by county
temp<-L2_missing %>% 
  group_by(county) %>% 
  mutate(sum_ethnic_na = sum(is.na(EthnicGroups_EthnicGroup1Desc_2018)))%>%
  ungroup()%>%
  select(county,sum_ethnic_na)%>%
  distinct()
# merge into missingness dataframe and calculate proportion
missingness <- left_join(missingness, temp, by='county')
missingness$prop_miss_ethnic<-missingness$sum_ethnic_na/missingness$reg_voter_pop

### Missingness of education
L2_missing$CommercialData_Education_2018 <- 
  factor(L2_missing$CommercialData_Education_2018)
#convert blanks to missing?
levels(L2_missing$CommercialData_Education_2018)[levels(L2_missing$CommercialData_Education_2018) == ""] <- 
  NA
# count missing by county
temp<-L2_missing %>% 
  group_by(county) %>% 
  mutate(sum_educ_na = sum(is.na(CommercialData_Education_2018)))%>%
  ungroup()%>%
  select(county,sum_educ_na)%>%
  distinct()
# merge into missingness dataframe and calculate proportion
missingness <- left_join(missingness, temp, by='county')
missingness$prop_miss_educ<-missingness$sum_educ_na/missingness$reg_voter_pop

#### save to csv
write.csv(missingness, 'missingness_by_county.csv')

##### Cross tabs of demographic characteristics and polling place categories






