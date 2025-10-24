#Nathaniel Flemming
# 15/9/25

# Difference in difference regressions, using data created by DiD_preprocessing script
# Propensity score matching done manually

#library(did) #difference in difference package, 3+ periods
#library(DRDID) #difference in difference package, 2 periods
library(tidyverse) #convenience
library(data.table) #read in data selectively
library(stringr) #id string manipulation
#library(psa) #all in on propensity score analysis package (no version for my version of R?)
library(MatchIt) #matching for DiD
#library(PSAgraphics)
library(marginaleffects) #estimates effects with robust standard errors

########## Functions
###

### plot did result
plot_did<-function(data, did_model, dvar, plot_title, group1='Control', group2='Parallel Trend',
                   group3='Treated'){
  ## set up data frame
  plot_data <- data.frame(matrix(ncol = 5, nrow = 6))
  x <- c("group","year", "turnout","linetype",'se')
  colnames(plot_data) <- x
  plot_data$group<-rep(c('control','simulated','treated'),2)
  plot_data$year<-rep(c(2018,2019),3)
  plot_data$linetype<-rep(c('a','b','a'),2)
  plot_data$se<-NA
  ## add in turnout values
  ###control
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='control')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==F)])
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='control')]<-
    mean(data$voted[(data$year==2019) & (data[dvar]==F)])
  ###treated
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='treated')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==T)])
  ###simulated
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='simulated')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==T)])
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='simulated')]<-
    ##2019  control value
    (plot_data$turnout[(plot_data$year==2019)&(plot_data$group=='control')]-
       ##diff between 2018 treatment and control
       (plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='control')]-
          plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='treated')]))
  ###treated
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='treated')]<-
    plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='simulated')]+did_model$ATT
  ###standard error
  plot_data$se[(plot_data$year==2019) & (plot_data$group=='treated')]<-did_model$se
  ## plot
  ggplot(data=plot_data)+
    geom_point(aes(x=year, y=turnout, color=group),size=2.5)+
    geom_line(aes(x=year, y=turnout, color=group, linetype = linetype),linewidth=1.5)+
    geom_errorbar(aes(x=year, ymin=turnout-se, ymax=turnout+se, color=group), 
                  width=0.075)+
    scale_x_continuous(breaks=seq(2018, 2019, 1))+
    #convert turnout to percentage
    scale_y_continuous(labels = scales::percent)+
    theme_minimal()+
    theme(plot.title = element_text(size=22),
          axis.title = element_text(size=20),
          axis.text = element_text(size=15),
          legend.title = element_text(size=18),
          legend.text =element_text(size=15))+
    #change text
    labs(title=plot_title,
         colour = "Group",
         x='Year', y='Probability of Voting')+
    scale_color_hue(labels = c(group1, group2, group3))+
    #remove linetype legend
    guides(linetype='none')
}

### blank plot of did result
blank_plot_did<-function(data, did_model, dvar, plot_title, group1='Control', group2='Parallel Trend',
                         group3='Treated'){
  ## set up data frame
  plot_data <- data.frame(matrix(ncol = 5, nrow = 6))
  x <- c("group","year", "turnout","linetype",'se')
  colnames(plot_data) <- x
  plot_data$group<-rep(c('control','simulated','treated'),2)
  plot_data$year<-rep(c(2018,2019),3)
  plot_data$linetype<-rep(c('a','b','a'),2)
  plot_data$se<-NA
  ## add in turnout values
  ###control
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='control')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==F)])
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='control')]<-
    mean(data$voted[(data$year==2019) & (data[dvar]==F)])
  ###treated
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='treated')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==T)])
  ###simulated
  #### 2018
  plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='simulated')]<-
    mean(data$voted[(data$year==2018) & (data[dvar]==T)])
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='simulated')]<-
    ##2019  control value
    (plot_data$turnout[(plot_data$year==2019)&(plot_data$group=='control')]-
       ##diff between 2018 treatment and control
       plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='control')]-
       plot_data$turnout[(plot_data$year==2018) & (plot_data$group=='treated')])
  ###treated
  #### 2019
  plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='treated')]<-
    plot_data$turnout[(plot_data$year==2019) & (plot_data$group=='simulated')]+did_model$ATT
  ###standard error
  plot_data$se[(plot_data$year==2019) & (plot_data$group=='treated')]<-did_model$se
  ## plot
  ggplot(data=plot_data, aes(x=year, y=turnout, color=group))+
    scale_x_continuous(breaks=seq(2018, 2019, 1))+
    #convert turnout to percentage
    scale_y_continuous(labels = scales::percent)+
    theme_minimal()+
    theme(plot.title = element_text(size=22),
          axis.title = element_text(size=20),
          axis.text = element_text(size=15),
          legend.title = element_text(size=18),
          legend.text =element_text(size=15))+
    #change text
    labs(title=plot_title,
         colour = "Group",
         x='Year', y='Probability of Voting')+
    scale_color_hue(labels = c(group1, group2, group3))+
    #remove linetype legend
    guides(linetype='none')
}


########################################################### Main
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
model_data$years_reg<-2017-as.numeric(model_data$year_reg)

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

## Factorize variables
#location categories
model_data$location_category<-as.factor(model_data$location_category)
model_data$location_category<-relevel(model_data$location_category, ref='other')
#model_data$location_category<-relevel(model_data$location_category, ref='multiple')

#race
model_data$pred_race <- as.factor(model_data$pred_race)
model_data$pred_race <- relevel(model_data$pred_race, ref = "pred.whi")
#political party
model_data$Parties_Description <- as.factor(model_data$Parties_Description)
model_data$Parties_Description <- relevel(model_data$Parties_Description, ref = "Democratic")
#religious
model_data$known_religious<-as.factor(model_data$known_religious)
#catholic
model_data$known_catholic<-as.factor(model_data$known_catholic)
#child present
model_data$has_child<-as.factor(model_data$has_child)
#union member
model_data$CommercialData_LikelyUnion<-as.factor(model_data$CommercialData_LikelyUnion)
#occupational group (Not a variable in 2017 data, so have to remove if using that year)
model_data$CommercialData_OccupationGroup<-as.factor(model_data$CommercialData_OccupationGroup)
model_data$CommercialData_OccupationGroup<-relevel(model_data$CommercialData_OccupationGroup, ref='Blue Collar')

### Create treatment indicators
two_data<-model_data%>%
  # group by voter
  group_by(LALVOTERID)%>%
  mutate(ever_changed_poll_loc=(sum(changed_poll_loc)>0),
         ever_moved_new_poll_loc=(sum(moved_new_poll_loc)>0),
         ever_no_move_new_poll_loc=(sum(no_move_new_poll_loc)>0),
         ever_moved_old_poll_loc=(sum(moved_old_poll_loc)>0))%>% 
  #create single variable that indicates if someone voted in a given year
  group_by(year)%>%
 #mutate(voted = ((year==2018 & General_2018_11_06==1)|(year==2019 & General_2019_11_05==1)),
  mutate(voted = ((year==2017 & General_2017_11_07==1)|(year==2019 & General_2019_11_05==1)),
         ## Calculate years registered by 2017
         years_reg = 2017-as.numeric(year_reg))%>%
  ungroup()%>%
  #remove duplicate voters (not sure where they came from)
  distinct(LALVOTERID, year, .keep_all = T)
#set missing voted to 0?
#two_data$voted[is.na(two_data$voted)]<-F

###### Split location category into years
two_data$location_category_2017<-two_data$location_category
two_data$location_category_2017[two_data$year==2019]<-NA
two_data$location_category_2019<-two_data$location_category
two_data$location_category_2019[two_data$year==2017]<-NA


######## Generate propensity scores
### new poll location without moving
dvar<-'ever_no_move_new_poll_loc'
### people who changed polling location without moving and people who didn't change location
no_chng_or_no_mv_two_data<-two_data%>%
  filter(((ever_no_move_new_poll_loc==T)|(ever_changed_poll_loc==F))&(ever_moved_new_poll_loc==F))

# remove missing to prevent different length Y1 and Y0
## Test with subsample
no_chng_or_no_mv_two_data<-no_chng_or_no_mv_two_data%>%
  select(all_of(c('voted',dvar,'LALVOTERID','VOTERID','year'
                  ,'Voters_Gender'
                  ,'Voters_Age','Parties_Description','pred_race',
                  'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
                  'known_religious','CommercialData_LikelyUnion',
                  'years_reg'
  )))%>%
  filter(complete.cases(.))%>%
  #filter out voters who aren't in both 2017 and 2019?
  group_by(LALVOTERID)%>%
  filter(length(year)==2)%>%
  ungroup()


# # Create a mini dataframe for testing by sampling from treated and untreated groups
mini<-no_chng_or_no_mv_two_data%>%
  group_by('ever_no_move_new_poll_loc')%>%
  slice_sample(n=500000)%>%
  ungroup()%>%
  mutate(treat=as.integer(ever_no_move_new_poll_loc),
         Voters_Gender=case_when(
           Voters_Gender=='M' ~ 0,
           Voters_Gender=='F'~1,
           .default = NA
         ),
         Voters_Age=as.integer(Voters_Age),
         Parties_Description=as.integer(Parties_Description),
         pred_race=as.integer(pred_race),
         known_religious=as.integer(known_religious),
         CommercialData_LikelyUnion=as.integer(CommercialData_LikelyUnion)
         #CommercialData_OccupationGroup=as.integer(CommercialData_OccupationGroup)
  )

# full<-no_chng_or_no_mv_two_data%>%
#   mutate(treat=as.integer(ever_no_move_new_poll_loc),
#          Voters_Gender=case_when(
#            Voters_Gender=='M' ~ 0,
#            Voters_Gender=='F'~1,
#            .default = NA
#          ),
#          Voters_Age=as.integer(Voters_Age),
#          Parties_Description=as.integer(Parties_Description),
#          pred_race=as.integer(pred_race),
#          known_religious=as.integer(known_religious),
#          CommercialData_LikelyUnion=as.integer(CommercialData_LikelyUnion)
#          #CommercialData_OccupationGroup=as.integer(CommercialData_OccupationGroup)
#   )


# fix covariates at 2017 values
## Filter data for year 2017
mini_2017 <- mini %>%
  filter(year == 2017) %>%
  select(all_of(c('LALVOTERID', 'Voters_Gender', 'Voters_Age', 'Parties_Description', 
                  'pred_race','CommercialData_EstimatedHHIncomeAmount', 
                  'Residence_Families_HHCount','known_religious', 
                  'CommercialData_LikelyUnion', 
                  #'CommercialData_OccupationGroup',
                  'years_reg')))
## Join back to the original dataset
mini <- mini %>%
  left_join(mini_2017, by = "LALVOTERID", suffix = c("", "_2017"))%>%
  mutate(
    Voters_Gender = Voters_Gender_2017,
    Voters_Age = Voters_Age_2017,
    Parties_Description = Parties_Description_2017,
    pred_race = pred_race_2017,
    CommercialData_EstimatedHHIncomeAmount = CommercialData_EstimatedHHIncomeAmount_2017,
    Residence_Families_HHCount = Residence_Families_HHCount_2017,
    known_religious = known_religious_2017,
    CommercialData_LikelyUnion = CommercialData_LikelyUnion_2017,
    #CommercialData_OccupationGroup = CommercialData_OccupationGroup_2017,
    years_reg = years_reg_2017
  )%>%
  select(-ends_with("_2017"))%>%  # Remove extra columns
  # Remove voters who have missing data
  filter(complete.cases(.))

# Logistic Regression based scores
## Change in polling location without moving
ps_formula <- treat ~ Voters_Gender + Voters_Age + Parties_Description+
  pred_race+CommercialData_EstimatedHHIncomeAmount+Residence_Families_HHCount+
  known_religious+CommercialData_LikelyUnion+
  years_reg
lr_out <- glm(formula = ps_formula,
              data = mini,
              family = binomial(link = 'logit'))

## Add propensity scores to data frame
mini$lr_ps <- fitted(lr_out)

## Examine distribution of propensity scores
ggplot(full, aes(x = lr_ps, color = as.logical(ever_no_move_new_poll_loc))) + 
  geom_density() +
  guides(col=guide_legend(title='Treated'))+
  xlab('Propensity Score')

# Stratify into quintiles and convert all variables to numeric
full <- full %>%
  mutate(ps_quintile = ntile(lr_ps, 5))

# Get the actual quintile cutpoints
quintile_cutpoints <- quantile(mini$lr_ps, probs = seq(0, 1, 0.2))

## Check stratification
table(mini$treat, mini$ps_quintile)
## Visalize quantiles
ggplot(mini, aes(x = lr_ps, color = as.logical(treat))) + 
  geom_density() +
  guides(col=guide_legend(title='Treated'))+
  xlab('Propensity Score')+
  geom_vline(xintercept = quintile_cutpoints)

# Check balance
## Simple comparison of means within strata
PSAgraphics::box.psa(continuous = mini$years_reg, 
                     treatment = mini$treat, 
                     strata = mini$ps_quintile,
                     xlab = "Strata", 
                     balance = FALSE)
## Simple comparison of proportions within strata
PSAgraphics::cat.psa(categorical = mini$CommercialData_OccupationGroup, 
                     treatment = mini$ever_no_move_new_poll_loc, 
                     strata = mini$ps_quintile, 
                     xlab = 'Strata',
                     balance = FALSE)

## Compare covariate effect size to check balance
covars <- c('treat','Voters_Gender','Voters_Age', 
            'Parties_Description','pred_race',
            'CommercialData_EstimatedHHIncomeAmount','Residence_Families_HHCount',
              'known_religious','CommercialData_LikelyUnion',
            #'CommercialData_OccupationGroup',
              'years_reg')
covars <- data.frame(full[,covars[-1]])
#All variables must be numeric for the function to work
PSAgraphics::cv.bal.psa(covariates = covars, 
                        treatment = full$treat,
                        propensity = full$lr_ps,
                        strata = full$ps_quintile)


########## DiD
# Matching from bryer book

# Matching using MatchIt package
## Mahalanobis Metric Matching
### matches observation to nearest, not exact match
m.mahal<-matchit(ps_formula,data=mini, replace=FALSE,distance="mahalanobis")
summary(m.mahal)
# Check balance
library(cobalt)
love.plot(m.mahal, drop.distance = TRUE)
## Convert match object into a dataset
mahal.match<-match.data(m.mahal)
## t-test comparing outcome
t.test(voted ~ treat, data = mahal.match)

#Linear model with covariates (covariates not necessarily needed if balance is good enough)
fit1 <- glm(voted ~ treat * (Voters_Gender+Voters_Age+Parties_Description+pred_race+
                               CommercialData_EstimatedHHIncomeAmount+Residence_Families_HHCount+
                               known_religious+CommercialData_LikelyUnion+years_reg),
              data = mahal.match,
              family = binomial(link = 'logit'),
            weights = weights)
#estimate ATT
avg_comparisons(fit1,
                variables = "treat",
                # I think this variable determines the clustering of the errors?:
                ## "this formula is passed to the cluster argument of the sandwich::vcovCL function"
                ## Clustering on matches created by prop score matching
                vcov = ~subclass,
                newdata = subset(treat == 1),
                # Calculate relative risk
                comparison = "lnratioavg",
                transform = "exp")
## PSM nearest neighbor
##m.nn<-matchit(lalonde.formu, data = lalonde, caliper=0.1, method ="nearest")
m.nn<-matchit(ps_formula, data = mini, ratio = 1, method ="nearest")
nn.match<-match.data(m.nn)












