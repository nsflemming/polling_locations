#Nathaniel Flemming
# 

# Difference in difference regression scraps

# #2 period data set (2017 and 18)
# two_data<-model_data%>%
#   filter(complete.cases(.))%>%
#   filter(year!=2016)%>%
#   group_by(LALVOTERID)%>%
#   filter(n()>1)%>%
#   ungroup()
# two_data<-model_data%>%
#   filter(complete.cases(.))%>% #remove rows with missing data
#   group_by(LALVOTERID)%>% 
#   filter(n()>1)%>% # remove voters with only one year
#   ungroup()


#3 period data set
# three_data<-model_data%>%
#   filter(complete.cases(.))%>%
#   group_by(LALVOTERID)%>%
#   filter(n()>2)%>%
#   ungroup()

### Treatment indicators
# group into voter-years
# create first year treated variable
# group_by(year)%>%
# # create year changed precinct variable
#mutate(yr_changed_prec=ifelse(changed_prec==1, year, 0),
#       yr_no_move_new_precinct=ifelse(no_move_new_precinct==1, year, 0),
#       yr_moved_new_precinct=ifelse(moved_new_precinct==1, year, 0),
#       yr_changed_poll_cat=ifelse(changed_poll_cat==1, year, 0))%>%
# ungroup back to voter
#ungroup()
# group_by(LALVOTERID)%>%
# # create first year treated by lowest year for which treatment indicator is 1, else 0
# mutate(FT_changed_prec = ifelse(1%in%changed_prec, min(year[changed_prec==1]),0),
#        FT_no_move_new_precinct = ifelse(1%in%no_move_new_precinct, min(year[no_move_new_precinct==1]),0),
#        FT_moved_new_precinct = ifelse(1%in%moved_new_precinct, min(year[moved_new_precinct==1]),0))

# three_data<-three_data%>%
#   # group by voter
#   group_by(LALVOTERID)%>%
#   mutate(ever_changed_precinct=sum(changed_prec),
#          ever_no_move_new_precinct=sum(no_move_new_precinct),
#          ever_moved_new_precinct=sum(moved_new_precinct))%>%
#   # group into voter-years
#   # set first treated year to year if treated is true
#   group_by(year)%>%
#   mutate(first_treated=ifelse(no_move_new_precinct==1, year, 0))%>%
#   ungroup()%>%
#   # set first treated year to minimum year that's not 0 for all voter's rows
#   ## if all zero, keep 0
#   group_by(LALVOTERID)%>%
#   mutate(first_treated=ifelse(length(first_treated[first_treated>0])>0,
#            min(first_treated[first_treated>0]), 0))%>%
#   # Add outcome variable of voting in each year
#   # group by voter-year
#   group_by(LALVOTERID, year)%>%
#   mutate(voted=case_when(
#     ((General_2016_11_08==1) & (year==2016)) ~ 1,
#     ((General_2017_11_07==1) & (year==2017)) ~ 1,
#     ((General_2018_11_06==1) & (year==2018)) ~ 1,
#     ((General_2019_11_05==1) & (year==2019)) ~ 1,
#     .default = 0 # Default value if none of previous conditional holds
#   ))%>%
#   ungroup()

################ DiD
## compare change in outcome over time b/t treatment & control groups
## Unit fixed effects will account for time-invariant differences (within unit variation).
## Time fixed effects will account for time-variant differences that are
##    constant across units (within time variation).
## Two-way fixed effect (unit and time FE) = DiD.

## compare change in probability of voting for people who changed precinct and people who didn't

###### Fixed effects regression framework, 2 periods, glm
### NOTE: some covariates switch year to year probably shouldn't (like gender), likely error
## Manual calculation
# c_pre<-mean(two_data$General_2018_11_06[(two_data$ever_changed_precinct==0 & two_data$time_period==0)])
# c_post<-mean(two_data$General_2019_11_05[(two_data$ever_changed_precinct==0 & two_data$time_period==1)])
# t_pre<-mean(two_data$General_2018_11_06[two_data$ever_changed_precinct==1 & two_data$time_period==0])
# t_post<-mean(two_data$General_2019_11_05[two_data$ever_changed_precinct==1 & two_data$time_period==1])
# 
# difference_treated = t_post - t_pre #diff in trtment grp
# difference_control = c_post - c_pre #diff in control grp
# difference_in_differences = difference_treated  - difference_control
# difference_in_differences #0.002115474

## TWFE glm
# mini_model<-glm(voted~ever_changed_precinct*year,
#            data = two_data, family = 'binomial')
# summary(mini_model)
# # save model summary
# setwd(results_dir)
# sink("move_changed_prec_2WFE_mini_model_summary.txt")
# print(summary(mini_model))
# sink()

## TWFE glm, control variables added incorrectly
# model<-glm(voted~ever_changed_precinct*time_period #interaction of being and treatment group and pre/post
#            +Voters_Gender
#            +Residence_Families_HHCount
#            +known_religious,
#            data = two_data, family = 'binomial')
# summary(model)
# # save model summary
# setwd(results_dir)
# sink("changed_prec_2WFE_model_summary.txt")
# print(summary(model))
# sink()



test<-ecp_wellknown_two_data%>%
  group_by(LALVOTERID)%>% 
  filter(n()>1)%>% # remove voters with only one year
  ungroup()
# Estimating Group-Time Average Treatment Effects,3+ periods
## issue with outcome variable
# set.seed(1234) # set seed important for bootstrap standard errors
# out0 <- att_gt(yname = "voted",
#                gname = "first_treated",
#                idname = "VOTERID",
#                tname = "year",
#                xformla = ~Voters_Gender + Voters_Age,
#                data = test_data)
# 
# out0 #
# ggdid(out0)
# 
# ############# example code
# # Estimating Group-Time Average Treatment Effects
# ### no pretreatment periods for some reason
# set.seed(1234) # set seed important for bootstrap standard errors
# ex0 <- att_gt(yname = "nationalism_s",
#                gname = "first_treat",
#                idname = "respondent",
#                tname = "year",
#                xformla = ~age + education,
#                data = d)
# ex0 #2018 -2018 = group 3 first exposure
# ggdid(ex0)




############################### 
########################### subsets for specific knowledge tests 
### people who changed polling station and their new station is a religious building
ecp_relig_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only people who changed polling location to a/another religious building
  filter(location_category_2019%in%c('religious',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is religious
    relig_new_poll_relig = sum(((known_religious==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station w/o moving and their new station is a religious building
no_mv_relig_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_no_move_new_poll_loc==T)%>%
  # only people who changed polling location to a/another religious building
  filter(location_category_2019%in%c('religious',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is religious
    relig_new_poll_relig = sum(((known_religious==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station by moving and their new station is a religious building
mv_relig_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_moved_new_poll_loc==T)%>%
  # only people who changed polling location to a/another religious building
  filter(location_category_2019%in%c('religious',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is religious
    relig_new_poll_relig = sum(((known_religious==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### religious people who changed polling station
voters_relig_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only religious people
  filter(known_religious==T)%>%
  group_by(LALVOTERID)%>%
  mutate(
    # Whether new polling location is religious
    relig_new_poll_relig = sum(((location_category=='religious')&(year==2019)),
                               na.rm=T)>0
  )%>%
  ungroup()


### people who changed polling station and their new station is a school
ecp_school_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only people who changed polling location to a/another school
  filter(location_category_2019%in%c('school',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter household has children
    parent_new_poll_school = sum(((has_child==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station w/o moving and their new station is a school
no_mv_school_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_no_move_new_poll_loc==T)%>%
  # only people who changed polling location to a/another school
  filter(location_category_2019%in%c('school',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter household has children
    parent_new_poll_school = sum(((has_child==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station by moving and their new station is a school
mv_school_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_moved_new_poll_loc==T)%>%
  # only people who changed polling location to a/another school
  filter(location_category_2019%in%c('school',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter household has children
    parent_new_poll_school = sum(((has_child==T)&(year==2019)),na.rm=T)>0
  )%>%
  ungroup()



### people who changed polling station and their new station is a public building
ecp_public_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only people who changed polling location to a/another public building
  filter(((location_category_2019%in%c('public',NA))|(location_category_2019%in%c('public_justice',NA))))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is a civil servant
    govemp_new_poll_public = sum(((CommercialData_OccupationIndustry=='Civil Servant')
                                  &(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station w/o moving and their new station is a public building
no_mv_public_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_no_move_new_poll_loc==T)%>%
  # only people who changed polling location to a/another school
  filter(location_category_2019%in%c('public','public_justice',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is a civil servant
    govemp_new_poll_public = sum(((CommercialData_OccupationIndustry=='Civil Servant')
                                  &(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### people who changed polling station by moving and their new station is a school
mv_public_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_moved_new_poll_loc==T)%>%
  # only people who changed polling location to a/another school
  filter(location_category_2019%in%c('public','public_justice',NA))%>%
  group_by(LALVOTERID)%>%
  mutate(
    # voter is a civil servant
    govemp_new_poll_public = sum(((CommercialData_OccupationIndustry=='Civil Servant')
                                  &(year==2019)),na.rm=T)>0
  )%>%
  ungroup()

### civil servants who changed polling station
voters_govemp_two_data<-two_data%>%
  # only people who have changed polling location
  filter(ever_changed_poll_loc==T)%>%
  # only parents
  filter(CommercialData_OccupationIndustry=='Civil Servant')%>%
  group_by(LALVOTERID)%>%
  mutate(
    # Whether new polling location is a public building
    govemp_new_poll_public = sum(((location_category%in%c('public','public_justice'))
                                  &(year==2019)),na.rm=T)>0
  )%>%
  ungroup()


############ Convert treatment indicators to numeric
no_mv_wellknown_two_data['new_poll_wellknown']<- sapply(no_mv_wellknown_two_data['new_poll_wellknown'],as.numeric)
mv_wellknown_two_data['new_poll_wellknown']<- sapply(mv_wellknown_two_data['new_poll_wellknown'],as.numeric)

#no_mv_wellknown_two_data['new_poll_wellknown']<- sapply(no_mv_wellknown_two_data['new_poll_wellknown'],as.numeric)
#mv_wellknown_two_data['new_poll_wellknown']<- sapply(mv_wellknown_two_data['new_poll_wellknown'],as.numeric)

#ecp_school_two_data['parent_new_poll_school'] <- sapply(ecp_school_two_data['parent_new_poll_school'],as.numeric)
#no_mv_school_two_data['parent_new_poll_school'] <- sapply(no_mv_school_two_data['parent_new_poll_school'],as.numeric)
#mv_school_two_data['parent_new_poll_school'] <- sapply(mv_school_two_data['parent_new_poll_school'],as.numeric)

#ecp_relig_two_data['relig_new_poll_relig'] <- sapply(ecp_relig_two_data['relig_new_poll_relig'],as.numeric)
#no_mv_relig_two_data['relig_new_poll_relig'] <- sapply(no_mv_relig_two_data['relig_new_poll_relig'],as.numeric)
#mv_relig_two_data['relig_new_poll_relig'] <- sapply(mv_relig_two_data['relig_new_poll_relig'],as.numeric)

ecp_public_two_data['govemp_new_poll_public'] <- sapply(ecp_public_two_data['govemp_new_poll_public'],as.numeric)
no_mv_public_two_data['govemp_new_poll_public'] <- sapply(no_mv_public_two_data['govemp_new_poll_public'],as.numeric)
mv_public_two_data['govemp_new_poll_public'] <- sapply(mv_public_two_data['govemp_new_poll_public'],as.numeric)
voters_govemp_two_data['govemp_new_poll_public'] <- sapply(voters_govemp_two_data['govemp_new_poll_public'],as.numeric)

