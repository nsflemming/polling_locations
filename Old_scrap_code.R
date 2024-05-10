# Make and plot predictions for interaction models for specific value of indp vars
boxplot_preds <- function(model, plotdata, var1, var2, num_ran_vars, xlab, ylab, color_lab, 
                          dir, plot_name){
  #calculate predicted probabilities
  preds<-predict(model, newdata=plotdata, type='response', se.fit=T)
  pred_probs<-preds$fit
  pred_errs<-preds$se.fit
  # Plot predictions
  ## create plot title based on random variable values
  var_settings = c()
  for(i in 1:num_ran_vars){
    var_settings=c(var_settings, paste0(colnames(plotdata)[i+2],': ',plotdata[[i+2]][1]))
  }
  plot_title=paste0(var_settings, collapse='\n')
  # plot
  pred_plot<-ggplot(plotdata, aes(x = plotdata[[var1]], y = pred_probs, 
                                  color = factor(plotdata[[var2]]))) +
    geom_boxplot() +
    geom_errorbar(aes(ymin=pred_probs-1.96*pred_errs,
                      ymax=pred_probs+1.96*pred_errs),
                  width=0.2, position=position_dodge(0.8))+
    labs(x = xlab, y = ylab, fill = color_lab, 
         title = plot_title) +
    scale_color_discrete(name = color_lab)+
    theme(plot.title = element_text(size=10),
          axis.title = element_text(size = 10),
          legend.position = "none")
  setwd(dir)
  return(pred_plot)
}

# Make grid of multiple boxplots 
#   of predictions for interaction models for specific value of indp vars
grid_boxplot <- function(model, data, var1, var2, num_ran_vars, xlab, ylab, color_lab, 
                         plot_dir, plot_name){
  num_rows=nrow(data)
  num_plots = num_rows/4
  start <- seq(from = 1, to = num_rows-3, by = 4)
  end <- seq(from = 4, to = num_rows, by = 4)
  plots_list<-list()
  for(i in 1:num_plots){
    p<-boxplot_preds(model=model, plotdata=data[start[i]:end[i],], var1 = var1, 
                     var2=var2, xlab = xlab, ylab = ylab, color_lab = color_lab,
                     num_ran_vars = num_ran_vars, dir=plot_dir, plot_name=plot_name)
    plots_list[[i]]<-p
  }
  setwd(plot_dir)
  ncols<-ceiling(sqrt(num_plots)) # can add 1 extra plot for legend
  nrows<-ceiling((num_plots)/ncols)
  grid.arrange(grobs=plots_list, ncol=ncols, nrow=nrows)
  g <- arrangeGrob(grobs=plots_list, ncol=ncols, nrow=nrows) #generates g for saving
  ggsave(file=plot_name, device='png', width=2000, height=2000, units='px', g) #saves g
  #ggsave(plot_name, device='png', width=2000, height=2000, units='px')
}



#####################
# Create dataframe for prediction
new_data <- expand.grid(has_child = c(TRUE,FALSE),
                        school = c(TRUE,FALSE),
                        Voters_Gender=levels(as.factor(model_data$Voters_Gender)),
                        Parties_Description=levels(as.factor(model_data$Parties_Description)),
                        #pred_race=levels(as.factor(model_data$pred_race)),
                        Voters_Age=mean(model_data$Voters_Age, na.rm=T), 
                        CommercialData_EstimatedHHIncomeAmount = mean(model_data$CommercialData_EstimatedHHIncomeAmount, na.rm=T),
                        Residence_Families_HHCount=mean(model_data$Residence_Families_HHCount, na.rm=T),
                        known_religious=c(TRUE,FALSE),
                        CommercialData_LikelyUnion =levels(as.factor(model_data$CommercialData_LikelyUnion)),
                        CommercialData_OccupationIndustry=levels(as.factor(model_data$CommercialData_OccupationIndustry))
)
preds<-predict(m_schl, newdata=new_data, type='response', se.fit=T)
pred_probs<-preds$fit
pred_errs<-preds$se.fit
#plot predictions
p1<-boxplot_preds(model=m_schl, plotdata=new_data[1:4,], var1 = 'has_child', 
                  var2='school', xlab = 'Has a child/children',
                  num_ran_vars=2,
                  ylab = 'Probability of Voting',color_lab = 'Votes at a School',
                  dir=plot_dir, plot_name='child_schl_pred_plot.png')
p2<-boxplot_preds(model=m_schl, plotdata=new_data[5:8,], var1 = 'has_child', 
                  var2='school', xlab = 'Has a child/children', 
                  num_ran_vars=2,
                  ylab = 'Probability of Voting',color_lab = 'Votes at a School',
                  dir=plot_dir, plot_name='child_schl_pred_plot.png')
grid.arrange(p1,p2,nrow=1)
grid_boxplot(m_schl, new_data, 'has_child', 'school', 2, xlab='Has a child/children', 
             ylab = 'Probability of Voting',color_lab = 'Votes at a School',
             plot_dir=plot_dir, plot_name='child_schl_pred_plot.png')

#vars
ind_vars_relig <-c(
  # var of interest
  'known_religious*relig_loc',
  common_covars
)
# model
m_relig<-log_reg(model_data, 'General_2017_11_07', ind_vars_relig)
summary(m_relig)
#save results
write_summ(results_dir, 'relig_2017', m_relig)
## Calculate and plot predicted probabilities
relig_pred<-predict_response(m_relig, terms=c('known_religious','relig_loc'), margin='marginalmeans')
#plot predicted probabilities
pred_prob_plot(relig_pred, plot_title = 'Probability of Voting of the (Non-)Religious at Religious Locations',
               xlab='Is Religious', legend_title ='Votes at a Religious Building', angle=0,legend_position = 'right',
               output_dir = plot_dir, image_name = 'Pred_Prob_relig_relig_2017')


#######

### Probability of Voting, if black and voting at justice system building
#vars
ind_vars_blk_just <-c(
  # var of interest
  'pred_black*justice_loc',
  # Demographics
  'Voters_Gender', 'Voters_Age', 'Parties_Description',
  'CommercialData_Education','CommercialData_EstimatedHHIncomeAmount'
)
# model
m_blk<-log_reg(model_data, 'General_2018_11_06', ind_vars_blk_just)
summary(m_blk)
#save results
write_summ(results_dir, 'black', m_blk)