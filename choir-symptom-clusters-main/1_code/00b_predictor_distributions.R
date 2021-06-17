# Distribution of population-wide observations
#
# Eric Cramer <emcramer@stanford.edu>
#
# Examining the distributions of variables in the pre-processed dataset. 
#

predictor_distributions <- function(){
  #--------------------------------------------------
  # LIBRARIES AND WORKSPACE
  #--------------------------------------------------
  # clear the workspace of everything including hidden files and detach packages not in the base
  # rm(list = ls(all.names = TRUE))
  
  # Load the required packages
  libs <- c("tidyverse", "glue", "magrittr", "here", "ggplot2")
  suppressPackageStartupMessages(easypackages::libraries(libs))
  
  # source utility files
  utility_dir <- here("1_code/utilities")
  if(length(list.files(utility_dir)) > 0) {
    utility_files <- paste(utility_dir,list.files(utility_dir), sep="/")
    invisible(sapply(utility_files, source))
  }
  
  #--------------------------------------------------
  # LOADING DATA
  #--------------------------------------------------
  load(file = here("0_data/clean/data.RData"))
  
  # isolate the PROMIS measures as predictors
  pred_data <- train %>% 
    dplyr::select(all_of(ind_vars))
  
  #--------------------------------------------------
  # PLOTTING
  #--------------------------------------------------
  # plot histograms of each variable
  chart_data <- pred_data %>%
    tidyr::pivot_longer(cols = everything()
                        , names_to = "Metric"
                        , values_to = "Value")
  pred_labels <- c("Fatigue", "Depression", "Anxiety", 
                        "Sleep Disturbance", "Sleep Impairment", "Anger", 
                        "Social Isolation", "Emotional Support", 
                   "Satisfaction with Social Roles and Activities")
  names(pred_labels) <- ind_vars
  p <- ggplot(data=chart_data, aes(Value)) +
    geom_histogram(bins=30, stat='bin', color = 'white') +
    facet_wrap(~Metric, scales='free', labeller = labeller(Metric=pred_labels)) +
    labs(title='Distribution of each Predictor Variable', y='Count', x='Predictor') + 
    theme_bw()
  
  # save the plot
  my_ggsave(p, here("2_output/img/predictor_distributions"))
  return(p)
}
