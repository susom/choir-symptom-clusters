# Characterizing Clusters from the Hierarchical Model
# 
# Author: Eric Cramer <emcramer@stanford.edu>
# 
# 
# 

# helper functions for data cleaning
# function to concatenate the bodymap regions into a csv string
concat_regions <- function(segments) {
  regions_list <- lapply(segments, segmentsToRegions)
  return(unlist(lapply(regions_list, paste, collapse="|")))
}

cluster_labelling <- function(){
  #--------------------------------------------------
  # LIBRARIES AND WORKSPACE
  #--------------------------------------------------
  # # clear the workspace of everything including hidden files
  # rm(list = ls(all.names = TRUE))
  
  # Load the required packages
  libs <- c("tidyverse", "glue", "magrittr", "here", 
            "ggplot2", "openxlsx", "readxl", "lubridate")
  suppressPackageStartupMessages(easypackages::libraries(libs))
  
  # source utility functions
  utility_dir <- here("1_code/utilities")
  if(length(list.files(utility_dir)) > 0) {
    utility_files <- paste(utility_dir,list.files(utility_dir), sep="/")
    invisible(sapply(utility_files, source))
  }
  
  #--------------------------------------------------
  # LOAD MODEL AND DATA
  #--------------------------------------------------
  load(file = here("0_data/clean/data.RData"))
  hc_res <- readRDS(here("2_output/models/hc_res.rds"))
  centroids <- read_csv(here("2_output/tables/train_centroids.csv"))
  
  # load the small cohort data
  # follow up data
  sm_tp1 <- read_excel(
    here("0_data/raw/followup_final.xlsx")
    , sheet="tp1") %>%
    mutate(SURVEY_DATE = date(dmy(SURVEY_DATE))) %>%
    # select_if(~sum(!is.na(.)) > 0) %>%
    mutate(
      PAIN_DURATION = PAIN_EXP_DURATION_YEARS*12 + 
        PAIN_EXP_DURATION_MONTHS + 
        PAIN_EXP_DURATION_DAYS/30,
      PAIN_SEVERITY = dplyr::select(., PAIN_INTENSITY_WORST:PAIN_INTENSITY_NOW) %>% 
        purrr::pmap(~ signif(mean(c(...), na.rm=TRUE)), 3) %>% 
        unlist(),
      N_BODYMAP_SEGMENTS = dplyr::pull(., BODYMAP_REGIONS_CSV) %>%
        count_segments(),
      BODYMAP_REGIONS = dplyr::pull(., BODYMAP_REGIONS_CSV) %>%
        concat_regions()
    )
  
  sm_tp2 <- read_excel(
    here("0_data/raw/followup_final.xlsx")
    , sheet="tp2") %>%
    mutate(SURVEY_DATE = date(dmy(SURVEY_DATE))) %>%
    # select_if(~sum(!is.na(.)) > 0) %>%
    mutate(
      PAIN_DURATION = PAIN_EXP_DURATION_YEARS*12 + 
        PAIN_EXP_DURATION_MONTHS + 
        PAIN_EXP_DURATION_DAYS/30,
      PAIN_SEVERITY = dplyr::select(., PAIN_INTENSITY_WORST:PAIN_INTENSITY_NOW) %>% 
        purrr::pmap(~ signif(mean(c(...), na.rm=TRUE)), 3) %>% 
        unlist(),
      N_BODYMAP_SEGMENTS = dplyr::pull(., BODYMAP_REGIONS_CSV) %>%
        count_segments(),
      BODYMAP_REGIONS = dplyr::pull(., BODYMAP_REGIONS_CSV) %>%
        concat_regions()
    )
  
  #--------------------------------------------------
  # ADDITIONAL DATA PROCESSING
  #--------------------------------------------------
  # get the labels for each patient in the training set
  train_labeled <- add_column(train, cluster = as.factor(hc_res$cluster))
  # save the labeled data
  write_csv(train_labeled, here("2_output/tables/train_labeled.csv"))
  
  # function to label new data
  centroid_classifier <- function(x, centers = centroids) {
    # return the cluster assignment based on the centroids
    return(which.min(apply(centers, 1, function(c, y){dist(rbind(c,y))}, y=x)))
  }
  # label the testing data
  test_labeled <- add_column(
    test
    , cluster = as.factor(apply(select(test, all_of(ind_vars))
                                , 1
                                , centroid_classifier))
  )
  write_csv(test_labeled, here("2_output/tables/test_labeled.csv"))
  
  # output fully labeled data
  full_labeled <- rbind(train_labeled, test_labeled)
  write_csv(full_labeled, here("2_output/tables/full_labeled.csv"))
  
  # reshape the small cohort with labels and mutate columns to match
  sm_vars <- all_vars[-grep("BODYMAP_REGIONS|PROMIS_PHYSICAL_FUNCTION", all_vars)]
  sm_df1 <- sm_tp1 %>%
    select(MRN, all_of(sm_vars), PAIN_SEVERITY, N_BODYMAP_SEGMENTS) %>%
    add_column(cluster = as.factor(apply(select(., all_of(ind_vars)), 1, centroid_classifier))
               , tp = "tp1") %>%
    add_column(
      BODYMAP_REGIONS = sm_tp1$BODYMAP_REGIONS
    )
  sm_df2 <- sm_tp2 %>%
    select(MRN, all_of(sm_vars), PAIN_SEVERITY, N_BODYMAP_SEGMENTS) %>%
    add_column(cluster = as.factor(apply(select(., all_of(ind_vars)), 1, centroid_classifier))
               , tp = "tp2") %>%
    add_column(
      BODYMAP_REGIONS = sm_tp2$BODYMAP_REGIONS
    )
  # combine the timepoint data frames
  sm_df <- data.frame(rbind(sm_df1, sm_df2))
  # save the labeled data
  write_csv(sm_df, here("2_output/tables/sm_labeled.csv"))
  
  #--------------------------------------------------
  # CLUSTER SIZES
  #--------------------------------------------------
  counts_df <- data.frame(
    Cluster = factor(1:nrow(centroids)),
    Train = data.frame(table(train_labeled$cluster))$Freq,
    Test = data.frame(table(test_labeled$cluster))$Freq,
    TP1 = data.frame(table(sm_df1$cluster))$Freq,
    TP2 = data.frame(table(sm_df2$cluster))$Freq
  ) %>%
    pivot_longer(cols = -Cluster, names_to = "Cohort", values_to = "Count")
  p <- ggplot(data = counts_df) + 
    geom_bar(aes(Cluster, Count, fill = Cohort)
             , stat='identity'
             , color='black'
             , position = 'dodge') + 
    scale_y_log10() +
    labs(x='Cluster'
         , y=expression('# Patients (Log'[10]*')')
         , title="Number of Patients in each Cluster") + 
    theme_bw()
  my_ggsave(p, here("2_output/img/cluster_sizes"))
}
