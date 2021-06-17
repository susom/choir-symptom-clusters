# Preprocess Data
# 
# Author: Eric Cramer <emcramer@stanford.edu>
# 
# Description:
# Preprocesses the data to isolate the appropriate measures for clustering and 
# remove the patients from the small cohort.
# 

# helper functions for data cleaning
# function to concatenate the bodymap regions into a csv string
concat_regions <- function(segments) {
  regions_list <- lapply(segments, segmentsToRegions)
  return(unlist(lapply(regions_list, paste, collapse="|")))
}

# function to count bodymap segments
count_segments <- function(x) {
  n_listed <- lapply(
    str_split(x, ",")
    , function(x) ifelse(
      x[[1]] == "NA"
      , 0
      , length(x)
    )
  )
  return(unlist(n_listed))
}

# partition a data set
partition <- function(df, p){
  set.seed(123)
  idx <- sample(1:nrow(df), p*nrow(df), replace = FALSE)
  return(list('train' = df[idx, ], 'test' = df[-idx, ]))
}

preprocess <- function(){
  #--------------------------------------------------
  # LIBRARIES AND WORKSPACE
  #--------------------------------------------------
  
  # Load the required packages
  libs <- c("tidyverse", "lubridate", "glue", "magrittr", "here", "readxl")
  suppressPackageStartupMessages(easypackages::libraries(libs))
  
  # source utility files
  # example: get the function to convert bodymap segments to regions
  utility_dir <- here("1_code/utilities")
  if(length(list.files(utility_dir)) > 0) {
    utility_files <- paste(utility_dir,list.files(utility_dir), sep="/")
    invisible(sapply(utility_files, source))
  }
  
  #--------------------------------------------------
  # LOADING DATA
  #--------------------------------------------------
  # variable to hold the predictors to use for clustering and the things to measure in each cluster
  ind_vars <- c("PROMIS_FATIGUE", "PROMIS_DEPRESSION", "PROMIS_ANXIETY", "PROMIS_SLEEP_DISTURB_V1_0", 
                "PROMIS_SLEEP_IMPAIR_V1_0", "PROMIS_ANGER_V1_0", "PROMIS_SOCIAL_ISO_V2_0", 
                "PROMIS_EMOT_SUPPORT_V2_0", "PROMIS_SAT_ROLES_ACT_V2_0")
  dep_vars <- c("PAIN_SEVERITY", "N_BODYMAP_SEGMENTS", "BODYMAP_REGIONS", 
                "PROMIS_PAIN_INTERFERENCE", "PROMIS_PAIN_BEHAVIOR", "PROMIS_PHYSICAL_FUNCTION_MOB",
                "PROMIS_PHYSICAL_FUNCTION_UPE", "PCS_SCORE", "IEQ_SCORE")
  dep_vars2 <- c("PAIN_SEVERITY", "N_BODYMAP_SEGMENTS", "BODYMAP_REGIONS", 
                 "PROMIS_PAIN_INTERFERENCE", "PROMIS_PAIN_BEHAVIOR", "PROMIS_PHYSICAL_FUNCTION",
                 "PCS_SCORE", "IEQ_SCORE")
  pain_duration <- c("PAIN_EXP_DURATION_YEARS", "PAIN_EXP_DURATION_MONTHS", "PAIN_EXP_DURATION_DAYS")
  all_vars <- c(ind_vars, dep_vars2, setdiff(dep_vars, dep_vars2), "PAIN_DURATION")
  
  # read in the raw data
  init_updata <- read_excel(here("0_data/raw/initial_final.xlsx"))
  
  #--------------------------------------------------
  # PROCESSING
  #--------------------------------------------------
  processed_data <- init_updata %>%
    select_if(~!all(is.na(.))) %>% # remove NA cols
    dplyr::mutate(
      PAIN_SEVERITY = select(., starts_with("PAIN_INTENSITY")) %>% 
        rowMeans(na.rm = TRUE),
      N_BODYMAP_SEGMENTS = dplyr::pull(., BODYMAP_REGIONS_CSV) %>%
        count_segments() %>%
        replace_na(0),
      BODYMAP_REGIONS = dplyr::pull(., BODYMAP_REGIONS_CSV) %>%
        concat_regions(),
      PAIN_DURATION = PAIN_EXP_DURATION_YEARS*12 + PAIN_EXP_DURATION_MONTHS + PAIN_EXP_DURATION_DAYS/30
    ) %>%
    dplyr::select(MRN, all_of(all_vars))
  #--------------------------------------------------
  # MUNGING
  #--------------------------------------------------
  # not imputing data in this project --> remove patients with NA values
  munged_data <- processed_data[complete.cases(select(processed_data, all_of(ind_vars))), ]
  write_csv(munged_data, here("0_data/clean/full_munged_data.csv"))
  
  #--------------------------------------------------
  # SPLITTING
  #--------------------------------------------------
  # train test split (75/25)
  parts <- partition(munged_data, 0.75)
  train <- parts$train
  test <- parts$test
  
  #--------------------------------------------------
  # SAVING
  save(ind_vars, dep_vars, dep_vars2, all_vars, train, test, munged_data
       , file = here("0_data/clean/data.RData"))
  print("Done Preprocessing!")
}
