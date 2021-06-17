# script for processing the followup cohort into 2 timepoints

#--------------------------------------------------
# LIBRARIES AND WORKSPACE
#--------------------------------------------------

# clear the workspace of everything including hidden files and detach packages not in the base
rm(list = ls(all.names = TRUE))

# Load the required packages
libs <- c("tidyverse", "lubridate", "glue", "magrittr", "here", "readxl")
suppressPackageStartupMessages(easypackages::libraries(libs))

# Set hard-coded variables
data_dir <- here("0_data")
out_dir <- here("2_output/no-pain-predictors")
temp_dir <- here("0_data/temp")

# get the function to convert bodymap segments to regions
source(here::here("1_code/segments_to_regions.R"))

# helper functions for data cleaning
# function to concatenate the bodymap regions into a csv string
concat_regions <- function(segments) {
  regions_list <- lapply(segments, segmentsToRegions)
  return(unlist(lapply(regions_list, paste, collapse="|")))
}

# function to count bodymap segments
count_segments <- function(x) {
  n_listed <- lapply(strsplit(x, ","), length)
  return(unlist(n_listed))
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
all_vars <- c(ind_vars, dep_vars, "PAIN_DURATION")

# IEQ score data
ieq_data <- read_csv(
  glue("{data_dir}/raw/choir_symptom_cluster_followup_ieq.csv")
  , col_types = cols(
    MRN = col_character()
    , SURVEY_ENDED = col_character()
    , SURVEY_SCHEDULED = col_character()
    , IEQ_SCORE = col_character()
  )
) %>%
  filter(!is.na(IEQ_SCORE)) %>%
  mutate(
    SURVEY_DATE1 = date(dmy_hms(SURVEY_ENDED))
    , SURVEY_DATE2 = date(dmy_hms(SURVEY_SCHEDULED))
    ) #%>%
  select(-SURVEY_ENDED, -SURVEY_SCHEDULED)

# follow up data
sm_tp1 <- read_excel(
  glue("{data_dir}/raw/followup_final.xlsx")
  , sheet="tp1"
  , col_types = cols(
    MRN = col_character()
  )) %>%
  mutate(SURVEY_DATE = date(dmy(SURVEY_DATE))) %>%
  select_if(~sum(!is.na(.)) > 0) %>% # remove NA columns
  left_join(ieq_data, by = c("MRN" = "MRN", "SURVEY_DATE" = "SURVEY_DATE2"))
sm_tp2 <- read_excel(
  glue("{data_dir}/raw/followup_final.xlsx")
  , sheet="tp2"
  , col_types = "text") %>%
  mutate(SURVEY_DATE = date(dmy(SURVEY_DATE))) %>%
  select_if(~sum(!is.na(.)) > 0) %>% # remove NA columns
  left_join(ieq_data, by = c("MRN" = "MRN", "SURVEY_DATE" = "SURVEY_DATE2"))

