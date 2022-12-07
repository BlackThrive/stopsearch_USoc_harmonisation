# 18/10/2022

# This script:
# - imports Understanding Society waves 6, 7, 8, 9, 10, 11, and 12
# - selects variables of interest
# - combines waves
# - removes non-response cases for critical variables (sex, ethnicity, long-term
# health condition, GHQ-12 score, and age)
# - selects age range below 30 years (effective range 15-30 years)
# - selects only data for England and Wales
# - sets appropriate variable types
# - writes the result to csv

# Updates
# 05/11/2022 - Addition of newly released wave 12 ("l")

rm(list = ls()) # clean environment

# install and library required packages
packages <- c('dplyr','readr','haven')
pkg_notinstall <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# inpaths can be configured in future to be specified by user in a function
inpath <- "./data/" 
# set waves to import and corresponding wave numbers and prefixes
waves <- data.frame("file" = c("l_indresp.dta", "k_indresp.dta","j_indresp.dta","i_indresp.dta", "h_indresp.dta", "g_indresp.dta", "f_indresp.dta"),
                    "number" = c(12, 11, 10, 9, 8, 7, 6),
                    "prefix" = c("l_", "k_","j_","i_","h_","g_","f_")) # make this so that it extracts from file col

combined_waves <- data.frame()

# iteratively load waves, subset to variables of interest, and combine
for(i in 1:nrow(waves)){
  this_wave <- haven::read_dta(paste0(inpath, waves[i,1]))
  
  # remove the wave prefixes from dataset
  names(this_wave) <- sub(waves$prefix[i],"", names(this_wave))
  
  this_wave <- this_wave %>%
    dplyr::select(pidp, # participant id
                  hidp, # household id
                  intdatd_dv, # interview date - day
                  intdatm_dv, # interview date - month
                  intdaty_dv, # interview date - year
                  intdatd_if, # interview date - imputation flag
                  age_dv, # age
                  age_if, # age imputation flag
                  sex_dv, # sex
                  ethn_dv, # ethnicity
                  # k_frpbulli, # physically bullied - this is alternate waves - 9 and 11
                  # k_frobulli, # bullied in other ways - this is alternate waves - 9 and 11
                  health, # long term health condition
                  eatlivu, # times in last 7 days eaten with family
                  # ypnpal, # number of close friends
                  fimnnet_dv, # net personal income
                  fimngrs_dv, # gross personal income
                  scghq1_dv, # ghq likert version - should we take sub-elements of ghq? n = 12
                  scghq2_dv, # ghq caseness version
                  gor_dv) # region 
  
  # create wave variable
  this_wave$wave <- waves[i,2]
  
  # use dplyr::bind_rows in case columns aren't identical
  combined_waves <- dplyr::bind_rows(combined_waves, this_wave) 
  rm(this_wave)
}

rm(waves)

combined_waves <- combined_waves %>%   
  dplyr::relocate(wave, .after = pidp) # move wave column to after pidp col

# drop non-response values (i.e. those with value below 0) from critical 
# variables (note non-critical vars will still have non-response values)
# and specify age range
combined_waves_reduced <- combined_waves %>%
  dplyr::filter(
      sex_dv > 0 & 
      ethn_dv > 0 & 
      health > 0 & 
      scghq1_dv >= 0 & 
      age_dv < 30) %>%
  haven::as_factor() %>% # convert all values to their labels
  # remove Scotland and NI
  filter(gor_dv != "Scotland" & gor_dv != "Northern Ireland") 

rm(combined_waves)

# set numeric variables to numeric
combined_waves_reduced_2 <- combined_waves_reduced %>%
  purrr::map_at(.at = c("intdatd_dv", 
                 "intdatm_dv", 
                 "intdaty_dv", 
                 "age_dv", 
                 "ypnpal", 
                 "fimnnet_dv", 
                 "fimngrs_dv", 
                 "scghq1_dv",
                 "scghq2_dv"), .f = function(x) as.numeric(as.character(x))) %>%
  purrr:::map_at(.at = c("pidp",
                         "hidp"),
                 .f = function(x) as.factor(x)) %>%
  as.data.frame() %>%
  # reapply haven labels from original dataframe (as.numeric strips labels)
  labelled::copy_labels_from(combined_waves_reduced)

rm(combined_waves_reduced)

write_csv(combined_waves_reduced_2, file = paste0("./data/out/", Sys.Date(), "_us_data_w6-12.csv"))
saveRDS(combined_waves_reduced_2, file = paste0("./data/out/", Sys.Date(), "_us_data_w6-12.rds"))
haven::write_dta(combined_waves_reduced_2, path = paste0("./data/out/", Sys.Date(), "_us_data_w6-12.dta"), label = NULL)

