# 18/10/2022

# This script:
# - imports Understanding Society waves 9, 10, and 11
# - selects variables of interest
# - combines waves
# - removes non-response cases for critical variables (sex, ethnicity, long-term
# health condition, GHQ-12 score, and age)
# - selects age range below 30 years (effective range 15-30 years)
# - selects only data for England and Wales
# - sets appropriate variable types
# - writes the result to csv

rm(list = ls()) # clean environment

# install and library required packages
packages <- c('dplyr','readr','haven')
pkg_notinstall <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# inpaths can be configured in future to be specified by user in a function
inpath <- "./data/" 
waves <- c("k_indresp.dta","j_indresp.dta","i_indresp.dta")
ss_file <- "ss_disp_eng_wales_2019-2021.csv"

# wave 11
k_indresp <- haven::read_dta(paste0(inpath, waves[1]))

# select vars
k_indresp_ex <- dplyr::select(k_indresp,
                              pidp, # participant id
                              k_hidp, # household id
                              k_intdatd_dv, # interview date - day
                              k_intdatm_dv, # interview date - month
                              k_intdaty_dv, # interview date - year
                              k_intdatd_if, # interview date - imputation flag
                              k_age_dv, # age
                              k_age_if, # age imputation flag
                              k_sex_dv, # sex
                              k_ethn_dv, # ethnicity
                              # k_frpbulli, # physically bullied - this is alternate waves - 9 and 11
                              # k_frobulli, # bullied in other ways - this is alternate waves - 9 and 11
                              k_health, # long term health condition
                              k_eatlivu, # times in last 7 days eaten with family
                              k_ypnpal, # number of close friends
                              k_fimnnet_dv, # net personal income
                              k_fimngrs_dv, # gross personal income
                              k_scghq1_dv, # ghq likert version - should we take sub-elements of ghq? n = 12
                              k_scghq2_dv, # ghq caseness version
                              k_gor_dv) # region 

# remove unwanted data and save space
rm(k_indresp) 

# wave 10
j_indresp <- haven::read_dta(paste0(inpath, waves[2]))

# select vars
j_indresp_ex <- dplyr::select(j_indresp,
                              pidp,# participant id
                              j_hidp, # household id
                              j_intdatd_dv, # interview date - day
                              j_intdatm_dv, # interview date - month
                              j_intdaty_dv, # interview date - year
                              j_intdatd_if, # interview date - imputation flag
                              j_age_dv, # age
                              j_age_if, # age imputation flag                       
                              j_sex_dv, # sex
                              j_ethn_dv, # ethnicity
                              #j_frpbulli, # physically bullied - this is alternate waves - 9 and 11
                              #j_frobulli, # bullied in other ways - this is alternate waves - 9 and 11
                              j_health, # long term health condition
                              j_eatlivu, # times in last 7 days eaten with family
                              j_ypnpal, # number of close friends
                              j_fimnnet_dv, # net personal income
                              j_fimngrs_dv, # gross personal income
                              j_scghq1_dv, # ghq likert version - should we take sub-elements of ghq? n = 12
                              j_scghq2_dv, # ghq caseness version
                              j_gor_dv) # region

# remove unwanted data and save space
rm(j_indresp) 

# wave 9
i_indresp <- haven::read_dta(paste0(inpath, waves[3]))

# select vars
i_indresp_ex <- dplyr::select(i_indresp,
                              pidp, # participant id
                              i_hidp, # household id
                              i_intdatd_dv, # interview date - day
                              i_intdatm_dv, # interview date - month
                              i_intdaty_dv, # interview date - year
                              i_intdatd_if, # interview date - imputation flag
                              i_age_dv, # age
                              i_age_if, # age imputation flag                      
                              i_sex_dv, # sex
                              i_ethn_dv, # ethnicity
                              # i_frpbulli, # physically bullied - this is alternate waves - 9 and 11
                              # i_frobulli, # bullied in other ways - this is alternate waves - 9 and 11
                              i_health, # long term health condition
                              i_eatlivu, # times in last 7 days eaten with family
                              #i_ypnpal, # number of close friends # for some reason doesn't exist in wave 9?
                              i_fimnnet_dv, # net personal income
                              i_fimngrs_dv, # gross personal income
                              i_scghq1_dv, # ghq likert version - should we take sub-elements of ghq? n = 12
                              i_scghq2_dv, # ghq caseness version
                              i_gor_dv) # region

# remove unwanted data and save space
rm(i_indresp)  

# create a wave variable and remove the wave prefixes from datasets
k_indresp_ex$wave <- 11
names(k_indresp_ex) <- sub("^k_","", names(k_indresp_ex))

j_indresp_ex$wave <- 10
names(j_indresp_ex) <- sub("^j_","", names(j_indresp_ex))

i_indresp_ex$wave <- 9
names(i_indresp_ex) <- sub("^i_","", names(i_indresp_ex))

# use dplyr::bind_rows in case columns aren't identical
combined_waves <- dplyr::bind_rows(k_indresp_ex, j_indresp_ex, i_indresp_ex) %>%   
  dplyr::relocate(wave, .after = pidp) # move wave column to after pidp col

# remove unwanted data
rm(i_indresp_ex)
rm(j_indresp_ex)
rm(k_indresp_ex)

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

# set numeric variables to as.numeric
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
  as.data.frame() %>%
  # reapply haven labels from original dataframe (as.numeric strips labels)
  labelled::copy_labels_from(combined_waves_reduced)


write.csv(combined_waves_reduced_2, file = paste0("./data/", Sys.Date(), "_us_data_w9-11.csv"))
