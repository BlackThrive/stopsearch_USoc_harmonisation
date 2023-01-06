# 04/11/2022
# 
# This script links IMD at LSOA levels to region level. The method for doing so 
# follows the English Indices of Deprivation 2019 Technical Report outline for 
# summarising indices to larger areas (point 3.8 and Appendix N), using the 'Extent'
# method. This method weights population estimates in each area according to the
# area's level of deprivation to produce a quantification of the proportion of
# people wihtin the higher level area living in a deprived area.

rm(list=ls())
# install and library required packages
packages <- c('dplyr','readr')
pkg_notinstall <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# IMD data from https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019

# Full data - File 1: Index of Multiple Deprivation
imd_data <- read_csv("./data/full_imd_data.csv") %>%
  rename(
    lsoa_code = `LSOA code (2011)`
  )


# Population denominators - File 6
pop_denom_data <- read_csv("./data/imd_pop_denominators.csv") %>%
  rename(
    lsoa_code = `LSOA code (2011)`,
    pop = `Total population: mid 2015 (excluding prisoners)`
  )

# 2019 LAD to region lookup
# From https://geoportal.statistics.gov.uk/datasets/ons::local-authority-district-to-region-april-2019-lookup-in-england-1/explore
lookup_data <- read_csv("./data/lad_region_lookup_2019.csv")

# join datasets
imd_data_2 <- imd_data %>%
  rename(
    LAD19CD = `Local Authority District code (2019)`,
    imd_score = `Index of Multiple Deprivation (IMD) Score`,
    imd_rank = `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`
  ) %>%
  select(lsoa_code:imd_rank) %>% # select just vars of present interest
  left_join(., lookup_data[,c("RGN19CD", "RGN19NM","LAD19CD")] , by = "LAD19CD") %>%
  left_join(., pop_denom_data[,c("lsoa_code", "pop")], by = "lsoa_code")


# for calculating summary statistics at region level

# calculate percentile
imd_data_2$percentile <- ceiling((imd_data_2$imd_rank / max(imd_data_2$imd_rank)) * 100)

# function for specifying population weighting according to percentile. Formula
# follows 'extent' method outlined in point 3.8 and Appendix N of English 
# Indices of Deprivation 2019 Technical Report
weighting_function <- function(x){
  # if in top 10% give weighting 1
  if(x <= 10){
    return(1)
  } 
  # if from 11-30% give weighting according to percentile (19 increments from 0.95 to 0.05)
  else if(x > 10 & x <= 30){
   return(0.95 - (((0.95 - 0.05)/19) * (x - 11)))
  }
  # if outside top 30% give weighting 0
  else{
    return(0)
  }
}
  
# apply the function
imd_data_2$weighting <- lapply(imd_data_2$percentile, weighting_function)


imd_data_2 <- imd_data_2 %>%
  mutate(
    weighting = as.numeric(weighting),
    weighted_population = pop * weighting # calculate weighted population
  )

# IMD summary
region_imd <- imd_data_2 %>%
  group_by(RGN19NM) %>%
  summarise(
    mean_imd_score = mean(imd_score),
    mean_imd_rank = mean(imd_rank),
    imd_score_var = var(imd_score),
    imd_score_std = sqrt(var(imd_score)),
    region_pop = sum(pop),
    imd_extent = sum(weighted_population) / region_pop
  )

# save data
write_csv(region_imd, file = "./data/out/region_imd.csv")
write_rds(region_imd, file = "./data/out/region_imd.rds")
