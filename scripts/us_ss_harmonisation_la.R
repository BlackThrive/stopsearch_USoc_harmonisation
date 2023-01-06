
rm(list = ls()) # clean environment

# !! MAKE THIS INTO A FUNCTION !! 
# So that user can specify how datasets should be combined (e.g., by month, year etc)

# install and library required packages
packages <- c('dplyr','readr','haven')
pkg_notinstall <- packages[!(packages %in% installed.packages()[,"Package"])]
lapply(pkg_notinstall, install.packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# us_data <- haven::read_dta("./data/2022-10-18_us_data_w9-11.dta") 
# us_data <- readr::read_csv("./data/2022-10-18_us_data_w9-11.csv") 
us_data <- readr::read_rds("./data/out/2023-01-05_us_data_w6-12.rds") %>%
  mutate(
    year_month = as.Date(paste(intdaty_dv, intdatm_dv, "01", sep = "-"), format = "%Y-%m-%d"),
    year = as.numeric(intdaty_dv)
  )


# # read stop and search data
# ss_data <- readr::read_rds("./data/ss_by_year_region.csv") %>%
#   # rename to make consistent with US data ready for merging - by year
#   rename(
#     "gor_dv" = "region",
#     "year" = "date"
#   )

# rds version
ss_data <- readr::read_rds("./data/2022-11-18_ss_by_year_region_mixed_included_2017_2021.rds") %>%
  # rename to make consistent with US data ready for merging - by year
  rename(
    "gor_dv" = "region",
    "year" = "date"
  )

# ss_data <- readr::read_rds("./data/ss_by_month_region.rds") %>%
#   # rename to make consistent with US data ready for merging - by month
#   rename(
#     "gor_dv" = "region",
#     "year_month" = "date"
#   )

population_density_data <- readr::read_csv("./data/2022-10-26_pop_data_by_ethn_region_wide.csv") %>%
  select(c(region, starts_with("population_density"))) %>%
  rename(
    gor_dv = region
  )

imd_data <- readr::read_csv("./data/out/region_imd.csv") %>%
  rename(
    gor_dv = RGN19NM
  ) %>%
  add_row(gor_dv = "Wales") # add Wales for now, even though it'll likely be excluded

# rename yorkshire and the humber to small t
imd_data[which(imd_data$gor_dv == "Yorkshire and The Humber"), "gor_dv"] <- "Yorkshire and the Humber"

# names(population_density_data) <- sub("^population_density_","", names(population_density_data))

# different way of summarising regional disparities
region_disp_data <- readr::read_csv("./data/out/2022-11-18_region_disproportionality_summary_17_21.csv") %>%
  rename(
    year = date,
    gor_dv = region,
    sum_disp_rank = rank
  ) %>%
  select(gor_dv, year, sum_disp_score, sum_disp_rank)

# make year var in US
# us_data$year <- us_data$intdaty_dv

## Merge datasets ##

# merge US and SS by region and year
harmonised_data <- 
  merge(us_data, ss_data, by = c("gor_dv","year"))

# add alernative disproportionality quantification method
harmonised_data <- merge(harmonised_data, region_disp_data, by = c("gor_dv","year"))

# add IMD data
harmonised_data <- merge(harmonised_data, imd_data, by = "gor_dv")

# add population density (need to fix columns to better names)
harmonised_data <- 
  merge(harmonised_data, population_density_data, by = c("gor_dv"))

# move IMD next to region variable
# harmonised_data <- harmonised_data %>%
#   relocate(
#     imd_extent, .after = gor_dv
#   )

# set variable types
# factors
harmonised_data <- harmonised_data %>%
  purrr:::map_at(.at = c("la",
                       "county",
                       "country",
                       "force"),
               .f = function(x) as.factor(x)) %>%
  as.data.frame()

readr::write_csv(harmonised_data, file = paste0("./data/out/", Sys.Date(), "_harmonised_data_by_region_by_year_2017-2021.csv"))
saveRDS(harmonised_data, file = paste0("./data/out/", Sys.Date(), "_harmonised_data_by_region_by_year_2017-2021.rds"))
# will throw error because of variable names for 
haven::write_dta(harmonised_data, path = paste0("./data/out/", Sys.Date(), "_harmonised_data_by_region_by_year_2017-2021.dta"), label = NULL)

