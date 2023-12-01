library(tidyverse)
library(acs)
library(tidycensus)
library(sf)
library(sfdep)
library(conflicted)

filter <- dplyr::filter
select <- dplyr::select


## need to pull data for 2014 and 2019 at the block group level

# wants:
# race
# income
# gross rent
# housing cost burden
# household size
# overcrowindg


phl_acs <- get_acs(geography = "tract", 
                     variables = c("B01003_001", #totalpop
                                   "B19013_001", #medinc
                                   "B02001_002", #white_pop
                                   "B08013_001", #travel_time
                                   "B08012_001", #num_commuters
                                   "B08301_001", #means_of_trans
                                   "B08301_010", #total_public_trans
                                   "B25064_001", #medgross_rent
                                   "B25070_002", #less_10p
                                   "B25070_003", #10to15
                                   "B25070_004", #15to20
                                   "B25070_005", #20to25
                                   "B25070_006", #25to30
                                   "B25070_007", #30to35
                                   "B25070_008", #35to40
                                   "B25070_009", #40to50
                                   "B25070_010", #50+
                                   
                                   ), 
                     year = 2014, 
                     state = "PA", 
                     geometry = TRUE, 
                     county="Philadelphia",
                     output = "wide") %>%
  rename(Total_Pop =  B01003_001E,
         Med_Inc = B19013_001E,
         White_Pop = B02001_002E,
         Travel_Time = B08013_001E,
         Num_Commuters = B08012_001E,
         Means_of_Transport = B08301_001E,
         Total_Public_Trans = B08301_010E,
         Med_Gross_Rent = B25064_001E, ) %>%
  select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
         Means_of_Transport, Total_Public_Trans,
         Med_Age,
         GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport) %>%
  mutate(
    nb = st_knn(geometry, 5), # need to impute census values for tracts with pop ~ 0
    wt = st_weights(nb),
    Med_Age = ifelse(is.na(Med_Age), purrr::map_dbl(find_xj(Med_Age, nb), mean, na.rm = TRUE), Med_Age),
    Med_Inc = ifelse(is.na(Med_Inc), purrr::map_dbl(find_xj(Med_Inc, nb), mean, na.rm = TRUE), Med_Inc),
    Travel_Time = ifelse(is.na(Travel_Time), purrr::map_dbl(find_xj(Travel_Time, nb), mean, na.rm = TRUE), Travel_Time),
    Percent_White = ifelse(is.na(Percent_White), purrr::map_dbl(find_xj(Percent_White, nb), mean, na.rm = TRUE), Percent_White),
    Mean_Commute_Time = ifelse(is.na(Mean_Commute_Time), purrr::map_dbl(find_xj(Mean_Commute_Time, nb), mean, na.rm = TRUE), Mean_Commute_Time),
    Percent_Taking_Public_Trans = ifelse(is.na(Percent_Taking_Public_Trans), purrr::map_dbl(find_xj(Percent_Taking_Public_Trans, nb), mean, na.rm = TRUE), Percent_Taking_Public_Trans))

saveRDS(acs_vars, acs_vars_path) # path already defined in config file
