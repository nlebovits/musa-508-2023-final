library(tidyverse)
library(acs)
library(tidycensus)
library(sf)
library(sfdep)
library(conflicted)

filter <- dplyr::filter
select <- dplyr::select


## need to pull data for 2010, 2015, 2020 at the block group level

# wants:
# race
# income
# gross rent
# housing cost burden
# household size
# overcrowindg


phl_acs <- get_acs(geography = "tract", 
                     variables = c("B01003_001", 
                                   "B19013_001", 
                                   "B02001_002", 
                                   "B08013_001",
                                   "B08012_001", 
                                   "B08301_001", 
                                   "B08301_010", 
                                   "B01002_001"), 
                     year = 2021, 
                     state = "PA", 
                     geometry = TRUE, 
                     county="Philadelphia",
                     output = "wide") %>%
  rename(Total_Pop =  B01003_001E,
         Med_Inc = B19013_001E,
         Med_Age = B01002_001E,
         White_Pop = B02001_002E,
         Travel_Time = B08013_001E,
         Num_Commuters = B08012_001E,
         Means_of_Transport = B08301_001E,
         Total_Public_Trans = B08301_010E) %>%
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
