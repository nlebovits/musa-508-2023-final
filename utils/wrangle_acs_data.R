library(dplyr)
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

### acs 14-------------------------------------------------------
phl_acs14 <- get_acs(geography = "block group", 
                     variables = c("B01003_001", #totalpop
                                   "B19013_001", #medinc
                                   "B02001_002", #white_pop
                                   "B08013_001", #travel_time
                                   "B08012_001", #num_commuters
                                   "B08301_001", #means_of_trans
                                   "B08301_010", #total_public_trans
                                   "B11012_001", #totalhh
                                   "B25003_003", #renter_occupied
                                   "B25064_001", #medgross_rent
                                   "B25070_002", #less_10p
                                   "B25070_003", #10to15
                                   "B25070_004", #15to20
                                   "B25070_005", #20to25
                                   "B25070_006", #25to30
                                   "B25070_007", #30to35
                                   "B25070_008", #35to40
                                   "B25070_009", #40to50
                                   "B25070_010" #50+
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
         Total_HH = B11012_001E,
         Renters = B25003_003E,
         Med_Gross_Rent = B25064_001E,
         Less_10 = B25070_002E,
         Less_15 = B25070_003E,
         Less_20 = B25070_004E,
         Less_25 = B25070_005E, 
         Less_30 = B25070_006E,
         Less_35 = B25070_007E,
         Less_40 = B25070_008E, 
         Less_50 = B25070_009E, 
         More_50 = B25070_010E) %>%
  select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
         Means_of_Transport, Total_Public_Trans, Total_HH, Renters, Med_Gross_Rent, Less_10, Less_15, Less_20,
         Less_25, Less_30, Less_35, Less_40, Less_50, More_50,
         GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Percent_Nonwhite = 1-Percent_White, 
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport,
         Percent_Renters = Renters / Total_HH,
         Rent_Burden = (Less_30 + Less_35) / Renters,
         Ext_Rent_Burden = (Less_40 + Less_50 + More_50) / Renters,
         No_Rent_Burden = (Less_10 + Less_15 + Less_20 + Less_25) / Renters) %>%
  mutate(
    nb = st_knn(geometry, 5), # need to impute census values for tracts with pop ~ 0
    wt = st_weights(nb),
    Med_Inc = ifelse(is.na(Med_Inc), purrr::map_dbl(find_xj(Med_Inc, nb), mean, na.rm = TRUE), Med_Inc),
    Travel_Time = ifelse(is.na(Travel_Time), purrr::map_dbl(find_xj(Travel_Time, nb), mean, na.rm = TRUE), Travel_Time),
    Percent_White = ifelse(is.na(Percent_White), purrr::map_dbl(find_xj(Percent_White, nb), mean, na.rm = TRUE), Percent_White),
    Percent_Nonwhite = ifelse(is.na(Percent_Nonwhite), purrr::map_dbl(find_xj(Percent_Nonwhite, nb), mean, na.rm = TRUE), Percent_Nonwhite),
    Mean_Commute_Time = ifelse(is.na(Mean_Commute_Time), purrr::map_dbl(find_xj(Mean_Commute_Time, nb), mean, na.rm = TRUE), Mean_Commute_Time),
    Percent_Taking_Public_Trans = ifelse(is.na(Percent_Taking_Public_Trans), purrr::map_dbl(find_xj(Percent_Taking_Public_Trans, nb), mean, na.rm = TRUE), Percent_Taking_Public_Trans),
    Percent_Renters =  ifelse(is.na(Percent_Renters), purrr::map_dbl(find_xj(Percent_Renters, nb), mean, na.rm = TRUE), Percent_Renters),
    Rent_Burden = ifelse(is.na(Rent_Burden), purrr::map_dbl(find_xj(Rent_Burden, nb), mean, na.rm = TRUE), Rent_Burden),
    Ext_Rent_Burden = ifelse(is.na(Ext_Rent_Burden), purrr::map_dbl(find_xj(Ext_Rent_Burden, nb), mean, na.rm = TRUE), Ext_Rent_Burden)) %>%
  select(-c(nb, wt))

# acs 19-------------------------------------------------
phl_acs19 <- get_acs(geography = "block group", 
                   variables = c("B01003_001", #totalpop
                                 "B19013_001", #medinc
                                 "B02001_002", #white_pop
                                 "B08013_001", #travel_time
                                 "B08012_001", #num_commuters
                                 "B08301_001", #means_of_trans
                                 "B08301_010", #total_public_trans
                                 "B11012_001", #totalhh
                                 "B25003_003", #renter_occupied
                                 "B25064_001", #medgross_rent
                                 "B25070_002", #less_10p
                                 "B25070_003", #10to15
                                 "B25070_004", #15to20
                                 "B25070_005", #20to25
                                 "B25070_006", #25to30
                                 "B25070_007", #30to35
                                 "B25070_008", #35to40
                                 "B25070_009", #40to50
                                 "B25070_010" #50+
                   ), 
                   year = 2019, 
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
         Total_HH = B11012_001E,
         Renters = B25003_003E,
         Med_Gross_Rent = B25064_001E,
         Less_10 = B25070_002E,
         Less_15 = B25070_003E,
         Less_20 = B25070_004E,
         Less_25 = B25070_005E, 
         Less_30 = B25070_006E,
         Less_35 = B25070_007E,
         Less_40 = B25070_008E, 
         Less_50 = B25070_009E, 
         More_50 = B25070_010E) %>%
  select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
         Means_of_Transport, Total_Public_Trans, Total_HH, Renters, Med_Gross_Rent, Less_10, Less_15, Less_20,
         Less_25, Less_30, Less_35, Less_40, Less_50, More_50,
         GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Percent_Nonwhite = 1-Percent_White, 
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport,
         Percent_Renters = Renters / Total_HH,
         Rent_Burden = (Less_30 + Less_35) / Renters,
         Ext_Rent_Burden = (Less_40 + Less_50 + More_50) / Renters,
         No_Rent_Burden = (Less_10 + Less_15 + Less_20 + Less_25) / Renters) %>%
  mutate(
    nb = st_knn(geometry, 5), # need to impute census values for tracts with pop ~ 0
    wt = st_weights(nb),
    Med_Inc = ifelse(is.na(Med_Inc), purrr::map_dbl(find_xj(Med_Inc, nb), mean, na.rm = TRUE), Med_Inc),
    Travel_Time = ifelse(is.na(Travel_Time), purrr::map_dbl(find_xj(Travel_Time, nb), mean, na.rm = TRUE), Travel_Time),
    Percent_White = ifelse(is.na(Percent_White), purrr::map_dbl(find_xj(Percent_White, nb), mean, na.rm = TRUE), Percent_White),
    Percent_Nonwhite = ifelse(is.na(Percent_Nonwhite), purrr::map_dbl(find_xj(Percent_Nonwhite, nb), mean, na.rm = TRUE), Percent_Nonwhite),
    Mean_Commute_Time = ifelse(is.na(Mean_Commute_Time), purrr::map_dbl(find_xj(Mean_Commute_Time, nb), mean, na.rm = TRUE), Mean_Commute_Time),
    Percent_Taking_Public_Trans = ifelse(is.na(Percent_Taking_Public_Trans), purrr::map_dbl(find_xj(Percent_Taking_Public_Trans, nb), mean, na.rm = TRUE), Percent_Taking_Public_Trans),
    Percent_Renters =  ifelse(is.na(Percent_Renters), purrr::map_dbl(find_xj(Percent_Renters, nb), mean, na.rm = TRUE), Percent_Renters),
    Rent_Burden = ifelse(is.na(Rent_Burden), purrr::map_dbl(find_xj(Rent_Burden, nb), mean, na.rm = TRUE), Rent_Burden),
    Ext_Rent_Burden = ifelse(is.na(Ext_Rent_Burden), purrr::map_dbl(find_xj(Ext_Rent_Burden, nb), mean, na.rm = TRUE), Ext_Rent_Burden)) %>%
  select(-c(nb, wt))


### acs 22----------------------------------------------------
phl_acs22 <- get_acs(geography = "block group", 
                     variables = c("B01003_001", #totalpop
                                   "B19013_001", #medinc
                                   "B02001_002", #white_pop
                                   "B08013_001", #travel_time
                                   "B08012_001", #num_commuters
                                   "B08301_001", #means_of_trans
                                   "B08301_010", #total_public_trans
                                   "B11012_001", #totalhh
                                   "B25003_003", #renter_occupied
                                   "B25064_001", #medgross_rent
                                   "B25070_002", #less_10p
                                   "B25070_003", #10to15
                                   "B25070_004", #15to20
                                   "B25070_005", #20to25
                                   "B25070_006", #25to30
                                   "B25070_007", #30to35
                                   "B25070_008", #35to40
                                   "B25070_009", #40to50
                                   "B25070_010" #50+
                     ), 
                     year = 2022, 
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
         Total_HH = B11012_001E,
         Renters = B25003_003E,
         Med_Gross_Rent = B25064_001E,
         Less_10 = B25070_002E,
         Less_15 = B25070_003E,
         Less_20 = B25070_004E,
         Less_25 = B25070_005E, 
         Less_30 = B25070_006E,
         Less_35 = B25070_007E,
         Less_40 = B25070_008E, 
         Less_50 = B25070_009E, 
         More_50 = B25070_010E) %>%
  select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
         Means_of_Transport, Total_Public_Trans, Total_HH, Renters, Med_Gross_Rent, Less_10, Less_15, Less_20,
         Less_25, Less_30, Less_35, Less_40, Less_50, More_50,
         GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Percent_Nonwhite = 1-Percent_White, 
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport,
         Percent_Renters = Renters / Total_HH,
         Rent_Burden = (Less_30 + Less_35) / Renters,
         Ext_Rent_Burden = (Less_40 + Less_50 + More_50) / Renters,
         No_Rent_Burden = (Less_10 + Less_15 + Less_20 + Less_25) / Renters) %>%
  mutate(
    nb = st_knn(geometry, 5), # need to impute census values for tracts with pop ~ 0
    wt = st_weights(nb),
    Med_Inc = ifelse(is.na(Med_Inc), purrr::map_dbl(find_xj(Med_Inc, nb), mean, na.rm = TRUE), Med_Inc),
    Travel_Time = ifelse(is.na(Travel_Time), purrr::map_dbl(find_xj(Travel_Time, nb), mean, na.rm = TRUE), Travel_Time),
    Percent_White = ifelse(is.na(Percent_White), purrr::map_dbl(find_xj(Percent_White, nb), mean, na.rm = TRUE), Percent_White),
    Percent_Nonwhite = ifelse(is.na(Percent_Nonwhite), purrr::map_dbl(find_xj(Percent_Nonwhite, nb), mean, na.rm = TRUE), Percent_Nonwhite),
    Mean_Commute_Time = ifelse(is.na(Mean_Commute_Time), purrr::map_dbl(find_xj(Mean_Commute_Time, nb), mean, na.rm = TRUE), Mean_Commute_Time),
    Percent_Taking_Public_Trans = ifelse(is.na(Percent_Taking_Public_Trans), purrr::map_dbl(find_xj(Percent_Taking_Public_Trans, nb), mean, na.rm = TRUE), Percent_Taking_Public_Trans),
    Percent_Renters =  ifelse(is.na(Percent_Renters), purrr::map_dbl(find_xj(Percent_Renters, nb), mean, na.rm = TRUE), Percent_Renters),
    Rent_Burden = ifelse(is.na(Rent_Burden), purrr::map_dbl(find_xj(Rent_Burden, nb), mean, na.rm = TRUE), Rent_Burden),
    Ext_Rent_Burden = ifelse(is.na(Ext_Rent_Burden), purrr::map_dbl(find_xj(Ext_Rent_Burden, nb), mean, na.rm = TRUE), Ext_Rent_Burden)) %>%
  select(-c(nb, wt))


### need to spatially interpolate to match up geometries
phila_blocks <- tigris::blocks(
  "PA",
  "Philadelphia",
  year = 2010
)

acs_22_to_2010_bgs_int <- interpolate_pw(
  from = phl_acs22,
  to = phl_acs19,
  to_id = "GEOID",
  weights = phila_blocks,
  crs = crs,
  extensive = FALSE
) %>% select(c(
  Med_Inc,
  Med_Gross_Rent,
  Percent_White,
  Percent_Nonwhite,
  Mean_Commute_Time,
  Percent_Taking_Public_Trans,
  Percent_Renters,
  Rent_Burden,
  Ext_Rent_Burden,
  No_Rent_Burden,
  GEOID
))

acs_22_to_2010_bgs_ext <- interpolate_pw(
  from = phl_acs22,
  to = phl_acs19,
  to_id = "GEOID",
  weights = phila_blocks,
  crs = crs,
  extensive = TRUE
) %>% select(c(
  Total_Pop,
  GEOID
)) %>% 
  st_drop_geometry() 

phl_acs22_final <- left_join(acs_22_to_2010_bgs_int, acs_22_to_2010_bgs_ext, by = "GEOID") %>% st_as_sf()


## check for NAs in % nonwhite, then impute


### write----------------------------------------------------------------------------
st_write(phl_acs14, "../data/acs_vars14.geojson") # paths already defined in config file
st_write(phl_acs19, "../data/acs_vars19.geojson")
st_write(phl_acs22_final, "../data/acs_vars22.geojson")
