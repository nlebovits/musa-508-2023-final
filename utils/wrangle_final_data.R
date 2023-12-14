required_packages <- c("tidyverse", "sf", "sfdep","conflicted","transformr", "janitor",
                       'igraph')
install_and_load_packages(required_packages)

### load data--------------------------------------------------------------------
urls <- c(
  phl = "https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson",
  phl_bgs = "https://opendata.arcgis.com/datasets/2f982bada233478ea0100528227febce_0.geojson",
  nbhoods = "https://raw.githubusercontent.com/opendataphilly/open-geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson",
  roads = 'https://opendata.arcgis.com/datasets/261eeb49dfd44ccb8a4b6a0af830fdc8_0.geojson',
  historic_districts = "https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+historicdistricts_local&filename=historicdistricts_local&format=geojson&skipfields=cartodb_id",
  overlays = "https://opendata.arcgis.com/datasets/04fd29a8c022471994900cb0fd791bfc_0.geojson",
  council_dists = "https://opendata.arcgis.com/datasets/9298c2f3fa3241fbb176ff1e84d33360_0.geojson",
  building_permits = building_permits_path,
  acs14 = acs_vars14_path,
  acs19 = acs_vars19_path,
  acs22 = acs_vars22_path,
  trolley_stops = "data/Trolley_Stations.geojson",
  subway_stops = "data/Highspeed_Stations.geojson"
)

suppressMessages({
  invisible(
    imap(urls, ~ assign(.y, phl_spat_read(.x), envir = .GlobalEnv))
  )
})

phl_bgs <- phl_bgs %>% 
            select(GEOID10)

broad_and_market <- roads %>% filter(ST_NAME %in% c('BROAD',"MARKET") | SEG_ID %in% c(440370, 421347,421338,421337,422413,423051,440403,440402,440391,440380))

subway_stops <- subway_stops[phl, ]
transit_stops <- st_union(trolley_stops, subway_stops)

historic_districts <- historic_districts %>%
  mutate(hist_dist = name) %>%
  select(hist_dist)

nbhoods <- nbhoods %>%
  select(mapname)

council_dists <- council_dists %>%
  select(DISTRICT)

overlays <- overlays %>% clean_names()
overlays$overlay_symbol <- gsub("/", "", overlays$overlay_symbol) 
overlays <- overlays %>%
  mutate(overlay_symbol = ifelse(overlay_symbol == "[NA]", "Other", overlay_symbol))

building_permits <- building_permits %>%
  filter(permittype %in% c("RESIDENTIAL BUILDING", "BP_ADDITION", "BP_NEWCNST"))

acs_reg_vars <- c(
  "GEOID",
  "Total_Pop", 
  "Med_Inc",
  "Percent_Nonwhite",
  "Percent_Renters",
  "Rent_Burden",
  "Ext_Rent_Burden"
)

acs14_reg_vars <- phl_spat_read(acs_vars14_path) %>%
  st_drop_geometry() %>%
  select(acs_reg_vars) %>%
  clean_names() %>%
  rename(GEOID10 = geoid)

acs19_reg_vars <- phl_spat_read(acs_vars19_path) %>%
  st_drop_geometry() %>%
  select(acs_reg_vars) %>%
  clean_names() %>%
  rename(GEOID10 = geoid)

acs22_reg_vars <- phl_spat_read(acs_vars22_path) %>%
  st_drop_geometry() %>%
  select(acs_reg_vars) %>%
  clean_names() %>%
  rename(GEOID10 = geoid)


### wrangle data--------------------------------------------------
# Create a complete grid of GEOID10 and year
geoid_years <- expand.grid(GEOID10 = unique(phl_bgs$GEOID10),
                           year = c(unique(building_permits$year), 2024))



# Joining your existing data with the complete grid
permits_bg <- st_join(phl_bgs, building_permits) %>%
  group_by(GEOID10, year) %>%
  summarize(permits_count = sum(permits_count, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
  right_join(geoid_years, by = c("GEOID10", "year")) %>%
  replace_na(list(permits_count = 0)) %>%
  left_join(phl_bgs, by = "GEOID10") %>%
  st_as_sf() 


### spat + temp lags----------------------------------
suppressMessages(
  permits_bg <- permits_bg %>%
    group_by(year) %>%
    mutate(nb = st_knn(geometry, 5),
           wt = st_weights(nb),
           lag_spatial = st_lag(permits_count, nb, wt)) %>% # calculate spat lag
    ungroup()%>%
    arrange(GEOID10, year) %>% # calculate time lag
    mutate(
      lag_1_year = lag(permits_count, 1),
      lag_2_years = lag(permits_count, 2),
      lag_3_years = lag(permits_count, 3),
      lag_4_years = lag(permits_count, 4),
      lag_5_years = lag(permits_count, 5),
      lag_6_years = lag(permits_count, 6),
      lag_7_years = lag(permits_count, 7),
      lag_8_years = lag(permits_count, 8),
      lag_9_years = lag(permits_count, 9),
      lag_spat_1_year = lag(lag_spatial, 1),
      lag_spat_2_years = lag(lag_spatial, 2),
      lag_spat_3_years = lag(lag_spatial, 3),
      lag_spat_4_years = lag(lag_spatial, 4),
      lag_spat_5_years = lag(lag_spatial, 5),
      lag_spat_6_years = lag(lag_spatial, 6),
      lag_spat_7_years = lag(lag_spatial, 7),
      lag_spat_8_years = lag(lag_spatial, 8),
      lag_spat_9_years = lag(lag_spatial, 9),
      dist_to_2022 = 2022 - year
    ) %>%
    select(-lag_spatial)
)

### distance to transit---------------------------------
phl_bgs <- phl_bgs %>%
  select(GEOID10) %>%
  rowwise() %>%
  mutate(
    dist_to_transit = as.numeric(min(st_distance(st_point_on_surface(geometry), transit_stops$geometry)))
  ) %>%
  ungroup()

### historic dists---------------------------------
hist_dists_x_bg <- st_join(phl_bgs, historic_districts) %>%
  mutate(hist_dist = ifelse(is.na(hist_dist), 0, 1))

hist_dists_x_bg <- st_join(phl_bgs, historic_districts) %>%
  st_drop_geometry() %>%
  mutate(hist_dist_present = 1) %>%
  group_by(GEOID10, hist_dist) %>%
  summarize(hist_dist_present = max(hist_dist_present, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = "hist_dist", values_from = "hist_dist_present", 
              names_prefix = "hist_dist_", values_fill = list("hist_dist_present" = 0))

phl_bgs <- left_join(phl_bgs, hist_dists_x_bg, by = "GEOID10")

### hoods---------------------------------              
intersection <- st_intersection(phl_bgs, nbhoods)
intersection$intersection_area <- st_area(intersection)
max_intersection <- intersection %>%
  group_by(GEOID10) %>%
  filter(intersection_area == max(intersection_area)) %>%
  ungroup() %>%
  select(GEOID10, mapname) %>%
  st_drop_geometry()

bgs_w_hood <- left_join(phl_bgs, max_intersection, by = c("GEOID10" = "GEOID10")) %>%
  st_drop_geometry() %>%
  select(mapname, GEOID10)

phl_bgs <- left_join(phl_bgs, bgs_w_hood, by = "GEOID10")

### council_dists---------------------------------              
cd_inter <- st_intersection(phl_bgs, council_dists)
cd_inter$intersection_area <- st_area(cd_inter)
cd_max_inter <- cd_inter %>%
  group_by(GEOID10) %>%
  filter(intersection_area == max(intersection_area)) %>%
  ungroup() %>%
  select(GEOID10, DISTRICT) %>%
  st_drop_geometry()

bgs_w_cd <- left_join(phl_bgs, cd_max_inter, by = c("GEOID10" = "GEOID10")) %>%
  st_drop_geometry() %>%
  select(DISTRICT, GEOID10)

phl_bgs <- left_join(phl_bgs, bgs_w_cd, by = "GEOID10")

### overlays---------------------------------
overlays_x_bg <- st_join(phl_bgs, overlays %>% select(overlay_symbol)) %>%
  st_drop_geometry() %>%
  mutate(overlay_present = 1) %>%
  group_by(GEOID10, overlay_symbol) %>%
  summarize(overlay_present = max(overlay_present, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = "overlay_symbol", values_from = "overlay_present", 
              names_prefix = "overlay_", values_fill = list("overlay_present" = 0))

phl_bgs <- left_join(phl_bgs, overlays_x_bg, by = "GEOID10")


### join back together----------------------
permits_bg <- left_join(permits_bg,
                        st_drop_geometry(phl_bgs),
                        by = "GEOID10")

### acs vars--------------------------------------------

permits_thru_2018 <- filter(permits_bg, year < 2019)
permits_2019_thru_2021 <- filter(permits_bg, year >= 2019 & year < 2022)
permits_since_2021 <- filter(permits_bg, year >= 2022)

permits_joined_thru_2018 <- left_join(permits_thru_2018, acs14_reg_vars, by = "GEOID10")
permits_joined_2019_thru_2021 <- left_join(permits_2019_thru_2021, acs19_reg_vars, by = "GEOID10")
permits_joined_since_2021 <- left_join(permits_since_2021, acs22_reg_vars, by = "GEOID10")

permits_bg <- bind_rows(permits_joined_thru_2018, 
                        permits_joined_2019_thru_2021,
                        permits_joined_since_2021)

### clean-----------------------------------------------------
final <- permits_bg %>%
  select(-c(nb, wt)) %>%
  clean_names()

### write final dataset to data folder
st_write(final, "./data/final_dataset.geojson")