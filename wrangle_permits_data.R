source("config.R")

required_packages <- c("tidyverse", "sf", "rphl")
install_and_load_packages(required_packages)



base_url = "https://phl.carto.com/api/v2/sql"

three_years_ago = (lubridate::ymd(Sys.Date()) - lubridate::years(11))
one_year_ago = (lubridate::ymd(Sys.Date()) - lubridate::years(1))


building_perms_query <- sprintf("
                  SELECT permitissuedate, typeofwork, ST_Y(the_geom) AS lat, ST_X(the_geom) AS lng
                  FROM permits
                  WHERE permitissuedate >= '%s' AND permitissuedate < '%s'
                 ", three_years_ago, one_year_ago)

building_permits <- st_as_sf(get_carto(building_perms_query,
                              format = 'csv',
                              base_url = base_url,
                              stringsAsFactors = FALSE) |>
                      dplyr::filter(
                             !is.na(lat),
                             !is.na(lng)),
                      coords = c("lng", "lat"),
                      crs = st_crs('EPSG:4326')) |>
                      st_transform(crs = st_crs("EPSG:2272")) %>%
                    mutate(permits_count = 1)


## need to write permits to the data subfolder
saveRDS(building_permits, building_permits_path) # path already defined in config file