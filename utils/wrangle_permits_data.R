required_packages <- c("tidyverse", "sf", "rphl")
install_and_load_packages(required_packages)



base_url = "https://phl.carto.com/api/v2/sql"

ten_years_ago = (lubridate::ymd(Sys.Date()) - lubridate::years(10))
one_year_ago = (lubridate::ymd(Sys.Date()) - lubridate::years(1))


building_perms_query <- sprintf("
                  SELECT 
                  address,
                  addressobjectid,
                  approvedscopeofwork,
                  commercialorresidential,
                  opa_account_num,
                  permittype,
                  status,
                  unit_num,
                  unit_type,
                  permitissuedate, 
                  typeofwork, 
                  ST_Y(the_geom) AS lat, ST_X(the_geom) AS lng
                  FROM permits
                  WHERE permitissuedate >= '%s'
                 ", ten_years_ago)

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
                    mutate(permits_count = 1,
                           year = year(as.Date(permitissuedate)))


## need to write permits to the data subfolder
st_write(building_permits, "../data/building_permits.geojson")
