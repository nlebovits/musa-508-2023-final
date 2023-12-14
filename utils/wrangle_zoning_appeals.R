required_packages <- c("tidyverse", "sf", "rphl")
install_and_load_packages(required_packages)

base_url <- "https://phl.carto.com/api/v2/sql"

one_year_ago <- (lubridate::ymd(Sys.Date()) - lubridate::years(1))

query <- sprintf("
        select scheduleddate,  address, appealnumber, appealtype, applicationtype, appealgrounds, 
                  ST_Y(the_geom) AS lat, ST_X(the_geom) AS lng
        from appeals
        where scheduleddate  >= '%s'
        ", one_year_ago)

appeals <- st_as_sf(get_carto(query,
                                       format = 'csv',
                                       base_url = base_url,
                                       stringsAsFactors = FALSE) |>
                               dplyr::filter(
                                 !is.na(lat),
                                 !is.na(lng)),
                             coords = c("lng", "lat"),
                             crs = st_crs('EPSG:4326')) |>
  st_transform(crs = st_crs("EPSG:2272")) %>%
  mutate(appeals_count = 1)