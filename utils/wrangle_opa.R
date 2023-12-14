required_packages <- c("tidyverse", "sf", "rphl")
install_and_load_packages(required_packages)



base_url = "https://phl.carto.com/api/v2/sql"

query <- "
                  SELECT 
                  zoning,
                  the_geom,
                  ST_Y(the_geom) AS lat, ST_X(the_geom) AS lng
                  FROM opa_properties_public
                 "

opa_properties <- st_as_sf(get_carto(query,
                                       format = 'csv',
                                       base_url = base_url,
                                       stringsAsFactors = FALSE) |>
                               dplyr::filter(
                                 !is.na(lat),
                                 !is.na(lng)),
                             coords = c("lng", "lat"),
                             crs = st_crs('EPSG:4326')) |>
  st_transform(crs = st_crs("EPSG:2272"))

st_write(opa_properties, '../data/opa_properties.geojson')