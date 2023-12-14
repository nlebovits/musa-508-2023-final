required_packages <- c("tidyverse", "sf", "sfdep","conflicted","transformr", "janitor",
                       'igraph')
install_and_load_packages(required_packages)

select <- dplyr::select
filter <- dplyr::filter


# load data---------------------------------------------------
urls <- c(
  zoning = "https://opendata.arcgis.com/datasets/0bdb0b5f13774c03abf8dc2f1aa01693_0.geojson",
  rf_val_preds = './data/model_outputs/rf_val_preds.geojson',  # Updated file path
  council_dists = "https://opendata.arcgis.com/datasets/9298c2f3fa3241fbb176ff1e84d33360_0.geojson"
)

suppressMessages({
  invisible(
    imap(urls, ~ assign(.y, phl_spat_read(.x), envir = .GlobalEnv))
  )
})

council_dists <- council_dists %>%
  select(DISTRICT)

# data wrangle-----------------------------------------------
filtered_zoning <- zoning %>%
  filter(str_detect(CODE, "RS") | str_detect(CODE, "I"),
         CODE != "I2",
         !str_detect(CODE, "SP")) %>%
  st_join(., rf_val_preds %>% select(rf_val_preds))


nbs <- filtered_zoning %>% 
  mutate(nb = st_contiguity(geometry))

# Create edge list while handling cases with no neighbors
edge_list <- tibble::tibble(id = 1:length(nbs$nb), nbs = nbs$nb) %>% 
  unnest(nbs) %>% 
  filter(nbs != 0)

# Create a graph with a node for each row in filtered_zoning
g <- make_empty_graph(n = nrow(filtered_zoning))
V(g)$name <- as.character(1:nrow(filtered_zoning))

# Add edges if they exist
if (nrow(edge_list) > 0) {
  edges <- as.matrix(edge_list)
  g <- add_edges(g, c(t(edges)))
}

# Calculate the number of contiguous neighbors and sum of contiguous areas
n_contiguous <- numeric(nrow(filtered_zoning))
sum_contig_area <- numeric(nrow(filtered_zoning))

for (i in 1:nrow(filtered_zoning)) {
  neighbors <- neighborhood(g, order = 1, nodes = i)[[1]]
  # Exclude the node itself from its list of neighbors
  neighbors <- neighbors[neighbors != i]
  n_contiguous[i] <- length(neighbors)
  sum_contig_area[i] <- sum(filtered_zoning$Shape__Area[neighbors], na.rm = TRUE)
}

contig_info <- data.frame(n_contig = unlist(n_contiguous), sum_contig_area = unlist(sum_contig_area))
filtered_zoning <- cbind(filtered_zoning, contig_info)

# join council dists----------------------------------------------
filtered_zoning <- st_join(filtered_zoning, council_dists) %>% 
                      clean_names()

# save data---------------------------------------------------
st_write(filtered_zoning, "./shiny/app_data.geojson")