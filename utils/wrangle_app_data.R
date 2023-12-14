required_packages <- c("tidyverse", "sf", "sfdep","conflicted","transformr", "janitor",
                       'igraph')
install_and_load_packages(required_packages)

select <- dplyr::select
filter <- dplyr::filter


# load data---------------------------------------------------
urls <- c(
  zoning = "https://opendata.arcgis.com/datasets/0bdb0b5f13774c03abf8dc2f1aa01693_0.geojson",
  rf_val_preds = './data/model_outputs/rf_val_preds.geojson'  # Updated file path
)

suppressMessages({
  invisible(
    imap(urls, ~ assign(.y, phl_spat_read(.x), envir = .GlobalEnv))
  )
})


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
  tidyr::unnest(nbs) %>% 
  filter(nbs != 0)

# Create a graph with a node for each row in filtered_zoning
g <- make_empty_graph(n = nrow(filtered_zoning))
V(g)$name <- as.character(1:nrow(filtered_zoning))

# Add edges if they exist
if (nrow(edge_list) > 0) {
  edges <- as.matrix(edge_list)
  g <- add_edges(g, c(t(edges)))
}

# Calculate the number of contiguous neighbors, handling nodes without neighbors
n_contiguous <- sapply(V(g)$name, function(node) {
  if (node %in% edges) {
    length(neighborhood(g, order = 1, nodes = as.numeric(node))[[1]])
  } else {
    1  # Nodes without neighbors count as 1 (themselves)
  }
})

filtered_zoning <- filtered_zoning %>%
  mutate(n_contig = n_contiguous)


# save data---------------------------------------------------
st_write(filtered_zoning, "./data/model_outputs/app_data.geojson")