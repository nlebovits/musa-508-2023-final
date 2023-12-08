library(utils)
library(devtools)

### functions

# load packages and install missing ones----------------------------------------
# this is useful so that people can run scripts from anywhere without needing to reinstall stuff
install_and_load_packages <- function(required_packages) {
  installed_packages <- installed.packages()[, "Package"]
  missing_packages <- setdiff(required_packages, installed_packages)
  
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    
    # Check for 'boxrdrive' package
    if ("boxrdrive" %in% missing_packages) {
      suppressWarnings({
        remotes::install_github("r-box/boxrdrive")
      })
      if (!require(boxrdrive, quietly = TRUE)) {
        warning("'boxrdrive' package is not available for this version of R. Please check the installation.")
      }
    }
    
    # Check for 'rphl' package
    if ("rphl" %in% missing_packages) {
      suppressWarnings({
        remotes::install_github("CityOfPhiladelphia/rphl")
      })
      if (!require(rphl, quietly = TRUE)) {
        warning("'rphl' package is not available for this version of R. Please check the installation.")
      }
    }
    
    install.packages(setdiff(missing_packages, c("boxrdrive", "rphl")), quietly = TRUE)
  }
  
  for(package.i in required_packages){
    suppressPackageStartupMessages(
      library(
        package.i, 
        character.only = TRUE
      )
    )
  }
}

# read and transform spatial data quietly----------------------------------------
phl_spat_read <- function(path){
  st_read(path, quiet = TRUE) %>%
    st_transform(crs = crs)
}

# run to install monochomeR if not installed already
required_packages <- c("monochromeR")
install_and_load_packages(required_packages)

### global vars
select <- dplyr::select
filter <- dplyr::filter
lag <- dplyr::lag

options(scipen = 999, tigris_use_cache = TRUE, tigris_class = 'sf')

crs <- 'epsg:2272'

building_permits_path <- "data/building_permits.geojson"
acs_vars14_path <- "data/acs_vars14.geojson"
acs_vars19_path <- "data/acs_vars19.geojson"
acs_vars22_path <- "data/acs_vars22.geojson"
final_dataset_path <- "data/final_dataset.geojson"


palette <- c("#ffab40", "#ff6c40", "#1cb979", "#bdeeda", "#eae8ec")
mono_5_orange <- rev(generate_palette(palette[2], modification = "go_lighter", n_colours = 5, view_palette = FALSE))  
mono_5_green <- rev(generate_palette(palette[3], modification = "go_lighter", n_colours = 5, view_palette = FALSE)) 