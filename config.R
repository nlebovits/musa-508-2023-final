library(utils)
library(devtools)

### functions

# load packages and install missing ones
# this is useful so that people can run scripts from anywhere without needing to reinstall stuff
install_and_load_packages <- function(required_packages) {
  installed_packages <- installed.packages()[, "Package"]
  missing_packages <- setdiff(required_packages, installed_packages)
  
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    if ("boxrdrive" %in% missing_packages) {
      suppressWarnings({
        remotes::install_github("r-box/boxrdrive")
      })
      if (!require(boxrdrive, quietly = TRUE)) {
        warning("'boxrdrive' package is not available for this version of R. Please check the installation.")
      }
    }
    install.packages(setdiff(missing_packages, "boxrdrive"), quietly = TRUE)
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