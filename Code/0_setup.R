# 0 SETUP ######################

# LOAD PACKAGES ---------------------------------------------
# install.packages("install.load")
library(install.load)

install_load(
  # essentials --
  "tidyverse",
  "magrittr",
  "here",
  "glue",
  "janitor",

  # Spatial --
  "terra",
  "raster",
  "rasterDT",
  "sp",
  "sf",
  "geojsonsf",
  "rgdal",
  "maptools",
  "mapproj",
  "geosphere",
  "rgeos",
  "spdplyr",
  "fasterize",
  "SearchTrees",
  "SpaDES",
  "exactextractr",  
  
  # Plotting --
  "cowplot",
  "patchwork",
  "paletteer",
  "gt",
  "ggtext",
  "ggnewscale",
  "shadowtext",
  "RColorBrewer",
  "viridis",
  "mapview",
  "ggspatial",

  # Stats, etc. --
  "lme4",
  "MuMIn",
  "MASS",
  
  # Other --
  "beepr",
  "mgcv"

  
)

# DEFINE CUSTOM FUNCTIONS ---------------------------------------------
# To avoid clashes
select <- dplyr::select
set_names <- magrittr::set_names

# To make rasterDT::subsDT work
isFALSE <- function(x){!x}

# DEFINE CONSTANTS ---------------------------------------------
# For consistency in subsequent analysis
wind_footprint <- 0.02
solar_footprint <- 0.05

# Set BGN projection
bng <- CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs') 

