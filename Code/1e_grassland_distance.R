# 1e GRASSLAND DISTANCE ######################
source("./code/0_setup.R")


# GRASSLAND DISTANCE FUNCTION SETUP -------------------------

## For testing
# countries <- c("eng", "nir", "sct", "wal")
# i <- 2
# load(here("data", "raster_lookup_lcm.RData"))
# lookup_lcm

# Function setup
grassland_distance_function <- function(countries){
  
  # Loop over each country in countries list
  for(i in 1:length(countries)){
   
     # Select country
    country_abbr <- countries[[i]]
    print(paste("Doing", country_abbr))
    
    # Load rasters
    r_lcm <- raster(here("data", "rasters", glue("r_lcm_new_{country_abbr}.tif"))) # 2015 LCM
    
    # Select semi-natural grassland
    r_lcm_grass <- raster(r_lcm)
    r_lcm_grass[r_lcm[] %in% c(5, 6, 7)] <- 1 # Assign semi-natural grassland 1
    r_lcm_grass[r_lcm[] %in% c(3, 4)] <- 99 # Assign agricultural land (a.h & i.g) 99 for selecting in distance function
    
    rm(r_lcm); gc() # remove rasters to free up space

    # Convert to Terra - SpatRaster
    sr_lcm_grass <- terra::rast(r_lcm_grass)
    rm(r_lcm_grass); gc() # remove rasters to free up space

    # Distance calculation - distance of 99/agricultural land to 1/built land
    sr_grass_dist <- terra::distance(sr_lcm_grass, target = 99, unit = "km")
    
    # Convert back to RasterLayer
    r_grass_dist <- raster(sr_grass_dist)
    
    # Save 
    writeRaster(r_grass_dist, here("data", "rasters", glue("r_grass_dist_{country_abbr}.tif")), overwrite = TRUE)
    
    # Tidy 
    rm(sr_lcm_grass, sr_grass_dist, r_grass_dist)
    removeTmpFiles(h = 0)
    gc()
    
    # Progress update
    print(paste("Finished", country_abbr))
    
  }
  
  # Progress update
  print("FINISHED ALL")
  
}


# RUN FUNCTION -------------------------

grassland_distance_function(c("nir", "wal")) # Only argument is country names - add/remove as needed

## Check function output
# r_dist_check <- raster(here("data", "rasters", "r_grass_dist_wal.tif"))
# r_dist_check
# plot(r_dist_check)




