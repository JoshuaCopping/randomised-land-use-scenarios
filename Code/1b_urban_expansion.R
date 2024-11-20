# 1b URBAN EXPANSION ######################
source("./code/0_setup.R")


# URBAN EXPANSION SUITABILITY AREAS FUNCTION SETUP -------------------------

## For testing
# countries <- c("eng", "nir", "sct", "wal")

# Function setup
urban_expansion_function <- function(countries){
  
  # Loop over each country in countries list
  for(i in 1:length(countries)){
  
    # Select country
    country_abbr <- countries[[i]]
    print(paste("Doing", country_abbr))
    
    # Load rasters
    r_lcm <- raster(here("data", "rasters", glue("r_lcm_new_{country_abbr}.tif"))) # 2015 LCM
    
    # Select built environment
    r_lcm_urban <- raster(r_lcm)
    r_lcm_urban[r_lcm[] >= 20] <- 1 # Assign built land 1
    r_lcm_urban[r_lcm[] %in% c(3, 4)] <- 99 # Assign agricultural land (a.h & i.g) 99 for selecting in distance function
    
    rm(r_lcm); gc() # remove rasters to free up space
 
    # Convert to Terra - SpatRaster
    sr_lcm_urban <- terra::rast(r_lcm_urban)
    rm(r_lcm_urban); gc() # remove rasters to free up space

    # Distance calculation - distance of 99/agricultural land to 1/built land
    sr_urban_dist <- terra::distance(sr_lcm_urban, target = 99, unit = "km")
    
    # Convert back to RasterLayer
    r_urban_dist <- raster(sr_urban_dist)
    
    # Save 
    writeRaster(r_urban_dist, here("data", "rasters", glue("r_urban_dist_{country_abbr}.tif")), overwrite = TRUE)
    
    # Tidy 
    rm(sr_lcm_urban, sr_urban_dist, r_urban_dist)
    removeTmpFiles(h = 0)
    gc()
    
    # Progress update
    print(paste("Finished", country_abbr))
    
  }
  
  # Progress update
  print("FINISHED ALL")
  
}


# RUN FUNCTION -------------------------

urban_expansion_function(c("eng", "nir", "sct", "wal")) # Only argument is country names - add/remove as needed

