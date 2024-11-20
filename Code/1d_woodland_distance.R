# 1b WOODLAND DISTANCES ######################
source("./code/0_setup.R")


# WOODLAND DISTANCES FUNCTION SETUP -------------------------

## For testing
# countries <- c("eng", "nir", "sct", "wal")
# i <- 2
# load(here("data", "raster_lookup_lcm.RData"))
# lookup_lcm

# Function setup
woodland_distance_function <- function(countries){
  
  # Loop over each country in countries list
  for(i in 1:length(countries)){
    
    # Select country
    country_abbr <- countries[[i]]
    print(paste("Doing", country_abbr))
    
    # Load rasters
    r_lcm <- raster(here("data", "rasters", glue("r_lcm_new_{country_abbr}.tif"))) # 2015 LCM
    r_woodop <- raster(here("data", "rasters", glue("r_woodop_{country_abbr}.tif")))
    
    # Data for loop and saving
    woodland_type <- tibble(
      value = c(1, 2),
      lcm = c("bw", "cw")
    )
    
    # Loop for BW and CW
    for (j in 1:length(woodland_type)) {
     
       # Select woodop and woodland
      r_lcm_woodland <- raster(r_lcm)
      r_lcm_woodland[r_lcm[] == woodland_type$value[[j]]] <- 1 # Assign 1 to woodland  
      r_lcm_woodland[r_woodop[] == 1] <- 99 # Assign woodop 99 for selecting in distance function
      
      # Convert to Terra - SpatRaster
      sr_lcm_woodland <- terra::rast(r_lcm_woodland)
      rm(r_lcm_woodland) # remove rasters to free up space
      gc()
      
      # Distance calculation - distance of 99/agricultural land to 1/built land
      sr_woodland_dist <- terra::distance(sr_lcm_woodland, target = 99, unit = "km")
      
      # Convert back to RasterLayer
      r_woodland_dist <- raster(sr_woodland_dist)
      
      # Save 
      writeRaster(r_woodland_dist, here("data", "rasters", glue("r_{woodland_type$lcm[[j]]}_dist_{country_abbr}.tif")), overwrite = TRUE)
      
      # Tidy 
      rm(sr_lcm_woodland, sr_woodland_dist, r_woodland_dist); gc()
      
      # Progress update
      print(paste("Finished", country_abbr, woodland_type$lcm[[j]], "-", Sys.time()))
      
    }
    
    # Tidy 
    rm(r_lcm, r_woodop)
    removeTmpFiles(h = 0)
    gc()
 
  }
  
  # Progress update 
  print("--- FINISHED ALL ---")
  
}


# RUN FUNCTION -------------------------

woodland_distance_function(c("eng", "nir", "sct", "wal")) # Only argument is country names - add/remove as needed

## Check function output
# r_dist_check <- raster(here("data", "rasters", "r_bw_dist_wal.tif"))
# r_dist_check
# plot(r_dist_check)

