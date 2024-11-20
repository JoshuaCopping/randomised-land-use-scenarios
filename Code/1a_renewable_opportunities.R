# 1a UPDATE OPPORTUNITY LAYERS ######################
source("./code/0_setup.R")


# WIND & SOLAR OPPORTUNITIES FUNCTION SETUP -------------------------

## For testing
# countries <- c("eng", "nir", "sct", "wal")
# i <- 2

# Function setup
opportunities_function <- function(countries){
  
  # Loop over each country in countries vector
  for (i in 1:length(countries)) {
   
    # Select country
    country_abbr <- countries[[i]]
    print(paste("Doing", country_abbr))
    
    # Load rasters
    r_lcm <- raster(here("data", "rasters", glue("r_lcm_new_{country_abbr}.tif"))) # 2015 LCM
    r_wind <- raster(here("data", "rasters", glue("r_wind_{country_abbr}.tif"))) # Wind speed 
    r_solar <- raster(here("data", "rasters", glue("r_solar_{country_abbr}.tif"))) # Solar irradiance 
    r_slope <- raster(here("data", "rasters", glue("r_slope_{country_abbr}.tif"))) # Slope %
    r_aspect <- raster(here("data", "rasters", glue("r_aspect_{country_abbr}.tif"))) # Aspect in degrees 
    r_woodop_con <- raster(here("data", "rasters", glue("r_woodop_conifers_{country_abbr}.tif"))) # Updated woodop with conifers - buffers around infrastructure/built environment
    r_wind_sensitivity <- raster(here("data", "rasters", glue("r_wind_sensitivity_{country_abbr}.tif"))) # Sensitive species - wind
    r_solar_sensitivity <- raster(here("data", "rasters", glue("r_solar_sensitivity_{country_abbr}.tif"))) # Sensitive species - solar
    r_airports <- raster(here("data", "rasters", glue("r_airport_{country_abbr}.tif"))) # Buffered airports 
    r_geese <- raster(here("data", "rasters", glue("r_geese_{country_abbr}.tif"))) # Buffered geese/swan SPAs
    r_peat <- raster(here("data", "rasters", glue("r_peat_{country_abbr}.tif"))) # Peat areas
    r_pinewood_sct <- raster(here("data", "rasters", "r_pinewood_sct.tif"))    # Caledonian pinewood - Scotland only
    
    ### WIND OPPORTUNITY
    print("Creating wind opportunities layer")
    r_windop <- raster(r_lcm) # Create blank raster
    r_windop[r_woodop_con[] == 1] <- 1 # HABITAT/FEATURE SENSITIVITY - allow on woodop layer
    r_windop[r_lcm[] < 2 | r_lcm[] >= 5] <- NA # LAND COVER - exclude land cover types other than 2,3,4
    r_windop[r_wind[] < 5] <- NA # WIND SPEED - exclude wind speed less than 5 m/s
    r_windop[r_wind_sensitivity[] > 0] <- NA # SPECIES SENSITIVITY - exclude medium and high areas - wind sensitivity
    r_windop[r_geese[] == 1] <- NA # GEESE/SWANS - exclude buffered geese/swan SPAs
    r_windop[r_peat[] == 1] <- NA # PEAT - exclude peat areas - issue with coniferous woodland on peat in woodop - not needed for solar
    r_windop[r_slope[] > 18] <- NA # SLOPE - exclude slopes more than 18%
    r_windop[r_airports[] == 1] <- NA # AIRPORTS - exclude 5km buffered airport areas
    # Caledonian pinewood - Scotland only
    if(country_abbr == "sct"){
      r_windop[r_pinewood_sct[] == 1] <- NA # Caledonian pinewood
    }
   
    print("Saving windop raster")
    writeRaster(r_windop, here("data", "rasters", glue("r_windop_{country_abbr}.tif")), overwrite = TRUE)
    
    ### SOLAR OPPORTUNITY 
    print("Creating solar opportunities layer")
    r_solarop <- raster(r_lcm) # Create blank raster
    r_solarop[r_woodop_con[] == 1] <- 1 # HABITAT/FEATURE SENSITIVITY - allow on woodop layer - buffers around infrastructure/built environment
    r_solarop[r_lcm[] < 3 | r_lcm[] >= 5] <- NA # LAND COVER - exclude land cover types other than 3,4
    r_solarop[r_solar[] < 1075.2] <- NA # IRRADIANCE - exclude irradiance values under 1075.2 kWh/m2
    r_solarop[r_solar_sensitivity[] > 0] <- NA # SPECIES SENSITIVITY - exclude medium and high areas - solar sensitivity
    r_solarop[r_peat[] == 1] <- NA # PEAT - exclude peat areas - issue with coniferous woodland on peat in woodop - not needed for solar
    r_solarop[r_slope[] > 5] <- NA # SLOPE - exclude slopes >5%
    r_solarop[r_aspect[] < 112.5 | r_aspect[] > 247.5] <- NA # ASPECT - exclude aspects other than southeast, south and southwest
    
    print("Saving solarop raster")
    writeRaster(r_solarop, here("data", "rasters", glue("r_solarop_{country_abbr}.tif")), overwrite = TRUE)
    
    print("Removing temporary files")
    removeTmpFiles(h = 0)
    
    # Progress update
    print(paste("Finished", country_abbr))
  
  }
  
  # Progress update
  print("FINSIHED ALL")
  
}


# RUN FUNCTION -------------------------

opportunities_function(c("eng", "nir", "sct", "wal")) # Only argument is country names - add/remove as needed


