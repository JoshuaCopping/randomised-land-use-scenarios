# 1c 10KM GRID CELLS ######################
source("./code/0_setup.R")


# LOAD & TIDY DATA --------------------------------------------------------------------
# Load country lcm rasters
sr_lcm_eng <- terra::rast(here("data", "rasters", "r_lcm_new_eng.tif")) # Load lcm raster
sr_lcm_nir <- terra::rast(here("data", "rasters", "r_lcm_new_nir.tif")) # Load lcm raster
sr_lcm_sct <- terra::rast(here("data", "rasters", "r_lcm_new_sct.tif")) # Load lcm raster
sr_lcm_wal <- terra::rast(here("data", "rasters", "r_lcm_new_wal.tif")) # Load lcm raster


# 10km atlas squares
# GBR
sf_10km_gbr <- st_read(here("data", "os_bng_grids.gpkg", layer = "10km_grid")) %>% 
  mutate(ID = row_number())

# NIR
sf_10km_nir <- geojson_sf(here("data", "OSNI_Open_Data_-_Coverage_Grid_-_10K.geojson")) %>% 
  st_transform(st_crs(sr_lcm_nir)) %>% 
  select(NAME) %>% 
  mutate(ID = row_number() + nrow(sf_10km_gbr))


# CREATE LOOKUP --------------------------------------------------------------------

lookup_10km_grid <- as_tibble(sf_10km_gbr) %>% 
  select(-geom) %>% 
  mutate(grid_name = "os_bng_grids") %>% 
  rbind(as_tibble(sf_10km_nir) %>% 
          select(-geometry) %>% 
          rename(tile_name = NAME) %>% 
          mutate(grid_name = "OSNI_Open_Data_-_Coverage_Grid_-_10K"))

save(lookup_10km_grid, file = here("data", "raster_lookup_10km_grid.RData"))


# RASTERISE GRIDS --------------------------------------------------------------------
# Convert grid to terra SpatVector
sv_10km_gbr <- terra::vect(sf_10km_gbr) 
sv_10km_nir <- terra::vect(sf_10km_nir)

# Rasterise per country
sr_10km_eng <- terra::rasterize(sv_10km_gbr, sr_lcm_eng, "ID")
sr_10km_nir <- terra::rasterize(sv_10km_nir, sr_lcm_nir, "ID") # uses separate NIR grid
sr_10km_sct <- terra::rasterize(sv_10km_gbr, sr_lcm_sct, "ID")
sr_10km_wal <- terra::rasterize(sv_10km_gbr, sr_lcm_wal, "ID")

# Crop
sr_10km_eng <- terra::mask(sr_10km_eng, sr_lcm_eng)
sr_10km_nir <- terra::mask(sr_10km_nir, sr_lcm_nir)
sr_10km_sct <- terra::mask(sr_10km_sct, sr_lcm_sct)
sr_10km_wal <- terra::mask(sr_10km_wal, sr_lcm_wal)

# Convert to raster RasterLayer
r_10km_eng <- raster(sr_10km_eng)
r_10km_nir <- raster(sr_10km_nir)
r_10km_sct <- raster(sr_10km_sct)
r_10km_wal <- raster(sr_10km_wal)

# Save rasters
writeRaster(r_10km_eng, here("data", "rasters", "r_10km_eng.tif"), overwrite = TRUE)
writeRaster(r_10km_nir, here("data", "rasters", "r_10km_nir.tif"), overwrite = TRUE)
writeRaster(r_10km_sct, here("data", "rasters", "r_10km_sct.tif"), overwrite = TRUE)
writeRaster(r_10km_wal, here("data", "rasters", "r_10km_wal.tif"), overwrite = TRUE)

# Remove files 
rm(list = ls()); gc()

