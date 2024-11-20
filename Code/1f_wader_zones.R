# 1f WADER ZONES ######################
source("./code/0_setup.R")


# BTO WADER ZONAL MAP (GB only) -------------------------
# Read shapefile
sf_wader_zones <- st_read(here("data", "Wader_Zonal_Map.shp"))

# Calculate highest strata per square across 8 species
sf_wader_zones <- sf_wader_zones %>% 
  select(cu:gk) %>% 
  rename("l." = l) #%>% mutate(max_zone = apply(X = st_drop_geometry(.), MARGIN = 1, FUN = max))

# Initialise 100-m raster
wader_rast <- rast(vect(sf_wader_zones), resolution = 500)

# Rasterize each species
wader_rast$sn_strata <- rasterize(vect(sf_wader_zones), wader_rast, field = "sn")
wader_rast$cu_strata <- rasterize(vect(sf_wader_zones), wader_rast, field = "cu")
wader_rast$l._strata <- rasterize(vect(sf_wader_zones), wader_rast, field = "l.")
wader_rast$rk_strata <- rasterize(vect(sf_wader_zones), wader_rast, field = "rk")
wader_rast$oc_strata <- rasterize(vect(sf_wader_zones), wader_rast, field = "oc")


# BTO ATLAS DATA -----------------------------------
# SNipe, CUrlew, L.apwing, RedshanK and OysterCatcher
sn <- read_csv(here("data/waders/00192SNBA20072011_002_0.csv"))
cu <- read_csv(here("data/waders/00203CUBA20072011_002_0.csv"))
l. <- read_csv(here("data/waders/00171L_BA20072011_002_0.csv"))
rk <- read_csv(here("data/waders/00206RKBA20072011_002_0.csv"))
oc <- read_csv(here("data/waders/00152OCBA20072011_002_0.csv"))

# Combine 5 species
wader_atlas_data <- bind_rows(sn %>% mutate(spp = "sn"),
                              cu %>% mutate(spp = "cu"),
                              l. %>% mutate(spp = "l."),
                              rk %>% mutate(spp = "rk"),
                              oc %>% mutate(spp = "oc")) %>% 
  # Use _median_ prediction
  select(-predmean) %>% 
  rename(abund = predmedian) %>% 
  # Spread
  spread(spp, abund) %>% 
  # Replace NAs with 0
  mutate_all(~ifelse(is.na(.), 0, .))


# Tetrads
# GB - from G Drive
sf_tetrad_gb <- st_read(here("data", "Grids.gdb"), layer = "BNG_Tetrads_2km")
# NI - from G Drive
sf_tetrad_ni <- st_read(here("data", "Grids.gdb"), layer = "tblIre2km")

# Fix tetrad codes in NI
wader_atlas_data <- wader_atlas_data %>% 
  mutate(tetrad = ifelse(tetrad %in% sf_tetrad_gb$TETRADS, tetrad, substr(tetrad, 2, 5)))


# Join atlas data to shapefile 
sf_wader_atlas <- sf_tetrad_gb %>%
  select(TETRADS) %>%
  inner_join(select(wader_atlas_data, -easting, -northing), by = c("TETRADS" = "tetrad")) %>%
  select(-TETRADS)

sf_wader_atlas_ni <- sf_tetrad_ni %>%
  select(tetrad) %>%
  inner_join(select(wader_atlas_data, -easting, -northing), by = "tetrad") %>%
  select(-tetrad)


# Rasterize
wader_rast$sn_abund <- rasterize(vect(sf_wader_atlas), wader_rast, field = "sn")
wader_rast$cu_abund <- rasterize(vect(sf_wader_atlas), wader_rast, field = "cu")
wader_rast$l._abund <- rasterize(vect(sf_wader_atlas), wader_rast, field = "l.")
wader_rast$rk_abund <- rasterize(vect(sf_wader_atlas), wader_rast, field = "rk")
wader_rast$oc_abund <- rasterize(vect(sf_wader_atlas), wader_rast, field = "oc")

# Initiliaze NI raster
wader_rast_ni <- rast(vect(sf_wader_atlas_ni), resolution = 500)

# Rasterize NI
wader_rast_ni$sn_abund <- rasterize(vect(sf_wader_atlas_ni), wader_rast_ni, field = "sn")
wader_rast_ni$cu_abund <- rasterize(vect(sf_wader_atlas_ni), wader_rast_ni, field = "cu")
wader_rast_ni$l._abund <- rasterize(vect(sf_wader_atlas_ni), wader_rast_ni, field = "l.")
wader_rast_ni$rk_abund <- rasterize(vect(sf_wader_atlas_ni), wader_rast_ni, field = "rk")
wader_rast_ni$oc_abund <- rasterize(vect(sf_wader_atlas_ni), wader_rast_ni, field = "oc")


# PREDICT STRATA FOR NORTHERN IRELAND -----------------------------------
install_load("e1071") # for SVM classifier

# Fun - predict NI strata given atlas abundance. Based on classifier for GB
strata_predict_fun <- function(strata, abund, abund_new){
  
  set.seed(1)
  data <- as.data.frame(c(strata, abund)) %>% 
    filter(complete.cases(.)) %>% 
    as_tibble() %>%
    set_names(c("strata", "abund")) %>% 
    mutate(strata = as.factor(strata)) %>% 
    group_by(strata) %>% 
    sample_n(5000) %>% 
    ungroup()
  
  mod <- svm(strata ~ abund, data = data, kernel = "linear", cost = 1000, scale = TRUE)
  
  tab <- table(data$strata, predict(mod))
  print(round((sum(diag(tab))/sum(tab))*100,2))
  # colSums(tab) / rowSums(tab)
  
  fit <- abund_new[]
  fit[!is.na(fit)] <- predict(mod, tibble(abund = abund_new[]))
  
  abund_new$fit <- fit
  
  return(abund_new$fit)
  
}


# Run 
wader_rast_ni$sn_strata <- strata_predict_fun(wader_rast$sn_strata, wader_rast$sn_abund, wader_rast_ni$sn_abund) # Accuracy = 53.4%
wader_rast_ni$cu_strata <- strata_predict_fun(wader_rast$cu_strata, wader_rast$cu_abund, wader_rast_ni$cu_abund) # Accuracy = 58.0%
wader_rast_ni$l._strata <- strata_predict_fun(wader_rast$l._strata, wader_rast$l._abund, wader_rast_ni$l._abund) # Accuracy = 53.4%
wader_rast_ni$rk_strata <- strata_predict_fun(wader_rast$rk_strata, wader_rast$rk_abund, wader_rast_ni$rk_abund) # Accuracy = 44.8%
wader_rast_ni$oc_strata <- strata_predict_fun(wader_rast$oc_strata, wader_rast$oc_abund, wader_rast_ni$oc_abund) # Accuracy = 53.1%


# RESAMPLE TO 25m GRID -----------------------------------
# Load lcm
sr_lcm_nir <- rast(here("data", "rasters", "r_lcm_new_nir.tif"))
sr_lcm_eng <- rast(here("data", "rasters", "r_lcm_new_eng.tif"))
sr_lcm_sct <- rast(here("data", "rasters", "r_lcm_new_sct.tif"))
sr_lcm_wal <- rast(here("data", "rasters", "r_lcm_new_wal.tif"))


# # Resample and mask to lcm data
# # NIR
# sr_wader_zones_nir <- rast(resolution = 500)
# sr_wader_zones_nir$sn <- mask(resample(wader_rast_ni$sn_strata, sr_lcm_nir, method = "near"), sr_lcm_nir)
# sr_wader_zones_nir$cu <- mask(resample(wader_rast_ni$cu_strata, sr_lcm_nir, method = "near"), sr_lcm_nir)
# sr_wader_zones_nir$l. <- mask(resample(wader_rast_ni$l._strata, sr_lcm_nir, method = "near"), sr_lcm_nir)
# sr_wader_zones_nir$rk <- mask(resample(wader_rast_ni$rk_strata, sr_lcm_nir, method = "near"), sr_lcm_nir)
# sr_wader_zones_nir$oc <- mask(resample(wader_rast_ni$oc_strata, sr_lcm_nir, method = "near"), sr_lcm_nir)
# 
# # WAL
# sr_wader_zones_wal <- rast(resolution = 500)
# sr_wader_zones_wal$sn <- mask(resample(wader_rast$sn_strata, sr_lcm_wal, method = "near"), sr_lcm_wal)
# sr_wader_zones_wal$cu <- mask(resample(wader_rast$cu_strata, sr_lcm_wal, method = "near"), sr_lcm_wal)
# sr_wader_zones_wal$l. <- mask(resample(wader_rast$l._strata, sr_lcm_wal, method = "near"), sr_lcm_wal)
# sr_wader_zones_wal$rk <- mask(resample(wader_rast$rk_strata, sr_lcm_wal, method = "near"), sr_lcm_wal)
# sr_wader_zones_wal$oc <- mask(resample(wader_rast$oc_strata, sr_lcm_wal, method = "near"), sr_lcm_wal)
# 
# # SCT
# sr_wader_zones_sct <- rast(resolution = 500)
# sr_wader_zones_sct$sn <- mask(resample(wader_rast$sn_strata, sr_lcm_sct, method = "near"), sr_lcm_sct)
# sr_wader_zones_sct$cu <- mask(resample(wader_rast$cu_strata, sr_lcm_sct, method = "near"), sr_lcm_sct)
# sr_wader_zones_sct$l. <- mask(resample(wader_rast$l._strata, sr_lcm_sct, method = "near"), sr_lcm_sct)
# sr_wader_zones_sct$rk <- mask(resample(wader_rast$rk_strata, sr_lcm_sct, method = "near"), sr_lcm_sct)
# sr_wader_zones_sct$oc <- mask(resample(wader_rast$oc_strata, sr_lcm_sct, method = "near"), sr_lcm_sct)
# 
# # ENG
# sr_wader_zones_eng <- rast(resolution = 500)
# sr_wader_zones_eng$sn <- mask(resample(wader_rast$sn_strata, sr_lcm_eng, method = "near"), sr_lcm_eng)
# sr_wader_zones_eng$cu <- mask(resample(wader_rast$cu_strata, sr_lcm_eng, method = "near"), sr_lcm_eng)
# sr_wader_zones_eng$l. <- mask(resample(wader_rast$l._strata, sr_lcm_eng, method = "near"), sr_lcm_eng)
# sr_wader_zones_eng$rk <- mask(resample(wader_rast$rk_strata, sr_lcm_eng, method = "near"), sr_lcm_eng)
# sr_wader_zones_eng$oc <- mask(resample(wader_rast$oc_strata, sr_lcm_eng, method = "near"), sr_lcm_eng)


# Resample without masking
# NIR
sr_wader_zones_nir <- rast(resolution = 500)
sr_wader_zones_nir$sn <- resample(wader_rast_ni$sn_strata, sr_lcm_nir, method = "near")
sr_wader_zones_nir$cu <- resample(wader_rast_ni$cu_strata, sr_lcm_nir, method = "near")
sr_wader_zones_nir$l. <- resample(wader_rast_ni$l._strata, sr_lcm_nir, method = "near")
sr_wader_zones_nir$rk <- resample(wader_rast_ni$rk_strata, sr_lcm_nir, method = "near")
sr_wader_zones_nir$oc <- resample(wader_rast_ni$oc_strata, sr_lcm_nir, method = "near")

# WAL
sr_wader_zones_wal <- rast(resolution = 500)
sr_wader_zones_wal$sn <- resample(wader_rast$sn_strata, sr_lcm_wal, method = "near")
sr_wader_zones_wal$cu <- resample(wader_rast$cu_strata, sr_lcm_wal, method = "near")
sr_wader_zones_wal$l. <- resample(wader_rast$l._strata, sr_lcm_wal, method = "near")
sr_wader_zones_wal$rk <- resample(wader_rast$rk_strata, sr_lcm_wal, method = "near")
sr_wader_zones_wal$oc <- resample(wader_rast$oc_strata, sr_lcm_wal, method = "near")

# SCT
sr_wader_zones_sct <- rast(resolution = 500)
sr_wader_zones_sct$sn <- resample(wader_rast$sn_strata, sr_lcm_sct, method = "near")
sr_wader_zones_sct$cu <- resample(wader_rast$cu_strata, sr_lcm_sct, method = "near")
sr_wader_zones_sct$l. <- resample(wader_rast$l._strata, sr_lcm_sct, method = "near")
sr_wader_zones_sct$rk <- resample(wader_rast$rk_strata, sr_lcm_sct, method = "near")
sr_wader_zones_sct$oc <- resample(wader_rast$oc_strata, sr_lcm_sct, method = "near")

# ENG
sr_wader_zones_eng <- rast(resolution = 500)
sr_wader_zones_eng$sn <- resample(wader_rast$sn_strata, sr_lcm_eng, method = "near")
sr_wader_zones_eng$cu <- resample(wader_rast$cu_strata, sr_lcm_eng, method = "near")
sr_wader_zones_eng$l. <- resample(wader_rast$l._strata, sr_lcm_eng, method = "near")
sr_wader_zones_eng$rk <- resample(wader_rast$rk_strata, sr_lcm_eng, method = "near")
sr_wader_zones_eng$oc <- resample(wader_rast$oc_strata, sr_lcm_eng, method = "near")


# SAVE -----------------------------------
# Original - strata and abundance
writeRaster(wader_rast, here("data", "rasters", "wader_rast_gb.tif"), overwrite = TRUE)
writeRaster(wader_rast_ni, here("data", "rasters", "wader_rast_ni.tif"), overwrite = TRUE)

# Resampled country zone rasters
writeRaster(sr_wader_zones_eng, here("data", "rasters", "r_wader_zones_eng.tif"), overwrite = TRUE)
writeRaster(sr_wader_zones_nir, here("data", "rasters", "r_wader_zones_nir.tif"), overwrite = TRUE)
writeRaster(sr_wader_zones_sct, here("data", "rasters", "r_wader_zones_sct.tif"), overwrite = TRUE)
writeRaster(sr_wader_zones_wal, here("data", "rasters", "r_wader_zones_wal.tif"), overwrite = TRUE)

