# 2 GET SIMPLE AREAS ######################
source("./code/0_setup.R")


# LOAD SPATIAL LAYERS -----------------------------
# LCM - 2015
r_lcm_nir <- raster(here("data", "rasters", "r_lcm_new_nir.tif"))
r_lcm_eng <- raster(here("data", "rasters", "r_lcm_new_eng.tif"))
r_lcm_wal <- raster(here("data", "rasters", "r_lcm_new_wal.tif"))
r_lcm_sct <- raster(here("data", "rasters", "r_lcm_new_sct.tif"))
load(here("data", "raster_lookup_lcm.RData"))

# NUTS
r_nuts1_nir <- raster(here("data", "rasters", "r_nuts1_nir.tif"))
r_nuts1_eng <- raster(here("data", "rasters", "r_nuts1_eng.tif"))
r_nuts1_wal <- raster(here("data", "rasters", "r_nuts1_wal.tif"))
r_nuts1_sct <- raster(here("data", "rasters", "r_nuts1_sct.tif"))
load(here("data", "raster_lookup_nuts1.RData"))

# ALC
r_alc_nir <- raster(here("data", "rasters", "r_alc_nir.tif"))
r_alc_eng <- raster(here("data", "rasters", "r_alc_eng.tif"))
r_alc_wal <- raster(here("data", "rasters", "r_alc_wal.tif"))
r_alc_sct <- raster(here("data", "rasters", "r_alc_sct.tif"))
load(here("data", "raster_lookup_alc.RData"))

# Peat
r_peat_nir <- raster(here("data", "rasters", "r_peat_nir.tif"))
r_peat_eng <- raster(here("data", "rasters", "r_peat_eng.tif"))
r_peat_wal <- raster(here("data", "rasters", "r_peat_wal.tif"))
r_peat_sct <- raster(here("data", "rasters", "r_peat_sct.tif"))

# Woodland opportunity
r_woodop_eng <- raster(here("data", "rasters", "r_woodop_eng.tif"))
r_woodop_sct <- raster(here("data", "rasters", "r_woodop_sct.tif"))
r_woodop_wal <- raster(here("data", "rasters", "r_woodop_wal.tif"))
r_woodop_nir <- raster(here("data", "rasters", "r_woodop_nir.tif"))

# Soil
r_soil_eng <- raster(here("data", "rasters", "r_soiltype_eng.tif"))
r_soil_sct <- raster(here("data", "rasters", "r_soiltype_sct.tif"))
r_soil_wal <- raster(here("data", "rasters", "r_soiltype_wal.tif"))
r_soil_nir <- raster(here("data", "rasters", "r_soiltype_nir.tif"))

# Yield class
r_cyc_eng <- stack(here("data", "rasters", "r_cyc_eng.tif"))[[c(6:9)]]
r_cyc_sct <- stack(here("data", "rasters", "r_cyc_sct.tif"))[[c(6:9)]]
r_cyc_wal <- stack(here("data", "rasters", "r_cyc_wal.tif"))[[c(6:9)]]
r_cyc_nir <- stack(here("data", "rasters", "r_cyc_nir.tif"))[[c(6:9)]]

names(r_cyc_eng) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")
names(r_cyc_sct) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")
names(r_cyc_wal) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")
names(r_cyc_nir) <- c("yc_sbi", "yc_sok", "yc_sp", "yc_ss")

# Saltmarsh opportunity
r_saltmarsh_eng <- raster(here("data", "rasters", "r_saltmarsh_eng.tif"))
r_saltmarsh_sct <- raster(here("data", "rasters", "r_saltmarsh_sct.tif"))
r_saltmarsh_wal <- raster(here("data", "rasters", "r_saltmarsh_wal.tif"))
r_saltmarsh_nir <- raster(here("data", "rasters", "r_saltmarsh_nir.tif"))

# Solar opportunity
r_solarop_eng <- raster(here("data", "rasters", "r_solarop_eng.tif"))
r_solarop_nir <- raster(here("data", "rasters", "r_solarop_nir.tif"))
r_solarop_sct <- raster(here("data", "rasters", "r_solarop_sct.tif"))
r_solarop_wal <- raster(here("data", "rasters", "r_solarop_wal.tif"))

# Wind opportunity 
r_windop_eng <- raster(here("data", "rasters", "r_windop_eng.tif"))
r_windop_nir <- raster(here("data", "rasters", "r_windop_nir.tif"))
r_windop_sct <- raster(here("data", "rasters", "r_windop_sct.tif"))
r_windop_wal <- raster(here("data", "rasters", "r_windop_wal.tif"))
                       
# Urban expansion distances
r_urban_dist_eng <- raster(here("data", "rasters", "r_urban_dist_eng.tif"))
r_urban_dist_nir <- raster(here("data", "rasters", "r_urban_dist_nir.tif"))
r_urban_dist_sct <- raster(here("data", "rasters", "r_urban_dist_sct.tif"))
r_urban_dist_wal <- raster(here("data", "rasters", "r_urban_dist_wal.tif"))

# Wader zones
r_wader_zones_nir <- stack(here("data", "rasters", "r_wader_zones_nir.tif"))
r_wader_zones_eng <- stack(here("data", "rasters", "r_wader_zones_eng.tif"))
r_wader_zones_sct <- stack(here("data", "rasters", "r_wader_zones_sct.tif"))
r_wader_zones_wal <- stack(here("data", "rasters", "r_wader_zones_wal.tif"))


# BIN URBAN DISTANCE DATA -----------------------------
## URBAN ----
# England 
r_urban_bin_eng <- raster(r_urban_dist_eng)
r_urban_bin_eng[r_urban_dist_eng[] <=1] <- 1
r_urban_bin_eng[r_urban_dist_eng[] >1 & r_urban_dist_eng[] <=2] <- 2 
r_urban_bin_eng[r_urban_dist_eng[] >2] <- 3 

# Northern Ireland
r_urban_bin_nir <- raster(r_urban_dist_nir)
r_urban_bin_nir[r_urban_dist_nir[] <=1] <- 1
r_urban_bin_nir[r_urban_dist_nir[] >1 & r_urban_dist_nir[] <=2] <- 2 
r_urban_bin_nir[r_urban_dist_nir[] >2] <- 3 

# Scotland 
r_urban_bin_sct <- raster(r_urban_dist_sct)
r_urban_bin_sct[r_urban_dist_sct[] <=1] <- 1
r_urban_bin_sct[r_urban_dist_sct[] >1 & r_urban_dist_sct[] <=2] <- 2 
r_urban_bin_sct[r_urban_dist_sct[] >2] <- 3 

# Wales
r_urban_bin_wal <- raster(r_urban_dist_wal)
r_urban_bin_wal[r_urban_dist_wal[] <=1] <- 1
r_urban_bin_wal[r_urban_dist_wal[] >1 & r_urban_dist_wal[] <=2] <- 2 
r_urban_bin_wal[r_urban_dist_wal[] >2] <- 3 

rm(r_urban_dist_eng, r_urban_dist_nir, r_urban_dist_sct, r_urban_dist_wal); gc()


# CROSSTABULATE -----------------------------
areas <- bind_rows(crosstabDT(stack(r_lcm_eng, r_nuts1_eng, r_alc_eng, r_peat_eng, r_woodop_eng, r_soil_eng, r_cyc_eng, r_saltmarsh_eng, r_solarop_eng, r_windop_eng, r_urban_bin_eng, r_wader_zones_eng), long = TRUE, useNA = TRUE) %>% 
                     as_tibble() %>% 
                     set_names(c("lcm_layer", "nuts1_layer", "alc_layer", "peat", "woodop", "organomin", "yc_sbi", "yc_sok", "yc_sp", "yc_ss", "saltmarsh", "solarop", "windop", "urban_distance", "sn_zones", "cu_zones", "l._zones", "rk_zones", "oc_zones", "area")),
                   crosstabDT(stack(r_lcm_nir, r_nuts1_nir, r_alc_nir, r_peat_nir, r_woodop_nir, r_soil_nir, r_cyc_nir, r_saltmarsh_nir, r_solarop_nir, r_windop_nir, r_urban_bin_nir, r_wader_zones_nir), long = TRUE, useNA = TRUE) %>% 
                     as_tibble() %>% 
                     set_names(c("lcm_layer", "nuts1_layer", "alc_layer", "peat", "woodop", "organomin", "yc_sbi", "yc_sok", "yc_sp", "yc_ss", "saltmarsh", "solarop", "windop", "urban_distance", "sn_zones", "cu_zones", "l._zones", "rk_zones", "oc_zones", "area")),
                   crosstabDT(stack(r_lcm_sct, r_nuts1_sct, r_alc_sct, r_peat_sct, r_woodop_sct, r_soil_sct, r_cyc_sct, r_saltmarsh_sct, r_solarop_sct, r_windop_sct, r_urban_bin_sct, r_wader_zones_sct), long = TRUE, useNA = TRUE) %>% 
                     as_tibble() %>% 
                     set_names(c("lcm_layer", "nuts1_layer", "alc_layer", "peat", "woodop", "organomin", "yc_sbi", "yc_sok", "yc_sp", "yc_ss", "saltmarsh", "solarop", "windop", "urban_distance", "sn_zones", "cu_zones", "l._zones", "rk_zones", "oc_zones", "area")),
                   crosstabDT(stack(r_lcm_wal, r_nuts1_wal, r_alc_wal, r_peat_wal, r_woodop_wal, r_soil_wal, r_cyc_wal, r_saltmarsh_wal, r_solarop_wal, r_windop_wal, r_urban_bin_wal, r_wader_zones_wal), long = TRUE, useNA = TRUE) %>% 
                     as_tibble() %>% 
                     set_names(c("lcm_layer", "nuts1_layer", "alc_layer", "peat", "woodop", "organomin", "yc_sbi", "yc_sok", "yc_sp", "yc_ss", "saltmarsh", "solarop", "windop", "urban_distance", "sn_zones", "cu_zones", "l._zones", "rk_zones", "oc_zones", "area")))


save(areas, file = here("outputs", "areas.RData"))


# TIDY DATA -----------------------------
load(here("outputs", "areas.RData"))

# Tidy ALC
lookup_alc <-lookup_alc %>% 
  mutate(alc = case_when(alc %in% c("Grade 1", "1") ~ 1,
                         alc %in% c("Grade 2", "2") ~ 2,
                         alc %in% c("3A", "Grade 3", "Grade 3A", "3.1", "3a", "3") ~ 3,
                         alc %in% c("3B", "Grade 3B", "3.2", "3b") ~ 4,
                         alc %in% c("4A", "4B", "Grade 4", "4.1", "4.2", "4a", "4b", "4") ~ 5,
                         alc %in% c("Grade 5", "5.1", "5.2", "5.3", "5") ~ 6,
                         TRUE ~ 6)) 

# Tidy areas data
master_lcfu <- areas %>% 
  filter(!is.na(lcm_layer)) %>% 
  mutate(area = area * 0.0625,
         woodop = ifelse(is.na(woodop), 0, 1),
         organomin = ifelse(is.na(organomin), 0, organomin - 1),
         saltmarsh = ifelse(is.na(saltmarsh), 0, 1),
         solarop = ifelse(is.na(solarop), 0, 1),
         windop = ifelse(is.na(windop), 0, 1)) %>% 
  left_join(lookup_lcm, by = "lcm_layer") %>% 
  select(-lcm_layer) %>% 
  left_join(lookup_nuts1, by = "nuts1_layer") %>% 
  select(-nuts1_layer) %>% 
  mutate(country = ifelse(nuts1 %in% c("Northern Ireland", "Scotland", "Wales"), nuts1, "England"),
         nuts1 = ifelse(nuts1 %in% c("London", "South East (England)"), "South East & London (England)", nuts1)) %>% 
  left_join(lookup_alc, by = c("alc_layer", "country")) %>% 
  select(-alc_layer) %>% 
  group_by_at(vars(-area)) %>% 
  summarise(area = sum(area), 
            .groups = "drop") %>% 
  select(country, nuts1, lcm, alc, peat, woodop, organomin, contains("yc"), saltmarsh, solarop, windop, contains("distance"), contains("zones"), area)

# Save
save(master_lcfu, file = here("outputs", "master_lcfu.RData"))


