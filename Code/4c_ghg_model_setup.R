# 4c GHG SETUP ######################
source("./code/0_setup.R")


# UPDATE LUC ---------------------------------------------
# GWP conversion factors
gwp100 <- read_csv(here("data", "gwp100.csv"), skip = 1) %>% filter(notes == "AR4") %>% select(-notes)

## Soil ----
# Read soil stock change params
luc_soil <- read_csv(here("data", "ghg_luc_soil.csv"), skip = 1) %>% select(-source, -units) 

# Update luc_soil with forest - settlement
luc_soil_extra <- tibble(
  from = "Forestland",
  to = "Settlement",
  country = unique(luc_soil$country),
  soil_c = c(104, 236, 124, 244),
  n2o = unique(luc_soil$n2o),
  time = 100
)

# Bind luc_soil and new fores - settlement
luc_soil <- rbind(luc_soil, luc_soil_extra)

# LCM crosswalk
lcm_crosswalk <- tibble(from_to = c("Forestland", "Forestland", "Cropland", "Settlement", "Grassland", "Grassland", "Grassland", "Grassland", "Grassland", "Grassland", "Grassland", "Miscanthus", "SRC", "SRF"),
                        lcm = c("b.w", "c.w", "a.h", "b.t", "i.g", "n.g", "c.g", "a.g", "h.r", "b.g", "f.s", "a.h_energy_misc", "a.h_energy_src", "i.g_energy"))

# Calculate annual flux to/from soil following LUC
gwp_luc_soil <- luc_soil %>%
  # Convert C to CO2
  mutate(gwp_t = soil_c * 44/12,
         ghg = "CO2") %>% 
  select(-soil_c, -n2o) %>% 
  # Join with LCM crosswalk
  left_join(lcm_crosswalk, by = c("from" = "from_to")) %>% 
  rename(lcm_from = lcm) %>% 
  left_join(lcm_crosswalk, by = c("to" = "from_to")) %>% 
  rename(lcm_to = lcm) %>% 
  select(lcm_from, lcm_to, country, gwp_t, time, ghg) %>% 
  # Cross with all years
  crossing(year = 0:80) %>%
  # Calculate annual flux
  group_by(country, lcm_from, lcm_to) %>% 
  mutate(stock = gwp_t * (1 - exp(year / (time / log(0.01)))),
         gwp_t = stock - lag(stock),
         gwp_t = ifelse(is.na(gwp_t), 0, gwp_t)) %>%
  ungroup() %>% 
  select(-stock, -time)

# Add N2O (mineralised due to loss of SOC. = 0 if SOC gain)
gwp_luc_soil <- gwp_luc_soil %>%
  mutate(n2o = ifelse(gwp_t < 0, 0, gwp_t * unique(luc_soil$n2o)),
         ghg = "N2O") %>% 
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = n2o * gwp100) %>% 
  select(-n2o, -gwp100) %>% 
  bind_rows(gwp_luc_soil, .)


## Save
# load existing data to get biomass table 
load(here("data", "ghg_pars_luc.RData"))
gwp_luc_biomass <- ghg_pars_luc$gwp_luc_biomass

ghg_pars_luc <- list(gwp_luc_soil = gwp_luc_soil, 
                     gwp_luc_biomass = gwp_luc_biomass)

save(ghg_pars_luc, file = here("data", "ghg_pars_luc.RData"))


## Renewable energy updates ----
# LUC
load(here("data", "ghg_pars_luc.RData"))

gwp_luc_renewables_soil <- ghg_pars_luc$gwp_luc_soil %>% 
  filter(lcm_from %in% c("a.h", "c.w", "i.g") & lcm_to == "b.t" | lcm_from == "a.h" & lcm_to == "i.g") %>% 
  mutate(lcm_to = case_when(lcm_from == "a.h" & lcm_to == "i.g" ~ "solar",
                            lcm_from %in% c("a.h", "c.w", "i.g") & lcm_to == "b.t" ~ "wind"),
         gwp_t = case_when(lcm_to == "wind" ~ gwp_t*wind_footprint,
                           lcm_to == "solar" ~ gwp_t*(1-solar_footprint)))

# gwp_luc_renewables_soil %>% filter(lcm_to == "solar")
# unique(gwp_luc_renewables_soil$lcm_from)

ghg_pars_luc[["gwp_luc_renewables_soil"]] <- gwp_luc_renewables_soil

save(ghg_pars_luc, file = here("data", "ghg_pars_luc.RData"))


# AGROFORESTRY  ---------------------------------------------
# Agroforestry footprint
ghg_pars_agroforestry <- read_csv(here("data", "ghg_agroforestry.csv"), skip = 1) 

ghg_pars_agroforestry <- ghg_pars_agroforestry %>% 
  filter(!str_detect(lcm, "c.g|n.g|h.r")) %>% 
  mutate(lcm = case_when(lcm == "a.g_woodpa" ~ "s.g_woodpa",
                         TRUE ~ lcm),
         organic = str_detect(lcm, "organic"),
         lcm = str_remove(lcm, "_organic"),
         lcm = case_when(organic ~ str_c(lcm, "_organic"),
                         TRUE ~ lcm)) %>% 
  select(-organic)

write_csv(ghg_pars_agroforestry, file = here("data", "ghg_agroforestry_updated.csv"), col_names = TRUE)

ghg_pars_agroforestry %>% filter(type != "Silvoarable agroforestry (apples)")


# PROP LIQUID ---------------------------------------------
# Manure management system info
manure_systems <- read_csv(here("data", "manure_systems.csv"), skip = 1) %>% select(-notes)

# Estimate fraction of manure emissions arising from liquid systems
prop_liquid <- manure_systems %>%
  filter(manure_system %in% c("Liquid", "Solid", "Other")) %>%
  group_by(animal) %>%
  mutate(prop_direct = (kg_n * ef3) / sum(kg_n * ef3),
         prop_vol = (kg_n * frac_gasm) / sum(kg_n * frac_gasm),
         prop_ch4 = (kg_n * bo * mcf) / sum(kg_n * bo * mcf)) %>%
  ungroup() %>%
  filter(manure_system == "Liquid") %>%
  select(animal, prop_direct, prop_vol, prop_ch4)

save(prop_liquid, file = here("data", "ghg_pars_prop_liquid"))


# PARS HEDGES NUTS1 ---------------------------------------------
# LOAD DATA 
# CEH Woody Linear Features - hedgerows
wlf <- terra::vect(here("data", "GB_WLF_V1_0.gdb"))
# NUTS1 raster & lookup
r_nuts1_eng <- terra::rast(here("data", "rasters", "r_nuts1_eng.tif"))
load(here("data", "raster_lookup_nuts1.RData"))

# CONVERT TO SF
# NUTS regions
v_nuts1_eng <- terra::as.polygons(r_nuts1_eng)
s_nuts1_eng <- st_as_sf(v_nuts1_eng) %>% st_set_crs(27700)
# WLF
s_wlf <- st_as_sf(wlf)


# CALCULATE AREAS
# Join WLF with NUTS1
s_intersected <- st_intersects(s_nuts1_eng, s_wlf)

# Sum WLF length per NUTS1 region 
hedge_nuts <- rbind(tibble(nuts1_layer = 1,
                           wlf = s_intersected[[1]]),
                    tibble(nuts1_layer = 2,
                           wlf = s_intersected[[2]]),
                    tibble(nuts1_layer = 3,
                           wlf = s_intersected[[3]]),
                    tibble(nuts1_layer = 4,
                           wlf = s_intersected[[4]]),
                    tibble(nuts1_layer = 5,
                           wlf = s_intersected[[5]]),
                    tibble(nuts1_layer = 8,
                           wlf = s_intersected[[6]]),
                    tibble(nuts1_layer = 9,
                           wlf = s_intersected[[7]]),
                    tibble(nuts1_layer = 11,
                           wlf = s_intersected[[8]]),
                    tibble(nuts1_layer = 12,
                           wlf = s_intersected[[9]])) %>% 
  left_join(as_tibble(s_wlf) %>% 
              select(-geometry) %>% 
              mutate(id = row_number()),
            by = c("wlf" = "id")) %>% 
  group_by(nuts1_layer) %>% 
  summarise(hedge_length = sum(SHAPE_Length)) %>% 
  left_join(lookup_nuts1,
            by = c("nuts1_layer")) %>% 
  mutate(nuts1 = case_when(nuts1 %in% c("London", "South East (England)") ~ "South East & London (England)",
                           TRUE ~ nuts1)) %>% 
  group_by(nuts1) %>% 
  summarise(hedge_length = sum(hedge_length)) %>% 
  # Calculate proportion
  mutate(prop = hedge_length / sum(hedge_length))

# SAVE
hedge_nuts_eng <- hedge_nuts
save(hedge_nuts_eng, file = here("data", "hedge_prop_nuts1.RDATA"))


# UPDATE GHG PARS
# Load 
load(here("data", "ghg_pars_hedges.RData"))
load(here("data", "hedge_prop_nuts1.RDATA"))

# Update hedge length with nuts proportions
ghg_pars_hedges$hedge_length_nuts <- ghg_pars_hedges$hedge_length %>% 
  left_join(hedge_nuts_eng %>% 
              mutate(country = "England") %>% 
              select(-hedge_length),
            by = "country") %>% 
  mutate(nuts1 = case_when(is.na(nuts1) ~ country,
                           TRUE ~ nuts1),
         prop = case_when(is.na(prop) ~ 1, 
                          TRUE ~ prop),
         length = length * prop)

save(ghg_pars_hedges, file = here("data", "ghg_pars_hedges.RData"))

