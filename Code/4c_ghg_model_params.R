# 4c GHG AGRICULTURAL PARAMETERS ######################
source("./code/0_setup.R")

# GWP conversion factors
gwp100 <- read_csv(here("data", "gwp100.csv"), skip = 1) %>% filter(notes == "AR4") %>% select(-notes)


# AG PARAMS FOR GHG MODEL --------------------------
## BASELINE FOOD PROPERTIES --------------------------
# Get food results for 2015
load(here("data", "food_production_2015.RData"))
food_baseline <- food_production_2015$food[[1]]

# Add rows for deer, goats and horses (as sheep) - for allocating to each country
food_baseline <- food_baseline %>% 
  select(-feed_nutrient_consumption) %>% 
  bind_cols(food_baseline %>% 
              select(feed_nutrient_consumption) %>% 
              unnest(feed_nutrient_consumption) %>% 
              filter(animal == "Sheep") %>%
              select(-animal) %>% 
              crossing(animal = c("Deer", "Goats", "Horses")) %>% 
              group_by(animal) %>% 
              mutate_at(vars(me, ge, cp), ~ . / sum(.)) %>% 
              ungroup() %>% 
              bind_rows(food_baseline %>% 
                          select(feed_nutrient_consumption) %>% 
                          unnest(feed_nutrient_consumption)) %>% 
              # Combine Eggs & Poultry
              mutate(animal = ifelse(animal == "Eggs", "Poultry", animal)) %>%
              group_by(country, nuts1, animal) %>%
              summarise_all(sum) %>%
              ungroup() %>% 
              nest(feed_nutrient_consumption = country:cp)) %>% 
  # Reorder
  select(food_type_sum, country_sums, crop_areas, feed_nutrient_consumption, feed_imported)


## BASELINE HUMAN POPULATION --------------------------
# Projections
# pop <- read_csv("data/population.csv", skip = 1) %>% select(-source, -notes) %>% 
#   # Use UN medium, which comes close (73.0m) to ONS projection of 72.4m by 2043 https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/bulletins/nationalpopulationprojections/2018based
#   filter(est %in% c("estimate", "medium variant")) %>%
#   filter(!(year == 2020 & est == "estimate")) %>% 
#   select(year, pop) %>% 
#   mutate(pop_rel = pop/pop[year == 2015]) #%>% 
# # filter(year %in% seq(2015, 2100, by = 5)) 
# 
# # Distribution across 4 countries
# pop_countries <- read_csv("data/population_countries.csv", skip = 1) %>% select(-source, -year) %>% 
#   mutate(pop = pop / sum(pop)) 


# New pop
pop_nuts <- read_csv(here("data", "ONS_NUTS1_pop.csv")) %>% select(NUTS1, NUTS2016, "T") %>% 
  rename("pop" = "T") %>% 
  filter(str_length(NUTS2016) == 3) %>% 
  mutate(nuts1 = case_when(NUTS1 %in% c("LONDON", "SOUTH EAST (ENGLAND)") ~ "South East & London (England)",
                           TRUE ~ NUTS1)) %>% 
  group_by(nuts1) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(nuts1 = str_to_title(nuts1),
         nuts1 = case_when(nuts1 == "Yorkshire And The Humber" ~ "Yorkshire and The Humber",
                           nuts1 == "East Of England" ~ "East of England",
                           TRUE ~ nuts1)) %>% 
  mutate(pop_rel = pop/sum(pop))

load(here("data", "pop_data.RData"))


## AGRICULTURE - LIVESTOCK --------------------------
# Load data 
# Manure management system info
manure_systems <- read_csv(here("data", "ag_params", "manure_systems.csv"), skip = 1) %>% select(-notes)

## CH4 emissions from enteric fermentation
gwp_entericferm_ch4 <- read_csv(here("data", "ag_params", "entericferm_ch4.csv"), skip = 1) %>% select(-source)
 
## CH4 emissions from enteric fermentation
# Calculate GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_entericferm_ch4 <- gwp_entericferm_ch4 %>% 
  # Calculate GWP100
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Get baseline GE consumption
  left_join(food_baseline %>% select(feed_nutrient_consumption) %>% unnest(), by = "animal") %>%
  rename("baseline_ge" = ge) %>% 
  # Allocate GWP to each country
  group_by(animal) %>% 
  mutate(gwp_t = gwp_t * baseline_ge / sum(baseline_ge)) %>%
  ungroup() %>% 
  # Tidy
  mutate(baseline_ge = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_ge)) %>% 
  select(category, ag_sector, country, nuts1, animal, gwp_t, ghg, baseline_ge)

## CH4 emissions from manure management
gwp_manureman_ch4 <- read_csv(here("data", "ag_params", "manureman_ch4.csv"), skip = 1) %>% select(-source)

# Calculate GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_manureman_ch4 <- gwp_manureman_ch4 %>%
  # Calculate GWP100
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Get baseline ME consumption
  left_join(food_baseline %>% select(feed_nutrient_consumption) %>% unnest(), by = "animal") %>%
  rename("baseline_me" = me) %>% 
  # Allocate GWP to each country
  group_by(animal) %>% 
  mutate(gwp_t = gwp_t * baseline_me / sum(baseline_me)) %>%
  ungroup() %>% 
  # Tidy
  mutate(baseline_me = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_me)) %>% 
  select(category, ag_sector, country, nuts1, animal, gwp_t, ghg, baseline_me) %>% 
  # Add multiplier for grass-fed ruminants (MCF lower for pasture than other manure systems) 
  bind_rows((.) %>% 
              filter(animal %in% c("Beef", "Milk", "Sheep")) %>%  
              left_join(manure_systems %>% 
                          filter(animal %in% c("Beef", "Milk", "Sheep")) %>%
                          group_by(animal) %>% 
                          mutate(prop = kg_n / sum(kg_n)) %>% 
                          summarise(grassfed_multiplier = mcf[manure_system == "Pasture"] / sum(prop * mcf), .groups = "drop"),
                        by = "animal") %>% 
              mutate(animal = paste0(animal, " (grass-fed)"))) %>%
  mutate(grassfed_multiplier = ifelse(is.na(grassfed_multiplier), 1, grassfed_multiplier))

## Direct N2O emissions from manure management
gwp_manureman_directn2o <- read_csv(here("data", "ag_params", "manureman_directn2o.csv"), skip = 1) %>% select(-source)

# Calculate GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_manureman_directn2o <- gwp_manureman_directn2o %>%
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Get baseline ME consumption
  left_join(food_baseline %>% select(feed_nutrient_consumption) %>% unnest(), by = "animal") %>%
  rename("baseline_cp" = cp) %>% 
  # Allocate GWP to each country
  group_by(animal) %>% 
  mutate(gwp_t = gwp_t * baseline_cp / sum(baseline_cp)) %>%
  ungroup() %>% 
  # Tidy
  mutate(baseline_cp = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_cp)) %>% 
  select(category, ag_sector, country, nuts1, animal, gwp_t, ghg, baseline_cp)
# No need for grassfed multiplier - column 'animal' excludes grass fed, as no handled manure

## Indirect N2O emissions from manure management
gwp_manureman_indirectn2o <- read_csv(here("data", "ag_params", "manureman_indirectn2o.csv"), skip = 1) %>% select(-source)

# Calculate GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_manureman_indirectn2o <- manure_systems %>%
  # Consider only indoor systems
  filter(manure_system %in% c("Liquid", "Solid", "Other")) %>% 
  group_by(animal) %>% 
  # Calculate (approx) fraction of total vol/leach attributable to each livestock type
  summarise(n_vol = sum(kg_n * frac_gasm),
            n_leach = sum(kg_n * frac_leach)) %>% 
  ungroup() %>% 
  mutate(prop_n_vol = n_vol / sum(n_vol),
         prop_n_leach = n_leach / sum(n_leach)) %>% 
  select(-n_vol, -n_leach) %>% 
  # Join with baseline emissions
  crossing(gwp_manureman_indirectn2o) %>% 
  mutate(prop = ifelse(category == "Indirect N2O manure management - leach", prop_n_leach, prop_n_vol)) %>%
  # Calculate GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100 * prop) %>% 
  # Get baseline CP content
  inner_join(food_baseline %>% select(feed_nutrient_consumption) %>% unnest() %>% select(country, nuts1, animal, cp), by = "animal") %>% 
  rename('baseline_cp' = cp) %>%
  # Allocate GWP to each country
  group_by(animal, category) %>% 
  mutate(gwp_t = gwp_t * baseline_cp / sum(baseline_cp),
         ag_sector = 1) %>% 
  ungroup() %>% 
  # Tidy
  mutate(baseline_cp = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_cp)) %>% 
  select(category, ag_sector, country, nuts1, animal, gwp_t, ghg, baseline_cp) 
# No need for grassfed multiplier - column 'animal' excludes grass fed, as no handled manure


## AGRICULTURE - SOIL ---------------------------------
## N2O emissions from agricultural soils
gwp_agsoils_n2o <- read_csv(here("data", "ag_params", "agsoils_n2o.csv"), skip = 1) %>% select(-source, -usage)

# Applied manure info
manure_agsoils <- read_csv(here("data", "ag_params", "manure_agsoils.csv"), skip = 1) %>% select(-notes)

# Deposited urine/dung info
urinedung_agsoils <- read_csv(here("data", "ag_params", "urinedung_agsoils.csv"), skip = 1) %>% select(-notes)

# Synthetic N fertiliser application rates 
synthn_rates <- read_csv(here("data", "ag_params", "synthn_rates.csv"), skip = 1) %>% select(-source, -notes, -units, -area) 

# Synthetic N EFs
synthn_efs <- read_csv(here("data", "ag_params", "synthn_efs.csv"), skip = 1) %>% select(-source) 

# Baseline N inputs to soil (for indirect N2O)
agsoils_inputs <- read_csv(here("data", "ag_params", "agsoils_inputs.csv"), skip = 1) 

# Update synthn_rates for organic farming
synthn_rates <- synthn_rates %>% 
  crossing(organic = c(TRUE, FALSE)) %>% 
  mutate(rate = ifelse(organic, 0, rate))

## Derived baseline indirect N2O emissions per source
gwp_agsoils_indirect_n2o <- agsoils_inputs %>%
  # Calculate (approx) fraction of total vol/leach attributable to each source
  mutate(prop_n_vol = (kg_n * frac_gas) / sum(kg_n * frac_gas),
         prop_n_leach = (kg_n * frac_leach) / sum(kg_n * frac_leach)) %>% 
  select(-frac_gas, -frac_leach, -ef4, -ef5) %>% 
  rename(category1 = category) %>% 
  # Join with baseline emissions
  crossing(filter(gwp_agsoils_n2o, category %in% c("Indirect N2O soils - leach", "Indirect N2O soils - vol"))) %>% 
  # Allocate total indirect N2O emissions to each source 
  mutate(ghg_t = ghg_t * ifelse(category == "Indirect N2O soils - leach", prop_n_leach, prop_n_vol)) %>% 
  mutate(category = paste0(category1, " - indirect ", gsub("Indirect N2O soils - ", "", category))) %>%
  # # Sum across leaching/vol
  # group_by(category, units, ghg, ag_sector) %>% 
  # summarise(ghg_t = sum(ghg_t)) %>% 
  # ungroup() %>% 
  # Tidy
  select(category, units, ghg_t, ghg, ag_sector) 

## Calculate manure GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_manure_agsoils_n2o <- manure_agsoils %>% 
  # Calculate (approx) fraction of total spread manure N2O attributable to each livestock type
  group_by(animal) %>% 
  summarise(n = sum(kg_n * (1 - frac_loss) * ef1)) %>%
  ungroup() %>% 
  mutate(prop = n / sum(n)) %>% 
  select(-n) %>% 
  # Join with baseline emissions - direct and indirect
  crossing(bind_rows(filter(gwp_agsoils_n2o, category == "Animal manure applied"),
                     filter(gwp_agsoils_indirect_n2o, category %in% c("Animal manure applied - indirect leach", "Animal manure applied - indirect vol")))) %>% 
  # Calculate GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100 * prop) %>% 
  # Get baseline CP content
  inner_join(food_baseline %>% select(feed_nutrient_consumption) %>% unnest() %>% select(country, nuts1, animal, cp), by = "animal") %>% 
  rename('baseline_cp' = cp) %>%
  # Allocate GWP to each country
  group_by(animal, category) %>% 
  mutate(gwp_t = gwp_t * baseline_cp / sum(baseline_cp),
         ag_sector = 1) %>% 
  ungroup() %>% 
  # Tidy
  mutate(baseline_cp = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_cp)) %>% 
  select(category, ag_sector, country, nuts1, animal, gwp_t, ghg, baseline_cp) 
# No need for grassfed multiplier - column 'animal' excludes grass fed, as no handled manure

## Calculate urine/dung GWP100 (t CO2e) - BASELINE EMISSIONS
gwp_urinedung_agsoils_n2o <- urinedung_agsoils %>%
  # Calculate (approx) fraction of total spread manure N2O attributable to each livestock type
  mutate(prop = (kg_n * ef3) / sum(kg_n * ef3)) %>% 
  select(-kg_n, -ef3) %>% 
  # Join with baseline emissions - direct and indirect
  crossing(bind_rows(filter(gwp_agsoils_n2o, category == "Urine and dung deposited"),
                     filter(gwp_agsoils_indirect_n2o, category %in% c("Urine and dung deposited - indirect leach", "Urine and dung deposited - indirect vol")))) %>% 
  # Calculate GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100 * prop) %>% 
  # Get baseline CP content
  inner_join(food_baseline %>% select(feed_nutrient_consumption) %>% unnest() %>% select(country, nuts1, animal, cp), by = "animal") %>% 
  rename('baseline_cp' = cp) %>%
  # Allocate GWP to each country
  group_by(animal, category) %>% 
  mutate(gwp_t = gwp_t * baseline_cp / sum(baseline_cp),
         ag_sector = 1) %>% 
  ungroup() %>% 
  # Tidy
  mutate(baseline_cp = ifelse(animal %in% c("Deer", "Goats", "Horses"), NA, baseline_cp)) %>% 
  select(category, ag_sector, country, nuts1, animal, gwp_t, ghg, baseline_cp) %>%
  # Add multiplier for grass-fed ruminants (more N deposited as urine/dung)
  # left_join(manure_systems %>% 
  #             filter(animal %in% c("Beef", "Milk", "Sheep")) %>%
  #             group_by(animal) %>% 
  #             mutate(prop = kg_n / sum(kg_n)) %>% 
  #             summarise(grassfed_multiplier = 1 / prop[manure_system == "Pasture"], .groups = "drop"),
  #           by = "animal")
  bind_rows((.) %>% 
              filter(animal %in% c("Beef", "Milk", "Sheep")) %>%  
              left_join(manure_systems %>% 
                          filter(animal %in% c("Beef", "Milk", "Sheep")) %>%
                          group_by(animal) %>% 
                          mutate(prop = kg_n / sum(kg_n))%>% 
                          summarise(grassfed_multiplier = 1 / prop[manure_system == "Pasture"], .groups = "drop"),
                        by = "animal") %>% 
              mutate(animal = paste0(animal, " (grass-fed)"))) %>% 
  mutate(grassfed_multiplier = ifelse(is.na(grassfed_multiplier), 1, grassfed_multiplier))

## Calculate synthetic N GWP100 (t CO2e) - PER HECTARE OF EACH CROP
gwp_synthn_agsoils_n2o <- synthn_efs %>%
  filter(category == "Soil N2O from inorganic N") %>%
  # Derived EF for indirect N2O
  bind_rows(gwp_agsoils_indirect_n2o %>% 
              filter(category %in% c("Inorganic N fertiliser - indirect leach", "Inorganic N fertiliser - indirect vol")) %>%
              mutate(ghg_t = ghg_t / agsoils_inputs$kg_n[agsoils_inputs$category == "Inorganic N fertiliser"],
                     units = "t N2O / kg N",
                     frac = 1)) %>% 
  # Convert to GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100 * frac) %>% 
  select(category, ag_sector, gwp_t, ghg) %>%
  # Cross with application rates
  crossing(synthn_rates) 

## Calculate sewage N GWP100 (t CO2e) - PROPORTIONAL TO POP GROWTH
gwp_sewage_agsoils_n2o <- bind_rows(filter(gwp_agsoils_n2o, category == "Sewage sludge applied"),
                                    filter(gwp_agsoils_indirect_n2o, category %in% c("Sewage sludge applied - indirect leach", "Sewage sludge applied - indirect vol"))) %>% 
  # Calculate GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Allocate to 4 countries on basis of 2015 population
  crossing(pop_nuts) %>% 
  mutate(gwp_t = gwp_t * pop_rel,
         country = case_when(nuts1 %in% c("Northern Ireland", "Scotland", "Wales")  ~ nuts1,
                             TRUE ~ "England")) %>% 
  # Get baseline population size - taken from pop_change variable in ghg_pars_fun
  mutate(baseline_pop = 65860149) %>% 
  select(category, ag_sector, country, nuts1, gwp_t, ghg, baseline_pop) 

## Calculate crop residue N GWP100 (t CO2e) - PROPORTIONAL TO CROP TONNAGE
gwp_cropresidues_agsoils_n2o <- bind_rows(filter(gwp_agsoils_n2o, category == "Crop residues"),
                                          filter(gwp_agsoils_indirect_n2o, category %in% c("Crop residues - indirect leach", "Crop residues - indirect vol"))) %>% 
  # Calculate GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Get baseline crop tonnage
  crossing(food_baseline %>% select(food_type_sum) %>% unnest() %>% 
             filter(product %in% c("Cereal", "Fruit & vegetables", "Legumes", "Oilseeds", "Potatoes", "Sugar beet")) %>% 
             group_by(country, nuts1) %>% 
             summarise(baseline_tonnage = sum(tonnage))) %>%
  # Allocate GWP to each country
  group_by(category) %>% 
  mutate(gwp_t = gwp_t * baseline_tonnage / sum(baseline_tonnage)) %>%
  ungroup() %>% 
  # Tidy
  select(category, ag_sector, country, nuts1, gwp_t, ghg, baseline_tonnage)


## AGRICULTURE - OTHER ---------------------------
# Read baseline emissions
gwp_otherag <- read_csv(here("data", "ag_params", "otherag_efs.csv"), skip = 1) %>% select(-source, -usage) 

## Pesticides
gwp_pesticides <- gwp_otherag %>%
  filter(category == "Pesticide manufacture and breakdown") %>% 
  # Convert to GWP - per ha of cropland
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Add organic row (no pesticides)
  crossing(organic = c(TRUE, FALSE)) %>% 
  mutate(gwp_t = ifelse(organic, 0, gwp_t)) %>% 
  # Tidy
  select(category, organic, ag_sector, gwp_t, ghg) 

## Other ag - liming and energy 
gwp_otherag <- gwp_otherag %>% 
  filter(category != "Pesticide manufacture and breakdown") %>% 
  # Convert to GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100) %>% 
  # Cross with total tonnage
  crossing(food_baseline %>% select(food_type_sum) %>% unnest() %>% 
             filter(product %in% c("Cereal", "Fruit & vegetables", "Legumes", "Oilseeds", "Potatoes", "Sugar beet")) %>% 
             group_by(country, nuts1) %>% 
             summarise(baseline_tonnage = sum(tonnage))) %>% 
  # Allocate GWP to each country
  group_by(category, ghg) %>% 
  mutate(gwp_t = gwp_t * baseline_tonnage / sum(baseline_tonnage)) %>%
  ungroup() %>% 
  # Tidy
  select(category, ag_sector, country, nuts1, gwp_t, ghg, baseline_tonnage)



## Fertilisers - manufacture and urea breakdown
gwp_synthn_other <- synthn_efs %>%
  filter(!category %in% c("Soil N2O from inorganic N")) %>%
  # Convert to GWP
  left_join(gwp100, by = "ghg") %>% 
  mutate(gwp_t = ghg_t * gwp100 * frac) %>% 
  select(category, ag_sector, gwp_t, ghg) %>%
  # Cross with application rates
  crossing(synthn_rates) 

## FEED IMPORTS -------------------------
load(here("data", "food_pars_fun.RData"))

# Emissions from imported feed
gwp_feed_imported <- food_baseline %>% select(feed_imported) %>% unnest() %>% 
  # Calculate mean energy density (MJ / t) of cereals
  crossing(food_params$.feed_mj %>% 
             filter(crop %in% c("Barley", "Cereals, other", "Oats", "Wheat")) %>% 
             summarise(mj_t = mean(mj*dm))) %>%
  # Calculate baseline GWP (t CO2e - 1.54 from Lamb et al.)
  mutate(gwp_t = 1.54 * mj_imported / mj_t) %>% 
  # Tidy
  mutate(category = "Feed imports",
         ag_sector = 0,
         ghg = "CO2") %>% 
  rename(baseline_mj_imported = mj_imported) %>% 
  select(category, ag_sector, country, nuts1, gwp_t, ghg, baseline_mj_imported) 

## SAVE --------------------------
ghg_pars_ag <- list(gwp_entericferm_ch4 = gwp_entericferm_ch4,
                    gwp_manureman_ch4 = gwp_manureman_ch4,
                    gwp_manureman_indirectn2o = gwp_manureman_indirectn2o,
                    gwp_manureman_directn2o = gwp_manureman_directn2o,
                    gwp_manure_agsoils_n2o = gwp_manure_agsoils_n2o,
                    gwp_urinedung_agsoils_n2o = gwp_urinedung_agsoils_n2o,
                    gwp_synthn_agsoils_n2o = gwp_synthn_agsoils_n2o,
                    gwp_sewage_agsoils_n2o = gwp_sewage_agsoils_n2o,
                    gwp_cropresidues_agsoils_n2o = gwp_cropresidues_agsoils_n2o,
                    gwp_pesticides = gwp_pesticides,
                    gwp_otherag = gwp_otherag,
                    gwp_synthn_other = gwp_synthn_other,
                    gwp_feed_imported = gwp_feed_imported)

save(ghg_pars_ag,
     pop_nuts, 
     pop_change,
     file = here("data", "ghg_pars_ag.RData"))


## GHG FUNCTION -------------------------------------

# params <- list(.gwp_ag_synthn = gwp_ag_synthn,
#                .gwp_ag_other_a = gwp_ag_other_a,
#                .gwp_ag_other_b = gwp_ag_other_b,
#                .gwp_ag_other_c = gwp_ag_other_c,
#                .gwp_ag_entericferm = gwp_ag_entericferm,
#                .gwp_peat_condition = gwp_peat_condition)
# 
# crop_areas <- baseline_ag$crop_areas
# country_sums <- baseline_ag$country_sums
# feed_nutrient_consumption <- baseline_ag$feed_nutrient_consumption
# peat_areas <- crosstab_data %>%
#   filter(peat == 1) %>%
#   group_by(country, lcm_layer) %>%
#   summarise(area = sum(area)) %>%
#   ungroup() %>%
#   mutate(age = "Existing",
#          scenario = "Baseline")


ghg_fun <- function(params, crop_areas, country_sums, feed_nutrient_consumption, peat_areas){
  # EXTRACT PARAMS -----------------
  .gwp_ag_synthn <- params$.gwp_ag_synthn
  .gwp_ag_other_a <- params$.gwp_ag_other_a
  .gwp_ag_other_b <- params$.gwp_ag_other_b
  .gwp_ag_other_c <- params$.gwp_ag_other_c
  .gwp_ag_entericferm <- params$.gwp_ag_entericferm
  .gwp_peat_condition <- params$.gwp_peat_condition

  gwp_ag <- bind_rows(
    # SYNTHETIC N -------------------
    crop_areas %>%
      left_join(.gwp_ag_synthn, by = "crop_fine") %>%
      group_by(scenario, country, category, ag_sector) %>%
      summarise(gwp_t = sum(area * yield_multiplier * gwp_t)) %>%
      ungroup(),

    # OTHER AG -------------------
    # A: proportional to cropped area
    .gwp_ag_other_a %>%
      left_join(country_sums %>% select(scenario, country, cropped_area), by = "country") %>%
      group_by(scenario, country, category, ag_sector) %>%
      summarise(gwp_t = gwp_t * (cropped_area / baseline_cropped_area)) %>%
      ungroup(),

    # B: proportional to crop tonnage
    .gwp_ag_other_b %>%
      left_join(country_sums %>% select(scenario, country, crop_tonnage), by = "country") %>%
      group_by(scenario, country, category, ag_sector) %>%
      summarise(gwp_t = gwp_t * (crop_tonnage / baseline_crop_tonnage)) %>%
      ungroup(),

    # C: proportional to crop + livestock tonnage
    .gwp_ag_other_c %>%
      left_join(country_sums %>% select(scenario, country, crop_tonnage, livestock_tonnage), by = "country") %>%
      group_by(scenario, country, category, ag_sector) %>%
      summarise(gwp_t = gwp_t * ((crop_tonnage + livestock_tonnage) / baseline_croplivestock_tonnage)) %>%
      ungroup(),

    # ENTERIC FERMENTATION -------------------
    .gwp_ag_entericferm %>%
      left_join(feed_nutrient_consumption %>% select(scenario, country, animal, ge), by = c("country", "animal")) %>%
      group_by(scenario, country, category, ag_sector) %>%
      # Sum across livestock
      summarise(gwp_t = sum(gwp_t * (ge / baseline_ge))) %>%
      ungroup())

  # PEATLANDS -------------------
  gwp_peat <- peat_areas %>%
    inner_join(.gwp_peat_condition,
               by = c('lcm_layer', 'country', 'age')) %>%
    group_by(scenario, country) %>%
    summarise(gwp_t = sum(area * gwp_t)) %>%
    mutate(category = "Peatlands")

  # RETURN ----------------
  return(bind_rows(gwp_ag,
                   gwp_peat))

}

