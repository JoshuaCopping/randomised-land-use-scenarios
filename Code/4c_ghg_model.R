# 4c GHG MODEL FUNCTION WITH NUTS1 ######################
source("./code/0_setup.R")


# GET GHG PARAMS ---------------------------------------------
# Ambition combinations
ambition_combos <- read_csv(here("data", "ambition_combinations_new.csv"), skip = 0) %>% 
  filter(scenario %in% c(0, 8)) %>% 
  mutate(scenario = case_when(scenario == 0 ~ "a",
                              scenario == 8 ~ "b"))

# Scenario params
scenario_params <- read_csv("data/scenario_parameters_LUSP.csv", skip = 1)

# Ag
load(here("data", "ghg_pars_ag.RData"))
load(here("data", "ghg_pars_prop_liquid"))

# Get low carbon farming params
lowcarbonfarming <- read_csv(here("data", "lowcarbonfarming.csv"), skip = 1) %>% select(-sruc_code)

# Peat
load(here("data", "ghg_pars_peat.RData"))

# Hedges
load(here("data", "ghg_pars_hedges.RData"))

# LUC
load(here("data", "ghg_pars_luc.RData"))

# Woodland/agroforestry
load(here("data", "ghg_pars_wood_2023.RData"))

# Agroforestry footprint
ghg_pars_agroforestry <- read_csv(here("data", "ghg_agroforestry_updated.csv")) %>% 
  select(-notes) %>%
  filter(type != "Silvoarable agroforestry (apples)")


# GHG MODEL FUNCTION SETUP ---------------------------------------------
do_ghg_fun <- function(ambitions,
                       scenario_areas,
                       scenario_params, ambition_combos,
                       ghg_pars_luc,
                       ghg_pars_ag, food_production, food_production_2015, pop_change, prop_liquid, lowcarbonfarming,
                       ghg_pars_peat,
                       ghg_pars_bioenergy,
                       ghg_pars_wood, 
                       ghg_pars_agroforestry,
                       ghg_pars_hedges){
  
  options(dplyr.summarise.inform = FALSE)
  
  # Get scenario name
  scenario_names <- scenario_areas %>% select(scenario) %>% unique()
  
  # Get all years 2015-2050
  years <- tibble(year = seq(2015, 2050, by = 1))
  
  
  ## LUC ---------------------------------------------
  # Get land-use change areas - focus on transitions to SNG, woodland and built land (everything else dealt with elsewhere)
  luc_areas <- scenario_areas %>% 
    filter(substr(lcm_from, 1, 3) != substr(lcm_to, 1, 3),
           peat == 0,
           !lcm_to %in% c("s.m", "s.g_woodpa", "solar")) %>% 
    group_by(lcm_from, lcm_to, country, nuts1) %>%
    summarise(area = sum(area)) %>% 
    # Annualise
    mutate(area = area/nrow(years)) %>%
    crossing(years) %>% 
    arrange(year, country) 

  # Calculate land-use change areas - RENEWABLES 
  luc_renewable_areas <- scenario_areas %>% 
    filter(str_detect(lcm_to, "wind|solar")) %>% 
    mutate(lcm_to = case_when(str_detect(lcm_to, "wind") ~ "wind",
                              TRUE ~ "solar")) %>% 
    group_by(country, nuts1, lcm_from, lcm_to) %>% 
    summarise(area = sum(area)) %>%
    # Annualise
    mutate(area = area/nrow(years)) %>%
    crossing(years) %>% 
    arrange(year, country)
  
  # Label categories 
  luc_areas <- luc_areas %>%
    mutate(category_1 = case_when(#lcm_to %in% c("i.g_energy") ~ "Bioenergy crops",
      #lcm_to %in% c("a.h_energy_misc", "a.h_energy_src") ~ "Bioenergy crops",
      lcm_to %in% c("b.w", "c.w", "c.w_pinewood") ~ "Woodland",
      lcm_to %in% c("s.g") ~ "Semi-natural grassland creation",
      # lcm_to %in% c("h.r") ~ "Heathland creation",
      lcm_to %in% c("b.t") ~ "Urban expansion"))
  
  luc_renewable_areas <- luc_renewable_areas %>%
    mutate(category_1 = case_when(lcm_to = str_detect(lcm_to, "wind|solar") ~ "Renewable energy"))
  
  ghg_pars_luc$gwp_luc_biomass <- ghg_pars_luc$gwp_luc_biomass %>% 
    mutate(lcm_to = ifelse(lcm_to %in% c("n.g", "c.g", "a.g"), "s.g", lcm_to)) %>% 
    unique()
  ghg_pars_luc$gwp_luc_soil <- ghg_pars_luc$gwp_luc_soil %>% 
    mutate(lcm_to = ifelse(lcm_to %in% c("n.g", "c.g", "a.g"), "s.g", lcm_to)) %>% 
    unique()
  
  gwp_luc <- bind_rows(
    # Calculate instant change in non-forest biomass
    luc_areas %>%
      left_join(ghg_pars_luc$gwp_luc_biomass %>% filter(!lcm_from %in% c("b.w", "c.w") & !lcm_to %in% c("b.w", "c.w")), 
                by = c("lcm_from", "lcm_to")) %>%
      group_by(year, country, nuts1, category_1) %>%
      summarise(gwp_t = sum(gwp_t * area), .groups = "drop") %>%
      mutate(gwp_t = ifelse(is.na(gwp_t), 0, gwp_t),
             category_2 = "Land-use change - non-forest biomass"),
    # Calculate annual change in soil C
    luc_areas %>%
      inner_join(ghg_pars_luc$gwp_luc_soil, by = c("lcm_from", "lcm_to", "country"), multiple = "all") %>%
      mutate(year = year.x + year.y) %>%
      select(-year.x, -year.y) %>%
      filter(year <= 2050) %>%
      group_by(year, country, nuts1, category_1) %>%
      summarise(gwp_t = sum(gwp_t * area), .groups = "drop") %>%
      mutate(category_2 = "Land-use change - soil carbon"),
    # Calculate annual change in soil C in renewable energy areas
    luc_renewable_areas %>%
      inner_join(ghg_pars_luc$gwp_luc_renewables_soil, by = c("lcm_from", "lcm_to", "country"), multiple = "all") %>%
      mutate(year = year.x + year.y) %>%
      select(-year.x, -year.y) %>%
      filter(year <= 2050) %>%
      group_by(year, country, nuts1, category_1) %>%
      summarise(gwp_t = sum(gwp_t * area), .groups = "drop") %>%
      mutate(category_2 = "Land-use change - soil carbon")) %>%
    mutate(ghg = "CO2")
  

    ## AGRICULTURE ---------------------------------------------
  ## Pull out activity data
  # Pull out livestock data
  livestock_data <- bind_rows(food_production_2015 %>%
                                unnest(food) %>%
                                unnest(feed_nutrient_consumption) %>%
                                select(country:cp) %>%
                                mutate(year = 2015) %>% 
                                # Combine Eggs & Poultry
                                mutate(animal = ifelse(animal == "Eggs", "Poultry", animal)) %>%
                                group_by(year, country, nuts1, animal) %>%
                                summarise_all(sum) %>%
                                ungroup(),
                              food_production %>%
                                unnest(food) %>%
                                unnest(feed_nutrient_consumption) %>%
                                select(country:cp) %>%
                                mutate(year = 2050) %>% 
                                # Combine Eggs & Poultry
                                mutate(animal = ifelse(animal == "Eggs", "Poultry", animal)) %>%
                                group_by(year, country, nuts1, animal) %>%
                                summarise_all(sum) %>%
                                ungroup()) %>%
    # Annualise (linear interpolate between years)
    full_join(years, by = "year") %>% 
    tidyr::complete(year, nesting(country, nuts1, animal)) %>% 
    drop_na(country, nuts1, animal) %>% 
    group_by(country, nuts1, animal) %>% 
    mutate_at(vars(me, ge, cp), ~zoo::na.approx(.)) %>%  
    ungroup() %>% 
    #Add rows for deer, goats and horses
    bind_rows((.) %>%
                select(year, country, nuts1) %>%
                unique() %>%
                crossing(animal = c("Deer", "Goats", "Horses"))) 
  
    # Pull out ha per crop
  crop_ha <- bind_rows(food_production_2015 %>%
                         select(-contains("flag")) %>% 
                         unnest(food) %>%
                         unnest(cols = c(crop_areas)) %>% 
                         select(country:tonnage) %>% 
                         mutate(year = 2015),
                       food_production %>%
                         unnest(food) %>%
                         unnest(crop_areas) %>% 
                         select(country:tonnage) %>% 
                         mutate(year = 2050)) %>% 
    # Annualise (linear interpolate between years)
    tidyr::complete(crop_fine, organic, year, nesting(country, nuts1)) %>% 
    full_join(years, by = "year") %>% 
    tidyr::complete(year, nesting(country, nuts1, crop_fine, organic)) %>% 
    drop_na(country, crop_fine, organic) %>% 
    mutate(area = ifelse(year %in% c(2015, 2050) & is.na(area), 0, area),
           tonnage = ifelse(year %in% c(2015, 2050) & is.na(tonnage) & !grepl("grass", crop_fine), 0, tonnage)) %>% 
    group_by(country, nuts1, crop_fine, organic) %>%
    mutate_at(vars(area, tonnage), ~zoo::na.approx(., na.rm = FALSE)) %>% 
    ungroup() 
  
  # Pull out crop tonnage
  crop_tonnage <- bind_rows(food_production_2015 %>%
                              unnest(food) %>%
                              unnest(food_type_sum) %>% 
                              select(product:protein) %>% 
                              mutate(year = 2015),
                            food_production %>%
                              unnest(food) %>%
                              unnest(food_type_sum) %>% 
                              select(product:protein) %>% 
                              mutate(year = 2050)) %>% 
    filter(product %in% c("Cereal", "Fruit & vegetables", "Legumes", "Oilseeds", "Potatoes", "Sugar beet")) %>%
    group_by(year, country, nuts1) %>% 
    summarise(tonnage = sum(tonnage), .groups = "drop") %>% 
    # Annualise (linear interpolate between years)
    full_join(years, by = "year") %>%
    tidyr::complete(year, nesting(country, nuts1)) %>% 
    drop_na(country) %>% 
    group_by(country) %>% 
    mutate_at(vars(tonnage), ~zoo::na.approx(.)) %>% 
    ungroup() 
  
  # Pull out feed imports
  feed_imports <- bind_rows(food_production_2015 %>%
                              unnest(food) %>%
                              unnest(feed_imported) %>%
                              select(country, nuts1, mj_imported) %>% 
                              mutate(year = 2015),
                            food_production %>%
                              unnest(food) %>%
                              unnest(feed_imported) %>%
                              select(country, nuts1, mj_imported) %>% 
                              mutate(year = 2050)) %>% 
    # Annualise (linear interpolate between years)
    full_join(years, by = "year") %>%
    tidyr::complete(year, nesting(country, nuts1)) %>% 
    drop_na(country) %>% 
    group_by(country) %>%
    mutate_at(vars(mj_imported), ~zoo::na.approx(.)) %>% 
    ungroup() 
  
  # Get spring crops % area
  spring_crop_props <- crop_ha %>%
    mutate(type = case_when(crop_fine %in% c("Permanent grass", "Temporary grass") ~ crop_fine,
                            crop_fine == "Temporary grass (new ley)" ~ "Temporary grass",
                            grepl("spring", crop_fine) | crop_fine %in% c("Potatoes", "Sugar beet", "Maize, whole", "Fodder crops", "Horticultural crops") ~ "Spring crop",
                            TRUE ~ "Other crop")) %>%
    group_by(year, country, nuts1, type) %>%
    summarise(area = sum(area),
              tonnage = sum(tonnage), .groups = "drop") %>%
    ungroup() %>%
    filter(type %in% c("Other crop", "Spring crop")) %>%
    group_by(year, country, nuts1) %>%
    mutate(prop_area = area / sum(area[type %in% c("Spring crop", "Other crop")]),
           prop_tonnage = tonnage / sum(tonnage[type %in% c("Spring crop", "Other crop")])) %>%
    ungroup() %>%
    filter(type == "Spring crop") %>%
    select(-type, -area, -tonnage) 
  
  ## Account for low carbon farming
  # Tidy measures
  uptake_lowcarbonfarming <- lowcarbonfarming %>%
    select(-scope, -impact) %>%
    gather(ambition, val, -measure, -start_year, -rollout_period) %>%
    mutate(ambition = gsub("uptake_", "", ambition)) %>%
    group_by(measure) %>%
    mutate(uptake_low = unique(val[ambition == "low"])) %>%
    # Cross with all years
    crossing(year = 2015:2050) %>%
    mutate(uptake = case_when(year < start_year ~ uptake_low,
                              year >= start_year & year < start_year + rollout_period ~ uptake_low + (year - start_year + 1) * (val - uptake_low) / rollout_period,
                              TRUE ~ val),
           uptake = uptake - uptake_low) %>%
    # Join with scenarios
    inner_join(ambition_combos %>%
                 select(scenario, ambition_lowcarbonfarming) %>%
                 rename(ambition = ambition_lowcarbonfarming),
               by = "ambition") %>%
    select(-val, -uptake_low, -start_year, -rollout_period, -ambition, -scenario) %>%
    arrange(year)
  
  ## Derive final emissions
  # Emissions from synth N
  gwp_synthn_agsoils_n2o <- crop_ha %>%
    inner_join(ghg_pars_ag$gwp_synthn_agsoils_n2o, by = c("crop_fine", "organic"), multiple = "all") %>%
    # NUE (crop): reduces N rate
    left_join(uptake_lowcarbonfarming %>% filter(measure == "nue_crop") %>% select(-measure), by = "year") %>%
    mutate(yield_growth = 1,
           rate = case_when((!crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)")) & yield_growth == 1 ~ rate * (1 - uptake),
                            (!crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)")) & yield_growth > 1 ~ rate,
                            TRUE ~ rate)) %>%
    select(-uptake) %>%
    # NUE (grass): reduces N rate
    left_join(uptake_lowcarbonfarming %>% filter(measure == "nue_grass") %>% select(-measure), by = "year") %>%
    mutate(yield_growth = 1,
           rate = case_when(crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)") & yield_growth == 1 ~ rate * (1 - uptake),
                            crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)") & yield_growth > 1 ~ rate,
                            TRUE ~ rate)) %>%
    select(-uptake) %>%
    # Grass-legume mix: reduce N input to 0 on permanent grass and 50 (if > 50) on temporary grass
    left_join(uptake_lowcarbonfarming %>% filter(measure == "grass_legume_mix") %>% select(-measure), by = "year") %>%
    mutate(yield_growth = 1,
           rate = case_when(crop_fine == "Permanent grass" ~ rate * (1 - uptake),
                            crop_fine  %in% c("Temporary grass", "Temporary grass (new ley)") ~ min(50, rate) + rate * (1 - uptake),
                            TRUE ~ rate)) %>%
    select(-uptake) %>%
    # Cover crops: reduce leaching in 34% of spring crops by 45%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_crops") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(category == "Inorganic N fertiliser - indirect leach" & grepl("spring", crop_fine),
                          gwp_t * (1 - (0.34 * uptake * 0.45)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Soil loosening: reduce direct N2O emissions by 40% across 20% of land
    left_join(uptake_lowcarbonfarming %>% filter(measure == "loosen_soils") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(category == "Soil N2O from inorganic N",
                          gwp_t * (1 - (0.2 * uptake * 0.4)),
                          gwp_t)) %>%
    select(-uptake) %>%
    mutate(category_2 = "Agriculture - N2O from synthetic N") %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(rate * area * gwp_t), .groups = "drop")
  
  # Emissions from synth N manufacture/breakdown
  gwp_synthn_other <- crop_ha %>%
    inner_join(ghg_pars_ag$gwp_synthn_other, by = c("crop_fine", "organic"), multiple = "all") %>%
    # NUE (crop): reduces N rate
    left_join(uptake_lowcarbonfarming %>% filter(measure == "nue_crop") %>% select(-measure), by = "year") %>%
    mutate(yield_growth = 1,
           rate = case_when((!crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)")) & yield_growth == 1 ~ rate * (1 - uptake),
                            (!crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)")) & yield_growth > 1 ~ rate,
                            TRUE ~ rate)) %>%
    select(-uptake) %>%
    # NUE (grass): reduces N rate
    left_join(uptake_lowcarbonfarming %>% filter(measure == "nue_grass") %>% select(-measure), "year") %>%
    mutate(yield_growth = 1,
           rate = case_when(crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)") & yield_growth == 1 ~ rate * (1 - uptake),
                            crop_fine %in% c("Permanent grass", "Temporary grass", "Temporary grass (new ley)") & yield_growth > 1 ~ rate,
                            TRUE ~ rate)) %>%
    select(-uptake) %>%
    # Grass-legume mix: reduce N input to 0 on  permanent grass and 50 on temporary grass
    left_join(uptake_lowcarbonfarming %>% filter(measure == "grass_legume_mix") %>% select(-measure), "year") %>%
    mutate(yield_growth = 1,
           rate = case_when(crop_fine == "Permanent grass" ~ rate * (1 - uptake),
                            crop_fine %in% c("Temporary grass", "Temporary grass (new ley)") ~ min(50, rate) + rate * (1 - uptake),
                            TRUE ~ rate)) %>%
    # Summarise
    mutate(category_2 = ifelse(category == "CO2 from urea application", "Agriculture - urea breakdown", "Agriculture - fertiliser manufacture")) %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(rate * area * gwp_t), .groups = "drop")
  
  # Emissions from pesticide manufacture/breakdown
  gwp_pesticides <- crop_ha %>%
    filter(!grepl("grass", crop_fine)) %>%
    left_join(ghg_pars_ag$gwp_pesticides, by = "organic") %>%
    # Summarise
    mutate(category_2 = "Agriculture - pesticides") %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(gwp_t * area), .groups = "drop")
  
  # Emissions from liming, energy & machinery
  gwp_otherag <- crop_tonnage %>%
    inner_join(ghg_pars_ag$gwp_otherag, by = c("country", "nuts1"), multiple = "all") %>%
    # Electrification: reduce emissions from farm energy use by 41%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "electrification") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(category == "Farm energy use",
                          gwp_t * (1 - uptake * 0.41),
                          gwp_t)) %>%
    # Summarise
    mutate(category_2 = case_when(category == "CO2 from liming" ~ "Agriculture - liming",
                                  category == "Farm energy use" ~ "Agriculture - energy use",
                                  category == "Machinery manufacture and maintenance" ~ "Agriculture - machinery")) %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(gwp_t * tonnage / baseline_tonnage), .groups = "drop")
  
  # C sequestration from grass leys
  gwp_c_agsoils <- crop_ha %>%
    filter((crop_fine == "Temporary grass") | crop_fine == "Temporary grass (new ley)") %>%
    # 0.202 t CO2 / ha / yr across 5% of temp grass (100% of new organic leys)
    left_join(uptake_lowcarbonfarming %>% filter(measure == "grass_leys") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(organic,
                          -0.202,
                          -0.202 * uptake * 0.05),
           ghg = "CO2",
           ag_sector = 1) %>%
    # Summarise
    mutate(category_2 = "Agriculture - soil C") %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(area * gwp_t), .groups = "drop") 
  
  # Enteric CH4
  gwp_entericferm_ch4 <- livestock_data %>% 
    mutate(animal = gsub("..grass.fed.", "", animal)) %>%
    inner_join(ghg_pars_ag$gwp_entericferm_ch4, by = c("animal", "country", "nuts1")) %>%
    # Feed additives: reduce methane emissions by 20% for beef and 30% for dairy
    left_join(uptake_lowcarbonfarming %>% filter(measure == "additive") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = case_when(animal == "Beef" ~ gwp_t * (1 - (uptake * 0.2)),
                             animal == "Milk" ~ gwp_t * (1 - (uptake * 0.3)),
                             TRUE ~ gwp_t)) %>%
    select(-uptake) %>%
    # Breeding with genomics: reduce methane emissions by 0.15% per year 2030-2050
    left_join(uptake_lowcarbonfarming %>% filter(measure == "breeding_genomics") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = case_when(animal %in% c("Beef", "Milk") ~ gwp_t * (1 - (uptake * 0.0015 * min(max(year - 2030 + 1, 0), 20))),
                             TRUE ~ gwp_t)) %>%
    select(-uptake) %>%
    # Breeding with GE: reduce methane emissions by 0.4% per year 2040-2050
    left_join(uptake_lowcarbonfarming %>% filter(measure == "breeding_genomics") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = case_when(animal %in% c("Beef", "Milk") ~ gwp_t * (1 - (uptake * 0.004 * min(max(year - 2040 + 1, 0), 10))),
                             TRUE ~ gwp_t)) %>%
    select(-uptake) %>%
    # Summarise
    mutate(category_2 = "Agriculture - CH4 from enteric fermentation") %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"),
                                 gwp_t,
                                 gwp_t * ge / baseline_ge)), .groups = "drop")
  
  # Manure CH4
  gwp_manureman_ch4 <- livestock_data %>%
    inner_join(ghg_pars_ag$gwp_manureman_ch4, by = c("animal", "country", "nuts1")) %>%
    # Precision feeding: reduces dairy & pig VS excretion by 2%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "precision_feeding") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(animal %in% c("Milk", "Pig"),
                          gwp_t * (1 - (uptake * 0.02)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Cover slurry tanks: Reduce CH4 emissions by 47% for liquid manure
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_slurry") %>% select(-measure), by = "year") %>%
    left_join(prop_liquid %>% select(animal, prop_ch4) %>% rename(prop_liquid = prop_ch4), by = "animal") %>%
    mutate(prop_liquid = ifelse(is.na(prop_liquid), 0, prop_liquid),
           gwp_t = gwp_t * (1 - (uptake * prop_liquid * 0.47))) %>%
    # Summarise
    mutate(category_2 = "Agriculture - CH4 from livestock manure") %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"),
                                 gwp_t,
                                 gwp_t * me / baseline_me * grassfed_multiplier)), .groups = "drop")
  
  # Manure N2O
  gwp_manureman_directn2o <- livestock_data %>%
    inner_join(ghg_pars_ag$gwp_manureman_directn2o, by = c("animal", "country", "nuts1")) %>%
    # HSG: reduces dairy N excretion by 9%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "hsg_dairy") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(animal == "Milk",
                          gwp_t * (1 - (uptake * 0.09)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Precision feeding: reduces dairy & pig N excretion by 2%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "precision_feeding") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(animal %in% c("Milk", "Pig"),
                          gwp_t * (1 - (uptake * 0.02)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Cover slurry tanks: Reduce N2O emissions by 100% for liquid manure
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_slurry") %>% select(-measure), by = "year") %>%
    left_join(prop_liquid %>% select(animal, prop_direct) %>% rename(prop_liquid = prop_direct), by = "animal") %>%
    mutate(prop_liquid = ifelse(is.na(prop_liquid), 0, prop_liquid),
           gwp_t = gwp_t * (1 - (uptake * prop_liquid))) %>%
    # Summarise
    mutate(category_2 = "Agriculture - N2O from livestock manure") %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"),
                                 gwp_t,
                                 gwp_t * cp / baseline_cp)), .groups = "drop")
  
  # Manure N2O (indirect)
  gwp_manureman_indirectn2o <- livestock_data %>%
    inner_join(ghg_pars_ag$gwp_manureman_indirectn2o, by = c("animal", "country", "nuts1"), multiple = "all") %>%
    # HSG: reduces dairy N excretion by 9%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "hsg_dairy") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(animal == "Milk",
                          gwp_t * (1 - (uptake * 0.09)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Precision feeding: reduces dairy & pig N excretion by 2%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "precision_feeding") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(animal %in% c("Milk", "Pig"),
                          gwp_t * (1 - (uptake * 0.02)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Cover slurry tanks: Reduce NH3 emissions by 80% for liquid manure
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_slurry") %>% select(-measure), by = "year") %>%
    left_join(prop_liquid %>% select(animal, prop_vol) %>% rename(prop_liquid = prop_vol), by = "animal") %>%
    mutate(prop_liquid = ifelse(is.na(prop_liquid), 0, prop_liquid),
           gwp_t = gwp_t * (1 - (uptake * prop_liquid * 0.8))) %>%
    # Summarise
    mutate(category_2 = "Agriculture - N2O from livestock manure") %>%
    group_by(year, country, ag_sector, nuts1, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"),
                                 gwp_t,
                                 gwp_t * cp / baseline_cp)), .groups = "drop")
  
  # Manure applied to soil
  gwp_manure_agsoils_n2o <- livestock_data %>%
    inner_join(ghg_pars_ag$gwp_manure_agsoils_n2o, by = c("animal", "country", "nuts1"), multiple = "all") %>%
    # HSG: reduces dairy N excretion by 9%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "hsg_dairy") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(animal == "Milk",
                          gwp_t * (1 - (uptake * 0.09)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Precision feeding: reduces dairy & pig N excretion by 2%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "precision_feeding") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(animal %in% c("Milk", "Pig"),
                          gwp_t * (1 - (uptake * 0.02)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Loosen soils: reduces direct N2O emissions by 40% on 20% on grassland
    left_join(uptake_lowcarbonfarming %>% filter(measure == "loosen_soils") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(category == "Animal manure applied",
                          gwp_t * (1 - (0.2 * uptake * 0.4)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Cover crops: reduce leaching in 34% of spring crops by 45%
    # Assume crops (vs grass) recieve 31.2% of manure
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_crops") %>% select(-measure), by = "year") %>%
    left_join(spring_crop_props, by = c("year", "country", "nuts1")) %>%
    mutate(gwp_t = ifelse(category == "Animal manure applied - indirect leach",
                          gwp_t * (1 - (0.34 * uptake * 0.45 * prop_area * 0.312)), # prop_area = proportional area of spring crops
                          gwp_t)) %>%
    # Summarise
    mutate(category_2 = "Agriculture - soil N2O from livestock manure") %>%
    group_by(year, country, ag_sector, nuts1, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"),
                                 gwp_t,
                                 gwp_t * cp / baseline_cp)), .groups = "drop")
  
  # Urine and dung deposited
  gwp_urinedung_agsoils_n2o <- livestock_data %>%
    inner_join(ghg_pars_ag$gwp_urinedung_agsoils_n2o, by = c("animal", "country", "nuts1"), multiple = "all") %>%
    # HSG: reduces dairy N excretion by 9%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "hsg_dairy") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(animal == "Milk",
                          gwp_t * (1 - (uptake * 0.09)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Precision feeding: reduces dairy & pig N excretion by 2%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "precision_feeding") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(animal %in% c("Milk", "Pig"),
                          gwp_t * (1 - (uptake * 0.02)),
                          gwp_t)) %>%
    select(-uptake) %>%
    # Loosen soils: reduces direct N2O emissions by 40% on 20% on grassland
    left_join(uptake_lowcarbonfarming %>% filter(measure == "loosen_soils") %>% select(-measure), by = "year") %>%
    mutate(gwp_t = ifelse(category == "Urine and dung deposited",
                          gwp_t * (1 - (0.2 * uptake * 0.4)),
                          gwp_t)) %>%
    # Summarise
    mutate(category_2 = "Agriculture - soil N2O from livestock manure") %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(ifelse(animal %in% c("Deer", "Goats", "Horses"),
                                 gwp_t,
                                 gwp_t * cp / baseline_cp * grassfed_multiplier)), .groups = "drop")
  
  # Sewage sludge applied
  gwp_sewage_agsoils_n2o <- pop_change %>%
    inner_join(ghg_pars_ag$gwp_sewage_agsoils_n2o, by = c("country", "nuts1"), multiple = "all") %>% 
    # Cover crops: reduce leaching in 34% of spring crops by 45%
    # Assume crops (vs grass) recieve 95% of biosolids
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_crops") %>% select(-measure), by = "year") %>% 
    left_join(spring_crop_props, by = c("year", "country", "nuts1")) %>%
    mutate(gwp_t = ifelse(category == "Sewage sludge applied - indirect leach",
                          gwp_t * (1 - (0.34 * uptake * 0.45 * prop_area * 0.95)), # prop_area = proportional area of spring crops
                          gwp_t)) %>% 
    # Summarise
    mutate(gwp_t = gwp_t * pop_rel) %>%
    mutate(category_2 = "Agriculture - N2O from other N") %>%
    select(year, country,  nuts1, ag_sector, ghg, gwp_t, category_2) 
  
  # Crop residues
  gwp_cropresidues_agsoils_n2o <- crop_tonnage %>%
    inner_join(ghg_pars_ag$gwp_cropresidues_agsoils_n2o, by = c("country", "nuts1"), multiple = "all") %>%
    # Cover crops: reduce leaching in 34% of spring crops by 45%
    left_join(uptake_lowcarbonfarming %>% filter(measure == "cover_crops") %>% select(-measure), by = "year") %>%
    left_join(spring_crop_props, by = c("year", "country", "nuts1")) %>%
    mutate(gwp_t = ifelse(category == "Crop residues - indirect leach",
                          gwp_t * (1 - (0.34 * uptake * 0.45 * prop_tonnage)), # prop_tonnage = proportional *tonnage* of spring crops
                          gwp_t)) %>%
    # Summarise
    mutate(gwp_t = gwp_t * tonnage / baseline_tonnage) %>%
    mutate(category_2 = "Agriculture - N2O from other N") %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(gwp_t), .groups = "drop") 
  
  # Feed imports
  gwp_feed_imported <- feed_imports %>%
    inner_join(ghg_pars_ag$gwp_feed_imported, by = c("country", "nuts1")) %>%
    # Summarise
    mutate(category_2 = "Agriculture - imported feed") %>%
    group_by(year, country, nuts1, ag_sector, ghg, category_2) %>%
    summarise(gwp_t = sum(gwp_t * mj_imported / baseline_mj_imported), .groups = "drop")
  
  ## COMBINE ALL
  gwp_ag <- bind_rows(gwp_synthn_agsoils_n2o,
                      gwp_synthn_other,
                      gwp_pesticides,
                      gwp_otherag,
                      gwp_c_agsoils,
                      gwp_entericferm_ch4,
                      gwp_manureman_directn2o ,
                      gwp_manureman_indirectn2o,
                      gwp_manure_agsoils_n2o,
                      gwp_urinedung_agsoils_n2o,
                      gwp_manureman_ch4,
                      gwp_sewage_agsoils_n2o,
                      gwp_cropresidues_agsoils_n2o,
                      gwp_feed_imported) %>%
    group_by(year, country, nuts1, category_2, ag_sector, ghg) %>%
    summarise(gwp_t = sum(gwp_t), .groups = "drop") %>%
    mutate(category_1 = "Agriculture") 
  
  
  ## PEAT ---------------------------------------------
  peat_areas <- bind_rows(scenario_areas %>%
                            filter(peat == 1 & lcm_to != "f.w") %>% 
                            group_by(country, nuts1, lcm_from) %>% 
                            summarise(area = sum(area), .groups = "drop") %>% 
                            mutate(year = 2015) %>% 
                            rename(lcm = lcm_from),
                          scenario_areas %>%
                            filter(peat == 1 & lcm_to != "f.w") %>% 
                            group_by(country, nuts1, lcm_to) %>% 
                            summarise(area = sum(area), .groups = "drop") %>% 
                            mutate(year = 2050) %>% 
                            rename(lcm = lcm_to)) %>% 
    # Make sure each country/lcm combo is present in both 2015 and 2050
    complete(year, nesting(country, nuts1, lcm), fill = list(area = 0)) %>% 
    # Annualise (interpolate between 5 years)
    add_row(year = 2016:2049) %>% 
    complete(year, nesting(country, nuts1, lcm), fill = list(area = NA)) %>%
    filter(!(is.na(country) | is.na(lcm))) %>%
    group_by(country, nuts1, lcm) %>% 
    mutate(area = zoo::na.approx(area)) %>% 
    ungroup() 
  
  # Identify bog / fen as near-natural (existing) or rewetted (new)
  peat_areas <- peat_areas %>%
    # Get baseline area
    filter(lcm %in% c("f.s", "b.g") & year == 2015) %>%
    mutate(area_existing = area) %>%
    select(-area, -year) %>%
    crossing(year = unique(peat_areas$year)) %>%
    # Get area of new bog/fen
    left_join(peat_areas %>%
                filter(lcm %in% c("f.s", "b.g")),
              by = c("country", "nuts1", "lcm", "year")) %>%
    mutate(area_new = area - area_existing) %>%
    select(-area) %>%
    # Tidy
    gather(condition, area, -country, -nuts1, -lcm, -year) %>%
    mutate(condition = ifelse(condition == "area_existing", "Existing", "New (rewetted)")) %>%
    # Join with non-bog/fen peat (age = Existing)
    bind_rows(peat_areas %>% 
                filter(!lcm %in% c("f.s", "b.g")) %>% 
                mutate(lcm = case_when(lcm %in% c("a.h_organic", "i.g_organic") ~ substr(lcm, 1, 3),
                                       TRUE ~ lcm),
                       condition = case_when(lcm %in% c("a.h_palud", "i.g_palud") ~ "Paludiculture",
                                             TRUE ~ "Existing")) %>% 
                group_by(country, nuts1, lcm, year, condition) %>%
                summarise(area = sum(area), .groups = "drop")) %>%
    select(year, country, nuts1, lcm, condition, area)
  
  # Adjust bog & a.h EF according to scenario-specific within-land-cover restoration
  gwp_peat <- peat_areas %>%
    # Get all combinations of scenario/year/country, with ambition level
    select(year, country, nuts1) %>%
    unique() %>%
    # Get emissions factors for b.g
    left_join(ghg_pars_peat$gwp_peat %>%
                filter(lcm == "b.g"),
              by = "country", multiple = "all") %>%
    # Fix initial prop of rewetted, paludiculture & sustman to 0
    mutate(prop = ifelse(condition == "New (rewetted)", 0, prop)) %>%
    # Get scenario params for each ambition level (crop and bog only)
    mutate(ambition_peatland_up = ambitions$peatland_up/6) %>% # From scenario ambition, divided into 6x 5-yearly periods
    # Make changes to bog - swap extraction/modified for re-wetted
    group_by(country, ghg, year, lcm) %>%
    mutate(prop = ifelse(condition == "Extraction" & year >= 2020, prop - prop * ambition_peatland_up * (year - 2020) / 5, prop),
           prop = ifelse(condition == "Modified" & year >= 2020,prop - prop * ambition_peatland_up * (year - 2020) / 5, prop),
           prop = ifelse(prop < 0, 0, prop),
           prop = ifelse(condition == "New (rewetted)", 1 - sum(prop), prop)) %>%
    ungroup() %>% 
    filter(prop != 0) %>%
    # Calculated weighted mean across categories within LCM
    group_by(year, country, nuts1, ghg, lcm) %>%
    summarise(gwp_t = sum(gwp_t * prop), .groups = "drop") %>%
    # All 'Existing' (i.e don't apply to land converted to bog)
    mutate(condition = "Existing") %>%
    # Combine with other EFs
    bind_rows(peat_areas %>%
                select(year, country, nuts1) %>%
                unique() %>%
                # Get emissions factors for everything else
                left_join(ghg_pars_peat$gwp_peat %>%
                            filter(!(lcm == "b.g" & condition != "New (rewetted)")),
                          by = "country", multiple = "all") %>%
                select(-prop, -category))
  
  # Join with peat areas
  # Add categories & summarise
  gwp_peat <- gwp_peat %>%
    inner_join(peat_areas, by = c("year", "country", "nuts1", "condition", "lcm")) %>% 
    mutate(category_1 = "Peatland",
           category_2 = case_when(lcm %in% c("a.h", "i.g", "f.s", "a.h_organic", "a.h_palud", "i.g_organic", "i.g_palud") ~ "Lowland peat",
                                  lcm %in% c("b.g", "n.g", "c.g", "a.g", "h.r", "b.g") ~ "Upland peat",
                                  lcm %in% c("c.w", "b.w", "c.w_pinewood") ~ "Forested peat",
                                  TRUE ~ NA_character_)) %>%
    # Summarise across land covers
    group_by(country, nuts1, year, category_1, category_2, ghg) %>%
    summarise(gwp_t = sum(gwp_t * area), .groups = "drop")
  
  
  ## WOODLAND -------------------------------------------
  # Set up woodland types
  woodland_type_props <- tibble(lcm = c("b.w", "b.w", "c.w"),
                                type = c("Semi-natural broadleaved woodland (unmanaged)", "Managed broadleaved woodland", "Managed coniferous woodland"),
                                prop = c(0.8, 0.2, 1))
  
  # New woodland
  new_wood_areas <- scenario_areas %>%
    filter(lcm_to %in% c("c.w", "b.w") & lcm_from != lcm_to) %>% 
    group_by(country, nuts1, lcm_to, yc_sbi, yc_sok, yc_sp, yc_ss) %>%
    summarise(area = sum(area), .groups = "drop") %>% 
    rename(lcm = lcm_to) %>% 
    # Annualise (same amount of new woodland each year)
    crossing(years) %>% 
    mutate(area = area / nrow(years)) %>% 
    # Account for open ground in new woodland(15%)
    mutate(area = area * (1 - 0.15)) %>%
    # Add woodland type
    left_join(woodland_type_props, by = "lcm", multiple = "all") %>%
    mutate(area = area * prop) %>%
    select(-prop) %>%
    # Get matching yield class
    mutate(yc = case_when(type == "Semi-natural broadleaved woodland (unmanaged)" ~ yc_sok,
                          type == "Managed broadleaved woodland" ~ yc_sok,
                          type == "Managed coniferous woodland" ~ yc_ss)) %>%
    # Establishment - reduce yc for natregen (semi-natural woodland only)
    mutate(natregen = 0) %>% 
    # Round YC down to nearest even number
    mutate(yc = floor(yc / 2) * 2,
           yc = case_when(type != "Managed coniferous woodland" & yc <= 4 ~ 4,
                          type == "Managed coniferous woodland" & yc <= 6 ~ 6,
                          TRUE ~ yc)) %>%
    # Summarise
    group_by(year, country, nuts1, lcm, type, natregen, yc) %>%
    summarise(area = sum(area), .groups = "drop") %>%
    # Cross with age cohort distribution
    crossing(age = seq(0, 35, by = 1)) %>%
    mutate(year = year + age) %>%
    filter(year >= 2015 & year <= 2050)

  # Lost woodland
  extinct_wood_areas <- scenario_areas %>% 
    filter(lcm_from %in% c("c.w", "b.w") & lcm_from != lcm_to) %>% 
    mutate(area = ifelse(lcm_to == "c.w_wind", area * wind_footprint, area)) %>%
    group_by(country, nuts1, lcm_from, yc_sbi, yc_sok, yc_sp, yc_ss) %>%
    summarise(area = sum(area), .groups = "drop") %>% 
    rename(lcm = lcm_from) %>% 
    # Annualise (same area lost each year)
    crossing(years) %>%
    mutate(area = area/nrow(years)) %>% 
    # Add woodland type
    left_join(woodland_type_props, by = "lcm", multiple = "all") %>%
    mutate(area = area * prop) %>% 
    select(-prop) %>% 
    # Get matching yield class
    mutate(yc = case_when(type == "Semi-natural broadleaved woodland (unmanaged)" ~ yc_sok,
                          type == "Managed broadleaved woodland" ~ yc_sok,
                          type == "Managed coniferous woodland" ~ yc_ss)) %>%
    # Establishment - reduce yc for natregen (semi-natural woodland only)
    mutate(natregen = 0) %>% 
    # Round YC down to nearest even number
    mutate(yc = floor(yc / 2) * 2,
           yc = case_when(type != "Managed coniferous woodland" & yc <= 4 ~ 4,
                          type == "Managed coniferous woodland" & yc <= 6 ~ 6,
                          TRUE ~ yc)) %>%
    # Summarise
    group_by(year, country, nuts1, lcm, type, yc) %>% 
    summarise(area = sum(area), .groups = "drop") %>% 
    # Get year felled
    rename(year_felled = year) %>% 
    crossing(year = 2015:2050) %>%
    # Cross with age cohort distribution - simplify by applying mean cohort
    left_join(ghg_pars_wood$existing_wood_age %>%
                filter(country == "England") %>%
                bind_rows((.) %>%
                            filter(lcm == "c.w") %>%
                            mutate(lcm = "c.w_pinewood")) %>%
                select(-country, -age),
              by = "lcm", multiple = "all") %>%
    mutate(area = area * prop,
           age = year - cohort) %>% 
    select(-prop)
  
  # Existing woodland (present 2015 through 2050)
  extant_wood_areas <- scenario_areas %>%
    filter((lcm_to %in% c("c.w", "b.w") & lcm_from == lcm_to) | lcm_to == "c.w_wind") %>%
    mutate(area = ifelse(lcm_to == "c.w_wind", area * (1-wind_footprint), area),
           lcm_to = gsub("_wind", "", lcm_to)) %>% 
    group_by(country, nuts1, lcm_to, yc_sbi, yc_sok, yc_sp, yc_ss) %>%
    summarise(area = sum(area), .groups = "drop") %>% 
    rename(lcm = lcm_to) %>%
    # Annualise (same area in every year)
    crossing(years) %>% 
    # Summarise
    group_by(country,nuts1, lcm, yc_sbi, yc_sok, yc_sp, yc_ss, year) %>%
    summarise(area = sum(area), .groups = "drop") %>% 
    # Add woodland type
    left_join(woodland_type_props, by = "lcm", multiple = "all") %>%
    mutate(area = area * prop) %>% 
    select(-prop) %>% 
    # Get matching yield class
    mutate(yc = case_when(type == "Semi-natural broadleaved woodland (unmanaged)" ~ yc_sok,
                          type == "Managed broadleaved woodland" ~ yc_sok,
                          type == "Semi-natural coniferous woodland (unmanaged)" ~ yc_sp,
                          type == "Managed coniferous woodland" ~ yc_ss),
           # Round down to nearest even number
           yc = floor(yc / 2) * 2,
           # Minimum allowed = lowest in WCC tables
           yc = case_when(type != "Managed coniferous woodland" & yc <= 4 ~ 4,
                          type == "Managed coniferous woodland" & yc <= 6 ~ 6,
                          TRUE ~ yc)) %>%
    # Summarise
    group_by(year, country, nuts1, lcm, type, yc) %>% 
    summarise(area = sum(area), .groups = "drop") %>% 
    # Cross with age cohort distribution 
    left_join(ghg_pars_wood$existing_wood_age %>% 
                filter(country == "England") %>% 
                bind_rows((.) %>% 
                            filter(lcm == "c.w") %>% 
                            mutate(lcm = "c.w_pinewood")) %>% 
                select(-country, -age),
              by = "lcm", multiple = "all") %>%
    mutate(area = area * prop,
           age = year - cohort) %>% 
    select(-prop, -cohort)
  
  # Combined areas with flux (note year_felled)
  gwp_woodland <- bind_rows(extant_wood_areas %>%
                              mutate(category_1 = "Woodland",
                                     category_2 = "Existing woodland",
                                     natregen = FALSE,
                                     year_felled = NA),
                            new_wood_areas %>%
                              mutate(category_1 = "Woodland",
                                     category_2 = "New woodland",
                                     year_felled = NA),
                            extinct_wood_areas %>%
                              mutate(category_1 = "Woodland",
                                     category_2 = "Cleared woodland",
                                     natregen = FALSE,
                                     year_felled = year_felled - cohort)) %>%
    # Join with WCC seq data
    left_join(ghg_pars_wood$ghg_wood_ann, by = c("type", "yc", "natregen", "age" = "year", "year_felled")) %>% 
    # Summarise
    group_by(country, nuts1, year, category_1, category_2) %>%
    summarise(gwp_t = -sum(area * annual_flux), .groups = "drop") %>%
    mutate(ghg = "CO2")

  
  ## AGROFORESTRY ---------------------------------------------
  # Calculate areas to agroforestry
  agroforestry_areas <- scenario_areas %>%
    mutate(area = case_when(str_detect(lcm_to, "wind") & str_detect(lcm_to, "silvoa|silvop") ~ area*1-wind_footprint,
                            TRUE ~ area),
           lcm_to = case_when(str_detect(lcm_to, "wind") & str_detect(lcm_to, "silvoa|silvop") ~ str_remove(lcm_to, "wind_"),
                              TRUE ~ lcm_to)) %>% 
    filter(str_detect(lcm_to, "silvoa|silvop")) %>% 
    group_by(country, nuts1, lcm_to) %>% 
    summarise(area = sum(area), .groups = "drop") %>%
    rename(lcm = lcm_to) %>% 
    # Add year data
    mutate(area = area/nrow(years)) %>%
    crossing(years) %>% 
    arrange(year, country) 
  
  # Calculate GWP
  gwp_agroforestry <- bind_rows(
    # Trees
    agroforestry_areas %>%
      left_join(ghg_pars_agroforestry %>% select(-contains("scrub")), by = "lcm") %>%
      # Cross with all possible ages
      crossing(age = seq(0, 35, by = 1)) %>%
      mutate(year = year + age) %>%
      filter(year >= 2015 & year <= 2050) %>% 
      # Join with WCC
      left_join(filter(ghg_pars_wood$ghg_wood_ann, is.na(year_felled)), by = c("type", "age" = "year")) %>% 
      # Sum annual flux across woodland types
      group_by(country, nuts1, year, type) %>%
      summarise(gwp_t = -sum(area * annual_flux * area_multiplier), .groups = "drop") %>%
      mutate(category_1 = "Agroforestry") %>% 
      rename(category_2 = type) %>%
      mutate(ghg = "CO2"),
    # Soil
    agroforestry_areas %>%
      # Assume arable -> silvoarable is halfway between arable -> grass and arable -> wood (country-specific)
      left_join(ghg_pars_luc$gwp_luc_soil %>%
                  filter(lcm_from == "a.h" & lcm_to %in% c("i.g", "b.w")) %>%
                  filter(ghg != "N2O") %>% 
                  group_by(country, ghg, year) %>%
                  summarise(gwp_t = mean(gwp_t), .groups = "drop") %>%
                  rename(age = year) %>%
                  mutate(age = age - 1) %>%
                  filter(age >= 0),
                by = "country") %>% 
      mutate(year = year + age) %>%
      filter(year <= 2050) %>%
      left_join(select(ghg_pars_agroforestry, lcm, type, area_multiplier), by = "lcm") %>%
      # Summarise and tidy
      group_by(country, nuts1, year, ghg, type) %>%
      summarise(gwp_t = sum(gwp_t * area * area_multiplier), .groups = "drop") %>%
      mutate(category_1 = "Agroforestry") %>%
      rename(category_2 = type)) %>% 
    # Sum across soil/biomass
    group_by(country, nuts1, year, category_1, category_2, ghg) %>% 
    summarise(gwp_t = sum(gwp_t), .groups = "drop")
  
  
  ## WOOD PASTURE ---------------------------------------------
  # Calculate areas to woodpasture
  woodpasture_areas <- scenario_areas %>% 
    filter(lcm_to == "s.g_woodpa" & lcm_from != lcm_to) %>% 
    mutate(yc = case_when(floor(yc_sok/2) * 2 <= 4 ~ 4,
                          TRUE ~ floor(yc_sok/2) * 2)) %>% 
    group_by(country, nuts1, lcm_to, yc) %>% 
    summarise(area = sum(area)) %>% 
    # Add year data
    mutate(area = area/nrow(years)) %>%
    crossing(years) %>% 
    rename(lcm = lcm_to) 
  
  # Calculate GWP
  gwp_woodpasture <- woodpasture_areas %>%
    left_join(ghg_pars_agroforestry, 
              by = "lcm") %>%
    crossing(age = seq(0, 35, by = 1)) %>%
    mutate(year = year + age) %>%
    filter(year >= 2015 & year <= 2050) %>%
    # Join with WCC
    left_join(filter(ghg_pars_wood$ghg_wood_ann, is.na(year_felled)), by = c("type", "yc", "age" = "year"), multiple = "all") %>%
    # Calculate scrub flux
    mutate(annual_flux_scrub = ifelse(age < scrub_years, scrub_c_density/scrub_years * scrub_footprint * 44/12, 0)) %>%
    select(-scrub_footprint, -scrub_c_density, -scrub_years) %>%
    # Sum annual flux across woodland types
    group_by(country, nuts1, year) %>%
    summarise(gwp_t = -sum(area * annual_flux * area_multiplier + area * annual_flux_scrub), .groups = "drop") %>%
    mutate(category_1 = "Wood pasture",
           category_2 = "Wood pasture",
           ghg = "CO2")
  
  
  ## HEDGES ---------------------------------------------
  # Calculate length of new hedge p/a
  hedge_add <- ambition_combos %>%
    # Get ambition level
    select(ambition_hedge) %>%
    rename(ambition = ambition_hedge) %>%
    left_join(scenario_params %>%
                filter(grepl("hedge", par) & par != "hedge_stopyear") %>%
                rename('frac' = val) %>%
                select(frac, ambition) %>%
                left_join(scenario_params %>%
                            filter(par == "hedge_stopyear") %>%
                            select(ambition, val) %>%
                            rename('stopyear' = val),
                          by = "ambition"),
              by = "ambition") %>%
    # Cross with baseline hedge length
    crossing(ghg_pars_hedges$hedge_length_nuts) %>%
    # Cross with all years 2015-2100
    crossing(years) %>% 
    # Calculate additional km per year
    mutate(add = ifelse(year >= 2020 & year < stopyear, length * frac / 5, 0))
  
  gwp_hedges <- bind_rows(
    # Calculate annual biomass stock change
    hedge_add %>%
      # Cross with 5-yarly GHG flux
      crossing(ghg_pars_hedges$gwp_hedges) %>%
      mutate(year = year + age) %>%
      filter(year <= 2050) %>%
      # Summarise and tidy
      group_by(country, nuts1, year, ghg) %>%
      summarise(gwp_t = sum(gwp_t * add), .groups = "drop") %>%
      mutate(category_1 = "Hedgerows",
             category_2 = "Hedgerows - biomass"),
    # Calculate annual soil carbon change
    hedge_add %>%
      # Assume arable -> hedge is halfway between arable -> grass and arable -> wood (country-specific)
      left_join(ghg_pars_luc$gwp_luc_soil %>%
                  filter(ghg != "N2O") %>% 
                  filter(lcm_from == "a.h" & lcm_to %in% c("i.g", "b.w")) %>%
                  group_by(country, ghg, year) %>%
                  summarise(gwp_t = mean(gwp_t), .groups = "drop") %>%
                  rename(age = year) %>%
                  mutate(age = age - 1) %>%
                  filter(age >= 0),
                by = "country", multiple = "all") %>%
      mutate(year = year + age) %>%
      filter(year <= 2050) %>%
      # Convert km to ha (1.5m wide)
      mutate(add = add * 0.0015 * 100) %>%
      # Summarise and tidy
      group_by(country, nuts1, year, ghg) %>%
      summarise(gwp_t = sum(gwp_t * add * 0.5), .groups = "drop") %>% # Apply to 50% of area (arable only)
      mutate(category_1 = "Hedgerows",
             category_2 = "Hedgerows - soils"))
  
  
  ## COMBINE ---------------------------------------------
  # Combined all
  gwp_combined <- bind_rows(gwp_luc,
                            gwp_ag,
                            gwp_peat,
                            gwp_woodland,
                            gwp_agroforestry,
                            gwp_woodpasture,
                            gwp_hedges) %>% 
    nest(gwp_combined = c(year:ag_sector))
  
  
  return(gwp_combined)

}

# SAVE FUNCTION & PARAMS  ---------------------------------------------

save(do_ghg_fun, 
     ambition_combos,
     scenario_params,
     ghg_pars_luc,
     ghg_pars_ag, pop_change, prop_liquid, lowcarbonfarming,
     ghg_pars_peat,
     ghg_pars_wood, 
     ghg_pars_agroforestry,
     ghg_pars_hedges, 
     file = here("data", "ghg_pars_funs.RData"))

