# 4b FOOD MODEL ################################
source("./code/0_setup.R")


# LOAD DATA ---------------------------------------------
# Load food function and data
load(here("data", "food_params.RData"))
load(here("data", "food_fun.RData"))


# FOOD MODEL FUNCTION SETUP ---------------------------------------------
## For use in 5_generate_scenarios
food_fun_run <- function(scenario_areas, food_params, ambition_combos){
  
  # Select hedge scenario
  hedge_area <- food_params$.hedge_areas %>% 
    filter(scenario == ambition_combos$scenario)
  
  # TIDY SCENARIOS DATA
  # Sum across woodland variables
  areas <- scenario_areas %>% 
    filter(!lcm_to %in% c("b.t", "b.w", "c.w", "c.w_wind", "c.l", "f.w", "r.k", "solar")) %>%
    group_by(scenario, country, nuts1, lcm_to, alc) %>% 
    summarise(area = sum(area),
              .groups = "drop") %>% 
    # Add organic identifier
    mutate(organic = case_when((grepl("organic", lcm_to)) ~ TRUE,
                               TRUE ~ FALSE)) %>% 
    # Yield penalty (shading from silvoarable) and area penalty (equivalent to area removed from production)
    # Join scenario areas with agroforestry footprint
    left_join(food_params$.agroforestry_footprint,
              by = c("lcm_to" = "lcm")) %>% 
    mutate(yield_multiplier = case_when(is.na(yield_multiplier) ~ 1,
                                        TRUE ~ yield_multiplier),
           area_multiplier = case_when(is.na(area_multiplier) ~ 1,
                                       TRUE ~ area_multiplier)) %>% 
    # Account for % taken out of production for wind turbine footprint - value from wind_footprint in 1_setup
    mutate(area_multiplier = case_when(str_detect(lcm_to, "wind") ~ 1-wind_footprint,
                                       lcm_to == "solar" ~ 1-solar_footprint,
                                       TRUE ~ area_multiplier)) %>%
    # Hedge adjustments
    # Calculate proportion of a.h and i.g removed
    mutate(prop_a.h_removed = hedge_area$ha_removed/2/sum(area[lcm_to %in% c("a.h", "a.h_organic", "a.h_wind", "a.h_wind_organic")]),
           prop_i.g_removed = hedge_area$ha_removed/2/sum(area[lcm_to %in% c("i.g", "i.g_organic", "i.g_wind", "i.g_wind_organic")])) %>%
    ungroup() %>%
    # Update areas
    mutate(area = case_when(lcm_to %in% c("a.h", "a.h_organic", "a.h_wind", "a.h_wind_organic") ~ area * (1-prop_a.h_removed),
                            lcm_to %in% c("i.g", "i.g_organic", "i.g_wind", "i.g_wind_organic") ~ area * (1-prop_i.g_removed),
                            TRUE ~ area)) %>%
    # Remove unnecessary columns
    select(-prop_a.h_removed, -prop_i.g_removed) %>%
    # Calculate n (0.0625-ha pixels) from area (ha)
    mutate(n = area*16) %>% 
    # Remove "_wind" from lcm_to classes - footprint alread accounted for
    mutate(lcm_to = str_remove(lcm_to, "_wind")) %>%
    ungroup() %>% 
    select(-scenario)
  
  # RUN FOOD PRODUCTION FUNCTION
  food_production_fun(areas, params = food_params, waste = FALSE, ambition_combos = ambition_combos)
  
}


# SAVE ---------------------
save(food_params,
     food_production_fun,
     food_fun_run, file = here("data", "food_pars_fun.RData"))

