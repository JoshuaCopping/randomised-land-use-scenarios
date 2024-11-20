# 5 GENERATE SCENARIOS UPDATED ######################
source("./code/0_setup.R")


# LOAD DATA ----------------------------
# Load opportunity areas from 3_urban_renewable_lans_update
load(here("outputs", "opportunity_areas.RData"))
# Food production function & params
load(here("data", "food_pars_fun.RData"))
# GHG function & params
load(here("data", "ghg_pars_funs.RData"))
# Bird function & params
load(here("data", "birds_pars_funs.RData"))


# Update opportunities to remove urban expansion and renewables from available area
opportunity_areas <- opportunity_areas %>% 
  mutate(peatland_lo_op = case_when(lcm_from != lcm_to ~ FALSE,
                                 TRUE ~ peatland_lo_op),
         peatland_up_op = case_when(lcm_from != lcm_to ~ FALSE,
                                 TRUE ~ peatland_up_op),
         # saltmarsh_op = case_when(lcm_from != lcm_to ~ FALSE,
         #                          TRUE ~ saltmarsh_op),
         woodland_op = case_when(lcm_from != lcm_to ~ FALSE,
                                 TRUE ~ woodland_op),
         woodpa_op = case_when(lcm_from != lcm_to ~ FALSE,
                               TRUE ~ woodpa_op),
         grassland_op = case_when(lcm_from != lcm_to ~ FALSE,
                                  TRUE ~ grassland_op),
         silvoa_op = case_when(lcm_to %in% c("b.t", "solar") | !woodland_op ~ FALSE,
                               TRUE ~ silvoa_op),
         silvop_op = case_when(lcm_to %in% c("b.t", "solar") | !woodland_op ~ FALSE,
                               TRUE ~ silvop_op),
         organic_op = case_when(lcm_to %in% c("b.t", "solar") ~ FALSE,
                                TRUE ~ organic_op))


# GENERATE SCENARIOS TESTING ----------------------------
# Create ambition levels/scenario testing if running outside main function

# n_scenario <- 100
# 
# ambitions <- tibble(scenario = seq(1, n_scenario, by = 1),
#                     peatland_lo = runif(n_scenario, 0, 1),
#                     peatland_up = runif(n_scenario, 0, 1),
#                     woodland = rbeta(n_scenario, 1, 2),
#                     woodpa = rbeta(n_scenario, 1, 2),
#                     grassland = rbeta(n_scenario, 2, 1),
#                     silvoa = rbeta(n_scenario, 2, 1),
#                     silvop = rbeta(n_scenario, 2, 1),
#                     organic = rbeta(n_scenario, 2, 1)) %>%
#   # Randomly replaces one value with 0 for each measure
#   mutate(peatland_lo = replace(peatland_lo, sample(row_number(), 1, replace = FALSE), 0),
#          peatland_up = replace(peatland_up, sample(row_number(), 1, replace = FALSE), 0),
#          woodland = replace(woodland, sample(row_number(), 1, replace = FALSE), 0),
#          woodpa = replace(woodpa, sample(row_number(), 1, replace = FALSE), 0),
#          grassland = replace(grassland, sample(row_number(), 1, replace = FALSE), 0),
#          silvoa = replace(silvoa, sample(row_number(), 1, replace = FALSE), 0),
#          silvop = replace(silvop, sample(row_number(), 1, replace = FALSE), 0),
#          organic = replace(organic, sample(row_number(), 1, replace = FALSE), 0))
# 
# ambitions <- slice(ambitions, 9)
# ambitions
# baseline = FALSE
# country = "All"


# GENERATE SCENARIOS FUNCTION SETUP ----------------------------
## Function ----
scenario_creation_fun <- function(opportunity_areas, ambitions, country = "All", baseline){
  
  # Filter country
  country_filter <- country
  
  if(country %in% c("England", "Northern Ireland", "Scotland", "Wales")){
    opportunity_areas <- opportunity_areas %>% filter(country == country_filter)
  }
  
  
  # Reset scenario 1 opportunity areas for 2015 scenario (remove urban and renewable energy)
  if(baseline == TRUE & ambitions$scenario == 1){
    opportunity_areas_updated <- opportunity_areas %>% 
      mutate(lcm_to = lcm_from)
  } else {
    opportunity_areas_updated <- opportunity_areas
  }
  
  
  # Calculate total opportunity areas and target areas
  targets <- bind_rows(opportunity_areas_updated %>% 
                         filter(peatland_lo_op) %>%
                         group_by(measure = "peatland_lo") %>% 
                         summarise(op_area = sum(area)) %>% 
                         mutate(target = op_area * ambitions$peatland_lo),
                       opportunity_areas_updated %>% 
                         filter(peatland_up_op) %>%
                         group_by(measure = "peatland_up") %>% 
                         summarise(op_area = sum(area)) %>% 
                         mutate(target = op_area * ambitions$peatland_up),
                       opportunity_areas_updated %>% 
                         filter(woodland_op) %>%
                         group_by(measure = "woodland") %>% 
                         summarise(op_area = sum(area)) %>% 
                         mutate(target = op_area * ambitions$woodland),
                       opportunity_areas_updated %>% 
                         filter(woodpa_op) %>%
                         group_by(measure = "woodpa") %>% 
                         summarise(op_area = sum(area)) %>% 
                         mutate(target = op_area * ambitions$woodpa),
                       opportunity_areas_updated %>% 
                         filter(grassland_op) %>%
                         group_by(measure = "grassland") %>% 
                         summarise(op_area = sum(area)) %>% 
                         mutate(target = op_area * ambitions$grassland),
                       opportunity_areas_updated %>% 
                         filter(silvoa_op) %>%
                         group_by(measure = "silvoa") %>% 
                         summarise(op_area = sum(area)) %>% 
                         mutate(target = op_area * ambitions$silvoa),
                       opportunity_areas_updated %>% 
                         filter(silvop_op) %>%
                         group_by(measure = "silvop") %>% 
                         summarise(op_area = sum(area)) %>% 
                         mutate(target = op_area * ambitions$silvop),
                       opportunity_areas_updated %>% 
                         filter(organic_op) %>%
                         group_by(measure = "organic") %>% 
                         summarise(op_area = sum(area)) %>% 
                         mutate(target = op_area * ambitions$organic),
                       tibble(measure = "scenario",
                              target = ambitions$scenario))
  
  
  ## UPDATE LCMs ----
  # Update opportunity areas with new land from measures
  
  ### Peatland lowland ----
  df <- opportunity_areas_updated %>%
    mutate(scenario = targets$target[targets$measure == "scenario"]) %>% 
    arrange(-peatland_lo_op, -alc) %>% 
    group_by(alc) %>%
    mutate(area_suitable = sum(area[peatland_lo_op])) %>% 
    group_by(alc, area_suitable) %>% 
    nest() %>%
    ungroup() %>% 
    mutate(target = targets$target[targets$measure == "peatland_lo"],
           area_suitable_cumul = cumsum(area_suitable),
           prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
           remainder = unique(target) - sum(prop_needed * area_suitable),
           prop_needed = ifelse(alc == alc[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed)) %>%
    select(alc, data, prop_needed) %>% 
    unnest(cols = c(data))
  
  df <- bind_rows(df %>% # land converted to peatland lowland
                    filter(prop_needed != 0 & peatland_lo_op) %>% 
                    mutate(lcm_to = "f.s",
                           area = area * prop_needed),
                  df %>% # land remaining from peatland_lo opportunity conversion
                    filter(prop_needed != 0 & peatland_lo_op) %>% 
                    mutate(area = area * (1 - prop_needed)),
                  df %>% # unused peatland opportunity or non-opportunity land
                    filter((prop_needed == 0 | !peatland_lo_op))) %>% 
    select(-prop_needed) %>% 
    filter(area != 0) 
  
  
  ### Peatland upland ----
  df <- df %>%
    mutate(scenario = targets$target[targets$measure == "scenario"]) %>% 
    arrange(-peatland_up_op, -alc) %>% 
    group_by(alc) %>%
    mutate(area_suitable = sum(area[peatland_up_op])) %>% 
    group_by(alc, area_suitable) %>% 
    nest() %>%
    ungroup() %>% 
    mutate(target = targets$target[targets$measure == "peatland_up"],
           area_suitable_cumul = cumsum(area_suitable),
           prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
           remainder = unique(target) - sum(prop_needed * area_suitable),
           prop_needed = ifelse(alc == alc[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed)) %>%
    select(alc, data, prop_needed) %>% 
    unnest(cols = c(data))
  
  df <- bind_rows(df %>% # land converted to peatland upland
                    filter(prop_needed != 0 & peatland_up_op) %>% 
                    mutate(lcm_to = "b.g",
                           area = area * prop_needed),
                  df %>% # land remaining from peatland opportunity conversion
                    filter(prop_needed != 0 & peatland_up_op) %>% 
                    mutate(area = area * (1 - prop_needed)),
                  df %>% # unused peatland opportunity or non-opportunity land
                    filter((prop_needed == 0 | !peatland_up_op))) %>% 
    select(-prop_needed) %>% 
    filter(area != 0) 
  
  
  ### Woodland ----
  df <- df %>%
    mutate(woodland_op = ifelse((lcm_to %in% c("b.g", "f.s") & lcm_from != lcm_to) | (lcm_to == "s.m" & lcm_from != lcm_to), 
                                FALSE, 
                                woodland_op)) %>% 
    arrange(-woodland_op, -alc) %>% 
    group_by(alc) %>%
    mutate(area_suitable = sum(area[woodland_op])) %>% 
    group_by(alc, area_suitable) %>% 
    nest() %>%
    ungroup() %>% 
    mutate(priority = row_number(),
           target = targets$target[targets$measure == "woodland"],
           area_suitable_cumul = cumsum(area_suitable),
           prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
           remainder = unique(target) - sum(prop_needed * area_suitable),
           prop_needed = ifelse(priority == priority[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed)) %>%
    select(alc, data, prop_needed) %>% 
    unnest(cols = c(data))
  
  df <- bind_rows(df %>% 
                    filter(prop_needed != 0 & woodland_op) %>% 
                    mutate(lcm_to = "b.w",
                           area = area * prop_needed * 2/3),
                  df %>% 
                    filter(prop_needed != 0 & woodland_op) %>% 
                    mutate(lcm_to = "c.w",
                           area = area * prop_needed * 1/3),
                  df %>% 
                    filter(prop_needed != 0 & woodland_op) %>% 
                    mutate(area = area * (1 - prop_needed)),
                  df %>% 
                    filter((prop_needed == 0 | !woodland_op))) %>% 
    select(-prop_needed) %>% 
    filter(area != 0) 
  
  
  ### Woodpasture ----
  df <-  df %>%
    mutate(woodpa_op = ifelse((lcm_to %in% c("b.g", "f.s") & lcm_from != lcm_to) | (lcm_to == "s.m" & lcm_from != lcm_to) | (lcm_to %in% c("b.w", "c.w") & lcm_from != lcm_to), 
                              FALSE,
                              woodpa_op)) %>% 
    arrange(-woodpa_op, -alc) %>% 
    group_by(alc) %>%
    mutate(area_suitable = sum(area[woodpa_op])) %>% 
    group_by(alc, area_suitable) %>% 
    nest() %>%
    ungroup() %>% 
    mutate(priority = row_number(),
           target = targets$target[targets$measure == "woodpa"])
  
  if(unique(df$target) <= sum(df$area_suitable)){
    df <- df %>%
      mutate(area_suitable_cumul = cumsum(area_suitable),
             prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
             remainder = unique(target) - sum(prop_needed * area_suitable),
             prop_needed = ifelse(priority == priority[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed))
  } else{
    df <- df %>%
      mutate(prop_needed = 1)
  }
  
  df <- df %>% 
    select(alc, data, prop_needed) %>% 
    unnest(cols = c(data))
  
  df <- bind_rows(df %>% 
                    filter(prop_needed != 0 & woodpa_op) %>% 
                    mutate(lcm_to = "s.g_woodpa",
                           area = area * prop_needed),
                  df %>% 
                    filter(prop_needed != 0 & woodpa_op) %>% 
                    mutate(area = area * (1 - prop_needed)),
                  df %>% 
                    filter((prop_needed == 0 | !woodpa_op))) %>% 
    select(-prop_needed) %>% 
    filter(area != 0) 
  
  
  ### Grassland ----
  df <- df %>%
    mutate(grassland_op = ifelse((lcm_to %in% c("b.g", "f.s") & lcm_from != lcm_to) | (lcm_to == "s.m" & lcm_from != lcm_to) | (lcm_to %in% c("b.w", "c.w") & lcm_from != lcm_to) | (lcm_to == "s.g_woodpa" & lcm_from != lcm_to), 
                                 FALSE, 
                                 grassland_op)) %>% 
    arrange(-grassland_op, -alc) %>% 
    group_by(alc) %>%
    mutate(area_suitable = sum(area[grassland_op])) %>% 
    group_by(alc, area_suitable) %>% 
    nest() %>%
    ungroup() %>% 
    mutate(priority = row_number(),
           target = targets$target[targets$measure == "grassland"])
  
  if(unique(df$target) <= sum(df$area_suitable)){
    df <- df %>%
      mutate(area_suitable_cumul = cumsum(area_suitable),
             prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
             remainder = unique(target) - sum(prop_needed * area_suitable),
             prop_needed = ifelse(priority == priority[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed))
  } else{
    df <- df %>%
      mutate(prop_needed = 1)
  }
  
  df <- df %>% 
    select(alc, data, prop_needed) %>% 
    unnest(cols = c(data))
  
  df <- bind_rows(df %>% 
                    filter(prop_needed != 0 & grassland_op) %>% 
                    mutate(lcm_to = "s.g",
                           area = area * prop_needed),
                  df %>% 
                    filter(prop_needed != 0 & grassland_op) %>% 
                    mutate(area = area * (1 - prop_needed)),
                  df %>% 
                    filter((prop_needed == 0 | !grassland_op))) %>% 
    select(-prop_needed) %>% 
    filter(area != 0) 
  
  
  ### Silvoarable ----
  df <- df %>%
    mutate(silvoa_op = ifelse((lcm_to %in% c("b.g", "f.s") & lcm_from != lcm_to) | (lcm_to == "s.m" & lcm_from != lcm_to) | (lcm_to %in% c("b.w", "c.w") & lcm_from != lcm_to) | (lcm_to == "s.g_woodpa" & lcm_from != lcm_to) | (lcm_to == "s.g" & lcm_from != lcm_to), 
                              FALSE, 
                              silvoa_op)) %>% 
    arrange(-silvoa_op, -alc) %>% 
    group_by(alc) %>%
    mutate(area_suitable = sum(area[silvoa_op])) %>% 
    group_by(alc, area_suitable) %>% 
    nest() %>%
    ungroup() %>% 
    mutate(target = targets$target[targets$measure == "silvoa"])
  
  if(unique(df$target) <= sum(df$area_suitable)){
    df <- df %>%
      mutate(area_suitable_cumul = cumsum(area_suitable),
             prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
             remainder = unique(target) - sum(prop_needed * area_suitable),
             prop_needed = ifelse(alc == alc[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed))
  } else{
    df <- df %>%
      mutate(prop_needed = 1)
  }
  
  df <- df %>% 
    select(alc, data, prop_needed) %>% 
    unnest(cols = c(data))
  
  df <- bind_rows(df %>% 
                    filter(prop_needed != 0 & silvoa_op) %>% 
                    mutate(lcm_to = paste0(lcm_to, "_silvoa"),
                           area = area * prop_needed),
                  df %>% 
                    filter(prop_needed != 0 & silvoa_op) %>% 
                    mutate(area = area * (1 - prop_needed)),
                  df %>% 
                    filter((prop_needed == 0 | !silvoa_op))) %>% 
    select(-prop_needed) %>% 
    filter(area != 0) 
  
  
  ### Silvopasture ----
  df <- df %>%
    mutate(silvop_op = ifelse((lcm_to %in% c("b.g", "f.s") & lcm_from != lcm_to) | (lcm_to == "s.m" & lcm_from != lcm_to) | (lcm_to %in% c("b.w", "c.w") & lcm_from != lcm_to) | (lcm_to == "s.g_woodpa" & lcm_from != lcm_to) | (lcm_to == "s.g" & lcm_from != lcm_to) | (lcm_to == "a.h_silvoa" & lcm_from != lcm_to) , 
                              FALSE, 
                              silvop_op)) %>% 
    arrange(-silvop_op, -alc) %>% 
    group_by(alc) %>%
    mutate(area_suitable = sum(area[silvop_op])) %>% 
    group_by(alc, area_suitable) %>% 
    nest() %>%
    ungroup() %>% 
    mutate(target = targets$target[targets$measure == "silvop"])
  
  if(unique(df$target) <= sum(df$area_suitable)){
    df <- df %>%
      mutate(area_suitable_cumul = cumsum(area_suitable),
             prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
             remainder = unique(target) - sum(prop_needed * area_suitable),
             prop_needed = ifelse(alc == alc[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed))
  } else{
    df <- df %>%
      mutate(prop_needed = 1)
  }
  
  df <- df %>% 
    select(alc, data, prop_needed) %>% 
    unnest(cols = c(data))
  
  df <- bind_rows(df %>% 
                    filter(prop_needed != 0 & silvop_op) %>% 
                    mutate(lcm_to = paste0(lcm_to, "_silvop"),
                           area = area * prop_needed),
                  df %>% 
                    filter(prop_needed != 0 & silvop_op) %>% 
                    mutate(area = area * (1 - prop_needed)),
                  df %>% 
                    filter((prop_needed == 0 | !silvop_op))) %>% 
    select(-prop_needed) %>% 
    filter(area != 0) 
  
  
  ### Organic ----
  df <- df %>%
    mutate(organic_op = ifelse((lcm_to %in% c("b.g", "f.s") & lcm_from != lcm_to) | (lcm_to == "s.m" & lcm_from != lcm_to) | (lcm_to %in% c("b.w", "c.w") & lcm_from != lcm_to) | (lcm_to == "s.g_woodpa" & lcm_from != lcm_to) | (lcm_to == "s.g" & lcm_from != lcm_to), 
                               FALSE, 
                               organic_op)) %>%
    arrange(-organic_op, -alc)%>%
    group_by(alc) %>%
    mutate(area_suitable = sum(area[organic_op])) %>% 
    group_by(alc, area_suitable) %>%
    nest() %>%
    ungroup() %>% 
    mutate(target = targets$target[targets$measure == "organic"])
  
  if(unique(df$target) <= sum(df$area_suitable)){
    df <- df %>%
      mutate(area_suitable_cumul = cumsum(area_suitable),
             prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
             remainder = unique(target) - sum(prop_needed * area_suitable),
             prop_needed = ifelse(alc == alc[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed))
  } else{
    df <- df %>%
      mutate(prop_needed = 1)
  }
  
  df <- df %>% 
    select(alc, data, prop_needed) %>% 
    unnest(cols = c(data))
  
  scenario_areas <- bind_rows(df %>% 
                                filter(prop_needed != 0 & organic_op) %>% 
                                mutate(lcm_to = paste0(lcm_to, "_organic"),
                                       area = area * prop_needed),
                              df %>% 
                                filter(prop_needed != 0 & organic_op) %>% 
                                mutate(area = area * (1 - prop_needed)),
                              df %>% 
                                filter((prop_needed == 0 | !organic_op))) %>% 
    select(-prop_needed) %>% 
    filter(area != 0)
  
  
  ## TARGET REACHED ----
  target_reached <- ambitions %>% 
    pivot_longer(-scenario, 
                 names_to = "measure",
                 values_to = "ambition") %>% 
    left_join(bind_rows(opportunity_areas %>% 
                          filter(peatland_lo_op) %>%
                          group_by(measure = "peatland_lo") %>% 
                          summarise(op_area = sum(area)),
                        opportunity_areas %>% 
                          filter(peatland_up_op) %>%
                          group_by(measure = "peatland_up") %>% 
                          summarise(op_area = sum(area)),
                        opportunity_areas %>% 
                          filter(woodland_op) %>%
                          group_by(measure = "woodland") %>% 
                          summarise(op_area = sum(area)),
                        opportunity_areas %>% 
                          filter(woodpa_op) %>%
                          group_by(measure = "woodpa") %>% 
                          summarise(op_area = sum(area)),
                        opportunity_areas %>% 
                          filter(grassland_op) %>%
                          group_by(measure = "grassland") %>% 
                          summarise(op_area = sum(area)),
                        opportunity_areas %>% 
                          filter(silvoa_op) %>%
                          group_by(measure = "silvoa") %>% 
                          summarise(op_area = sum(area)),
                        opportunity_areas %>% 
                          filter(silvop_op) %>%
                          group_by(measure = "silvop") %>% 
                          summarise(op_area = sum(area)),
                        opportunity_areas %>% 
                          filter(organic_op) %>%
                          group_by(measure = "organic") %>% 
                          summarise(op_area = sum(area))),
              by = "measure") %>% 
    mutate(target = op_area * ambition) %>% 
    select(-op_area) %>%
    left_join(bind_rows(scenario_areas %>% 
                          filter(lcm_to != lcm_from & lcm_to == "f.s") %>% 
                          group_by(measure = "peatland_lo") %>% 
                          summarise(area = sum(area)),
                        scenario_areas %>% 
                          filter(lcm_to != lcm_from & lcm_to == "b.g") %>% 
                          group_by(measure = "peatland_up") %>% 
                          summarise(area = sum(area)),
                        scenario_areas %>% 
                          filter(lcm_to != lcm_from & lcm_to %in% c("c.w", "b.w")) %>% 
                          group_by(measure = "woodland") %>% 
                          summarise(area = sum(area)),
                        scenario_areas %>% 
                          filter(lcm_to != lcm_from & lcm_to == "s.g_woodpa") %>% 
                          group_by(measure = "woodpa") %>% 
                          summarise(area = sum(area)),
                        scenario_areas %>% 
                          filter(lcm_to != lcm_from & lcm_to == "s.g") %>% 
                          group_by(measure = "grassland") %>% 
                          summarise(area = sum(area)),
                        scenario_areas %>% 
                          filter(lcm_to != lcm_from & str_detect(lcm_to, "silvoa")) %>% 
                          group_by(measure = "silvoa") %>% 
                          summarise(area = sum(area)),
                        scenario_areas %>% 
                          filter(lcm_to != lcm_from & str_detect(lcm_to, "silvop")) %>% 
                          group_by(measure = "silvop") %>% 
                          summarise(area = sum(area)),
                        scenario_areas %>% 
                          filter(lcm_to != lcm_from & str_detect(lcm_to, "organic")) %>% 
                          group_by(measure = "organic") %>% 
                          summarise(area = sum(area))),
              by = "measure") %>% 
    mutate(percent = area/target*100) %>% 
    nest(target_reached = c(scenario, measure, ambition, target, area, percent))
  
  
  ## ARABLE CHECK ----
  arable_area_initial <- opportunity_areas_updated %>% 
    filter(grepl("a.h", lcm_to)) %>% 
    summarise(area = sum(area)) %$%
    area
  arable_area_end <- scenario_areas %>% 
    filter(grepl("a.h", lcm_to)) %>% 
    summarise(area = sum(area)) %$%
    area
  
  if(arable_area_end / arable_area_initial < 0.1) {
    # COMBINE OUTPUTS
    outputs <- bind_cols(target_reached)
    
    return(outputs)
  }
  
  
  ## AMBITION COMBOS ----
  if (ambitions$scenario == 1) {
    ambition_combos <- ambition_combos %>% 
      filter(scenario == "a")
  } else if(baseline == TRUE & ambitions$scenario == 2) {
    ambition_combos <- ambition_combos %>% 
      filter(scenario == "a")
  } else {
    ambition_combos <- ambition_combos %>% 
      filter(scenario == "b")
  }
  
  
  ## FOOD ----
  food_production <- food_fun_run(scenario_areas, food_params, ambition_combos)
  food_production_2015 <- food_fun_run(scenario_areas %>% mutate(lcm_to = lcm_from), food_params, ambition_combos)
  
  
  ## GHGs ----
  ghg_output <- do_ghg_fun(ambitions, 
                           scenario_areas %>% 
                             group_by(scenario, country, nuts1, lcm_from, lcm_to, peat, yc_sbi, yc_sok, yc_sp, yc_ss) %>% 
                             summarise(area = sum(area), .groups = "drop"),
                           scenario_params, ambition_combos,
                           ghg_pars_luc,
                           ghg_pars_ag, food_production, food_production_2015, pop_change, prop_liquid, lowcarbonfarming,
                           ghg_pars_peat,
                           #ghg_pars_intertidal,
                           ghg_pars_bioenergy,
                           ghg_pars_wood,
                           ghg_pars_agroforestry,
                           ghg_pars_hedges)
  
  
  ## BIRDS ----
  birds_output <- bird_model(scenario_areas %>% 
                               group_by(country, nuts1, lcm_from, lcm_to) %>% 
                               summarise(area = sum(area), .groups = "drop"), spp_groups)
  
  
  ## LAND COVER AREAS ----
  land_cover <- scenario_areas %>%
    group_by(country, lcm_from, lcm_to) %>%
    summarise(area = sum(area), .groups = "drop") %>%
    nest(land_cover = c(country, lcm_from, lcm_to, area))
  
  
  ## COMBINE OUTPUTS ----
  outputs <- bind_cols(target_reached, food_production, ghg_output, birds_output, land_cover)
  
  return(outputs)
  
}

#save(scenario_creation_fun, file = here("data", "scenario_creation_fun.RData"))


# RUN FUNCTION: SCENARIOS - RANDOM AMBITION ----------------------------

# Function arguments
n_scenario <- 10000 
beta_shape_1 <- 0.75
beta_shape_2 <- 3
set.seed(30)

# Run function
t1 <- Sys.time() # Get start time

scenarios_outputs <- tibble(scenario = seq(1, n_scenario, 1),
                                peatland_lo = runif(n_scenario, 0, 1),
                                peatland_up = runif(n_scenario, 0, 1),
                                # saltmarsh = runif(n_scenario, 0, 1),
                                woodland = rbeta(n_scenario, beta_shape_1, beta_shape_2),
                                woodpa = rbeta(n_scenario, beta_shape_1, beta_shape_2),
                                grassland = rbeta(n_scenario, beta_shape_1, beta_shape_2),
                                silvoa = rbeta(n_scenario, beta_shape_1, beta_shape_2),
                                silvop = rbeta(n_scenario, beta_shape_1, beta_shape_2),
                                organic = rbeta(n_scenario, beta_shape_1, beta_shape_2)) %>%
  # Create baseline scenario by assigning scenario 1 to have 0 ambition in all measures
  mutate(peatland_lo = replace(peatland_lo, 1:2, 0),
         peatland_up = replace(peatland_up, 1:2, 0),
         # saltmarsh = replace(saltmarsh, 1:2, 0),
         woodland = replace(woodland, 1:2, 0),
         woodpa = replace(woodpa, 1:2, 0),
         grassland = replace(grassland, 1:2, 0),
         silvoa = replace(silvoa, 1:2, 0),
         silvop = replace(silvop, 1:2, 0),
         organic = replace(organic, 1:2, 0)) %>%
  # Randomly replace two values with 0 and 1 for each measure - except scenario 1 & 2
  mutate(peatland_lo = replace(peatland_lo, sample(3:n_scenario, 2, replace = FALSE), c(0, 1)),
         peatland_up = replace(peatland_up, sample(3:n_scenario, 2, replace = FALSE), c(0, 1)),
         # saltmarsh = replace(saltmarsh, sample(3:n_scenario, 2, replace = FALSE), c(0, 1)),
         woodland = replace(woodland, sample(3:n_scenario, 2, replace = FALSE), c(0, 1)),
         woodpa = replace(woodpa, sample(3:n_scenario, 2, replace = FALSE), c(0, 1)),
         grassland = replace(grassland, sample(3:n_scenario, 2, replace = FALSE), c(0, 1)),
         silvoa = replace(silvoa, sample(3:n_scenario, 2, replace = FALSE), c(0, 1)),
         silvop = replace(silvop, sample(3:n_scenario, 2, replace = FALSE), c(0, 1)),
         organic = replace(organic, sample(3:n_scenario, 2, replace = FALSE), c(0, 1))) %>%
  mutate(n = row_number()) %>% 
  group_by(n) %>% 
  nest(ambition = c(scenario, peatland_lo, peatland_up, woodland, woodpa, grassland, silvoa, silvop, organic)) %>%
  ungroup() %>% 
  # Run function
  mutate(out = purrr::map(ambition, ~scenario_creation_fun(opportunity_areas, ., "All", TRUE))) %>% 
  select(-ambition) %>% 
  unnest(cols = c(out))

# Check time to run function
t2 <- Sys.time() # End time
t2-t1 # Run length

# Save outputs
save(scenarios_outputs, file = here("outputs", "scenarios_outputs_10k.RData"))


# RUN FUNCTION: SCENARIOS - INDIVIDUAL MEASURE INCREMENTS ----------------------------
# scenarios_outputs_increments <- tibble(peatland_lo = c(0, 0, 0, seq(0.1, 1, 0.1), rep(0, 70)),
#                                           peatland_up = c(0, 0, 0, rep(0, 10), seq(0.1, 1, 0.1), rep(0, 60)),
#                                           woodland = c(0, 0, 0, rep(0, 20), seq(0.1, 1, 0.1), rep(0, 50)),
#                                           woodpa = c(0, 0, 0, rep(0, 30), seq(0.1, 1, 0.1), rep(0, 40)),
#                                           grassland = c(0, 0, 0, rep(0, 40), seq(0.1, 1, 0.1), rep(0, 30)),
#                                           silvoa = c(0, 0, 0, rep(0, 50), seq(0.1, 1, 0.1), rep(0, 20)),
#                                           silvop = c(0, 0, 0, rep(0, 60), seq(0.1, 1, 0.1), rep(0, 10)),
#                                           organic = c(0, 0, 0, rep(0, 70), seq(0.1, 1, 0.1)),
#                                           scenario = seq(1, 83, 1)) %>%
#   mutate(n = row_number()) %>%
#   group_by(n) %>% 
#   nest(ambition = c(scenario, peatland_lo, peatland_up, woodland, woodpa, grassland, silvoa, silvop, organic)) %>%
#   ungroup() %>% 
#   # Run function
#   mutate(out = purrr::map(ambition, ~scenario_creation_fun(opportunity_areas, ., "All", baseline = TRUE))) %>% 
#   select(-ambition) %>% 
#   unnest(cols = c(out))
# 
# save(scenarios_outputs_increments, file = here("outputs", "scenarios_outputs_increments.RData"))
