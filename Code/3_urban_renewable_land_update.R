# 3 URBAN & RENEWABLE LAND UPDATE ######################
source("./code/0_setup.R")


# COLLAPSE MASTER LCFU INTO AREAS SIMPLE ----------------------------
# Load baseline areas - Master LCFU table
load(here("outputs", "master_lcfu.RData"))

areas_simple <- master_lcfu %>% 
  mutate(across(contains("zones"), ~ ifelse(. < 4 | is.na(.), 0, .))) %>% 
  rowwise() %>% 
  mutate(wader_score = sum(sn_zones, cu_zones, l._zones, rk_zones, oc_zones),
         wader_mask = wader_score > 10) %>% 
  select(-sn_zones, -cu_zones, -l._zones, -rk_zones, -oc_zones) %>% 
  group_by_at(vars(-area)) %>% 
  summarise(area = sum(area), .groups = "drop") 

# Save areas simple 
save(areas_simple, file = here("outputs", "areas_simple.RData"))


# CREATE EXPANSION TARGETS ----------------------------
# Load areas simple
load(here("outputs", "areas_simple.RData"))

# Set targets for urban expansion and renewable energy installation.
targets <- bind_rows(areas_simple %>%
                       filter(lcm == "b.t") %>% 
                       group_by(country, measure = "urban") %>% 
                       summarise(current_urban = sum(area)) %>% 
                       mutate(target = current_urban * 0.232) %>% # 23.2% based on CEH technical annex in CCC 6CB
                       select(-current_urban),
                     areas_simple %>% 
                       filter(windop == 1) %>% 
                       group_by(country, measure = "wind") %>% 
                       summarise(area = sum(area)) %>%
                       ungroup() %>% 
                       mutate(total = sum(area),
                              prop = area/total,
                              needed = 418000,
                              target = needed * prop) %>% 
                       select(country, measure, target),
                     areas_simple %>% 
                       filter(solarop == 1) %>% 
                       group_by(country, measure = "solar") %>% 
                       summarise(area = sum(area)) %>%
                       ungroup() %>% 
                       mutate(total = sum(area),
                              prop = area/total,
                              needed = 170000,
                              target = needed * prop) %>% 
                       select(country, measure, target))

## Check wind/solar energy conversion areas: 
# ha = new capcity GW, into MW/energy density, from km into ha
# wind ha = (20.9*1000)/5*100 
# solar ha = (76.5*1000)/45*100


# IDENTIFY OPPORTUNITIES ----------------------------
# opportunity for all measures
opportunity_areas <- areas_simple %>% 
  mutate(urban_op = lcm %in% c("a.h", "i.g") & alc %in% c(4, 5, 6) & peat == 0,
         solar_op = solarop == 1 & wader_mask == FALSE,
         wind_op = windop == 1  & wader_mask == FALSE,
         peatland_lo_op = peat == 1 & lcm %in% c("a.h", "i.g"),
         peatland_up_op = peat == 1 & !lcm %in% c("b.g", "f.s"),
         saltmarsh_op = saltmarsh == 1,
         woodland_op = woodop == 1 & organomin == 0 & wader_mask == FALSE,
         woodpa_op = woodop == 1 & lcm %in% c("i.g", "n.g", "c.g", "a.g") & wader_mask == FALSE,
         grassland_op = lcm %in% c("a.h", "i.g"),
         silvoa_op = woodop == 1 & lcm == "a.h" & organomin == 0 & wader_mask == FALSE,
         silvop_op = woodop == 1 & lcm == "i.g" & organomin == 0 & wader_mask == FALSE,
         organic_op = lcm %in% c("a.h", "i.g")) %>% 
  select(-woodop, -saltmarsh, -solarop, -windop) %>% 
  group_by_at(vars(-area)) %>% 
  summarise(area = sum(area), .groups = "drop") 


# UPDATE OPPORTUNITIES WITH URBAN & RENEWABLE LAND----------------------------
## URBAN EXPANSION ----
opportunity_areas <- opportunity_areas %>%
  arrange(-urban_op, urban_distance, -alc) %>% 
  group_by(country, urban_distance, alc) %>%
  mutate(area_suitable = sum(area[urban_op])) %>% 
  group_by(country, urban_distance, alc, area_suitable) %>% 
  nest() %>%
  group_by(country) %>%
  left_join(targets %>% 
              filter(measure == "urban") %>% 
              select(-measure),
            by = "country") %>%
  mutate(priority = row_number(),
         area_suitable_cumul = cumsum(area_suitable),
         prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
         remainder = unique(target) - sum(prop_needed * area_suitable),
         prop_needed = ifelse(priority == priority[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed)) %>% 
  ungroup() %>% 
  select(country, alc, data, prop_needed) %>% 
  unnest(cols = c(data))

opportunity_areas <- bind_rows(opportunity_areas %>% # land converted through urban expansion
                                 filter(prop_needed != 0 & urban_op) %>% 
                                 rename(lcm_from = lcm) %>% 
                                 mutate(lcm_to = "b.t",
                                        area = area * prop_needed),
                               opportunity_areas %>% # land remaining from urban expansion opportunity conversion
                                 filter(prop_needed != 0 & urban_op) %>% 
                                 rename(lcm_from = lcm) %>% 
                                 mutate(lcm_to = lcm_from,
                                        area = area * (1 - prop_needed)),
                               opportunity_areas %>% # unused urban expansion opportunity or non-opportunity land
                                 filter((prop_needed == 0 | !urban_op)) %>% 
                                 rename(lcm_from = lcm) %>% 
                                 mutate(lcm_to = lcm_from,
                                        area = area)) %>% 
  select(-prop_needed, -urban_op) %>% 
  filter(area != 0) 


## WIND ----
opportunity_areas <- opportunity_areas %>%
  mutate(wind_op = ifelse((lcm_from != lcm_to & lcm_to == "b.t"), 
                          FALSE, 
                          wind_op)) %>% 
  arrange(-wind_op, -alc) %>% 
  group_by(country, alc) %>%
  mutate(area_suitable = sum(area[wind_op])) %>% 
  group_by(country, alc, area_suitable) %>% 
  nest() %>%
  group_by(country) %>%
  left_join(targets %>% 
              filter(measure == "wind") %>% 
              select(-measure),
            by = "country") %>%
  mutate(area_suitable_cumul = cumsum(area_suitable),
         prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
         remainder = unique(target) - sum(prop_needed * area_suitable),
         prop_needed = ifelse(alc == alc[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed)) %>% 
  ungroup()%>% 
  select(country, alc, data, prop_needed) %>% 
  unnest(cols = c(data))

opportunity_areas <- bind_rows(opportunity_areas %>% 
                                 filter(prop_needed != 0 & wind_op) %>% 
                                 mutate(lcm_to = paste0(lcm_from, "_wind"),
                                        area = area * prop_needed),
                               opportunity_areas %>% 
                                 filter(prop_needed != 0 & wind_op) %>% 
                                 mutate(area = area * (1 - prop_needed)),
                               opportunity_areas %>% 
                                 filter((prop_needed == 0 | !wind_op))) %>% 
  select(-prop_needed, -wind_op) %>% 
  filter(area != 0) 


## SOLAR ----
opportunity_areas <- opportunity_areas %>%
  mutate(solar_op = ifelse(lcm_from != lcm_to & lcm_to == "b.t" | grepl("wind", lcm_to),
                           FALSE,
                           solar_op)) %>%
  arrange(-solar_op, -alc) %>%
  group_by(country, alc) %>%
  mutate(area_suitable = sum(area[solar_op])) %>%
  group_by(country, alc, area_suitable) %>%
  nest() %>% 
  group_by(country) %>%
  left_join(targets %>%
              filter(measure == "solar") %>%
              select(-measure),
            by = "country") %>%
  mutate(area_suitable_cumul = cumsum(area_suitable),
         prop_needed = ifelse(area_suitable_cumul < target, 1, 0),
         remainder = unique(target) - sum(prop_needed * area_suitable),
         prop_needed = ifelse(alc == alc[first(which(prop_needed == 0))], remainder/area_suitable, prop_needed)) %>%
  ungroup()%>%
  select(country, alc, data, prop_needed) %>%
  unnest(cols = c(data))

opportunity_areas <- bind_rows(opportunity_areas %>%
                                 filter(prop_needed != 0 & solar_op) %>%
                                 mutate(lcm_to = "solar",
                                        area = area * prop_needed),
                               opportunity_areas %>%
                                 filter(prop_needed != 0 & solar_op) %>%
                                 mutate(area = area * (1 - prop_needed)),
                               opportunity_areas %>%
                                 filter((prop_needed == 0 | !solar_op))) %>%
  select(-prop_needed, -solar_op) %>%
  filter(area != 0)


# SAVE ----------------------------
save(opportunity_areas, file = here("outputs", "opportunity_areas.RData"))


# URBAN EXPANSION & RENEWABLE INSTALLATION AREA RESULTS ----------------------------
# areas_simple %>% 
#   filter(lcm == "b.t") %>% 
#   group_by(country) %>% 
#   summarise(original_area = sum(area)) %>% 
#   left_join(opportunity_areas %>% 
#               filter(lcm_to == "b.t") %>% 
#               group_by(country) %>% 
#               summarise(new_area = sum(area)),
#             by = "country") %>% 
#   mutate(perc_increase = (new_area-original_area)/original_area*100)
# 
# opportunity_areas %>% filter(grepl("solar", lcm_to)) %>% summarise(area = sum(area))
# opportunity_areas %>% filter(grepl("wind", lcm_to)) %>% summarise(area = sum(area))
