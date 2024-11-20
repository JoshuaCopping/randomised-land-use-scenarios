# 4c BIRD MODEL ######################
source("./code/0_setup.R")


# LOAD DATA ----------------------------
load(here("data", "bbs_mods_b_2023.RData"))

bbs_species <- read_csv(here("data", "bird_spp.csv"), skip = 1) %>% 
  filter(keep == "yes") %>%
  select(spp, gb_only, bocc4, bocc5, farmland_list, woodland_list, wetland_list, rspb_list)

# Get species groups
spp_groups <- bind_rows(bbs_species %>% 
                          select(spp) %>% 
                          mutate(group = "All species"),
                        bbs_species %>%
                          filter(!is.na(farmland_list)) %>%
                          select(spp) %>%
                          mutate(group = "Farmland birds"),
                        bbs_species %>% 
                          filter(farmland_list == "specialist") %>% 
                          select(spp) %>% 
                          mutate(group = "Farmland specialists"),
                        bbs_species %>% 
                          filter(farmland_list == "generalist") %>% 
                          select(spp) %>% 
                          mutate(group = "Farmland generalists"),
                        bbs_species %>%
                          filter(!is.na(woodland_list)) %>%
                          select(spp) %>%
                          mutate(group = "Woodland birds"),
                        bbs_species %>% 
                          filter(woodland_list  == "specialist") %>% 
                          select(spp) %>% 
                          mutate(group = "Woodland specialists"),
                        bbs_species %>% 
                          filter(woodland_list  == "generalist") %>% 
                          select(spp) %>% 
                          mutate(group = "Woodland generalists"),
                        bbs_species %>% 
                          filter(bocc4 != "green") %>% 
                          select(spp) %>% 
                          mutate(group = "BoCC Red/Amber (old)"),
                        bbs_species %>% 
                          filter(bocc5 != "green") %>% 
                          select(spp) %>% 
                          mutate(group = "BoCC Red/Amber"),
                        bbs_species %>% 
                          filter(rspb_list == "priority") %>% 
                          select(spp) %>% 
                          mutate(group = "RSPB Priority Species"),
                        bbs_species %>% 
                          filter(spp %in% c("CU", "RK", "SN", "L.", "OC")) %>% 
                          select(spp) %>% 
                          mutate(group = "Farmland waders")) %>% 
  left_join(bbs_species %>% select(spp, gb_only),
            by = "spp") %>% 
  select(spp, group, gb_only)


# FUNCTION SETUP ----------------------------

## For testing
# scenario_areas <- scenario_areas %>% 
#   group_by(country, nuts1, lcm_from, lcm_to) %>% 
#   summarise(area = sum(area), .groups = "drop")


bird_model <- function(scenario_areas, spp_groups){
  
  # Update scenario areas
  areas <- scenario_areas %>% 
    # Tidy country/island attributes
    mutate(nuts1 = ifelse(nuts1 %chin% c("South East (England)", "London"), "South East & London (England)", nuts1)) %>% 
    # Merge categories
    mutate(lcm = case_when(str_detect(lcm_to, "a.h") ~ "a.h",
                           str_detect(lcm_to, "i.g|solar") ~ "i.g",
                           str_detect(lcm_to, "c.w") ~ "c.w",
                           str_detect(lcm_to, "s.g|s.g_woodpa|a.g|c.g|n.g") ~ "s.g",
                           str_detect(lcm_to, "c.l|s.m") ~ "c.l",
                           TRUE ~ lcm_to),
           lcm_from = case_when(str_detect(lcm_from, "s.g|a.g|c.g|n.g") ~ "s.g",
                                str_detect(lcm_from, "c.l|s.m") ~ "c.l",
                                TRUE ~ lcm_from),
           organic = str_detect(lcm_to, "organic"),
           renewable_energy = str_detect(lcm_to, "wind|solar"),
           agroforestry = str_detect(lcm_to, "woodpa|silvoa|silvop")) %>% 
    select(country, nuts1, lcm, area, organic, renewable_energy, agroforestry) %>% 
    filter(area != 0) 
  
  # Create blank table for populating in loops
  birds_out <- tibble(pop = double(),
                      spp = character(),
                      species = character())
  
  mod_b_out <- mod_b_out %>% filter(!spp %in% c("CS", "DI", "GL", "GN", "GD", "RM", "BV", "RH", "SZ")) # Remove species not typically influenced by land cover/changes made here
  
  # Loop per species
  for (i in 1:nrow(mod_b_out)){
    
    mod <- mod_b_out$mod[[i]] %>% 
      filter(method == "B1") %>% 
      mutate(beta_nuts1 = ifelse(beta_nuts1 == 0 & nuts1 == "Northern Ireland", -Inf, beta_nuts1)) %>% 
      mutate(density = exp(density) * exp(beta_nuts1)) %>%
      select(-beta_a.h_buf, -beta_b.t_buf, -beta_w.d_buf, -beta_hedge)
    
    bird_loop <- bind_rows(mod %>% 
                             mutate(agroforestry = FALSE),
                           # silvoa
                           mod %>% 
                             filter(lcm %in% c("a.h", "b.w"))  %>%
                             group_by(nuts1) %>% 
                             summarise(density = min(density) + abs(diff(density)) * 0.25) %>% 
                             mutate(lcm = "a.h",
                                    agroforestry = TRUE),
                           # silvop
                           mod %>% 
                             filter(lcm %in% c("i.g", "b.w")) %>% 
                             group_by(nuts1) %>% 
                             summarise(density = min(density) + abs(diff(density)) * 0.25) %>% 
                             mutate(lcm = "i.g",
                                    agroforestry = TRUE),
                           # woodpa
                           mod %>% 
                             filter(lcm %in% c("s.g", "b.w")) %>% 
                             group_by(nuts1) %>% 
                             summarise(density = min(density) + abs(diff(density)) * 0.25) %>% 
                             mutate(lcm = "s.g",
                                    agroforestry = TRUE)) %>% 
      select(-beta_nuts1, -method, -value) %>% 
      full_join(areas, 
                by = c("lcm", "nuts1", "agroforestry")) %>%
      mutate(density = density + density * (organic * (mod_b_out$organic_multiplier[i] - 1)),
             density = if_else(renewable_energy, density/2, density)) %>% 
      drop_na(country) %>% 
      group_by(country, nuts1) %>%
      summarise(pop = sum(area/100 * density),
                .groups = "drop") %>%
      mutate(spp = mod_b_out$spp[[i]],
             species = mod_b_out$species[[i]]) 
    
    birds_out <- rbind(birds_out, bird_loop)
    
  }
  
  # Geometric mean per group
  birds_sum_geomean <- birds_out %>%
    #select(spp, species, pop) %>% 
    left_join(spp_groups, by = "spp", multiple = "all") %>%
    filter(!(gb_only == 1 & country == "Northern Ireland")) %>% 
    bind_rows((.) %>% mutate(country = "UK")) %>% 
    group_by(country, group, spp, species) %>%
    summarise(pop = sum(pop)) %>%
    #group_by(group, country, nuts1) %>% 
    group_by(group, country) %>% 
    summarise(pop = exp(mean(log(pop))),
              n = length(species),
              .groups = "drop") %>% 
    nest(birds_sum_geomean = everything())
  
  #birds_spp_out <- birds_out %>% select(country, nuts1, spp, species, pop) %>%  nest(birds_spp_out = everything())
  
  return(birds_sum_geomean)
  #return(bind_cols(birds_sum_geomean, birds_spp_out))
  
}
  
# Save model and params
save(mod_b_out,
     spp_groups,
     bird_model,
     file = here("data", "birds_pars_funs.RData"))
