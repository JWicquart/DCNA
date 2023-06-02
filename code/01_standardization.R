# 1. Load packages ----

library(tidyverse)
library(readxl)

# 2. Load data ----

# 2.1 Site data --

site_data <- read_xlsx("data/GCRMN Survey Sites Coordinates.xlsx") %>% 
  select(-"...1", -"Done?") %>% 
  rename(Sitename = "Site Name", verbatimDepth = "Max depth (m)", 
         decimalLatitude = Latitude, decimalLongitude = Longitude)

# 2.2 Code data --

code_data <- read_xlsx("data/code.xlsx")

# 3. Load and standardize main data ----

main_data <- map_dfr(c("T1_raw", "T2_raw", "T3_raw", "T4_raw", "T5_raw"), 
             ~read_xls("data/Hangover(1).xls", sheet = .)) %>% 
  select("Raw Data", "Frame image name", Sitename, Transect) %>% 
  rename(code = "Raw Data", photoquadrat = "Frame image name") %>% 
  mutate(photoquadrat = str_sub(photoquadrat, -8, -5)) %>% 
  group_by(Sitename, photoquadrat, Transect, code) %>% 
  count() %>% 
  ungroup() %>% 
  left_join(., site_data) %>% 
  left_join(., code_data) %>% 
  select(-code) %>% 
  rename(locality = Sitename, parentEventID = Transect, eventID = photoquadrat)

# 4. Export data ----

write.csv(main_data, file = "data/clean_data.csv", row.names = FALSE)
