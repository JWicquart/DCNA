# 1. Load packages ----

library(tidyverse)
library(readxl)

# 2. Load site coordinates and benthic code data ----

# 2.1 Site data --

site_data <- read_xlsx("data/site.xlsx", range = "B1:F25") %>% 
  rename(locality = "Site Name", verbatimDepth = "Max depth (m)", 
         decimalLatitude = Latitude, decimalLongitude = Longitude) %>% 
  mutate(locality = str_replace_all(locality, c("STENAPA Reef\\*\\*" = "Stenapa Reef",
                                                "Anchor Point South" = "Anchor Point",
                                                "SE11" = "SE_11",
                                                "SE8" = "SE_8"))) %>% 
  select(-Zone) %>% 
  filter(locality != "Stenapa Reef") %>% 
  bind_rows(., tibble(locality = c("Blocks", "Mushroom Garden", "Stenapa Reef"),
                      verbatimDepth = c(17, 16, 17),
                      decimalLatitude = c(17.46400, 17.46265, 17.48425),
                      decimalLongitude = c(-62.98508, -62.97762, -62.99717)))

# 2.2 Code data --

code_data <- read_xlsx("data/code.xlsx")

# 3. Main data ----

# 3.1 Path files --

files_path <- list.files("data", full.names = TRUE, recursive = TRUE) %>% 
  as_tibble() %>% 
  rename(path = value) %>% 
  filter(!(path %in% c("data/code.xlsx", "data/site.xlsx", "data/clean_data.csv"))) %>% 
  rowwise() %>% 
  mutate(sheet = list(rep(1:5))) %>% 
  unnest(cols = c(sheet)) %>% 
  ungroup() %>% 
  mutate(year = str_sub(path, 6, 9)) %>% 
  # Remove sheets for some files
  filter(!(year == 2017 & sheet %in% c(2, 3, 4, 5)),
         !(path == "data/2019/DoubleWreck.xls" & sheet %in% c(4, 5)),
         !(path == "data/2020/DoubleWreck.xls" & sheet %in% 5),
         !(path == "data/2021/Double Wreck.xls" & sheet %in% 5)) %>% 
  mutate(id = 1:nrow(.))

# 3.2 Combine data --
  
data_benthos <- map2_dfr(files_path$path, files_path$sheet, 
                         ~read_xls(path = .x, sheet = .y, col_types = "text"), .id = "id") %>% 
  select("Raw Data", "Frame image name", Sitename, Transect, id) %>% 
  rename(locality = Sitename, code = "Raw Data", parentEventID = Transect, 
         eventID = "Frame image name") %>% 
  mutate(eventID = str_sub(eventID, -8, -5),
         id = as.numeric(id)) %>% 
  left_join(., files_path) %>% 
  mutate(parentEventID = str_sub(parentEventID, -1)) %>% 
  mutate(locality = str_replace_all(locality, c("AnchorPointSouth" = "Anchor Point",
                                                "AnchorPoint" = "Anchor Point",
                                                "BarracudaReef" = "Barracuda Reef",
                                                "Barracuda Point" = "Barracuda Reef",
                                                "CorreCorre" = "Corre Corre",
                                                "TheCave" = "The Cave",
                                                "Cave" = "The Cave",
                                                "The The Cave" = "The Cave",
                                                "CrooksCastle" = "Crooks Castle",
                                                "Crook's Castle" = "Crooks Castle",
                                                "DoubleWreck" = "Double Wreck",
                                                "The Dump" = "Dump",
                                                "Mushroom Gardens" = "Mushroom Garden",
                                                "FiveFingersSouth" = "Five Fingers South",
                                                "OuterJenkinsBay" = "Outer Jenkins Bay",
                                                "SE11" = "SE_11",
                                                "SE8" = "SE_8",
                                                "STE19" = "STE_19",
                                                "StenapaReef" = "Stenapa Reef",
                                                "TripleWreck" = "Triple Wreck",
                                                "TwinSisters" = "Twin Sisters",
                                                "Twinsisters" = "Twin Sisters",
                                                "ValleyOfTheSponges" = "Valley of the Sponges",
                                                "ValleyoftheSponges" = "Valley of the Sponges",
                                                "VenusBay" = "Venus Bay",
                                                "WhiteWall" = "White Wall",
                                                "Whitewall" = "White Wall"))) %>% 
  left_join(., site_data) %>% 
  left_join(., code_data) %>% 
  group_by(locality, parentEventID, eventID, decimalLatitude, decimalLongitude,
           verbatimDepth, year, category, subcategory) %>% 
  count() %>% 
  ungroup() 

# 4. Export the data ----

write.csv(data_benthos, file = "data/clean_data.csv", row.names = FALSE)

# 5. Quality checks ----

# Missing equivalence for benthic codes

pb_code <- data_benthos %>% 
  filter(is.na(category)) %>% 
  select(category, code) %>% 
  distinct()

# Missing site coordinates

pb_sites <- data_benthos %>%
  select(locality, decimalLatitude, decimalLongitude) %>%
  distinct()

# Number of points per photoquadrat -> 3 PQ with 50 points instead of 25

pb_count <- data_benthos %>% 
  group_by(Sitename, photoquadrat, Transect, path, sheet, year) %>% 
  count() %>% 
  ungroup()
