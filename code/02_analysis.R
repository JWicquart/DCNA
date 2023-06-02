# 1. Load packages ----

library(tidyverse)
library(readxl)

# 2. Load data ----

data <- read.csv("data/clean_data.csv")

# 3. Plot data per transect ----

# 3.1 Calculate cover per transect --

data_transect <- data %>% 
  group_by(locality, parentEventID, category) %>% 
  summarise(n = sum(n)) %>%
  mutate(cover = n*100/sum(n)) %>% 
  ungroup()

# 3.2 Plot --

ggplot(data = data_transect, aes(x = parentEventID, y = cover, fill = category)) +
  geom_bar(stat = "identity", position = "stack")

ggsave("figs/hangover-transect-benthic-community.png")

# 4. Plot data per site ----

# 4.1 Calculate cover per site --

data_site <- data %>% 
  group_by(locality, category) %>% 
  summarise(n = sum(n)) %>%
  mutate(cover = n*100/sum(n)) %>% 
  ungroup()

# 4.2 Plot --

ggplot(data = data_site, aes(x = locality, y = cover, fill = category)) +
  geom_bar(stat = "identity", position = "stack")

ggsave("figs/hangover-site-benthic-community.png")

# 5. Coral species per transect ----

# 3.1 Calculate cover per transect --

data_transect <- data %>% 
  filter(category == "Coral") %>% 
  group_by(locality, parentEventID, subcategory) %>% 
  summarise(n = sum(n)) %>%
  mutate(cover = n*100/sum(n)) %>% 
  ungroup()

# 3.2 Plot --

ggplot(data = data_transect, aes(x = parentEventID, y = cover, fill = subcategory)) +
  geom_bar(stat = "identity", position = "stack")

ggsave("figs/hangover-transect-coral-community.png")
