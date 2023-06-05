# 1. Load packages ----

library(tidyverse)
library(readxl)

# 2. Load data ----

data <- read.csv("data/clean_data.csv")

# 3. Trends of benthic categories for all sites ----

# 3.1 Aggregate the data --

data_aggregated <- data %>% 
  group_by(locality, parentEventID, year, category) %>% 
  summarise(n = sum(n)) %>%
  mutate(cover = n*100/sum(n)) %>% 
  ungroup()

# 3.2 Make the plot --

ggplot(data = data_aggregated, aes(x = year, y = cover)) +
  geom_point(alpha = 0.25, color = "#446CB3") +
  geom_smooth() +
  facet_wrap(~category) +
  labs(x = "Year", y = "Benthic cover (%)")

# 3.3 Save the plot --

ggsave("figs/st-eustatius-benthic-trends.png")

# 4. Cover of benthic categories per year and site ----

# 4.1 Aggregate the data --

data_aggregated <- data %>% 
  group_by(locality, year, category) %>% 
  summarise(n = sum(n)) %>%
  mutate(cover = n*100/sum(n)) %>% 
  ungroup()

# 4.2 Make the plot --

ggplot(data = data_aggregated, aes(x = year, y = cover, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~locality) +
  labs(x = "Year", y = "Benthic cover (%)")

# 4.3 Save the plot --

ggsave("figs/st-eustatius-benthic-community.png", height = 10, width = 18)

# 5. Cover of coral species per year and site ----

# 5.1 Aggregate the data --

data_aggregated <- data %>% 
  filter(category == "Coral") %>% 
  group_by(locality, year, subcategory) %>% 
  summarise(n = sum(n)) %>%
  mutate(cover = n*100/sum(n)) %>% 
  ungroup()

# 5.2 Make the plot --

ggplot(data = data_aggregated, aes(x = year, y = cover, fill = subcategory)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~locality) +
  labs(x = "Year", y = "Benthic cover (%)") +
  theme(legend.text = element_text(face = "italic")) +
  guides(fill = guide_legend(ncol = 1, title = "Species"))

# 5.3 Save the plot --

ggsave("figs/st-eustatius-coral-community.png", height = 12, width = 18)
