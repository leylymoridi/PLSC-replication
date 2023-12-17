#Figure 1 Map - Leyly Moridi and Isabel Linzer

library(tidyverse)
library(scales)
library(giscoR)
library(sf)
library(ggthemes)
library(dplyr)
library(ggplot2)


#load and set up data ----
data <- read_dta("data/Final_Main.dta")

data <- data |>
  filter(year >= 1987) 

#add quartiles ----
data[data == -99.00] <- NA

data <- data |>
  group_by(ccode) |>
  mutate(edu_mean_ccode = mean(covihme_ayem, na.rm = TRUE)) |>
  ungroup()

data <- data |>
  filter(!is.na(edu_mean_ccode)) %>%
  mutate(quartile = ntile(edu_mean_ccode, 4))

#merge country codes ----
codes <- read_csv("data/COW-country-codes.csv")

codes <- codes |>
  rename(ccode = CCode)

map_data <- left_join(data, codes, by = "ccode") |>
  rename(NAME_ENGL = StateNme)

# Load world shapefiles from giscoR ----
world <- gisco_get_countries()

# Merge the quartile information with the world shapefiles
map_data <- left_join(world, map_data, by = "NAME_ENGL")

# Create map ----
quartile_map <- ggplot(map_data, aes(fill = factor(quartile))) +
  geom_sf() +
  scale_fill_manual(name = "Quartile", values = c("1" = "wheat1", "2" = "lightsalmon", "3" = "indianred2", "4" = "indianred4"),
                    labels = c("Q1", "Q2", "Q3", "Q4"),
                    na.value = "grey", drop = FALSE) +
  theme_minimal()

#view map
print(quartile_map)
