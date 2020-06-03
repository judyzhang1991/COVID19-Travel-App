## STAT302 ##
## Final Project ##
## Jingyang Zhang ##
## June 2nd, 2020 ##
## dataprocess.R ##

# Load packages ----
library(tidyverse)
library(janitor)


# Load data ----

### AIRPORT DATA ###
## The dataset is downloaded from https://ourairports.com/
## Filename: airports.csv
airports <- read_csv("data/airports.csv") %>%
  clean_names() %>%
  filter(type != "closed")

### COUNTRY DATA ###
## The dataset is downloaded from https://ourairports.com/
## Filename: countries.csv
countries <- read_csv("data/countries.csv") %>%
  clean_names()

country_airports <- left_join(airports, countries, by = "id") %>%
  select(id, type, iata_code, local_code, name.x, name.y, iso_country, latitude_deg, longitude_deg)




