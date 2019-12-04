library(readr)
library(tidyverse)
library(dplyr)
library(maps)
library(ggplot2)

master = read_csv("maindata.csv")

world_data <- ggplot2::map_data('world') %>%
  fortify() %>%
  rename(country = region) %>%
  group_by(country) %>%
  mutate(longitude = mean(long),
         latitude = mean(lat)
  ) %>%
  select(country, longitude, latitude) %>%
  unique() %>%
  ungroup()
master$country[master$country == "Cabo Verde"] <- "Cape Verde"
master$country[master$country == "Republic of Korea"] <- "South Korea"
master$country[master$country == "Russian Federation"] <- "Russia"
master$country[master$country == "United Kingdom"] <- "UK"
anti_bar = world_data %>%
  filter(country == "Antigua" | country == "Barbuda")
trin_tob = world_data %>%
  filter(country == "Trinidad" | country == "Tobago")
world_data = world_data[-c(122),] %>%
  add_row(country = "Antigua and Barbuda",
          longitude = mean(anti_bar$longitude),
          latitude = mean(anti_bar$latitude)) %>%
  add_row(country = "Trinidad and Tobago",
          longitude = mean(trin_tob$longitude),
          latitude = mean(trin_tob$latitude)) %>%
  add_row(country = "Macau",
          longitude = 113.552971,
          latitude = 22.210928) %>%
  add_row(country = "Saint Kitts and Nevis",
          longitude =  -62.754593,
          latitude = 17.363747) %>%
  add_row(country = "Saint Vincent and Grenadines",
          longitude = -61.2872276,
          latitude = 12.9843054) %>%
  add_row(country = "United States",
          longitude = -95.7129,
          latitude = 37.0902) %>%
  add_row(country = "Kiribati",
          longitude =  -168.7340393,
          latitude =  -3.3704171) 
  

maindata = left_join(master, world_data, by = "country") %>%
  group_by(country, gdp_for_year, gdp_per_capita, longitude, latitude, year) %>%
  summarise(population = sum(population),
          suicides_no = sum(suicides_no),
          suicides_100k_pop = sum(suicides_no)/sum(population)*100000
  ) %>%
  ungroup() %>%
  mutate(id = str_c(country, year))

cleantable = maindata %>%
  select(
    Country = country,
    Year = year,
    Population = population,
    GDPforYear = gdp_for_year,
    GDPperCapita = gdp_per_capita,
    SuicidesNumber = suicides_no,
    SuicidesRate = suicides_100k_pop,
    Lat = latitude,
    Long = longitude,
    id = id
  )    


