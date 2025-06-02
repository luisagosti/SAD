# ====================================================================================
# File: scripts/2_limpeza_disputa.R
# Description: Limpeza e padronização dos dados raw
# ====================================================================================

# 1. Carregar pacotes necessários
library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)

# 2. Diretórios de entrada/saída
dir.create("data_clean", showWarnings = FALSE)

# ----------------------------------------------------------------------------
# 2.1. Bike Sharing Systems (raw_bike_sharing_systems.csv)
# ----------------------------------------------------------------------------
bike_raw <- read_csv("data_raw/raw_bike_sharing_systems.csv")

bike_clean <- bike_raw %>%
  clean_names() %>%
  rename_with(~ str_replace_all(., "\\s+", "_")) %>%
  mutate(
    city = str_remove_all(city_region, "\\[.*?\\]"),
    country = str_remove_all(country_2, "\\[.*?\\]")
  ) %>%
  select(country, city, name, system, operator, launched, discontinued)

write_csv(bike_clean, "data_clean/bike_systems_clean.csv")

# ----------------------------------------------------------------------------
# 2.2. World Cities (raw_worldcities.csv)
# ----------------------------------------------------------------------------
wc_raw <- read_csv("data_raw/raw_worldcities.csv")

wc_clean <- wc_raw %>%
  clean_names() %>%
  select(city, country, lat, lng, population) %>%
  drop_na(lat, lng)

write_csv(wc_clean, "data_clean/worldcities_clean.csv")

# ----------------------------------------------------------------------------
# 2.3. Cities Weather Forecast (raw_cities_weather_forecast.csv)
# ----------------------------------------------------------------------------
cw_raw <- read_csv("data_raw/raw_cities_weather_forecast.csv")

cw_clean <- cw_raw %>%
  clean_names() %>%
  rename(
    datetime = dt_txt,
    temperature = main_temp,
    humidity = main_humidity,
    wind_speed = wind_speed,
    cloud_cover = clouds_all
  ) %>%
  mutate(
    datetime = suppressWarnings(parse_date_time(datetime, orders = c("ymd HMS", "ymd_HMS"))),
    city = str_remove(city, "\\s+")
  ) %>%
  drop_na(temperature, humidity)

write_csv(cw_clean, "data_clean/cities_weather_clean.csv")

# ----------------------------------------------------------------------------
# 2.4. Seoul Bike Sharing (raw_seoul_bike_sharing.csv)
# ----------------------------------------------------------------------------
# Read as raw lines to detect encoding issues
raw_lines <- readLines("data_raw/raw_seoul_bike_sharing.csv", encoding = "latin1", warn = FALSE)

# Remove any invalid characters
clean_lines <- iconv(raw_lines, from = "latin1", to = "UTF-8", sub = "")

# Write temporary fixed file
writeLines(clean_lines, "data_raw/seoul_clean_temp.csv")

# Now read the cleaned CSV
seoul_raw <- read_csv("data_raw/seoul_clean_temp.csv")

# Continue cleaning
seoul_clean <- seoul_raw %>%
  clean_names() %>%
  rename(
    date = date,
    hour = hour,
    rented_bike_count = rented_bike_count,
    temperature = temperature_c,
    humidity = humidity_,
    wind_speed = wind_speed_m_s,
    visibility = visibility_10m,
    dew_point_temperature = dew_point_temperature_c,
    solar_radiation = solar_radiation_mj_m2,
    precipitation = rainfall_mm,
    snow_fall = snowfall_cm,
    season = seasons,
    holiday = holiday,
    functioning_day = functioning_day
  ) %>%
  mutate(
    date = dmy(date),
    hour = factor(hour, levels = as.character(0:23)),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")),
    holiday = if_else(holiday == "Holiday", 1, 0),
    functioning_day = if_else(functioning_day == "Yes", 1, 0)
  ) %>%
  drop_na(temperature, humidity, wind_speed, visibility,
          dew_point_temperature, solar_radiation, precipitation, snow_fall)


write_csv(seoul_clean, "data_clean/seoul_bike_clean.csv")

