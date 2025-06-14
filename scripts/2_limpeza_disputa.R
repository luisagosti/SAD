# ====================================================================================
# File: scripts/2_limpeza_disputa.R
# Description: Limpeza e padronização dos dados raw para todas as fontes
# ====================================================================================

# 1. Carregar pacotes necessários
library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)

# 2. Criar diretório de saída se não existir
dir.create("data_clean", showWarnings = FALSE)

# ----------------------------------------------------------------------------
# 2.1. Bike Sharing Systems (raw_bike_sharing_systems.csv)
# ----------------------------------------------------------------------------

# 2.1.1. Ler o CSV bruto
bike_raw <- read_csv("data_raw/raw_bike_sharing_systems.csv")

# 2.1.2. Aplicar clean_names() para ver quais nomes vieram
bike_tmp <- bike_raw %>% clean_names()
print("Nomes após clean_names() em bike_raw:")
print(names(bike_tmp))
# Exemplo de saída:
# [1] "country_1"    "country_2"    "city_region"  "name"        
#     "system"       "operator"     "launched"     "discontinued"

# 2.1.3. Renomear e tratar campos
bike_clean <- bike_tmp %>%
  rename(
    country = country_2,    # Segunda coluna é o país
    city    = city_region   # city_region → city
  ) %>%
  mutate(
    city    = str_remove_all(city, "\\[.*?\\]"),
    country = str_remove_all(country, "\\[.*?\\]")
  ) %>%
  select(
    country,
    city,
    name,
    system,
    operator,
    launched,
    discontinued
  )

write_csv(bike_clean, "data_clean/bike_systems_clean.csv")
message("✅ bike_systems_clean.csv gerado em data_clean/")

# ----------------------------------------------------------------------------
# 2.2. World Cities (raw_worldcities.csv)
# ----------------------------------------------------------------------------

wc_raw <- read_csv("data_raw/raw_worldcities.csv")

wc_clean <- wc_raw %>%
  clean_names() %>%
  select(city, country, lat, lng, population) %>%
  drop_na(lat, lng)

write_csv(wc_clean, "data_clean/worldcities_clean.csv")
message("✅ worldcities_clean.csv gerado em data_clean/")

# ----------------------------------------------------------------------------
# 2.3. Cities Weather Forecast (raw_cities_weather_forecast.csv)
# ----------------------------------------------------------------------------

cw_raw <- read_csv("data_raw/raw_cities_weather_forecast.csv")

cw_clean <- cw_raw %>%
  clean_names() %>%
  rename(
    datetime    = dt_txt,
    temperature = main_temp,
    humidity    = main_humidity,
    wind_speed  = wind_speed,
    cloud_cover = clouds_all
  ) %>%
  mutate(
    datetime = ymd_hms(datetime),
    city     = str_trim(city)
  ) %>%
  drop_na(temperature, humidity)

write_csv(cw_clean, "data_clean/cities_weather_clean.csv")
message("✅ cities_weather_clean.csv gerado em data_clean/")

# ----------------------------------------------------------------------------
# 2.4. Seoul Bike Sharing (raw_seoul_bike_sharing.csv)
# ----------------------------------------------------------------------------

seoul_raw <- read_csv(
  "data_raw/raw_seoul_bike_sharing.csv",
  col_types = cols(
    Date                  = col_character(),
    Hour                  = col_integer(),
    `Rented Bike Count`   = col_integer(),
    Temperature           = col_double(),
    Humidity              = col_double(),
    `Wind Speed`          = col_double(),
    Visibility            = col_double(),
    `Dew Point Temperature` = col_double(),
    `Solar Radiation`       = col_double(),
    Rainfall              = col_double(),
    Snowfall              = col_double(),
    Season                = col_character(),
    Holiday               = col_character(),
    `Functioning Day`     = col_character()
  )
)

# 2.4.1. Aplicar clean_names() e inspecionar os nomes reais
seoul_tmp <- seoul_raw %>% clean_names()
print("Nomes após clean_names() em seoul_raw:")
print(names(seoul_tmp))
# Saída esperada:
# [1] "date"                   "rented_bike_count"     
#     "hour"                   "temperature_c"        
#     "humidity_percent"      "wind_speed_m_s"       
#     "visibility_10m"        "dew_point_temperature_c"
#     "solar_radiation_mj_m2" "rainfall_mm"          
#     "snowfall_cm"           "seasons"              
#     "holiday"               "functioning_day"      

# 2.4.2. Limpeza e conversão de tipos usando os nomes exibidos acima
seoul_clean <- seoul_tmp %>%
  mutate(
    # Converter data/hora
    date                  = dmy(date),
    hour                  = factor(hour, levels = as.character(0:23)),
    # Converter campos numéricos
    rented_bike_count     = as.integer(rented_bike_count),
    temperature           = as.numeric(temperature_c),
    humidity              = as.numeric(humidity_percent),
    wind_speed            = as.numeric(wind_speed_m_s),
    visibility            = as.numeric(visibility_10m),
    dew_point_temperature = as.numeric(dew_point_temperature_c),
    solar_radiation       = as.numeric(solar_radiation_mj_m2),
    precipitation         = as.numeric(rainfall_mm),   # renomear rainfall_mm → precipitation
    snow_fall             = as.numeric(snowfall_cm),   # renomear snowfall_cm → snow_fall
    season                = factor(seasons, levels = c("Winter", "Spring", "Summer", "Autumn")),
    holiday               = if_else(holiday == "Holiday", 1L, 0L),
    functioning_day       = if_else(functioning_day == "Yes", 1L, 0L)
  ) %>%
  drop_na(
    date, hour, rented_bike_count,
    temperature, humidity, wind_speed, visibility,
    dew_point_temperature, solar_radiation, precipitation, snow_fall,
    season, holiday, functioning_day
  ) %>%
  select(
    date,
    hour,
    rented_bike_count,
    temperature,
    humidity,
    wind_speed,
    visibility,
    dew_point_temperature,
    solar_radiation,
    precipitation,
    snow_fall,
    season,
    holiday,
    functioning_day
  )

write_csv(seoul_clean, "data_clean/seoul_bike_clean.csv")
message("✅ seoul_bike_clean.csv gerado em data_clean/ com ", nrow(seoul_clean), " registros.")

# ------------------------------------------------------------------------------------
# Fim do script de limpeza/disputa
# ------------------------------------------------------------------------------------



