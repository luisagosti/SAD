# ====================================================================================
# File: scripts/1_coleta_dados.R
# Description: Coleta de dados raw para o projeto
# ====================================================================================

# 1. Carregar pacotes necessários
library(rvest)
library(tidyverse)
library(httr)
library(jsonlite)

# 2. Diretórios de entrada/saída
dir.create("data_raw", showWarnings = FALSE)

# 3. Coleta da tabela de Bike Sharing Systems da Wikipedia
url_bike_systems <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
page <- read_html(url_bike_systems)

bike_table <- page %>%
  html_node("table.wikitable") %>%
  html_table(fill = TRUE)

write_csv(bike_table, "data_raw/raw_bike_sharing_systems.csv")

# 4. Importação manual do arquivo World Cities
# OBSERVAÇÃO: Baixe manualmente o CSV de SimpleMaps (ou outra fonte) e salve como:
# data_raw/raw_worldcities.csv
# Para referência, deixamos um stub para leitura caso já exista:
if (file.exists("data_raw/raw_worldcities.csv")) {
  message("raw_worldcities.csv encontrado em data_raw/.")
} else {
  message("⚠️ Coloque raw_worldcities.csv (SimpleMaps ou similar) em data_raw/.")
}

# 5. Coleta de previsão do tempo via OpenWeather API
#    – Defina sua API key abaixo (crie conta grátis em openweathermap.org)
api_key <- "2810b3593651a930239d07e75e3409f7"  # substitua pela sua chave real

# Lista de cidades desejadas (formato: "Cidade,País")
cidades <- c("Seoul,KR", "New York,US", "Paris,FR", "Suzhou,CN", "London,UK")

get_forecast <- function(city) {
  url <- paste0(
    "https://api.openweathermap.org/data/2.5/forecast?q=",
    URLencode(city),
    "&units=metric&appid=", api_key
  )
  resp <- GET(url)
  data <- fromJSON(rawToChar(resp$content), flatten = TRUE)
  df <- data$list %>%
    as_tibble() %>%
    select(
      dt_txt,
      main.temp, main.humidity,
      wind.speed, visibility,
      clouds.all,
      weather
    ) %>%
    mutate(city = city)
  return(df)
}

all_forecasts <- map_dfr(cidades, get_forecast)
write_csv(all_forecasts, "data_raw/raw_cities_weather_forecast.csv")

# 6. Importação manual do dataset Seoul Bike Sharing
# OBSERVAÇÃO: Baixe manualmente de https://www.kaggle.com/datasets/rtgacusan/seoul-bike-sharing-demand
# e salve como data_raw/raw_seoul_bike_sharing.csv
if (file.exists("data_raw/raw_seoul_bike_sharing.csv")) {
  message("raw_seoul_bike_sharing.csv encontrado em data_raw/.")
} else {
  message("⚠️ Coloque raw_seoul_bike_sharing.csv em data_raw/.")
}
