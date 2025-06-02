# ====================================================================================
# File: scripts/3_EDA_SQL.R
# Description: Execução de queries SQL conforme Tarefas 1–11
# ====================================================================================

# 1. Carregar pacotes
library(DBI)
library(RSQLite)
library(tidyverse)

# 2. Conectar/Cria banco SQLite
if (file.exists("projeto_sad.sqlite")) {
  file.remove("projeto_sad.sqlite")
}
con <- dbConnect(RSQLite::SQLite(), "projeto_sad.sqlite")

# 3. Carregar dataframes limpos e gravar como tabelas SQL
seoul_df <- read_csv("data_clean/seoul_bike_clean.csv")
bike_systems_df <- read_csv("data_clean/bike_systems_clean.csv")
worldcities_df <- read_csv("data_clean/worldcities_clean.csv")
cities_weather_df <- read_csv("data_clean/cities_weather_clean.csv")

dbWriteTable(con, "seoul_bike", seoul_df, overwrite = TRUE)
dbWriteTable(con, "bike_systems", bike_systems_df, overwrite = TRUE)
dbWriteTable(con, "worldcities", worldcities_df, overwrite = TRUE)
dbWriteTable(con, "cities_weather", cities_weather_df, overwrite = TRUE)

# 4. Tarefa 1: Contagem de registros em Seoul Bike Sharing
res1 <- dbGetQuery(con, "SELECT COUNT(*) AS total_registros FROM seoul_bike;")
print("Tarefa 1: Total de registros em seoul_bike")
print(res1)

# 5. Tarefa 2: Horas com rented_bike_count ≠ 0
res2 <- dbGetQuery(con, "
  SELECT COUNT(*) AS horas_com_aluguel
  FROM seoul_bike
  WHERE rented_bike_count > 0;
")
print("Tarefa 2: Horas com alugueis > 0")
print(res2)

# 6. Tarefa 3: Previsão do tempo para Seul nas próximas 3 horas
res3 <- dbGetQuery(con, "
  SELECT *
  FROM cities_weather
  WHERE city LIKE 'Seoul%'
  ORDER BY datetime
  LIMIT 1;
")
print("Tarefa 3: Próximo registro de previsão para Seul")
print(res3)

# 7. Tarefa 4: Quais estações existem em seoul_bike?
res4 <- dbGetQuery(con, "
  SELECT DISTINCT season
  FROM seoul_bike;
")
print("Tarefa 4: Estações distintas em seoul_bike")
print(res4)

# 8. Tarefa 5: Primeira e última data no dataset
res5 <- dbGetQuery(con, "
  SELECT MIN(date) AS primeira_data, MAX(date) AS ultima_data
  FROM seoul_bike;
")
print("Tarefa 5: Primeira e última data em seoul_bike")
print(res5)

# 9. Tarefa 6: Data/hora com máximo histórico de alugueis
res6 <- dbGetQuery(con, "
  SELECT date, hour, rented_bike_count
  FROM seoul_bike
  WHERE rented_bike_count = (
    SELECT MAX(rented_bike_count) FROM seoul_bike
  );
")
print("Tarefa 6: Registro com maior rented_bike_count")
print(res6)

# 10. Tarefa 7: Popularidade horária e temperatura média por estação (top 10)
res7 <- dbGetQuery(con, "
  SELECT season,
         hour,
         ROUND(AVG(temperature), 2) AS temp_media,
         ROUND(AVG(rented_bike_count), 2) AS alugueis_medios
  FROM seoul_bike
  GROUP BY season, hour
  ORDER BY alugueis_medios DESC
  LIMIT 10;
")
print("Tarefa 7: Top 10 season+hour por alugueis médios")
print(res7)

# 11. Tarefa 8: Sazonalidade do aluguel (média, min, max, desvio padrão)
res8 <- dbGetQuery(con, "
  SELECT season,
         ROUND(AVG(rented_bike_count), 2) AS media,
         MIN(rented_bike_count) AS minimo,
         MAX(rented_bike_count) AS maximo,
         ROUND(
           SQRT(
             AVG(rented_bike_count * rented_bike_count)
             - AVG(rented_bike_count) * AVG(rented_bike_count)
           ), 2
         ) AS desvio_padrao
  FROM seoul_bike
  GROUP BY season;
")
print("Tarefa 8: Estatísticas de rented_bike_count por estação")
print(res8)

# 12. Tarefa 9: Sazonalidade meteorológica por estação
res9 <- dbGetQuery(con, "
  SELECT season,
         ROUND(AVG(temperature), 2) AS temp_media,
         ROUND(AVG(humidity), 2) AS hum_media,
         ROUND(AVG(wind_speed), 2) AS vento_media,
         ROUND(AVG(visibility), 2) AS vis_media,
         ROUND(AVG(dew_point_temperature), 2) AS dew_media,
         ROUND(AVG(solar_radiation), 2) AS solar_media,
         ROUND(AVG(precipitation), 2) AS precip_media,
         ROUND(AVG(snow_fall), 2) AS neve_media,
         ROUND(AVG(rented_bike_count), 2) AS alugueis_media
  FROM seoul_bike
  GROUP BY season
  ORDER BY alugueis_media DESC;
")
print("Tarefa 9: Médias meteorológicas e de alugueis por estação")
print(res9)

# 13. Tarefa 10: Total de bicicletas em Seul + info da cidade
res10 <- dbGetQuery(con, "
  SELECT b.city AS city_bike, b.country AS country_bike,
         w.city AS city_world, w.country AS country_world,
         w.lat, w.lng, w.population
  FROM bike_systems b
  JOIN worldcities w
    ON LOWER(b.city) = LOWER(w.city)
   AND LOWER(b.country) = LOWER(w.country)
  WHERE LOWER(b.city) = 'seoul'
    AND LOWER(b.country) = 'south korea';
")
print("Tarefa 10: Dados de Seul (sem número de bicicletas)")
print(res10)


# 14. Tarefa 11: Cidades com número de bikes entre 15000 e 20000
res11 <- dbGetQuery(con, "
  SELECT b.city, b.country,
         w.lat, w.lng, w.population
  FROM bike_systems b
  JOIN worldcities w
    ON LOWER(b.city) = LOWER(w.city)
   AND LOWER(b.country) = LOWER(w.country)
  WHERE b.discontinued IS NULL -- sistemas ativos
  ORDER BY b.city;
")
print("Tarefa 11: Cidades com sistemas de bike-sharing ativos")
print(res11)


# 15. Fechar conexão
dbDisconnect(con)
