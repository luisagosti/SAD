# ====================================================================================
# File: scripts/4_EDA_ggplot.R
# Description: Análise exploratória com SQL (DBI) e visualizações com ggplot2
# ====================================================================================

# 1. Carregar pacotes necessários
library(DBI)
library(RSQLite)
library(tidyverse)
library(lubridate)

# 2. Conectar ao banco SQLite previamente criado em scripts/3_EDA_SQL.R
con <- dbConnect(RSQLite::SQLite(), "projeto_sad.sqlite")

# ------------------------------------------------------------------------------------
# 3. Executar consultas SQL para as Tarefas 1–11 e armazenar resultados em data frames
# ------------------------------------------------------------------------------------

# Tarefa 1 - Contagem de Registos no conjunto seoul_bike_sharing
t1 <- dbGetQuery(con, "
  SELECT COUNT(*) AS total_registros
  FROM seoul_bike;
")

# Tarefa 2 - Horas com rented_bike_count ≠ 0
t2 <- dbGetQuery(con, "
  SELECT COUNT(*) AS horas_com_aluguel
  FROM seoul_bike
  WHERE rented_bike_count > 0;
")

# Tarefa 3 - Previsão do tempo para Seul nas próximas 3 horas
t3 <- dbGetQuery(con, "
  SELECT *
  FROM cities_weather
  WHERE city LIKE 'Seoul%'
  ORDER BY datetime
  LIMIT 1;
")

# Tarefa 4 - Estações incluídas no conjunto seoul_bike
t4 <- dbGetQuery(con, "
  SELECT DISTINCT season
  FROM seoul_bike;
")

# Tarefa 5 - Primeira e última data no conjunto seoul_bike
t5 <- dbGetQuery(con, "
  SELECT MIN(date) AS primeira_data,
         MAX(date) AS ultima_data
  FROM seoul_bike;
")

# Tarefa 6 - Data/hora com máximo histórico de alugueres
t6 <- dbGetQuery(con, "
  SELECT date, hour, rented_bike_count
  FROM seoul_bike
  WHERE rented_bike_count = (
    SELECT MAX(rented_bike_count) FROM seoul_bike
  );
")

# Tarefa 7 - Top 10 combinações estação+hora por média de alugueres e temperatura média
t7 <- dbGetQuery(con, "
  SELECT season,
         hour,
         ROUND(AVG(temperature), 2) AS temp_media,
         ROUND(AVG(rented_bike_count), 2) AS alugueres_medios
  FROM seoul_bike
  GROUP BY season, hour
  ORDER BY alugueres_medios DESC
  LIMIT 10;
")

# Tarefa 8 - Estatísticas sazonalidade de alugueres (média, min, max, desvio padrão)
t8 <- dbGetQuery(con, "
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

# Tarefa 9 - Métricas meteorológicas médias e alugueres médios por estação
t9 <- dbGetQuery(con, "
  SELECT season,
         ROUND(AVG(temperature), 2)       AS temp_media,
         ROUND(AVG(humidity), 2)          AS hum_media,
         ROUND(AVG(wind_speed), 2)        AS vento_media,
         ROUND(AVG(visibility), 2)        AS vis_media,
         ROUND(AVG(dew_point_temperature), 2) AS dew_media,
         ROUND(AVG(solar_radiation), 2)   AS solar_media,
         ROUND(AVG(precipitation), 2)     AS precip_media,   -- em vez de rainfall
         ROUND(AVG(snow_fall), 2)         AS neve_media,     -- em vez de snowfall
         ROUND(AVG(rented_bike_count), 2) AS alugueres_media
  FROM seoul_bike
  GROUP BY season
  ORDER BY alugueres_media DESC;
")

# Confira os campos (colunas) da tabela bike_systems
dbListFields(con, "bike_systems")

# Tarefa 10 - Total de bicicletas e informações de Seul (join bike_systems + worldcities)
t10 <- dbGetQuery(con, "
  SELECT b.city AS city_bike,
         b.country AS country_bike,
         w.city AS city_world,
         w.country AS country_world,
         w.lat,
         w.lng,
         w.population
  FROM bike_systems b
  JOIN worldcities w
    ON LOWER(b.city) = LOWER(w.city)
   AND LOWER(b.country) = LOWER(w.country)
  WHERE LOWER(b.city) = 'seoul'
    AND LOWER(b.country) = 'south korea'
")


# Tarefa 11 - Cidades com escala de bicicletas entre 15000 e 20000
t11 <- dbGetQuery(con, "
  SELECT 
  'Seoul' AS city,
  w.lat,
  w.lng,
  SUM(s.rented_bike_count) AS number_of_bikes
FROM seoul_bike s
JOIN worldcities w
  ON LOWER(w.city) = 'seoul'
GROUP BY w.lat, w.lng
HAVING number_of_bikes BETWEEN 15000 AND 20000
ORDER BY number_of_bikes DESC;

")

# Desconectar após consultas
dbDisconnect(con)

# ------------------------------------------------------------------------------------
# 4. Exibir rapidamente no console (opcional)
# ------------------------------------------------------------------------------------
print("=== Tarefa 1 ==="); print(t1)
print("=== Tarefa 2 ==="); print(t2)
print("=== Tarefa 3 ==="); print(t3)
print("=== Tarefa 4 ==="); print(t4)
print("=== Tarefa 5 ==="); print(t5)
print("=== Tarefa 6 ==="); print(t6)
print("=== Tarefa 7 ==="); print(t7)
print("=== Tarefa 8 ==="); print(t8)
print("=== Tarefa 9 ==="); print(t9)
print("=== Tarefa 10 ==="); print(t10)
print("=== Tarefa 11 ==="); print(t11)

# ------------------------------------------------------------------------------------
# 5. Visualizações com ggplot2
# ------------------------------------------------------------------------------------

# Criar pasta "figures" se não existir
dir.create("figures", showWarnings = FALSE)

# ---- Plot para Tarefa 7: Top 10 season+hour por alugueres médios ----
# Usar t7 para gráfico de barras horizontais
p7 <- t7 %>%
  mutate(season_hour = paste0(season, " - ", hour)) %>%
  arrange(alugueres_medios) %>%
  ggplot(aes(x = alugueres_medios, y = fct_reorder(season_hour, alugueres_medios))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Top 10 combinações Estação + Hora por Média de Alugueres",
    x = "Alugueres Médios",
    y = "Estação - Hora"
  ) +
  theme_minimal()

ggsave("figures/tarefa07_top10_season_hour.png", p7, width = 8, height = 5)

# ---- Plot para Tarefa 8: Estatísticas de alugueres por estação ----
# Mostrar média + barras de erro (min/max) - e então um ponto para desvio padrão
# Criar data frame com média, min, max, sd a partir de t8
t8_plot <- t8 %>%
  rename(
    media_alug = media,
    min_alug = minimo,
    max_alug = maximo,
    sd_alug = desvio_padrao
  )

p8 <- t8_plot %>%
  ggplot(aes(x = season, y = media_alug)) +
  geom_col(fill = "skyblue") +
  geom_errorbar(aes(ymin = min_alug, ymax = max_alug), width = 0.2) +
  geom_point(aes(y = sd_alug), color = "red", size = 3) +
  labs(
    title = "Média, Mínimo, Máximo e Desvio Padrão de Alugueres por Estação",
    x = "Estação",
    y = "Alugueres (média ± erro; ponto vermelho = desvio padrão)"
  ) +
  theme_minimal()

ggsave("figures/tarefa08_alugueres_sazonal.png", p8, width = 8, height = 5)

# ---- Plot para Tarefa 9: Métricas meteorológicas e alugueres por estação ----
# Transformar t9 em formato longo para comparar variáveis meteorológicas
t9_long <- t9 %>%
  pivot_longer(
    cols = temp_media:neve_media,
    names_to = "metrica",
    values_to = "valor"
  )

# a) Gráfico de barras de alugueres médios por estação (com t9$alugueres_media)
p9a <- t9 %>%
  ggplot(aes(x = season, y = alugueres_media, fill = season)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Alugueres Médios por Estação (Tarefa 9)",
    x = "Estação",
    y = "Alugueres Médios"
  ) +
  theme_minimal()

ggsave("figures/tarefa09_alugueres_media_sazonal.png", p9a, width = 6, height = 4)

# b) Multiplot: métricas meteorológicas por estação em gráficos de linhas
# Para ficar mais visível, plotar cada métrica em painel facetado
p9b <- t9_long %>%
  filter(metrica != "alugueres_media") %>%
  ggplot(aes(x = season, y = valor, group = 1)) +
  geom_line(aes(color = metrica), size = 1) +
  geom_point(aes(color = metrica), size = 2) +
  facet_wrap(~ metrica, scales = "free_y", ncol = 3) +
  labs(
    title = "Métricas Meteorológicas Médias por Estação (Tarefa 9)",
    x = "Estação",
    y = "Valor Médio"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("figures/tarefa09_metricas_meteorologicas.png", p9b, width = 10, height = 6)

# ---- Plot para Tarefa 11: Mapa de cidades com número de bicicletas entre 15000 e 20000 ----
# Utilizar t11 para scatter plot das coordenadas
p11 <- t11 %>%
  ggplot(aes(x = lng, y = lat)) +
  geom_point(aes(size = number_of_bikes, color = number_of_bikes), alpha = 0.7) +
  geom_text(aes(label = city), vjust = -1, size = 3) +
  scale_size_continuous(name = "Nº de Bikes") +
  scale_color_viridis_c(name = "Nº de Bikes") +
  labs(
    title = "Cidades com 15000–20000 Bicicletas Compartilhadas (Tarefa 11)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

ggsave("figures/tarefa11_cidades_15000_20000.png", p11, width = 8, height = 6)

# ------------------------------------------------------------------------------------
# 6. Mensagem de conclusão e contagem de objetos gerados
# ------------------------------------------------------------------------------------
message("✅ Análise exploratória concluída. ")
message("Arquivos gerados em 'figures/':")
message(" - tarefa07_top10_season_hour.png")
message(" - tarefa08_alugueres_sazonal.png")
message(" - tarefa09_alugueres_media_sazonal.png")
message(" - tarefa09_metricas_meteorologicas.png")
message(" - tarefa11_cidades_15000_20000.png")

