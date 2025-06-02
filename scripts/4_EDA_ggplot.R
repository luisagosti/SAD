# ====================================================================================
# File: scripts/4_EDA_ggplot.R
# Description: Análise exploratória e visualizações com ggplot2
# ====================================================================================

# 1. Carregar pacotes
library(tidyverse)
library(lubridate)

# 2. Carregar dataset limpo de Seoul
seoul <- read_csv("data_clean/seoul_bike_clean.csv")

# 3. Conferir estrutura e estatísticas básicas
glimpse(seoul)
summary(seoul)

# 4. Tarefa 5: Quantos registros são feriados?
num_feriados <- seoul %>%
  filter(holiday == 1) %>%
  nrow()
message("Número de registros em feriado: ", num_feriados)

# 5. Tarefa 6: Percentual de registros que são feriado
perc_feriados <- seoul %>%
  summarise(pct = mean(holiday == 1) * 100) %>%
  pull(pct)
message("Percentual de registros em feriado: ", round(perc_feriados, 2), "%")

# 6. Tarefa 7: Quantos registros esperaríamos para um ano completo?
registros_info <- seoul %>%
  summarise(
    min_date = min(date),
    max_date = max(date),
    diff_days = as.numeric(difftime(max_date, min_date, units = "days")),
    total_registros = n()
  )
print(registros_info)

# 7. Tarefa 8: Frequência de functioning_day
freq_function <- seoul %>%
  count(functioning_day)
print(freq_function)

# 8. Tarefa 9: Precipitação total e queda de neve por estação
precip_sazonal <- seoul %>%
  group_by(season) %>%
  summarise(
    total_precip = sum(precipitation, na.rm = TRUE),
    total_snow = sum(snow_fall, na.rm = TRUE)
  )
print(precip_sazonal)

# 9. Tarefa 10: Scatter RENTED_BIKE_COUNT vs DATE
p1 <- ggplot(seoul, aes(x = date, y = rented_bike_count)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Contagem Horária de Aluguel vs Data",
    x = "Data",
    y = "Rented Bike Count"
  )
ggsave("figures/scatter_bike_date.png", p1, width = 8, height = 4)

# 10. Tarefa 11: Scatter colorido por HORA
p2 <- ggplot(seoul, aes(x = date, y = rented_bike_count, color = hour)) +
  geom_point(alpha = 0.3) +
  labs(
    title = "Rented Bike Count vs Date (color por hora)",
    x = "Data",
    y = "Rented Bike Count",
    color = "Hora"
  )
ggsave("figures/scatter_bike_date_hour.png", p2, width = 8, height = 4)

# 11. Tarefa 12: Histograma + densidade de RENTED_BIKE_COUNT
p3 <- ggplot(seoul, aes(x = rented_bike_count)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.4) +
  geom_density() +
  labs(
    title = "Histograma e Densidade de Rented Bike Count",
    x = "Rented Bike Count",
    y = "Densidade"
  )
ggsave("figures/hist_density_rented.png", p3, width = 6, height = 4)

# 12. Tarefa 13: Scatter RENTED_BIKE_COUNT vs TEMPERATURE por estação, color por HORA
p4 <- ggplot(seoul, aes(x = temperature, y = rented_bike_count, color = hour)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~ season) +
  labs(
    title = "Rented Bike x Temperature por estação",
    x = "Temperatura (°C)",
    y = "Rented Bike Count",
    color = "Hora"
  )
ggsave("figures/scatter_bike_temp_season.png", p4, width = 10, height = 6)

# 13. Tarefa 14: Boxplots RENTED_BIKE_COUNT vs HORA por estação
p5 <- ggplot(seoul, aes(x = hour, y = rented_bike_count)) +
  geom_boxplot() +
  facet_wrap(~ season, nrow = 2) +
  labs(
    title = "Boxplot de Rented Bike Count vs Hour por estação",
    x = "Hour",
    y = "Rented Bike Count"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("figures/boxplot_bike_hour_season.png", p5, width = 10, height = 6)

# 14. Tarefa 15: Precipitação e queda de neve diárias
diario <- seoul %>%
  group_by(date) %>%
  summarise(
    total_precip = sum(precipitation, na.rm = TRUE),
    total_snow = sum(snow_fall, na.rm = TRUE)
  )
write_csv(diario, "data_clean/diario_precip_snow.csv")

# 15. Tarefa 16: Quantos dias tiveram queda de neve?
dias_neve <- diario %>%
  filter(total_snow > 0) %>%
  nrow()
message("Número de dias com neve: ", dias_neve)
