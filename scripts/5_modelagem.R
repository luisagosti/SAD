# ====================================================================================
# File: scripts/5_modelagem.R
# Description: Modelagem preditiva com Tidymodels
# ====================================================================================

# 1. Carregar pacotes
library(tidyverse)
library(lubridate)
library(tidymodels)

# 2. Carregar dados limpos de Seoul
seoul <- read_csv("data_clean/seoul_bike_clean.csv")

# 3. Dividir dados em treino (80%) e teste (20%)
set.seed(123)
data_split <- initial_split(seoul, prop = 0.8, strata = rented_bike_count)
train_data <- training(data_split)
test_data  <- testing(data_split)

# ====================================================================================
# MODELO 1: Apenas variáveis meteorológicas
# ====================================================================================

# 4. Definir recipe para variáveis meteorológicas
recipe_meteo <- recipe(rented_bike_count ~ temperature + humidity + wind_speed +
                         visibility + dew_point_temperature + solar_radiation +
                         precipitation + snow_fall,
                       data = train_data) %>%
  step_normalize(all_numeric_predictors())

# 5. Definir modelo linear
lin_mod <- linear_reg() %>%
  set_engine("lm")

# 6. Criar workflow
wf_meteo <- workflow() %>%
  add_recipe(recipe_meteo) %>%
  add_model(lin_mod)

# 7. Ajustar modelo no conjunto de treino
fit_meteo <- fit(wf_meteo, data = train_data)

# 8. Avaliar no conjunto de teste
pred_meteo <- predict(fit_meteo, test_data) %>%
  bind_cols(test_data %>% select(rented_bike_count))

metrics_meteo <- pred_meteo %>%
  metrics(truth = rented_bike_count, estimate = .pred) %>%
  filter(.metric %in% c("rmse", "rsq", "mae"))

print("Desempenho do Modelo 1 (Meteorológicas):")
print(metrics_meteo)

# Salvar modelo ajustado
dir.create("models", showWarnings = FALSE)
saveRDS(fit_meteo, "models/fit_meteo_linear.rds")

# ====================================================================================
# MODELO 2: Apenas variáveis de data/hora
# ====================================================================================

# 9. Definir recipe para variáveis temporais
recipe_datahora <- recipe(rented_bike_count ~ hour + holiday +
                            functioning_day + season + date,
                          data = train_data) %>%
  step_mutate(
    weekday = wday(date, label = TRUE),
    month = month(date, label = TRUE)
  ) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_rm(date)

# 10. Workflow modelo linear
wf_datahora <- workflow() %>%
  add_recipe(recipe_datahora) %>%
  add_model(linear_reg() %>% set_engine("lm"))

# 11. Ajustar no conjunto de treino
fit_datahora <- fit(wf_datahora, data = train_data)

# 12. Avaliar no conjunto de teste
pred_datahora <- predict(fit_datahora, test_data) %>%
  bind_cols(test_data %>% select(rented_bike_count))

metrics_datahora <- pred_datahora %>%
  metrics(truth = rented_bike_count, estimate = .pred) %>%
  filter(.metric %in% c("rmse", "rsq", "mae"))

print("Desempenho do Modelo 2 (Data/Hora):")
print(metrics_datahora)

# Salvar modelo ajustado
saveRDS(fit_datahora, "models/fit_datahora_linear.rds")

# ====================================================================================
# MODELO 3 (OPCIONAL): Regularização com glmnet (tuning)
# ====================================================================================

# 13. Definir especificação do modelo glmnet
glmnet_spec <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")

# 14. Reusar recipe_meteo para variáveis meteorológicas
wf_glmnet <- workflow() %>%
  add_recipe(recipe_meteo) %>%
  add_model(glmnet_spec)

# 15. Definir resamples e grid de tuning
set.seed(234)
folds <- vfold_cv(train_data, v = 5, strata = rented_bike_count)

grid <- grid_regular(penalty(), mixture(), levels = 10)

# 16. Executar tuning
tune_res <- tune_grid(
  wf_glmnet,
  resamples = folds,
  grid = grid,
  metrics = metric_set(rmse, rsq)
)

best_params <- select_best(tune_res, "rmse")

# 17. Finalizar workflow com melhores parâmetros
final_glmnet <- finalize_workflow(wf_glmnet, best_params)

# 18. Ajustar o modelo final no conjunto de treino
fit_glmnet_final <- fit(final_glmnet, data = train_data)

# 19. Avaliar no conjunto de teste
pred_glmnet <- predict(fit_glmnet_final, test_data) %>%
  bind_cols(test_data %>% select(rented_bike_count))

metrics_glmnet <- pred_glmnet %>%
  metrics(truth = rented_bike_count, estimate = .pred) %>%
  filter(.metric %in% c("rmse", "rsq", "mae"))

print("Desempenho do Modelo 3 (Glmnet):")
print(metrics_glmnet)

# Salvar modelo final
saveRDS(fit_glmnet_final, "models/fit_glmnet_final.rds")

# ====================================================================================
# COMPARAÇÃO DOS MODELOS
# ====================================================================================

modelesults <- bind_rows(
  metrics_meteo %>% mutate(modelo = "Linear_Meteo"),
  metrics_datahora %>% mutate(modelo = "Linear_DataHora"),
  metrics_glmnet %>% mutate(modelo = "Glmnet_Meteo")
) %>%
  select(modelo, .metric, .estimate)

write_csv(modelesults, "models/metrics_comparison.csv")
