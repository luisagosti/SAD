# ====================================================================================
# File: scripts/6_shiny_dashboard.R
# Description: Aplicação Shiny para previsão de demanda de bicicletas
# ====================================================================================

library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(DT)

# 1. Carregar modelo treinado (prefixo de modelo meteorológico final)
model_meteo <- readRDS("models/fit_glmnet_final.rds")

# 2. Dados fixos de cidades de interesse
cidades_info <- tribble(
  ~city,          ~lat,     ~lng,
  "New York,US",  40.7128, -74.0060,
  "Paris,FR",     48.8566,   2.3522,
  "Suzhou,CN",    31.2989, 120.5853,
  "London,UK",    51.5074,  -0.1278
)

# 3. Função para obter previsão de demanda usando OpenWeather + modelo
api_key <- "YOUR_API_KEY"  # Substitua com sua chave real

get_weather_and_demand <- function(city_name) {
  # 3.1. Chamar API OpenWeather 5-day/3h
  url <- paste0(
    "https://api.openweathermap.org/data/2.5/forecast?q=",
    URLencode(city_name),
    "&units=metric&appid=", api_key
  )
  resp <- GET(url)
  data <- fromJSON(rawToChar(resp$content), flatten = TRUE)
  df_forecast <- data$list %>%
    as_tibble() %>%
    select(
      dt_txt,
      main.temp, main.humidity,
      wind.speed, visibility,
      clouds.all
    ) %>%
    rename(
      datetime = dt_txt,
      temperature = main.temp,
      humidity = main.humidity,
      wind_speed = wind.speed,
      cloud_cover = clouds.all
    ) %>%
    mutate(
      datetime = ymd_hms(datetime),
      city = city_name
    )
  
  # 3.2. Preparar dados para o modelo
  df_model_input <- df_forecast %>%
    transmute(
      temperature = temperature,
      humidity = humidity,
      wind_speed = wind_speed,
      visibility = visibility,
      dew_point_temperature = 0,  # imputação default
      solar_radiation = 0,        # imputação default
      precipitation = 0,          # imputação default
      snow_fall = 0               # imputação default
    )
  
  # 3.3. Prever demanda
  preds <- predict(model_meteo, df_model_input) %>%
    bind_cols(df_forecast %>% select(datetime, temperature, humidity, wind_speed, visibility, city)) %>%
    rename(predicted_demand = .pred)
  
  return(preds)
}

# 4. UI da aplicação
ui <- fluidPage(
  titlePanel("Previsão de Demanda de Bicicletas Compartilhadas"),
  sidebarLayout(
    sidebarPanel(
      helpText("Selecione a cidade para ver a previsão de demanda"),
      selectInput("sel_city", "Cidade", choices = cidades_info$city)
    ),
    mainPanel(
      leafletOutput("mapa_cidades", height = 400),
      hr(),
      h4(textOutput("titulo_previsao")),
      plotOutput("plot_previsao"),
      DTOutput("table_previsao")
    )
  )
)

# 5. Server da aplicação
server <- function(input, output, session) {
  
  # 5.1. Renderizar mapa com marcadores das cidades
  output$mapa_cidades <- renderLeaflet({
    leaflet(cidades_info) %>%
      addTiles() %>%
      addMarkers(
        lng = ~lng,
        lat = ~lat,
        label = ~city
      )
  })
  
  # 5.2. Reactive: Dados de previsão para a cidade selecionada
  forecast_data <- reactive({
    get_weather_and_demand(input$sel_city)
  })
  
  # 5.3. Título dinâmico acima do gráfico
  output$titulo_previsao <- renderText({
    paste("Previsão de Demanda para", input$sel_city)
  })
  
  # 5.4. Plot de demanda prevista ao longo do tempo
  output$plot_previsao <- renderPlot({
    df <- forecast_data()
    ggplot(df, aes(x = datetime, y = predicted_demand)) +
      geom_line() +
      geom_point() +
      labs(
        x = "Data/Hora",
        y = "Demanda Prevista",
        title = paste("Demanda Prevista para", input$sel_city)
      ) +
      theme_minimal()
  })
  
  # 5.5. Tabela com data, temperatura e demanda prevista
  output$table_previsao <- renderDT({
    df <- forecast_data()
    datatable(
      df %>% select(datetime, temperature, humidity, wind_speed, visibility, predicted_demand),
      options = list(pageLength = 10),
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        'Fonte: OpenWeather API + modelo preditivo'
      )
    )
  })
}

# 6. Executar a aplicação
shinyApp(ui = ui, server = server)
