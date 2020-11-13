library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)

interactive <- FALSE

data <- read.csv("AirPassengers.csv")

head(data)
data %>% glimpse()

data[!complete.cases(data),] %>% View()

data$Month <- paste0(data$Month, "-01") %>% as.Date("%Y-%m-%d") 

#visualize
data %>% plot_time_series(Month, X.Passengers, .interactive = interactive)

# Split Data 80/20
splits <- initial_time_split(data, prop = 0.9)


# Model 1: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(X.Passengers ~ Month + as.numeric(Month) + factor(month(Month, label = TRUE), ordered = F),
      data = training(splits))


# Model 2: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(X.Passengers ~ Month, data = training(splits))

# Model 3: prophet ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(X.Passengers ~ Month, data = training(splits))



#Add fitted models to a Model Table.----

models_tbl <- modeltime_table(
  model_fit_arima_boosted,
  model_fit_ets,
  model_fit_prophet
)

models_tbl



#Calibrate the model to a testing set.----
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl


# Visualizing the Forecast Test----
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = data
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )


#Accuracy Metrics----
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )


#Refit to Full Dataset & Forecast Forward----

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data)

refit_tbl %>%
  modeltime_forecast(h = "3 years", actual_data = data) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )




