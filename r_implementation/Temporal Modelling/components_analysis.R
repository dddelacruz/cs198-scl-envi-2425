library("prophet")
library("INLA")
library("ggplot2")
library("tidyverse")
library("dplyr")

agg_data <- read.csv('../aggregated_data/CATANDUANES2.csv')
filtered <- agg_data %>% filter(Municipality == "SAN MIGUEL")

filtered$Date <- as.Date(filtered$Date, format = "%Y-%m-%d")

filtered_xy <- filtered %>% 
  rename(ds= Date,
         y = NewCases)

filtered_model <- prophet(filtered_xy, seasonality.mode = 'additive')

forecast <- predict(filtered_model)

prophet_plot_components(filtered_model, forecast)