library("tidyquant")
library(TTR)
library("INLA")
library("ggplot2")
library("tidyverse")

agg_data <- read.csv('../aggregated_data/CATANDUANES2.csv')
filtered <- agg_data %>% filter(Municipality == "SAN MIGUEL")
head(filtered)

filtered$Date <- as.Date(filtered$Date, format = "%Y-%m-%d")

ggplot(data=filtered, aes(x=Date, y=NewCases))+ geom_ma(ma_fun = SMA, color = "orange", n = 4) + geom_ma(ma_fun = SMA, n=5, color = "lightblue") + geom_ma(ma_fun = SMA, color = "red", n = 7)  + geom_ma(ma_fun = SMA, color = "violet", n = 14) + geom_line() + theme_classic()
