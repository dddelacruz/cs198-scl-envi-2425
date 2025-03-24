library("MASS")
library("INLA")
library("ggplot2")
library("tidyverse")
library(INLAutils)

summary(cement)

head(cement)

m1 <- inla(y ~ x1, data = cement)
summary(m1)

p <- autoplot(m1)

cement_data <- cement

formula <- y ~ x1 +x2+x3+x4
result <- inla(formula, data=cement_data,control.predictor=list(compute=TRUE, link=1), control.compute=list(return.marginals.predictor=TRUE))

autoplot(result)

plot(result, plot.prior=TRUE)

observed <- cement_data[1:13, 'y']
cement_data <- rbind(cement_data, cement_data[1:13,])

cement_data[1:13, 'y'] <- NA

ggplot_inla_residuals(result, observed, binwidth = 0.1)