# based off the book

library(sp)
library(INLA)
library(dplyr)
library(tidyverse)
library(INLAutils)
library(ggplot2)
library(ggregplot)

# load location data and create mesh
location_data <- read.csv("../location_data/filtered_data/CATANDUANES.csv")

locations <- cbind(location_data$Longitude, location_data$Latitude)

mesh <- inla.mesh.2d(
  locations, 
  min.angle=26, 
  max.edge = c(100, 200)
)

plot(mesh)

points(locations, col = "red", pch = 20)


# load cases data
cases <- read.csv("../aggregated_data/CATANDUANES_total.csv")


# create model based off example 9.12 from book
sigma0 = 1
size = min(c(diff(range(mesh$loc[,1])),
             diff(range(mesh$loc[,2]))))
range0 = size/5
kappa0 = sqrt(8)/range0
tau0 = 1/(sqrt(4*pi)*kappa0*sigma0)
spde = inla.spde2.matern(
  mesh,
  B.tau = cbind(log(tau0), -1, +1),
  B.kappa = cbind(log(kappa0), 0, -1),
  theta.prior.mean = c(0,0),
  constr = TRUE
)

municipalities <- factor(c('SAN MIGUEL', 'BAGAMANOC', 'VIRAC', 'CARAMORAN', 'GIGMOTO', 'BARAS', 'BATO', 'PANDAN', 'PANGANIBAN', 'SAN ANDRES', 'VIGA'))

# fitting model 
formula = log(Cases) ~ 1 + f(Municipality, model=spde, values=municipalities)

model = inla(
  formula,
  family="gaussian",
  data = cases,
  control.predictor=list(compute=TRUE),
  control.compute=list(dic=TRUE, config=TRUE)
)

summary(model)

plot(model)

autoplot(model)

autoplot(mesh)

proj = inla.mesh.projector(mesh)
inla.mesh.project(mesh)


#from https://becarioprecario.bitbucket.io/inla-gitbook/ch-temporal.html#separable-models
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

model2 <- inla(NewCases ~ 1 + f(Date, model="rw1", hyper = prec.prior)
               + f(as.numeric(Mun_ID), model ="besag", graph = mesh, hyper = prec.prior),
               data = cases, family = "poisson",  control.predictor = list(compute = TRUE, link = 1))
summary(model2)
