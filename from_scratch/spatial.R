rm(list=ls())

library(dplyr)
library(tidyr)
library(INLA)
library(spdep)
library(lubridate)
library(stringr)
library(purrr)
library(zoo)
library(tibble)
library(tsibble)
library(Matrix) 
library(ggplot2)
library(INLAutils)

# load data
total_data <- read.csv('../aggregated_data/CATANDUANES_total.csv')

# load shapefile
shapefile <- st_read('catanduanes_shapefile.shp')

ggplot() + 
  geom_sf(data = shapefile, size = 1.5, color = "black", fill = "cyan1") + 
  ggtitle("Catanduanes") + 
  coord_sf()

sf::sf_use_s2(FALSE)

# define prior
sdunif = "expression: logdens=-log_precision/2; return(logdens);"

pc_prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)
  ),
  phi = list(
    prior = "pc",
    param = c(0.5, 2/3)
  )
)


# build adjancency matrix from shapefile
nb <- poly2nb(shapefile)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

# define index for every polygon
shapefile$idarea <- 1:nrow(shapefile)

# combine data with shapefile

shapefile <- shapefile %>%
  rename(Municipality = Labels)

map <- shapefile
map <- map %>%
  full_join(total_data, by="Municipality")


# run inla model (just for cases)
formula = Deaths ~ f(idarea, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)))

mod <- inla(formula, family="poisson", data=map,control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))

summary(mod)
plot(mod)

fitted_values <- mod$summary.fitted.values[, "mean"]