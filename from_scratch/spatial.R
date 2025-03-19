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
library(rgdal)
library(sp)

# load data
total_data <- read.csv('../aggregated_data/CATANDUANES_total.csv')

# load shapefile
shapefile <- readOGR('catanduanes_shapefile.shp')

ggplot() + 
  geom_polygon(data = shapefile, colour = "black", fill = NA,  aes(x = long, y = lat, group = group, fill=Cases)) +
  coord_fixed()

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
shapefile$idarea <- 1:nrow(shapefile@data)

# combine data with shapefile

shapefile@data <- shapefile@data %>%
  rename(Municipality = Labels)

shapefile@data <- shapefile@data %>%
  full_join(total_data, by="Municipality")

# remove unnecessary columns
shapefile@data <- select(shapefile@data,-c(X,FID))

# run inla model (just for cases)
formula <- Cases ~ f(idarea, model = "bym", graph = g, hyper = list(prec.unstruct = list(prior = sdunif), prec.spatial = list(prior = sdunif)))

bym <- inla(formula, family="poisson", data=shapefile@data,control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))

formula2 <- Cases ~ f(idarea, model = "bym2", graph = g, hyper = pc_prior)
bym2 <- inla(formula2, family="poisson", data=shapefile@data,control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))

# add fitted values to dataframe
shapefile@data$bym <-  bym$summary.fitted.values[, "mean"]
shapefile@data$bym2 <-  bym2$summary.fitted.values[, "mean"]

summary(bym)
summary(bym2)

# compute relative risks


# plot relative risks on shapefile
