Sys.setlocale("LC_ALL","English")
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
df <- read.csv('../aggregated_data/CATANDUANES.csv')

# filter data to only include 2021-2022 data
df <- df %>%
  filter(Date <= ymd('2022-12-31')) %>%
  filter(Date >= ymd('2021-01-01'))

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
shapefile@data <-  shapefile@data %>%
  rename(Municipality = Labels) %>%
  full_join(df, by="Municipality")

# remove unnecessary columns
shapefile@data <- select(shapefile@data,-c(FID))

# define an index per date and area
shapefile@data <- shapefile@data %>%
  mutate(
    idarea1 = idarea,
    idtime = as.numeric(factor(Date)),
    idtime1 = idtime,
    idareatime = 1:nrow(df)
  )

## run inla model (just for cases) (adapts type II model from study)
# define variables for constraints
s <- length(unique(shapefile@data$idarea))
t <- length(unique(shapefile@data$idtime))

#Define the temporal structure matrix of a RW1
D1 <- diff(diag(t), differences = 1)
Rt <- t(D1) %*% D1

#Define the spatial structure matrix
Rs <- matrix(0, g$n, g$n)
for (i in 1:g$n) {
  Rs[i, i] = g$nnbs[[i]]
  Rs[i, g$nbs[[i]]] = -1
}

# define formula for type II interaction
#Define the structure matrix of this type of interaction effect
R <- kronecker(Rt, Diagonal(s))
r <- s
#Define the constraints
A <- kronecker(matrix(1, 1, t), diag(s))
A <- A[-1, ]
e <- rep(0, s - 1)

formula_II <-
  NewCases ~ f(
    idarea,
    model = "bym2",
    graph = g,
    hyper = pc_prior,
    constr = TRUE
  ) +
  f(
    idtime,
    model = "rw1",
    hyper = list(prec = list(prior = sdunif)),
    constr = TRUE
  ) +
  f(idareatime,
    model = "generic0", Cmatrix=R, rankdef=r,
    hyper=list(prec=list(prior=sdunif)),
    constr = TRUE, extraconstr=list(A=A, e=e)
  )

# run INLA model
mod_II <- inla(formula_II, family="poisson", data=shapefile@data, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))


# add fitted values to dataframe
shapefile@data$typeII <- mod_II$summary.fitted.values[, "mean"]


# compute relative risks


# plot relative risks on shapefile

# export results
write.csv(shapefile@data, "Results/spatio_temporal.csv")

