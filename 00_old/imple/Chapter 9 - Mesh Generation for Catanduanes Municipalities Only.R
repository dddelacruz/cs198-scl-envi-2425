library(sp)
library(INLA)
library(dplyr)

locations <- read.csv("C:/Users/user/Documents/Academics/CS 198/municipalities.csv")

locations <- filter(locations, Province == "Catanduanes")

locations$Longitude <- as.numeric(as.character(locations$Longitude))
locations$Latitude <- as.numeric(as.character(locations$Latitude))

locations <- locations[!is.na(locations$Longitude) & !is.na(locations$Latitude), ]

coords <- as.matrix(locations[, c("Longitude", "Latitude")])

mesh <- inla.mesh.create(
  coords,
  extend = list(offset = -0.2),
  cutoff = 0.01,
  refine = list(
    min.angle = 10,
    max.edge.data = 100,
    max.edge.extra = 200
  )
)

plot(mesh, col = "gray", main = "Mesh of Municipalities")
points(coords[,1], coords[,2], col = "red", pch = 20, bg = "red")