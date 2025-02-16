library(sp)
library(INLA)
library(dplyr)

# Create Mesh

# Load Location Data
location_data <- read.csv("../location_data/filtered_data/CATANDUANES.csv")

locations <- cbind(location_data$Longitude, location_data$Latitude)

Mesh <- inla.mesh.2d(locations)

plot(Mesh)

points(locations, col = "red", pch = 2)

# convert to A matrix

martixA <- inla