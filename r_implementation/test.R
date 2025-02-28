# based off https://ourcodingclub.github.io/tutorials/inla/#spatial

library(sp)
library(INLA)
library(dplyr)

# Create Mesh

# Load Location Data
location_data <- read.csv("../location_data/filtered_data/CATANDUANES.csv")

# Load cases data
cases <- read.csv("../aggregated_data/CATANDUANES.csv")

head(cases)


locations <- cbind(location_data$Longitude, location_data$Latitude)

Mesh <- inla.mesh.2d(locations, min.angle = 26, max.edge = c(10, 20))

plot(Mesh)

points(locations, col = "red", pch = 20)



# convert to A matrix

Matrix <- inla.spde.make.A(Mesh, loc = locations)
cases.spde = inla.spde2.pcmatern(mesh = Mesh, prior.range = c(10, 0.5), prior.sigma = c(.5, .5))
w.cases <- inla.spde.make.index('w', n.spde = cases.spde$n.spde)


# create model matrix
X0 <- model.matrix(as.formula(" ~ -1 + Date"), data = cases)
X <- as.data.frame(X0)

head(X)

#make stacks

N <- nrow(cases)
StackHost <- inla.stack(
  data = list(y= cases[,"NewCases"]),
  A = list(1, 1, Matrix),
  effects = list(
    Intercept = rep(1, N),
    X = X,
    w = w.cases
  )
)