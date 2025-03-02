# from https://becarioprecario.bitbucket.io/inla-gitbook/ch-temporal.html#sec:spacetime

library("DClusterm")
library("INLA")
data(brainNM)

nm.adj <- poly2nb(brainst@sp)
adj.mat <- as(nb2mat(nm.adj, style = "B"), "Matrix")

# Prior of precision
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

brain.st <- inla(Observed ~ 1 + f(Year, model = "rw1",
                                  hyper = prec.prior) + 
                   f(as.numeric(ID), model = "besag", graph = adj.mat,
                     hyper = prec.prior),
                 data = brainst@data, E = Expected, family = "poisson",
                 control.predictor = list(compute = TRUE, link = 1))
summary(brain.st)


brainst@data$ID.Year <- brainst@data$Year - 1973 + 1
brainst@data$ID2 <- brainst@data$ID


brain.st2 <- inla(Observed ~ 1 + 
                    f(as.numeric(ID2), model = "besag", graph = adj.mat,
                      group = ID.Year, control.group = list(model = "ar1"),
                      hyper = prec.prior),
                  data = brainst@data, E = Expected, family = "poisson",
                  control.predictor = list(compute = TRUE, link = 1))
summary(brain.st2)
