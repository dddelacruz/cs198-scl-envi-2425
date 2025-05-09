Sys.setlocale("LC_ALL","English")
rm(list=ls())

library(dplyr)
library(INLA)
library(Matrix) 
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

#ggplot() + 
#  geom_polygon(data = shapefile, colour = "black", fill = NA,  aes(x = long, y = lat, group = group, fill=Cases)) +
#  coord_fixed()

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
shapefile@data <- select(shapefile@data,-c(Longitude, Latitude, X, FID, d_cases, Recoveries, Deaths, NewCases))

temp <- shapefile@data

## define function for inla models
inla_mod_st <- function(df, model="bym2", iid=FALSE, rw="rw1", interaction="no"){
  # define an index for each area, time, and areatime
  df <- df %>% 
    mutate(
      idarea1 = idarea,
      idtime = as.numeric(factor(Date)),
      idtime1 = idtime,
      idareatime = 1:nrow(df)
    )
  
  temp<<-df
  
  # define variables for constraints
  s <- length(unique(df$idarea))
  t <- length(unique(df$idtime))
  
  #Define the temporal structure matrix of a RW1
  D1 <- diff(diag(t), differences = 1)
  Rt <- t(D1) %*% D1
  
  #Define the spatial structure matrix
  Rs <- matrix(0, g$n, g$n)
  for (i in 1:g$n) {
    Rs[i, i] = g$nnbs[[i]]
    Rs[i, g$nbs[[i]]] = -1
  }
  
  # for the base model
  if(interaction == "no"){
    formula <-
      n ~ 
      f(
        idarea,
        model = "bym",
        graph = g,
        hyper = list(
          prec.unstruct = list(prior = sdunif),
          prec.spatial = list(prior = sdunif)
        ),
        constr = TRUE
      ) +
      f(
        idtime,
        model = rw,
        hyper = list(prec = list(prior = sdunif)),
        constr = TRUE
      )
  }
  # type I interaction
  else if (interaction=="I"){
    if(model=="bym2"){
      if(!iid){
        formula <-
          n ~ f(
            idarea,
            model = "bym2",
            graph = g,
            hyper = pc_prior,
            constr = TRUE
          ) +
          f(
            idtime,
            model = rw,
            hyper = list(prec = list(prior = sdunif)),
            constr = TRUE
          ) +
          f(
            idareatime,
            model = "iid",
            hyper = list(prec = list(prior = sdunif)),
            constr = TRUE
          )
      }
      else{
        formula <-
          n ~ f(
            idarea,
            model = "bym2",
            graph = g,
            hyper = pc_prior,
            constr = TRUE
          ) +
          f(
            idtime,
            model = rw,
            hyper = list(prec = list(prior = sdunif)),
            constr = TRUE
          ) +
          f(
            idtime1,
            model = "iid",
            hyper = list(prec = list(prior = sdunif)),
            constr = TRUE
          ) +
          f(
            idareatime,
            model = "iid",
            hyper = list(prec = list(prior = sdunif)),
            constr = TRUE
          )
      }
    }
    
    else{
      formula <-
        n ~ f(
          idarea,
          model = "bym",
          graph = g,
          hyper = list(
            prec.unstruct = list(prior = sdunif),
            prec.spatial = list(prior = sdunif)
          ),
          constr = TRUE
        ) +
        f(
          idtime,
          model = rw,
          hyper = list(prec = list(prior = sdunif)),
          constr = TRUE
        ) +
        f(
          idareatime,
          model = "iid",
          hyper = list(prec = list(prior = sdunif)),
          constr = TRUE
        )
    }
  }
  
  # formula for type II interaction
  else if(interaction=="II"){
    #Define the structure matrix of this type of interaction effect
    R <- kronecker(Rt, Diagonal(s))
    r <- s
    #Define the constraints
    A <- kronecker(matrix(1, 1, t), diag(s))
    A <- A[-1, ]
    e <- rep(0, s - 1)
    
    formula <-
      n ~ f(
        idarea,
        model = "bym2",
        graph = g,
        hyper = pc_prior,
        constr = TRUE
      ) +
      f(
        idtime,
        model = rw,
        hyper = list(prec = list(prior = sdunif)),
        constr = TRUE
      ) +
      f(idareatime,
        model = "generic0", Cmatrix=R, rankdef=r,
        hyper=list(prec=list(prior=sdunif)),
        constr = TRUE, extraconstr=list(A=A, e=e)
      )
  }
  # for type III interaction
  else if(interaction=="III"){
    #Define the structure matrix of this type of interaction effect
    R <- kronecker(Diagonal(t), Rs)
    r <- t
    #Define the constraints
    A <- kronecker(Diagonal(t),matrix(1,1,s))
    A <- A[-1,]
    e <- rep(0,t-1)
    
    formula <-
      n ~ f(
        idarea,
        model = "bym2",
        graph = g,
        hyper = pc_prior,
        constr = TRUE
      ) +
      f(
        idtime,
        model = rw,
        hyper = list(prec = list(prior = sdunif)),
        constr = TRUE
      ) +
      f(idareatime,
        model = "generic0", Cmatrix=R, rankdef=r,
        hyper=list(prec=list(prior=sdunif)),
        constr = TRUE, extraconstr=list(A=A, e=e)
      )
  }
  # for type IV interaction
  else{
    #Define the structure matrix of this type of interaction effect
    R <- kronecker(Rt, Rs)
    r <- s+t-1
    #Define the constraints
    A1 <- kronecker(matrix(1,1,t),Diagonal(s))
    A2 <- kronecker(Diagonal(t),matrix(1,1,s))
    A <- rbind(A1[-1,], A2[-1,])
    e <- rep(0, s+t-2)
    
    formula <-
      n ~ f(
        idarea,
        model = "bym2",
        graph = g,
        hyper = pc_prior,
        constr = TRUE
      ) +
      f(
        idtime,
        model = rw,
        hyper = list(prec = list(prior = sdunif)),
        constr = TRUE
      ) +
      f(idareatime,
        model = "generic0", Cmatrix=R, rankdef=r,
        hyper=list(prec=list(prior=sdunif)),
        constr = TRUE, extraconstr=list(A=A, e=e)
      )
  }
  
  
  set.seed(316)
  # return inla model
  mod <- inla(formula, family="poisson", data=df, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))
  
  mod
}

ptm <- proc.time()

# run INLA models
#mod_II <- inla(formula_II, family="poisson", data=shapefile@data, control.compute=list(dic = TRUE, cpo = TRUE, waic = TRUE), control.predictor=list(compute=TRUE, cdf=c(log(1))))
mod_base <- inla_mod_st(df=shapefile@data)
mod_I <- inla_mod_st(df=shapefile@data, interaction="I")
mod_II <- inla_mod_st(df=shapefile@data, interaction="II")
mod_III <- inla_mod_st(df=shapefile@data, interaction="III")
mod_IV <- inla_mod_st(df=shapefile@data, interaction="IV")

# add fitted values to dataframe
shapefile@data$base <- mod_base$summary.fitted.values[, "mean"]
shapefile@data$typeI <- mod_I$summary.fitted.values[, "mean"]
shapefile@data$typeII <- mod_II$summary.fitted.values[, "mean"]
shapefile@data$typeIII <- mod_III$summary.fitted.values[, "mean"]
shapefile@data$typeIV <- mod_IV$summary.fitted.values[, "mean"]

print(proc.time()-ptm)

# compute relative risks


# plot relative risks on shapefile

# export results
write.csv(shapefile@data, "Results/spatio_temporal.csv")

# save models
save(mod_base, file = "Models/SpatioTemporal/base.Rda")
save(mod_I, file = "Models/SpatioTemporal/typeI.Rda")
save(mod_II, file = "Models/SpatioTemporal/typeII.Rda")
save(mod_III, file = "Models/SpatioTemporal/typeIII.Rda")
save(mod_IV, file = "Models/SpatioTemporal/typeIV.Rda")