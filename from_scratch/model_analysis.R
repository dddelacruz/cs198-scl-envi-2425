Sys.setlocale("LC_ALL","English")
rm(list=ls())

library(dplyr)
library(INLA)
library(Matrix) 
library(rgdal)
library(sp)
library(tidyverse)

# load models
load(file="Models/SpatioTemporal/base.Rda")
load(file="Models/SpatioTemporal/typeI.Rda")
load(file="Models/SpatioTemporal/typeII.Rda")
load(file="Models/SpatioTemporal/typeIII.Rda")
load(file="Models/SpatioTemporal/typeIV.Rda")

# get waic and dic
mod_criteria <- data.frame(
  Name=character(),
  WAIC=double(),
  DIC=double()
)

mod_criteria <- mod_criteria %>% 
  add_row(Name = "Base", WAIC = mod_base$waic$waic, DIC = mod_base$dic$dic) %>% 
  add_row(Name = "TypeI", WAIC = mod_I$waic$waic, DIC = mod_I$dic$dic)%>% 
  add_row(Name = "TypeII", WAIC = mod_II$waic$waic, DIC = mod_II$dic$dic)%>% 
  add_row(Name = "TypeIII", WAIC = mod_III$waic$waic, DIC = mod_III$dic$dic)%>% 
  add_row(Name = "TypeIV", WAIC = mod_IV$waic$waic, DIC = mod_IV$dic$dic)



# save waic and dic per model
write.csv(mod_criteria, "Results/model_selection.csv")