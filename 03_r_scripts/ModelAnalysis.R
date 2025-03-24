Sys.setlocale("LC_ALL","English")
rm(list=ls())

library(dplyr)
library(INLA)
library(Matrix) 
library(rgdal)
library(sp)
library(tidyverse)
library(stringr)


#### set province ####
province <- 'CATANDUANES'

# load models
load(file=str_glue("../04_results/{province}/01_spatiotemporal/models/base.Rda"))
load(file=str_glue("../04_results/{province}/01_spatiotemporal/models/typeI.Rda"))
load(file=str_glue("../04_results/{province}/01_spatiotemporal/models/typeII.Rda"))
load(file=str_glue("../04_results/{province}/01_spatiotemporal/models/typeIII.Rda"))
load(file=str_glue("../04_results/{province}/01_spatiotemporal/models/typeIV.Rda"))

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
write.csv(mod_criteria, str_glue("../04_results/{province}/01_spatiotemporal/ModelCriteria.csv"))