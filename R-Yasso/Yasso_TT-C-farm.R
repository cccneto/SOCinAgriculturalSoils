# This file contains an R-function of Yasso07. The version is
# based on matrix-version created by Jaakko Heikkinen with Matlab and
# Yasso07 description by Tuomi & Liski 17.3.2008  (Yasso07.pdf)
# Created by Taru Palosuo, Jaakko Heikkinen & Anu Akujärvi in December 2011

# Modified for calculating farm level carbon stock 
# by Maria Yli-Heikkilä, 2018-2022.
# Using initialization pool on regional level (made in Yasso_TT-C-initialization.R)
initialPoolFile <- "../data/initialPool.RData"

# input data: for each farm we calculate AWENH variables from data 
# on the fields, crops, animal numbers, N application rates (since 2016), fuels and farm economy and investments

# input data have the following variables:
# fadn_region, productionType, carbonA, carbonW, carbonE, carbonN, carbonH, year
# where:
#   fadn_region: region ID
#   productionType: production type
#   carbonA, carbonW, carbonE, carbonN, carbonH: AWENH inputs
#   year: year

# input data is unbalanced panel data.

# read in an example data:
farms <- read.csv("../data/example-farm.csv")

# read in climate data:
load("../data/ilmastokaudet1990-2021.RData")

library(expm)
library(pmdplyr)

  ####################################################################
  ########## YASSO for farms
  ####################################################################
  
  # Read in initial pool:
  load(file = initialPoolFile) 

  # save the initial state of a farm:
  initialState <- farms %>% select(!starts_with('carbon'))
  
  # are there years missing between years?
  puuttuukoVuosia <- farms %>% arrange(year) %>%
    group_by(farmID) %>%
    mutate(diff = year - lag(year)) %>%
    filter(diff > 1) %>% as.data.frame()
  
  # if there are years missing between years, fill in with the preceding year's values:
  if(nrow(puuttuukoVuosia) > 0){ 
    farms <- panel_fill(farms, 
                       .i = farmID,
                       .t = year
    )  %>%
      as.data.frame()
  }
  
  # set year counter id_nr:
  ekaLC0 <- farms %>%
    group_by(farmID) %>%
    arrange(year) %>% 
    mutate(id_nr = 1:n()) %>%
    as.data.frame()
  
  # add climate data by region and year:
  ekaLC <- ekaLC0 %>%   
    left_join(ilmasto, by = c("fadn_region" = "fadn_alue", "year" = "vuosi")) 
  
  # list all farm ids:
  farmIDs <- unique(ekaLC$farmID)
  # prepare results-table:
  tokaLC <- ekaLC %>% select(year, fadn_region, productionType, farmID, id_nr)
  tokaLC$summaHiili <- rep(0, nrow(ekaLC))
  
  
  # iterate over all farms:
  for (i in farmIDs){
    tmp <- subset(ekaLC, farmID == i)
    for(k in tmp$year){
      tmp2 <- tmp[tmp$year == k,]
      id_nr <- tmp2$id_nr
      if(id_nr == 1){
        # In the first year, find the reference group and take the initial pool from the group:
        if(nrow(initialPoolviite %>% 
                filter(fadn_region == tmp2['fadn_region'][[1]] & productionType == tmp2['productionType'][[1]] & year == tmp2['year'][[1]])) == 0){
          # if reference group not found, take the mean of the region (-1) to be the initial pool:
          initialPool <- initialPoolviite %>% 
            filter(fadn_region == tmp2['fadn_region'][[1]] & productionType == -1 & year == tmp2['year'][[1]]) %>%
            select(result)
        }else{
          # in next years we take the farm's C-pool from its previous years:
          initialPool <- initialPoolviite %>% 
            filter(fadn_region == tmp2['fadn_region'][[1]] & productionType == tmp2['productionType'][[1]] & year == tmp2['year'][[1]]) %>%
            select(result)
        }
        # YASSO, 1st year:
        tmp$result[[id_nr]] <- solve(tmp2$A[[1]]) %*% (expm(tmp2$A[[1]]) %*% (tmp2$A[[1]] %*% unlist(initialPool) + c(tmp2$carbonA, tmp2$carbonW, tmp2$carbonE, tmp2$carbonN, tmp2$carbonH)) - c(tmp2$carbonA, tmp2$carbonW, tmp2$carbonE, tmp2$carbonN, tmp2$carbonH))
      } else {
        # YASSO, next years:
        tmp$result[[id_nr]] <- solve(tmp2$A[[1]]) %*% (expm(tmp2$A[[1]]) %*% (tmp2$A[[1]] %*% tmp$result[[id_nr-1]] + c(tmp2$carbonA, tmp2$carbonW, tmp2$carbonE, tmp2$carbonN, tmp2$carbonH)) - c(tmp2$carbonA, tmp2$carbonW, tmp2$carbonE, tmp2$carbonN, tmp2$carbonH))
        
      }
      # the sum of AWENH compartments:
      tokaLC$summaHiili[which(tokaLC$year == k & tokaLC$farmID == i)] <- sum(tmp$result[[id_nr]])
    }
  }
  
  tokaLC <- tokaLC %>% select(year, farmID, id_nr, summaHiili)  
  
  # calculate the change (delta) of the sum of AWENH:
  kolmasLC <-  tokaLC %>% 
    group_by(farmID) %>% 
    mutate(delta = lag(summaHiili, default = NA) - summaHiili) %>% 
    arrange(farmID) %>%
    select(!id_nr) %>%
    as.data.frame() 
  
  
  # bind results:
  hiilisummat <-  initialState %>% left_join(kolmasLC, by = c("farmID", "year"))
  # print out results:
  hiilisummat
  