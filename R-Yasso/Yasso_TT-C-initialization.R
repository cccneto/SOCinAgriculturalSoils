# This file contains an R-function of Yasso07. The version is
# based on matrix-version created by Jaakko Heikkinen with Matlab and
# Yasso07 description by Tuomi & Liski 17.3.2008  (Yasso07.pdf)
# Created by Taru Palosuo, Jaakko Heikkinen & Anu Akujärvi in December 2011

# Modified for calculating farm level carbon stock 
# by Maria Yli-Heikkilä, 2018-2022.
# Initialization on regional level

# input data: for each region and production type we calculate AWENH variables from data 
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
farmGroups <- read.csv("../data/example-initialization.csv")
# read in climate data:
load("../data/ilmastokaudet1990-2021.RData")

# if a production type is not represented in a region (not a single farm), we will use the mean AWENH inputs of all farms in the region.
# The mean AWENH per region is marked by setting production type value to -1.


library(expm)
library(pmdplyr)

  # when grouped by region and production type, if there are years missing between years, 
  # first fill with NA, then replace NA with the mean of the region (production type -1):
  puuttuukoVuosia <- farmGroups %>% arrange(year) %>%
    group_by(fadn_region, productionType) %>%
    mutate(diff = year - lag(year)) %>%
    filter(diff > 1) %>% as.data.frame()
  
  if(nrow(puuttuukoVuosia) > 0){ 
    farmGroups <- panel_fill(farmGroups, 
                       .i = c(fadn_region, productionType),
                       .t = year,
                       .set_NA = TRUE
    )  %>%
      as.data.frame()
  }
  
  if(anyNA(farmGroups)) {
    puuttuvat <- farmGroups %>% group_by(fadn_region, year) %>% filter(productionType == -1 | is.na(carbonA)) %>%
      mutate_at(.vars = vars(starts_with('carbon')), .funs = funs(ifelse(is.na(.), mean(., na.rm = T), .))) %>%
      filter(!(productionType == -1)) %>%
      as.data.frame()
    farmGroups4 <- na.omit(farmGroups)
    farmGroups <- rbind(farmGroups4, puuttuvat)
  }
  
  
  ####### Initialization with the mean of the first 10 years of AWENH inputs.
  
  # The output of the function is the vector AWENH compartments:
  yasso07.light =  function(LC,LI,A,TI) {
    # analytical solution as in Eq. 1.3 in model description
    LC = as.array(solve(A)%*% (expm(A*TI)%*%(A%*%LC+LI)-LI))
    LC  
  }  

  # we take the mean of the group (region, production type) from the earliest 10 years (typically years 2000-2009).
  # if the group emerges later (or earlier), the starting year is its first year. If there are less than 10 years
  # in the data, we take those years we can get.
  LI <- farmGroups %>% group_by(fadn_region, productionType) %>% 
    arrange(year) %>% 
    slice(1:10) %>% select(starts_with("carbon")) %>% 
    summarise_each(mean) %>% as.data.frame()
  
  # Climate data for the initialization is the mean from the years 1961-1990:
  # because the earliest weather data we have is from 1961.
  ilmasto1990 <- ilmasto %>% filter(vuosi == 1990) %>% select(fadn_alue, A)
  
  # to get the starting year of a group (region and productions type):
  LIalku <- farmGroups %>% group_by(fadn_region, productionType) %>% 
    arrange(year) %>% 
    slice(1) %>% 
    select(year) %>% as.data.frame()
  
  # The initial pool of AWENH (similarly in the GHG inventory).
  # Northern initial pool:
  LCP <- c(7546,  1012,   822, 32441, 29778) 
  # Southern initial pool:
  LCE <- c(8639,  1145,   882, 40397, 31377)
  
  
  result <- list(); metaalue <- list(); metats <- list(); metay <- list();
  # Iterate over all groups:
  for (i in 1:dim(LI)[1]) {
    
    # Set the inial pool for the group, based on location (north, if region == 4, else: south):
    if (LI[i,"fadn_region"] < 4){
      LC <- LCE
    }else{
      LC <- LCP
    }
    
    # TI: the lenght of simulation in years. Starting year is 1900. Ending years is typically 2000, but it can be 
    # also later (or earlier). Here we check when the group has first appeared in the data set:
    LIalkuvuosi <- LIalku %>% filter(fadn_region == LI[i, "fadn_region"] & productionType == LI[i, "productionType"]) %>% 
      select(year) %>% as.data.frame()
    
    alkuvuosi <- LIalkuvuosi$year
    TI <- alkuvuosi - 1901
    
    # Litter input (AWENH) of the group:
    LI0 <- LI[i,] %>% select(starts_with('carbon')) %>% unlist(use.names = FALSE)
    
    # The decomposition matrix (climate data) of the region:
    A0 <- ilmasto1990 %>% filter(fadn_alue == LI[i, "fadn_region"]) %>% 
      select(A) %>% as.data.frame()
    
    A <- A0[1,][[1]]
    
    # Yasso function takes as input: litter AWENH LI0, decomposition matix A (climate),
    # initial pool LC, simulation time TI:
    result[[i]] <- as.vector(yasso07.light(LC,LI0,A,TI))
    
    metaalue[[i]] <- LI[i, "fadn_region"]
    metats[[i]] <- LI[i, "productionType"]
    metay[[i]] <- alkuvuosi 
  }
  
  # bind initialization results into dataframe:
  dfalku <- as.data.frame(do.call(rbind, result))
  names(dfalku) <- c("alku_syote_a_ha", "alku_syote_w_ha", "alku_syote_e_ha", "alku_syote_n_ha", "alku_syote_h_ha")
  
  dfalku['fadn_region'] <- as.data.frame(do.call(rbind, metaalue))
  dfalku['productionType'] <- as.data.frame(do.call(rbind, metats))
  dfalku['year'] <- as.data.frame(do.call(rbind, metay))
  
  #### Run YASSO model for regions:
  
  # set year counter id_nr:
  ekaLC0 <- farmGroups %>%
    group_by(fadn_region, productionType) %>%
    arrange(year) %>% 
    mutate(id_nr = 1:n()) %>%
    as.data.frame()
  
  # add climate data by region and year:
  ekaLC <- ekaLC0 %>%   
    left_join(ilmasto, by = c("fadn_region" = "fadn_alue", "year" = "vuosi")) 
  
  # prepare results-table:
  tokaLC <- ekaLC %>% select(year, fadn_region, productionType, id_nr)
  tokaLC$summaHiili <- rep(0, nrow(ekaLC))
  
  
  # iterate over all groups:
  for (i in unique(ekaLC$fadn_region)){
    for (j in unique(ekaLC$productionType)){
      tmp <- subset(ekaLC, fadn_region == i & productionType == j)
      
      
      for(k in tmp$year){
        tmp2 <- tmp[tmp$year == k,]
        id_nr <- tmp2$id_nr
        
        if(id_nr == 1){
          # In the first year the initial pool comes from initialization:
          initialPool <- dfalku %>% 
            filter(fadn_region == tmp2['fadn_region'][[1]] & productionType == tmp2['productionType'][[1]] & year == tmp2['year'][[1]]) %>%
            select(starts_with('alku'))
          
          # YASSO:
          # first year:
          tmp$result[[id_nr]] <- solve(tmp2$A[[1]]) %*% (expm(tmp2$A[[1]]) %*% (tmp2$A[[1]] %*% unlist(initialPool) + c(tmp2$carbonA, tmp2$carbonW, tmp2$carbonE, tmp2$carbonN, tmp2$carbonH)) - c(tmp2$carbonA, tmp2$carbonW, tmp2$carbonE, tmp2$carbonN, tmp2$carbonH))
          
        } else {
          # YASSO:
          # in next years we take the previous year's C-pool.
          tmp$result[[id_nr]] <- solve(tmp2$A[[1]]) %*% (expm(tmp2$A[[1]]) %*% (tmp2$A[[1]] %*% tmp$result[[id_nr-1]] + c(tmp2$carbonA, tmp2$carbonW, tmp2$carbonE, tmp2$carbonN, tmp2$carbonH)) - c(tmp2$carbonA, tmp2$carbonW, tmp2$carbonE, tmp2$carbonN, tmp2$carbonH))
          
        }
        # Save the change of in AWENH compartments:
        tokaLC$result[[which(tokaLC$year == k & tokaLC$fadn_region == i & tokaLC$productionType == j)]] <- tmp$result[[id_nr]]
        # and the sum of AWENH compartments:
        tokaLC$summaHiili[which(tokaLC$year == k & tokaLC$fadn_region == i & tokaLC$productionType == j)] <- sum(tmp$result[[id_nr]]) 
        
      }
    }
  }
  
  # add to the initialization pool and save to file:
  initialPoolviite <- tokaLC %>% select(-c(summaHiili,id_nr))
  save(initialPoolviite, file = '../data/initialPool.RData')

