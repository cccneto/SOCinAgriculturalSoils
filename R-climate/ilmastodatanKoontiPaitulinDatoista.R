# MY 9.6.2019


###### Paitulista ladatut datat: https://avaa.tdata.fi/web/paituli/latauspalvelu
# Hallintorajojen aineiston tuottaja MML (kunta 2017)
# Kuukauden keskilämpötila, päivittäinen min ja max lämopötila, sadesumma, kaikki 10kmx10km hilassa

# melko raskasta laskentaa tuo Min ja Max osuus, minulla meni yht. 1h 12 corella (32Gb RAM)

# tarvittavat tiedostot:
kunnat2017_polku = "~/Documents/GISdata/mml/hallintorajat_milj_tk/2017" 
kunnat2017_shapefile = "kunnat_2017_milj" 

# päivittäinen max lämpötila:
filepathMax <- "~/Documents/ilmastoData/ilmatiede/10km_daily_maximum_temperature/geotiff/daily_maximum_temperature_1961_2018_geotiff/"
# päivittäinen min lämpötila:
filepathMin <- "~/Documents/ilmastoData/ilmatiede/10km_daily_minimum_temperature/geotiff/daily_minimum_temperature_1961_2018_geotiff/"
# keskilämpötila:
filepathMean <- "~/Documents/ilmastoData/ilmatiede/10km_monthly_mean_temp/geotiff/monthly_mean_temp_1961_2018_geotiff/"
# vuoden sadesumma:
filepathPrec <- "~/Documents/ilmastoData/ilmatiede/10km_monthly_precipitation/geotiff/monthly_precipitation_1961_2018_geotiff/"

alkuvuosi <- 1961
loppuvuosi <- 2018

years <- alkuvuosi:loppuvuosi

# tulostetiedostot hakemistoon:
fp <- "~/Documents/ilmastoData/kuntakohtainen/"

library(raster)
#library(foreach)
library(doParallel)
# library(sp)
library(rgdal)
library(rgeos)

# Jos halutaan ajaa rinnakkaislaskentana:
registerDoParallel(6) # 6 corea, voi kokeilla myös 12 corea, tsekkaa: sysctl hw.ncpu hw.physicalcpu


############################################
######## Lasketaan kuntien keskipisteet vuoden 2017 mukaan
############################################

#### kuntien geometriat 2017
setwd(kunnat2017_polku)

# Import a polygon shapefile: readOGR("path","fileName")

kunnat2017 <- readOGR(dsn = kunnat2017_polku, kunnat2017_shapefile)
# crs         : +proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs
# eli sama kuin säädata

keskipisteet <- gCentroid(kunnat2017, byid = TRUE, id = kunnat2017@data$NATCODE)


############################################
######## Temp Max ja Min jokaiselle kunnalle
############################################

filesMax <- list.files(path = filepathMax, pattern = "*.tif$")
filesMin <- list.files(path = filepathMin, pattern = "*.tif$")

# jos ei halua käyttää rinnakkaisajoa, tässä tavallinen looppi:
# for (i in alkuvuosi:loppuvuosi){ 

foreach (i=alkuvuosi:loppuvuosi) %dopar% {
  
  # open raster data
  setwd(filepathMax)
  vuositmpmax <- stack(x = filesMax[i])
  
  # +proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs 
  
  setwd(filepathMin)
  vuositmpmin <- stack(x = filesMin[i])
  
  # antaa jokaiselle pisteelle vuoden maksimin
  maksit <- max(vuositmpmax)
  minit <- min(vuositmpmin)
  
  
  # seuraavaksi haetaan jokaiselle kuntapisteelle min ja max:
  
  tmpmax <- raster::extract(maksit, keskipisteet, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                            fun=NULL, na.rm=TRUE, df=TRUE, factors=FALSE)
  
  tmpmin <- raster::extract(minit, keskipisteet, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                            fun=NULL, na.rm=TRUE, df=TRUE, factors=FALSE)
  
  tmp <- cbind(tmpmax, tmpmin, row.names(coordinates(keskipisteet)))
  
  names(tmp)[c(2,4,5)] <- c("annualMaxTemp", "annualMinTemp", "kuntaid")
  
  write.csv(tmp, file = paste(fp, "annualMinMaxTemp-", i, ".csv", sep=""), row.names = F)
}


################################
######## Temp Mean
################################




filesMean <- list.files(path = filepathMean, pattern = "*.tif$")
setwd(filepathMean)




# for (i in 1:length(files)){
foreach (i=1:length(filesMean)) %dopar% {
  
  # open raster data
  vuositmp <- stack(x = filesMean[i])
  keskiarvot <- mean(vuositmp)
  
  tmpmean <- raster::extract(keskiarvot, keskipisteet, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                             fun=NULL, na.rm=TRUE, df=TRUE, factors=FALSE)
  
  tmp <- cbind(tmpmean, row.names(coordinates(keskipisteet)))
  names(tmp)[2:3] <- c("annualMeanTemp", "kuntaid")
  write.csv(tmp, file = paste(fp, "annualMeanTemp-", years[i], ".csv", sep=""), row.names = F)
}

################################
######## Precipitation
################################

filesPrec <- list.files(path = filepath, pattern = "*.tif$")

setwd(filepathPrec)


# for (i in 1:length(files)){
foreach (i=1:length(filesPrec)) %dopar% {
  
  # open raster data
  vuositmp <- stack(x = filesPrec[i])
  sade <- sum(vuositmp)
  
  tmpsade <- raster::extract(sade, keskipisteet, method='simple', buffer=NULL, small=FALSE, cellnumbers=FALSE, 
                             fun=NULL, na.rm=TRUE, df=TRUE, factors=FALSE)
  
  tmp <- cbind(tmpsade, row.names(coordinates(keskipisteet)))
  names(tmp)[2:3] <- c("annualPrecipitation", "kuntaid")
  write.csv(tmp, file = paste(fp, "annualPrecipitation-", years[i], ".csv", sep=""), row.names = F)
  
}  

# When you're done, clean up the cluster
stopImplicitCluster()
