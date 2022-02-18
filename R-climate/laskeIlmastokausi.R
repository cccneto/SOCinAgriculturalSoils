###################################
# MY 9.6.2019
# Säädatan yhdistämiseen
# funktio laskeIlmastokausi laskee ilmastokausille keskiarvoja kunnittain
# funktioon annetaan muuttujanimi, joko: 
# "annualPrecipitation" : kauden keskiarvo vuotuisista kumulatiivisista sademääristä
# "annualMeanTemp" : kauden keskiarvo vuotuisista lämpötilan keskiarvoista
# tai "annualMinMaxTemp" : kauden keskiarvo vuotuisista lämpötilavaihteluista (Tmax-Tmin)/2 
# sekä alkuvuosi ja loppuvuosi
# yksilöivä tunniste on kuntaid (vuoden 2017 kunnat)

# Käyttöesimerkkejä,
# kun ilmastoMuuttuja on joko "annualPrecipitation", "annualMeanTemp" tai "annualMinMaxTemp":
#df <- laskeIlmastokausi("annualPrecipitation", 2012, 2015)
#df <- laskeIlmastokausi("annualMeanTemp", 2012, 2015)
#df <- laskeIlmastokausi("annualMinMaxTemp", 2012, 2015) # laskee amplitudin


# editoi tähän tiedostopolku, josta löytyy kuntakohtaisesti tiedostoissa per vuosi 
# annualPrecipitation, annualMeanTemp ja annualMinMaxTemp
filepath <- "~/Documents/ilmastoData/kuntakohtainen/"
setwd(filepath)

laskeIlmastokausi = function(ilmastoMuuttuja, alku, loppu){
  files <- list();
  pick <- list();
  datalist <- list();
  #  files <- list.files(path = ".", pattern = "*.csv")
  # otetaan vain valitut vuodet:
  for (i in alku:loppu){
    pick <- c(pick, paste(ilmastoMuuttuja, "-", i, ".csv", sep=""))
  }
  
  files <- unlist(pick)
  
  # tallennetaan kaikki filet to a nested list
  datalist = lapply(files, function(x){read.csv(file=x,header=T)})
  
  if(ilmastoMuuttuja == "annualMinMaxTemp"){ # pitää laskea amplitude 
    
    df3 <- data.frame(lapply(datalist, function(x) {amplitude <- (x$annualMaxTemp - x$annualMinTemp)/2}))
    df4 <- data.frame(datalist[[1]]$kuntaid, apply(df3, 1, mean))
    names(df4) <- c("kuntaid", paste("annualMeanAmplitude", alku, "_", loppu, sep=""))
    
  } else {
    
    
    df2 <- Reduce(function(x,y) {merge(x,y, by = "kuntaid")}, datalist)
    cols <- grep(ilmastoMuuttuja, names(df2))
    df4 <- data.frame(df2$kuntaid, apply(df2[, cols], 1, mean))
    names(df4) <- c("kuntaid", paste(ilmastoMuuttuja, alku, "_", loppu, sep=""))
    
  }
  
  df4$kuntaid2 <- as.factor(sprintf("%03d", df4$kuntaid))
  return(df4)
}
