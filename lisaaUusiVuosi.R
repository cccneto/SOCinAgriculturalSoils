# MY 17.1.2020
# Haetaan Yassoon tarvittava uuden vuoden ilmastodata Luken säätietokannasta 
# - tarvitaan: Kuukauden keskilämpötila, päivittäinen min ja max lämopötila, sadesumma,
# lasketaan sille takautuva 30 vuoden ilmastokausi


filepath <- "~/Documents/ilmastoData/kuntakohtainen/"
outputpath <- "~/Documents/ilmastoData/kuntakohtainenIlmastokausi/"

# Tarvittavat tiedostot:
kunnat2017_polku = "~/Documents/GISdata/mml/hallintorajat_milj_tk/2017" 
kunnat2017_shapefile = "kunnat_2017_milj" 



library(RPostgreSQL)
library(rgdal)
library(rgeos)
library(dplyr)
library(purrr)

kayttis = "kayttis"
loppuvuosi <-  2019
alkuvuosi <- loppuvuosi-29

# ilmastokausi ilmastomuuttujineen:
output <- paste(outputpath, "ilmastokausi", alkuvuosi, "-", loppuvuosi, ".RData", sep ="")
# ilmastokausi, pelkkä hajotusmatriisi A:
outputA <- paste(outputpath, "ilmastokausi", alkuvuosi, "-", loppuvuosi, "A.RData", sep ="")


###########################################################
######## Lasketaan kuntien keskipisteet vuoden 2017 mukaan
###########################################################

#### kuntien geometriat 2017
setwd(kunnat2017_polku)

# Import a polygon shapefile: readOGR("path","fileName")

kunnat2017 <- readOGR(dsn = kunnat2017_polku, kunnat2017_shapefile)
# crs         : +proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs

keskipisteet <- gCentroid(kunnat2017, byid = TRUE, id = kunnat2017@data$NATCODE)

# IL toimittaa Lukelle tiedot KKJ-koordinaatistossa, ei ETRS-TM35FIN!
# eli ilmeisesti tämä:
# KKJ / Finland Uniform Coordinate System EPSG:2393
ykj = "+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=3500000 +y_0=0 +ellps=intl +towgs84=-96.0617,-82.4278,-121.7535,4.80107,0.34543,-1.37646,1.4964 +units=m +no_defs"

# muuta projektio:
keskipisteetYKJ <- spTransform(keskipisteet,
                              CRS = CRS(ykj))

# Hanna Huitulta:
keskipisteetYKJ$yRounded <- signif((keskipisteetYKJ$y-500),digits = 4)+500  
keskipisteetYKJ$xRounded <- signif((keskipisteetYKJ$x-500),digits = 4)+500

############################################
######## # Avataan tietokantayhteys
############################################

tryCatch({
  print("Connecting to database...")
  con <- dbConnect(dbDriver("PostgreSQL"),
                   host="lukedb1.ns.luke.fi",
                   port = 5432,
                   dbname = "weather",
                   user = kayttis,
                   password = .rs.askForPassword("Enter password:"))
  print("Connected!")
},
error=function(con) {
  print("Unable to connect to database.")
})

#######################################################################
######## Vuoden avg_temp, Max ja Min, ja sum(prec) jokaiselle kunnalle
#######################################################################

df <- data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("avg", "min", "max", "sum"))))

for(i in 1:nrow(keskipisteet)){
haku = paste("SELECT AVG(temp_avg), MIN(temp_min), MAX(temp_max), SUM(prec) FROM grid_month WHERE x = ", keskipisteetYKJ$xRounded[i], " AND y = ", keskipisteetYKJ$yRounded[i], " AND date_start BETWEEN '2019-01-01' AND '2019-12-01'")  
res <- dbSendQuery(con, haku)
data <- fetch(res, n = -1) # fetch all records
df <- rbind(df, data)
}

df$kuntaid <- as.numeric(row.names(keskipisteetYKJ))

  
############################################
######## Katkaistaan yhteys tietokantaan
############################################

dbDisconnect(con)

############################################
######## Lasketaan lämpötilan amplitudi
############################################


names(df)[1:3] <- c("annualMeanTemp", "annualMinTemp", "annualMaxTemp")


df$annualPrecipitation <- df$sum

#df$annualMeanAmplitude <- (df$annualMaxTemp - df$annualMinTemp)/2



##########################################################
######## Tallennetaan välituloksia
##########################################################

setwd(filepath)

write.csv(df %>% select(kuntaid, annualPrecipitation), file = "annualPrecipitation-2019.csv", row.names = FALSE)
write.csv(df %>% select(kuntaid, annualMinTemp, annualMaxTemp), file = "annualMinMaxTemp-2019.csv", row.names = FALSE)
write.csv(df %>% select(kuntaid, annualMeanTemp), file = "annualMeanTemp-2019.csv", row.names = FALSE)



##########################################
########### Lasketaan kausi funktiolla
#########################################


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
    names(df4) <- c("kuntaid", "annualMeanAmplitude")
    
  } else {
    
    
    df2 <- Reduce(function(x,y) {merge(x,y, by = "kuntaid")}, datalist)
    cols <- grep(ilmastoMuuttuja, names(df2))
    df4 <- data.frame(df2$kuntaid, apply(df2[, cols], 1, mean))
    names(df4) <- c("kuntaid", ilmastoMuuttuja)
    
  }
  
  df4$kuntaid2 <- as.factor(sprintf("%03d", df4$kuntaid))
  return(df4)
}


    
    # ilmastoMuuttuja on joko "annualPrecipitation", "annualMeanTemp" tai "annualMinMaxTemp"
    # yleisön pyynnöstä ei leading zeros kuntaid
    
    df <- laskeIlmastokausi("annualPrecipitation", alkuvuosi, loppuvuosi) %>% select(-kuntaid2)
    df2 <- laskeIlmastokausi("annualMeanTemp",  alkuvuosi, loppuvuosi) %>% select(-kuntaid2)
    df3 <- laskeIlmastokausi("annualMinMaxTemp",  alkuvuosi, loppuvuosi)  %>% select(-kuntaid2) # laskee amplitudin
    
    df4 <- merge(df, df2, by = "kuntaid")
    df5 <- merge(df3, df4,  by = "kuntaid")
    
    
    

    ##########################################################
    ######## Lisätään puuttuvat kunnat (ennen kuntaliitoksia)
    ##########################################################
    
    
    # Luvia (442) liittyy Eurajokeen (051);
    luvia <- df5 %>% filter(kuntaid == 51)
    luvia$kuntaid <- 442
    # Säkylä (783) ja Köyliö (319) yhdistyvät. Uuden kunnan nimi on Säkylä ja sen kuntanumero on 783.
    koylio <- df5 %>% filter(kuntaid == 783)
    koylio$kuntaid <- 319
    # Pori (609) ja Lavia (413) yhdistyivät (pakkoliitos). Uuden kunnan nimi on Pori ja sen kuntanumero 609.
    lavia <- df5 %>% filter(kuntaid == 609)
    lavia$kuntaid <- 413
    # Lahti (398) ja Nastola (532) yhdistyvät. Uuden kunnan nimi on Lahti ja sen kuntanumero on 398. 
    nastola <- df5 %>% filter(kuntaid == 398)
    nastola$kuntaid <- 532
    # Tarvasjoen kunta (838) yhdistyy Liedon kuntaan (423)
    tarvasjoki <- df5 %>% filter(kuntaid == 423)
    tarvasjoki$kuntaid <- 838
    # Hämeenkoski (283) yhdistyy Hollolan kuntaan (098);
    hameenkoski <- df5 %>% filter(kuntaid == 98)
    hameenkoski$kuntaid <- 283
    # Juankosken kaupunki (174) yhdistyy Kuopion kaupunkiin (297)
    juankoski <- df5 %>% filter(kuntaid == 297)
    juankoski$kuntaid <- 174
    # Maaningan kunta (kuntakoodi 476) yhdistyy Kuopion kaupunkiin (297) 
    maaninka <- df5 %>% filter(kuntaid == 297)
    maaninka$kuntaid <- 476
    # Kurikka (301) ja Jalasjärvi (164) yhdistyvät.
    jalasjarvi <- df5 %>% filter(kuntaid == 301)
    jalasjarvi$kuntaid <- 164
    # Vöyri-Maksamaa (945) ja Oravainen (559) yhdistyivät. Uuden kunnan nimi on Vöyri ja sen kuntanumero on 946. 
    voyri <- df5 %>% filter(kuntaid == 946)
    voyri$kuntaid <- 945
    voyri0 <- df5 %>% filter(kuntaid == 946)
    voyri0$kuntaid <- 944
    # Töysä (863) ja Alavus (010) yhdistyivät. 
    toysa <- df5 %>% filter(kuntaid == 10)
    toysa$kuntaid <- 863
    # 698, Rovaniemi, 699, Rovaniemen mlk.
    rovaniemi <- df5 %>% filter(kuntaid == 698)
    rovaniemi$kuntaid <- 699
    # Ylistaro 975 -> Seinäjoki 743
    ylistaro <- df5 %>% filter(kuntaid == 743)
    ylistaro$kuntaid <- 975
    
    dfkausi <- rbind(df5, luvia, lavia, koylio, nastola, tarvasjoki, hameenkoski, juankoski, maaninka, jalasjarvi, voyri, voyri0, toysa, rovaniemi, ylistaro)
    
    # ja Yasso haluaa sademäärän metreissä
    dfkausi$annualPrecipitation <- dfkausi$annualPrecipitation/1000

##########################################################
######## Yassoa varten hajoitusmatriisi A
##########################################################

# ja Yasso haluaa sademäärän metreissä

#        7. Yasso07Parameters - these in the format applied in the fortran version, length 44
Yasso07Parameters <- c(-0.7300,-5.8000,-0.2900,-0.031,0.4800,0.0100,0.8300,0.9900,0.0000,0.0100,0.0000,0.0000,0.0300,0.0000,0.0100,0.9200,0.0960,-0.0014,0.000000,0.000000,0.0000,0.0000,0.0000,0.0000,0.000000,-1.2100,0.000000,0.000000,0.000000,0.00000,0.00000,0.00000,0.000000,0.000000,-0.001700,0.004500,0.000000,0.000000,-1.7000,0.8600,-0.3060,0.0000,0.0000,0.0000)
PA=Yasso07Parameters
alfa=c(-PA[1], -PA[2], -PA[3], -PA[4], -PA[35])   # Vector of decomposition rates

# Creating the matrix A_p (here called p)

row1 = c(-1, PA[5], PA[6], PA[7], 0)
row2 = c(PA[8], -1, PA[9], PA[10], 0)
row3 = c(PA[11], PA[12], -1, PA[13], 0)
row4 = c(PA[14], PA[15], PA[16], -1, 0)
row5 = c(PA[36], PA[36], PA[36], PA[36], -1)

p = matrix(c(row1, row2, row3, row4, row5), 5, 5, byrow=T)

# temperature dependence parameters
beta1 = PA[17]
beta2 = PA[18]
gamma = PA[26]

# Woody litter size dependence parameters
delta1 = PA[39]
delta2 = PA[40]
r = PA[41]

ilmastoA <- dfkausi %>%
  mutate(T1=annualMeanTemp + 4*annualMeanAmplitude/pi*(1/sqrt(2)-1),          # Eq. 2.4 in model description
         T2=annualMeanTemp - 4*annualMeanAmplitude/(sqrt(2)*pi),              # Eq. 2.5 in model description
         T3=annualMeanTemp + 4*annualMeanAmplitude/pi*(1-1/sqrt(2)),          # Eq. 2.6 in model description
         T4=annualMeanTemp + 4*annualMeanAmplitude/(sqrt(2)*pi)              # Eq. 2.7 in model description 
  ) %>%  # Eq. 2.9 in model description follows:
  mutate(k = pmap(list(T1, T2, T3, T4, annualPrecipitation), function(t1, t2, t3, t4, pr) alfa*mean(exp(beta1*c(t1, t2, t3, t4)+beta2*(c(t1, t2, t3, t4)^2))*(1-exp(gamma*pr)))
  ) ) %>% # matrix A follows:
  mutate(A = map(k, function(k) p%*%diag(k))) %>%
  select(kuntaid, A)

#### SAVE vain muuttujat kuntaid, annualMeanAmplitude, annualPrecipitation, annualMeanTemp:

ilmasto <- dfkausi %>% select(kuntaid, annualMeanAmplitude, annualPrecipitation, annualMeanTemp)

save(ilmasto, file = output)

#### SAVE ilmastomatriisi:


save(ilmastoA, file = outputA)


# sitten käy vielä kokoamassa uusi RData, jossa kaikki kaudet, eli aja: ilmastokaudet-Yassolle-yhdessa-tiedostossa.R







