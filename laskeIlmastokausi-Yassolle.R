###################################
# MY 1.10.2019
# Lasketaan Yassoa varten hajoamismatriisi A per kunta
# Säädatan yhdistämiseen
# funktio laskeIlmastokausi laskee ilmastokausille keskiarvoja kunnittain
# funktioon annetaan muuttujanimi, joko: 
# "annualPrecipitation" : kauden keskiarvo vuotuisista kumulatiivisista sademääristä
# "annualMeanTemp" : kauden keskiarvo vuotuisista lämpötilan keskiarvoista
# tai "annualMinMaxTemp" : kauden keskiarvo vuotuisista lämpötilavaihteluista (Tmax-Tmin)/2 
# sekä alkuvuosi ja loppuvuosi
# yksilöivä tunniste on kuntaid (vuoden 2017 kunnat)
# otetaan mukaan myös muutamia puuttuvia kuntia
# sitten lasketaan Yassoa varten hajoamismatriisi A per kunta
# tallennetaan pelkkä annualMeanAmplitude, annualPrecipitation, annualMeanTemp ja toiseen tiedostoon A-versioon hajoamismatriisi.

library(dplyr)
library(purrr)


alkuvuodet <- 1971:1989
ikkuna <- 29 # kuinka pitkän aikavälin ikkunaa tarkastellaan (vuosissa)

inputfilepath <- "~/Documents/ilmastoData/kuntakohtainen/"
outputpath <- "~/Documents/ilmastoData/kuntakohtainenIlmastokausi/"


##########################################
########### FUNKTIOT
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



laskeilmastokaudet <- function(alkuvuodet, ikkuna){

for (i in alkuvuodet){
  alkuvuosi <- i
  loppuvuosi <- i+ikkuna

  output <- paste(outputpath, "ilmastokausi", alkuvuosi, "-", loppuvuosi, ".RData", sep ="")
  outputA <- paste(outputpath, "ilmastokausi", alkuvuosi, "-", loppuvuosi, "A.RData", sep ="")


setwd(inputfilepath)



# ilmastoMuuttuja on joko "annualPrecipitation", "annualMeanTemp" tai "annualMinMaxTemp"
# yleisön pyynnöstä ei leading zeros kuntaid


df <- laskeIlmastokausi("annualPrecipitation", alkuvuosi, loppuvuosi) %>% select(-kuntaid2)
df2 <- laskeIlmastokausi("annualMeanTemp",  alkuvuosi, loppuvuosi) %>% select(-kuntaid2)
df3 <- laskeIlmastokausi("annualMinMaxTemp",  alkuvuosi, loppuvuosi)  %>% select(-kuntaid2) # laskee amplitudin

# ja Yasso haluaa sademäärän metreissä
df$annualPrecipitation <- df$annualPrecipitation/1000

df4 <- merge(df, df2, by = "kuntaid")
df5 <- merge(df3, df4,  by = "kuntaid")



# lisätään extra kunnat (ennen kuntaliitoksia):
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

df6 <- rbind(df5, luvia, lavia, koylio, nastola, tarvasjoki, hameenkoski, juankoski, maaninka, jalasjarvi, voyri, voyri0, toysa, rovaniemi, ylistaro)

# Yassoa varten hajoitusmatriisi A:
  
# This code is a part of R implementation of Yasso07. The version is
# based on matrix-version created by Jaakko Heikkinen with Matlab and
# Yasso07 description by Tuomi & Liski 17.3.2008  (Yasso07.pdf)
# Created by Taru Palosuo, Jaakko Heikkinen & Anu Akujärvi in December 2011

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

ilmastoA <- df6 %>%
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

ilmasto <- df6 %>% select(kuntaid, annualMeanAmplitude, annualPrecipitation, annualMeanTemp)

save(ilmasto, file = output)

#### SAVE ilmastomatriisi:


save(ilmastoA, file = outputA)


} # for loop over
} # function over


##############################
# function call:
##############################

laskeilmastokaudet(alkuvuodet, ikkuna)


