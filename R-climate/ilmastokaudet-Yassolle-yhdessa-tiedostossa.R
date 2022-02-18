# MY 11.11.2019
# Lue kaikki ilmastokaudet ja tallenna yhteen tiedostoon:


files = list.files(path = "~/Documents/ilmastoData/fadnaluekohtainenIlmastokausi", pattern = "A.RData", full.names = TRUE)
perpos <- which(strsplit(files[1], "")[[1]]==".")

load(files[1])
ilmasto$vuosi <- as.numeric(substr(files[1], perpos-5, perpos-2))
ilmasto0 <- ilmasto

for(file in files[-1]){
    load(file)
    ilmasto$vuosi <- as.numeric(substr(file, perpos-5, perpos-2))
    ilmasto0 <- rbind(ilmasto0, ilmasto)
}

# tehdään vielä pari ylimääräistä ilmastokautta tulevaisuuteen kopioimalla viimeisin ilmastokausi:

load(last(files))
ilmasto$vuosi <- 2019
ilmasto0 <- rbind(ilmasto0, ilmasto)

load(last(files))
ilmasto$vuosi <- 2020
ilmasto0 <- rbind(ilmasto0, ilmasto)

ilmasto <- ilmasto0
save(ilmasto, file = "../data/ilmastokaudet2000-2020.RData")
