# SOC balance in mineral soils of arable land

## YASSO-model for calculating farm level SOC in mineral soils

### initialization of the carbon stock

### simulation of the C balance from annual carbon input

## Preparing the climate data for YASSO

TT-C projektiin liittyvät ilmastoaineiston kokoamiseen tarvittavat koodit hakemistossa R-climate.


Ensin ladataan Paitulista (https://paituli.csc.fi/download.html) kuntien hallintorajat (MML) ja ilmastodataa (FMI) 10kmx10km hilassa: 
 - 10km_daily_maximum_temperature
 - 10km_daily_minimum_temperature
 - 10km_monthly_precipitation

Sitten ajetaan tiedosto **ilmastodatanKoontiPaitulinDatoista.R**. Tämä koostaa kuntakohtaisesti ilmastoaineistoa (annualPrecipitation, annualMeanTemp, annualMinMaxTemp) per vuosi. Tulosteena esim. annualMeanTemp-1961.csv, jossa muuttujina: "ID","annualMeanTemp","kuntaid"

Muista alkuun editoida oikeat tiedostopolut, ja vuosiluvut alku ja loppu. Jos haluaa käyttää rinnakkaislaskentaa, editoi koneellesi sopiva ytimien määrä. 

Seuraavaksi lasketaan kuntakohtaisia ilmastokausia, koska käytämme liukuvia keskiarvoja. Yassoa varten ilmastodata jalostetaan hajotusmatriisiksi käyttäen Yasson parametreja. Aja **laskeIlmastokausi-Yassolle.R**. Tulosteena sekä tavallinen ilmastokausi esim. ilmastokausi1971-2000.RData, jossa muuttujina: "kuntaid",  "annualMeanAmplitude", "annualPrecipitation", "annualMeanTemp" että pelkkä hajotusmatriisi ilmastokausi1971-2000A.RData, muuttujina: "kuntaid", "A".

Jos haluaa laskea ilmastokausia ilman hajostusmatriisin laskemista, voi käyttää tiedostoa **laskeIlmastokausi.R**. Tämä koostaa kausittain ilmastoaineiston tiedostoihin. Tulosteena esim. ilmastokausi1971-2000.RData, jossa muuttujina: "kuntaid",  "annualMeanAmplitude", "annualPrecipitation", "annualMeanTemp"   

Lopuksi ilmastokaudet säilötään yhteen tiedostoon Yasson ajamista varten. Aja **ilmastokaudet-Yassolle-yhdessa-tiedostossa.R**. Tulosteena 
ilmastokaudet2000-2020.RData, jossa vuosille 2019 ja 2020 on kopioitu ilmastokausi 1989-2018. Vuosi 2019 ja 2020 pitää tietysti sitten päivittää, kun 
säädata on saatavilla.

Jos tarvitaan FADN-aluekohtaisia ilmastokausia, käytä **ilmastokaudetFADNalueille.R** ja **ilmastokaudet-Yassolle-yhdessa-tiedostossa.R**, saadaan hajotusmatriisit, muuttujina: "fadn_alue", "A", "vuosi".

Kun tarvitaan ilmastodata uudelle vuodelle, käytä **lisaaUusiVuosi.R**. Tämä hakee uuden vuoden datat Luken säätietokannasta.

### Climate images

Koko maan keskiarvot: img/kokoMaanKeskiarvot.pdf

Hajotuskerroin k liukuvana keskiarvona: img/decompositionRateCoef.pdf
