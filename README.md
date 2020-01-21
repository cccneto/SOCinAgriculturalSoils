# SOCinAgriculturalSoils
TT-C projektiin liittyvät ilmastoaineiston kokoamiseen tarvittavat koodit ja ehkä myös Yasso-koodi myöhemmin.


Ensin ladataan Paitulista kuntien hallintorajat (MML) ja ilmastodataa (FMI) 10kmx10km hilassa: 
 - 10km_daily_maximum_temperature
 - 10km_daily_minimum_temperature
 - 10km_monthly_precipitation

Sitten ajetaan tiedosto **ilmastodatanKoontiPaitulinDatoista.R**. Tämä koostaa kuntakohtaisesti ilmastoaineistoa (annualPrecipitation, annualMeanTemp, annualMinMaxTemp) per vuosi. Tulosteena esim. annualMeanTemp-1961.csv, jossa muuttujina: "ID","annualMeanTemp","kuntaid"

Muista alkuun editoida oikeat tiedostopolut, ja vuosiluvut alku ja loppu. Jos haluaa käyttää rinnakkaislaskentaa, editoi koneellesi sopiva ytimien määrä. 

Seuraavaksi lasketaan ilmastokausia, koska käytämme liukuvia keskiarvoja. Yassoa varten ilmastodata jalostetaan hajotusmatriisiksi käyttäen Yasson parametreja. Aja **laskeIlmastokausi-Yassolle.R**. Tulosteena sekä tavallinen ilmastokausi esim. ilmastokausi1971-2000.RData, jossa muuttujina: "kuntaid",  "annualMeanAmplitude", "annualPrecipitation", "annualMeanTemp" että pelkkä hajotusmatriisi ilmastokausi1971-2000A.RData, muuttujina: "kuntaid", "A".

Jos haluaa laskea ilmastokausia ilman hajostusmatriisin laskemista, voi käyttää tiedostoa **laskeIlmastokausi.R**. Tämä koostaa kausittain ilmastoaineiston tiedostoihin. Tulosteena esim. ilmastokausi1971-2000.RData, jossa muuttujina: "kuntaid",  "annualMeanAmplitude", "annualPrecipitation", "annualMeanTemp"   

Lopuksi ilmastokaudet säilötään yhteen tiedostoon Yasson ajamista varten. Aja **ilmastokaudet-Yassolle-yhdessa-tiedostossa.R**. Tulosteena ilmastokaudet2000-2020.RData.

TODO: tee ilmastodata vuodelle 2019.

### Koko maan keskiarvot

![Koko maan keskiarvot](kokoMaanKeskiarvot.pdf "Koko maan keskiarvot")


### Hajotuskerroin k liukuvana keskiarvona 

![hajotuskerroin](decompositionRateCoef.pdf "Decomposition Coefficient Rates")
