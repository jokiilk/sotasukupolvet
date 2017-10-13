


library(dplyr)

# Datan lukeminen R:n sisään, korjailuja vuosilukuihin ja rajaus vain sodassa kuolleisiin

menehtyneet3945 <- read.csv("Menehtyneet3945.csv", sep = ";")


library(stringr)
menehtyneet3945$SVUOSI <- substr(menehtyneet3945$SAIKA, 7, 10)
menehtyneet3945$KVUOSI <- substr(menehtyneet3945$KUOLINAIKA, 7, 10)

menehtyneet3945$KVUOSI[menehtyneet3945$KVUOSI == 940] <- 1940
menehtyneet3945$KVUOSI[menehtyneet3945$KVUOSI == 941] <- 1941
menehtyneet3945$KVUOSI[menehtyneet3945$KVUOSI == 944] <- 1944

menehtyneet3945 <- subset(menehtyneet3945, menehtyneet3945$KVUOSI < 1946)
menehtyneet3945 <- subset(menehtyneet3945, menehtyneet3945$KVUOSI != "")

menehtyneet3945$SVUOSI[menehtyneet3945$SVUOSI == 920] <- 1920
menehtyneet3945$SVUOSI[menehtyneet3945$SVUOSI == 911] <- 1911
menehtyneet3945$SVUOSI[menehtyneet3945$SVUOSI == 10] <- 1910
menehtyneet3945$SVUOSI[menehtyneet3945$SVUOSI == 906] <- 1906
menehtyneet3945$SVUOSI[menehtyneet3945$SVUOSI == 913] <- 1913
menehtyneet3945$SVUOSI[menehtyneet3945$SVUOSI == 910] <- 1910
menehtyneet3945$SVUOSI[menehtyneet3945$SVUOSI == 191] <- 1901


menehtyneet3945$SVUOSI <- as.numeric(menehtyneet3945$SVUOSI)

ikaluokat3945 <- plyr::count(menehtyneet3945$SVUOSI)
names(ikaluokat3945)[1] <- "vuosi"
names(ikaluokat3945)[2] <- "Yhteensä"

     #naisten ja miesten erottelu
naiset <- menehtyneet3945 %>% subset(menehtyneet3945$SPUOLI == "F") 
naiset_ikaluokat <- plyr::count(naiset$SVUOSI)
names(naiset_ikaluokat)[1] <- "Vuosi"

miehet <- menehtyneet3945 %>% subset(menehtyneet3945$SPUOLI != "F") 
miehet_ikaluokat <- plyr::count(miehet$SVUOSI)
names(miehet_ikaluokat)[1] <- "Vuosi"


# Sotasurma-tietokanta: datan lukeminen, vain vuosi 1918
      # kaikki kuolinsyyt mukana, pitäisikö rajata vain rintamalla kuolleisiin
sotasurmat <- read.csv("sotasurmat_joined.csv")

sotasurmat$kuolinaika <- as.character(sotasurmat$kuolinaika)
testi <- str_split(sotasurmat$kuolinaika, "[.]")
testi <- matrix(unlist(testi), ncol=3, byrow=TRUE)
testi <- as.data.frame(testi)
sotasurmat$KVUOSI <- testi$V3

sotasurmat1918 <- subset(sotasurmat, sotasurmat$KVUOSI == 1918)

ikaluokat1918 <- plyr::count(sotasurmat1918$syntymavuosi)


sotas_miehet <- sotasurmat1918 %>% subset(sotasurmat1918$sukupuoli == "Mies")
sotas_miehet_ikaluokat <- plyr::count(sotas_miehet$syntymavuosi)
names(sotas_miehet_ikaluokat)[1] <- "Vuosi"

sotas_naiset <- sotasurmat1918 %>% subset(sotasurmat1918$sukupuoli != "Mies")
sotas_naiset_ikaluokat <- plyr::count(sotas_naiset$syntymavuosi)
names(sotas_naiset_ikaluokat)[1] <- "Vuosi"


## Tilastokeskuksen ikäluokka-aineisto pohjaksi

ikaluokat <- read.csv("ikaluokat_tilastokeskus.csv", sep =";")
ikaluokat$Yhteensä <- ikaluokat$Pojat + ikaluokat$Tytöt

## liitokset

ikaluokat<- left_join(ikaluokat, naiset_ikaluokat, by = "Vuosi")
names(ikaluokat)[5] <- "naiset3945"

ikaluokat<- left_join(ikaluokat, miehet_ikaluokat, by = "Vuosi")
names(ikaluokat)[6] <- "miehet3945"

ikaluokat<- left_join(ikaluokat, sotas_naiset_ikaluokat, by = "Vuosi")
names(ikaluokat)[7] <- "naiset1918"

ikaluokat<- left_join(ikaluokat, sotas_miehet_ikaluokat, by = "Vuosi")
names(ikaluokat)[8] <- "miehet1918"

#yhteenlaskua

ikaluokat$Yhteensä3945 <- rowSums(ikaluokat[5:6], na.rm = TRUE)
ikaluokat$Yhteensä1918 <- rowSums(ikaluokat[7:8], na.rm = TRUE)
ikaluokat$KuolematYht <- ikaluokat$Yhteensä3945 + ikaluokat$Yhteensä1918

ikaluokat$osuus_ikaluokasta <- ikaluokat$KuolematYht / ikaluokat$Yhteensä * 100
ikaluokat$osuus_ikaluokasta3945 <- ikaluokat$Yhteensä3945 / ikaluokat$Yhteensä * 100
ikaluokat$osuus_ikaluokasta1918 <- ikaluokat$Yhteensä1918 / ikaluokat$Yhteensä * 100

write.csv(ikaluokat, file = "ikaluokat.csv")






