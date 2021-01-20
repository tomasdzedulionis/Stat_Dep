if(!require("readxl")) install.packages("readxl"); library("readxl")
if(!require("car")) install.packages("car"); library("car")
if(!require("forecast")) install.packages("forecast"); library("forecast")
if(!require("lmtest")) install.packages("lmtest"); library("lmtest")
if(!require("tseries")) install.packages("tseries"); library("tseries")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")

{
# Pridėtinės vertės

BVP_gamvkTM <- read_excel("BVP darbinis.xlsx", sheet = "TM_ketv", col_names = TRUE)
BVP_gamvkTM <- rename(BVP_gamvkTM,TM_N80="TM__N80")
Date <- BVP_gamvkTM$...1

BVP <- na.omit(BVP_gamvkTM$TM_BVP)
BVP <- ts(BVP, start=1995, frequency = 4)
BVP_gamvkTM <- select(BVP_gamvkTM, -TM_BVP)

kategorijos <- unique(substr(names(BVP_gamvkTM[,-1]), 1, 4)) ## Išsitraukiame ekonomines klasifikacijas (pagal pirmus keturis ženklus TM_..)
BVP_gamvkTM <- sapply(kategorijos, function(xx) rowSums(BVP_gamvkTM[,grep(xx, names(BVP_gamvkTM)), drop=FALSE])) ## Sumuojame į bendras klasifikacijas
BVP_gamvkTM <- as.data.frame(BVP_gamvkTM) ## Paverčiame į data frame

pv <- lapply(BVP_gamvkTM, ts, start = 1995, frequency = 4) # Paverčiame visus stulpelius į laiko eilutes
for(i in 1:length(pv)) assign(names(pv)[i], pv[[i]]) # Ištraukiame į atskirus objektus


##Papildomi kintamieji
now=end(BVP)+c(0,1)#1 keisti pagal prognozuojamo ketvirčio numerį nuo l.e. pabaigos

## PVM
PVM <- read_excel("BVP darbinis.xlsx", sheet = "PVM", col_names = TRUE)
PVMviso <- ts(PVM$pvm_total, start=2010, frequency = 4)
PVM_A <- ts(rowSums(PVM[,c(3:5)]), start=2010, frequency = 4)
PVM_B <- ts(rowSums(PVM[,c(6:7)]), start=2010, frequency = 4) # B veiklos PVM
PVM_C <- ts(rowSums(PVM[,c(8:31)]), start=2010, frequency = 4)# C veiklos PVM
PVM_D <- ts(PVM$pvm_35, start=2010, frequency = 4) # D veiklos PVM
pvm_39 <- ifelse(is.na(PVM$pvm_39)==TRUE, 0,PVM$pvm_39)#praleistų reikšmių pavertimas į 0
PVM_E <- ts(rowSums(PVM[,c(33:35)])+pvm_39, start=2010, frequency = 4) # E veiklos PVM
PVM_F <- ts(rowSums(PVM[,c(37:39)]), start=2010, frequency = 4)
PVM_G <- ts(rowSums(PVM[,c(40:42)]), start=2010, frequency = 4)
PVM_H <- ts(rowSums(PVM[,c(43:47)]), start=2010, frequency = 4)
PVM_I <- ts(rowSums(PVM[,c(48:49)]), start=2010, frequency = 4)
PVM_J <- ts(rowSums(PVM[,c(50:55)]), start=2010, frequency = 4)
PVM_K <- ts(rowSums(PVM[,c(56:58)]), start=2010, frequency = 4)
PVM_L <- ts(PVM$pvm_68, start=2010, frequency = 4)
PVM_M <- ts(rowSums(PVM[,c(60:66)]), start=2010, frequency = 4)
PVM_N <- ts(rowSums(PVM[,c(67:72)]), start=2010, frequency = 4)
PVM_O <- ts(rowSums(PVM[,73]), start=2010, frequency = 4)
PVM_P <- ts(rowSums(PVM[,74]), start=2010, frequency = 4)
PVM_Q <- ts(rowSums(PVM[,c(75:77)]), start=2010, frequency = 4)
PVM_R <- ts(rowSums(PVM[,c(78:81)]), start=2010, frequency = 4)
PVM_S <- ts(rowSums(PVM[,c(82:84)]), start=2010, frequency = 4)

## Pramonės produkcija

Pram <- read_excel("BVP darbinis.xlsx", sheet = "Pramone", col_names = TRUE)
Pram_B <- ts(Pram$pram_B, start=2007, frequency = 4)
Pram_C <- ts(rowSums(Pram[,c(5:29)]), start=2007, frequency = 4)
Pram_D <- ts(Pram$pram_D, start=2007, frequency = 4)
Pram_E <- ts(Pram$pram_E, start=2007, frequency = 4)

## SODRA
sodra <- read_excel("BVP darbinis.xlsx", sheet = "Soc_apsaug", col_names = TRUE)
sodra <- sodra[nrow(sodra):1,]
sodra <- lapply(sodra, ts, start = 2005, frequency = 4) # Paverčiame visus stulpelius į laiko eilutes
for(i in 1:length(sodra)) assign(names(sodra)[i], sodra[[i]]) # Ištraukiame į atskirus objektus



## Paslaugos 
Paslaugos <- read_excel("BVP darbinis.xlsx", sheet = "Paslaugos", col_names = TRUE)
kategorijos <- unique(substr(names(Paslaugos[,-1]), 1, 7))
Paslaugos <- sapply(kategorijos, function(xx) rowSums(Paslaugos[,grep(xx, names(Paslaugos)), drop=FALSE])) ## Sumuojame į bendras klasifikacijas
Paslaugos <- as.data.frame(Paslaugos)

Pasl_H <- ts(rowSums(Paslaugos[,c(1:5)]), start=2007, frequency = 4)
Pasl_I <- ts(Paslaugos$pasl_55, start=2007, frequency = 4)
Pasl_J <- ts(rowSums(Paslaugos[,c(7:12)]), start=2007, frequency = 4)
Pasl_L <- ts(Paslaugos$pasl_68, start=2007, frequency = 4)
Pasl_M <- ts(rowSums(Paslaugos[,c(14:19)]), start=2007, frequency = 4)
Pasl_N <- ts(rowSums(Paslaugos[,c(20:25)]), start=2007, frequency = 4)
Pasl_P <- ts(Paslaugos$pasl_85, start=2007, frequency = 4)
Pasl_Q <- ts(rowSums(Paslaugos[,c(27:29)]), start=2007, frequency = 4)
Pasl_R <- ts(rowSums(Paslaugos[,c(30:33)]), start=2007, frequency = 4)
Pasl_S <- ts(rowSums(Paslaugos[,c(34:35)]), start=2007, frequency = 4)

Menesiniai <- read_excel("BVP darbinis.xlsx", sheet = "Menesiniai", col_names = TRUE)
Menesiniai <- Menesiniai[,c(1:14)]
names(Menesiniai)=c("Data", "MazmP","Mait", "PP",
                    "G46_DP_ap", "G_ap", "G45_V_ap","G47_MP_ap", "I56_Mait_ap",
                    "G46_DP_ind", "G_ind", "G45_V_ind","G47_MP_ind", "I56_Mait_ind")

G46_DP_ap <- aggregate(ts(Menesiniai$G46_DP_ap, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
PP <- aggregate(ts(Menesiniai$PP, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
G_ap <- aggregate(ts(Menesiniai$G_ap, start=2010, frequency = 12), nfrequency = 4, FUN = sum) 
G45_V_ap <- aggregate(ts(Menesiniai$G45_V_ap, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
G47_MP_ap <- aggregate(ts(Menesiniai$G47_MP_ap, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
I56_Mait_ap <- aggregate(ts(Menesiniai$I56_Mait_ap, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
G46_DP_ind <- aggregate(ts(Menesiniai$G46_DP_ind, start=2010, frequency = 12), nfrequency = 4, FUN = mean)
G_ind <- aggregate(ts(Menesiniai$G_ind, start=2010, frequency = 12), nfrequency = 4, FUN = mean)
G45_V_ind <- aggregate(ts(Menesiniai$G45_V_ind, start=2010, frequency = 12), nfrequency = 4, FUN = mean)
G47_MP_ind <- aggregate(ts(Menesiniai$G47_MP_ind, start=2010, frequency = 12), nfrequency = 4, FUN = mean)
I56_Mait_ind <- aggregate(ts(Menesiniai$I56_Mait_ind, start=2010, frequency = 12), nfrequency = 4, FUN = mean)


## Eksportas
Eksport <- read_excel("BVP darbinis.xlsx", sheet = "EKSP", col_names = TRUE)
kategorijos <- unique(substr(names(Eksport[,-1]), 1, 6))
Eksport <- sapply(kategorijos, function(xx) rowSums(Eksport[,grep(xx, names(Eksport)), drop=FALSE])) ## Sumuojame į bendras klasifikacijas
Eksport <- as.data.frame(Eksport)
Eksport <- lapply(Eksport, ts, start = 2010, frequency = 4) # Paverčiame visus stulpelius į laiko eilutes
for(i in 1:length(Eksport)) assign(names(Eksport)[i], Eksport[[i]]) # Ištraukiame į atskirus objektus

## Importas
Import <- read_excel("BVP darbinis.xlsx", sheet = "IMP", col_names = TRUE)
kategorijos <- unique(substr(names(Import[,-1]), 1, 5))
Import <- sapply(kategorijos, function(xx) rowSums(Import[,grep(xx, names(Import)), drop=FALSE])) ## Sumuojame į bendras klasifikacijas
Import <- as.data.frame(Import)
Import <- lapply(Import, ts, start = 2010, frequency = 4) # Paverčiame visus stulpelius į laiko eilutes
for(i in 1:length(Import)) assign(names(Import)[i], Import[[i]]) # Ištraukiame į atskirus objektus

## Transportas
Transport <- read_excel("BVP darbinis.xlsx", sheet = "Transport", col_names = TRUE)
Transport <- Transport[,-1]
Transport <- lapply(Transport, ts, start = 2010, frequency = 4) # Paverčiame visus stulpelius į laiko eilutes
for(i in 1:length(Transport)) assign(names(Transport)[i], Transport[[i]]) # Ištraukiame į atskirus objektus

## Uzimtumas
Uzimtumas <- read_excel("BVP darbinis.xlsx", sheet = "Uzimtumas", col_names = TRUE)
colnames(Uzimtumas) <- paste0('uzimt_', colnames(Uzimtumas))
Uzimtumas <- Uzimtumas[nrow(Uzimtumas):1,]
Uzimtumas[is.na(Uzimtumas)] <- 0
Uzimtumas <- lapply(Uzimtumas, ts, start = 2008, frequency = 4)
for(i in 1:length(Uzimtumas)) assign(names(Uzimtumas)[i], Uzimtumas[[i]])

## Žemės ūkio produkcija
Kiti <- read_excel("BVP darbinis.xlsx", sheet = "Kiti", col_names = TRUE)
zu_prod <- ts(Kiti$zu_prod, start=2007, frequency = 4)

C_pard_pajamos <- ts(Kiti$pard_pajamos_C22, start=2007, frequency = 4)

## EKSPORTAS IR IMPORTAS BENDRI
eksport <- ts(Kiti$eksportas, start=2007, frequency = 4)
import <- ts(Kiti$importo_duom, start=2007, frequency = 4)

## Materialines investicijos
inv <- read_excel("BVP darbinis.xlsx", sheet = "mat_inv", col_names = TRUE)
inv <- lapply(inv, ts, start = 2007, frequency = 4)
for(i in 1:length(inv)) assign(names(inv)[i], inv[[i]])

## Imones
imones <- read_excel("BVP darbinis.xlsx", sheet = "imoniu_skaic", col_names = TRUE)
kategorijos <- unique(substr(names(imones[]), 1, 7))
imones$im_sk_C12 <- as.numeric(imones$im_sk_C12)
imones <- sapply(kategorijos, function(xx) rowSums(imones[,grep(xx, names(imones)), drop=FALSE])) ## Sumuojame į bendras klasifikacijas
imones <- as.data.frame(imones)
imones <- lapply(imones, ts, start = 2010, frequency = 4) # Paverčiame visus stulpelius į laiko eilutes
for(i in 1:length(imones)) assign(names(imones)[i], imones[[i]]) # Ištraukiame į atskirus objektus

## Statybos
pastatai <- ts(Kiti$pastatai, start=2007, frequency = 4)
gyv_pastatai <- ts(Kiti$gyv_pastatai, start=2007, frequency = 4)
negyv_pastatai <- ts(Kiti$negyv_pastatai, start=2007, frequency = 4)
inz_statiniai <- ts(Kiti$inz_statiniai, start=2007, frequency = 4)
leidimai <- ts(Kiti$leidimai, start=2007, frequency = 4)
statybos <- read_excel("BVP darbinis.xlsx", sheet = "Statybiniai", col_names = TRUE)
statybos <- statybos[nrow(statybos):1,]
atlikti_statiniai <-ts(statybos$atlikti_statiniai, start=1998, frequency = 4) 
baigt_gyv_stat <- ts(statybos$baigt_gyv_stat, start=1998, frequency = 4) 
visi_statyb_darbai <- ts(Kiti$Visi_statyb_darbai, start=2007, frequency = 4) 
leidimai_nauj_gyv <- ts(Kiti$leidim_nauj_gyv, start=2007, frequency = 4)

## TUI
TUI <- read_excel("BVP darbinis.xlsx", sheet = "TUI", col_names = TRUE, skip = 1)
TUI <- TUI[nrow(TUI):1,]
TUI <- lapply(TUI, ts, start = 2010, frequency = 4)
for(i in 1:length(TUI)) assign(names(TUI)[i], TUI[[i]])


## Sezonas
sezonas <- read_excel("BVP darbinis.xlsx", sheet = "Sezonas", col_names = TRUE)
sezonas <- lapply(sezonas, ts, start = 2007, frequency = 4)
for(i in 1:length(sezonas)) assign(names(sezonas)[i], sezonas[[i]])

## Salyginis darbuotoju skaicius
sal_d_sk <- read_excel("BVP darbinis.xlsx", sheet = "Salyg_d_sk", col_names = TRUE)
kategorijos <- unique(substr(names(sal_d_sk[]), 1, 12))
sal_d_sk <- sapply(kategorijos, function(xx) rowSums(sal_d_sk[,grep(xx, names(sal_d_sk)), drop=FALSE])) ## Sumuojame į bendras klasifikacijas
sal_d_sk <- as.data.frame(sal_d_sk)
sal_d_sk <- lapply(sal_d_sk, ts, start = 2010, frequency = 4) # Paverčiame visus stulpelius į laiko eilutes
for(i in 1:length(sal_d_sk)) assign(names(sal_d_sk)[i], sal_d_sk[[i]]) # Ištraukiame į atskirus objektus

## Darbo uzmokestis 
DU <- read_excel("BVP darbinis.xlsx", sheet = "DU", col_names = TRUE)
DU <- DU[,-1]
DU <- lapply(DU, ts, start = 2010, frequency = 4)
for(i in 1:length(DU)) assign(names(DU)[i], DU[[i]])

## Darbo sanaudos
DS <- read_excel("BVP darbinis.xlsx", sheet = "DS", col_names = TRUE)
DS <- DS[,-1]
DS$DS_S_valand <- as.numeric(DS$DS_S_valand)
DS <- lapply(DS, ts, start = 2010, frequency = 4)
for(i in 1:length(DS)) assign(names(DS)[i], DS[[i]])

## Gamintoju kainu indeksas
GKI <- read_excel("BVP darbinis.xlsx", sheet = "GKI", col_names = TRUE)
GKI <- GKI[,-1]
GKI <- lapply(GKI, ts, start = 2006, frequency = 4)
for(i in 1:length(GKI)) assign(names(GKI)[i], GKI[[i]])

## D21 & D31
D <- read_excel("BVP darbinis.xlsx", sheet = "D21&D31", col_names = TRUE)
D <- lapply(D, ts, start = 2005, frequency = 4)
for(i in 1:length(D)) assign(names(D)[i], D[[i]])
}

#### 1.A – Žemės ūkis, miškininkystė ir žuvininkystė ####
A_W <- window(TM_A, start=c(2011,1),end(BVP))
ZUP_W <- window(zu_prod, start=c(2011,1), end(BVP))
Uzimt_A_W <- window(uzimt_A, start=c(2011,1), end(BVP))
PVM_A_W <- window(PVM_A, start=c(2011,1), end(BVP))
Salyg_d_sk_A_W <- window(salyg_d_sk_A, start=c(2011,1), end(BVP))
DU_A_W <- window(DU_A, start=c(2011,1), end(BVP))

mod_A <- lm(A_W~ZUP_W+Uzimt_A_W+PVM_A_W)
summary(mod_A)
e_A <- resid(mod_A)
shapiro.test(e_A)
MAPE_A <- mean(abs(e_A)/A_W*100)
MAPE_A
vif(mod_A)
dwtest(mod_A)

## prognozė
Af=predict(mod_A, list(ZUP_W=c(window(zu_prod, start=c(2011,1), end=c(now))), 
                       Uzimt_A_W <- c(window(uzimt_A, start=c(2011,1), end=c(now))),
                       PVM_A_W=c(window(PVM_A, start=c(2011,1), end=c(now))), interval = "confidence"))
Af=ts(Af, start=2011, frequency=4)

##grafikas
graphics.off()
plot(A_W, lwd=2)
lines(Af, col="blue")

#### 2.B - KASYBA IR KARJERŲ EKSPLOATAVIMAS ####
B_W <- window(TM_B, start=c(2011,1),end(BVP))
Pram_B_W <- window(Pram_B, start=c(2011,1), end(BVP))
PVM_B_W <- window(PVM_B, start=c(2011,1), end(BVP))
Uzimt_B_W <- window(uzimt_B, start=c(2011,1), end(BVP))
B_W_l1 <- window(stats::lag(TM_B,-1),start=c(2011,1),end(BVP))
DS_B_l1 <- window(stats::lag(DS_B,-1),start=c(2011,1),end(BVP))
Pram_B_W_l1 <-window(stats::lag(Pram_B,-3),start=c(2011,1),end(BVP))
GKI_B_W <- window(gki_B, start=c(2011,1), end(BVP))
GKI_B_W_l1 <- window(stats::lag(gki_B,-1), start=c(2011,1), end(BVP))
S3_W <- window(S3, start=c(2011,1),end(BVP))


mod_B <- lm(B_W~B_W_l1+PVM_B_W+Uzimt_B_W+DS_B_l1+GKI_B_W_l1 + S3_W)
summary(mod_B)
e_B <- resid(mod_B)
shapiro.test(e_B)
MAPE_B <- mean(abs(e_B)/B_W*100)
MAPE_B
vif(mod_B)
dwtest(mod_B)


Bf <- predict(mod_B, list(B_W_l1 <- c(window(stats::lag(TM_B,-1),start=c(2011,1),end=c(now))),
                          S3_W <- c(window(S3, start=c(2011,1), end=c(now))),
                       PVM_B_W <- c(window(PVM_B, start=c(2011,1), end=c(now))),
                       Uzimt_B_W <- c(window(uzimt_B, start=c(2011,1), end=c(now))),
                       DS_B_l1 <- c(window(stats::lag(DS_B,-1), start=c(2011,1), end=c(now))),
                       GKI_B_W_l1 <- c(window(stats::lag(gki_B,-1),start=c(2011,1),end=c(now))),
                       interval = "confidence"))

Bf=ts(Bf, start=2011, frequency=4)
plot(B_W, lwd=2)
lines(Bf, col="blue")

#### 3. C - APDIRBAMOJI GAMYBA ####

PVM_C_W <- window(PVM_C, start=c(2011,1),end(BVP))
C_W <- window(TM_C, start=c(2011,1),end(BVP))
C_W_l1 <- window(stats::lag(TM_C,-1), start=c(2011,1),end(BVP))
gki_C_W <- window(gki_C, start=c(2011,1), end(BVP))
C_pard_pajamos_W <- window(C_pard_pajamos, start=c(2011,1), end(BVP))
S2_W <- window(S2, start=c(2011,1), end(BVP))
mat_inv_C_W <-  window(mat_inv_C, start=c(2011,1), end(BVP))

mod_C <- lm(C_W ~ PVM_C_W + gki_C_W + C_W_l1 + C_pard_pajamos_W + mat_inv_C_W + S2_W)
summary(mod_C)
e_C <- resid(mod_C)
shapiro.test(e_C)
MAPE_C <- mean(abs(e_C)/C_W*100)
MAPE_C
vif(mod_C)
dwtest(mod_C)

Cf <- predict(mod_C, list(PVM_C_W <- c(window(PVM_C, start=c(2011,1), end=c(now))),
                        gki_C_W <- c(window(gki_C, start=c(2011,1), end=c(now))),
                        S2_W <- c(window(S2, start=c(2011,1),end=c(now))),
                        C_W_l1 <- c(window(stats::lag(TM_C,-1), start=c(2011,1), end=c(now))),
                        C_pard_pajamos_W <- c(window(C_pard_pajamos, start=c(2011,1), end=c(now))),
                        mat_inv_C_W <- c(window(mat_inv_C, start=c(2011,1),end=c(now))),
                        
                          interval = "confidence"))
Cf=ts(Cf, start=2011, frequency=4)
plot(C_W, lwd=2)
lines(Cf, col="blue")

#### 4. D - ELEKTROS, DUJŲ, GARO TIEKIMAS IR ORO KONDICIONAVIMAS ####
D_W <- window(TM_D, start=c(2011,1),end(BVP))
D_W_l3 <- window(stats::lag(TM_D,-3), start=c(2011,1),end(BVP))
PVM_D_W <- window(PVM_D, start=c(2011,1),end(BVP))
S3_W <- window(S3, start=c(2011,1),end(BVP))
S2_W <- window(S2, start=c(2011,1), end(BVP))
mat_inv_D_W <-  window(mat_inv_D, start=c(2011,1), end(BVP))
gki_D_W <- window(gki_D, start=c(2011,1), end(BVP))

mod_D <- lm(D_W ~  PVM_D_W +D_W_l3 + S2_W + S3_W + mat_inv_D_W)
summary(mod_D)
e_D <- resid(mod_D)
shapiro.test(e_D)
MAPE_D <- mean(abs(e_D)/D_W*100)
MAPE_D
vif(mod_D)
dwtest(mod_D)     


Df <- predict(mod_D, list(PVM_D_W <- c(window(PVM_D, start=c(2011,1), end=c(now))),
                          D_W_l3 <- c(window(stats::lag(TM_D,-3), start=c(2011,1), end=c(now))),
                          gki_D_W <- c(window(gki_D, start=c(2011,1), end=c(now))),
                          S2_W <- c(window(S2, start=c(2011,1), end=c(now))),
                          S3_W <- c(window(S3, start=c(2011,1), end=c(now))),
                          mat_inv_D_W <- c(window(mat_inv_D, start=c(2011,1), end=c(now))),
                          interval = "confidence"))


Df=ts(Df, start=2011, frequency=4)
plot(D_W, lwd=2)
lines(Df, col="blue")


#### 5. E - VANDENS TIEKIMAS NUOTEKŲ VALYMAS, ATLIEKŲ TVARKYMAS IR REGENERAVIMAS ####
E_W <- window(TM_E, start=c(2011,1),end(BVP))
Pram_E_W <- window(Pram_E, start=c(2011,1),end(BVP))
S3_W <- window(S3, start=c(2011,1),end(BVP))
mat_inv_E_W_l3 <- window(stats::lag(mat_inv_E,-3), start=c(2011,1), end(BVP))
uzimt_E_W <- window(uzimt_E, start=c(2011,1), end(BVP))
uzimt_E_W_l3 <- window(stats::lag(uzimt_E,-3), start=c(2011,1), end(BVP))


mod_E <- lm(E_W ~ Pram_E_W + S3_W +  mat_inv_E_W_l3 + uzimt_E_W + uzimt_E_W_l3)
summary(mod_E)
e_E <- resid(mod_E)
shapiro.test(e_E)
MAPE_E <- mean(abs(e_E)/E_W*100)
MAPE_E
vif(mod_E)
dwtest(mod_E) 

Ef <- predict(mod_E, list(Pram_E_W <- c(window(Pram_E, start=c(2011,1), end=c(now))),
                          mat_inv_E_W_l3 <- c(window(stats::lag(mat_inv_E,-3), start=c(2011,1), end=c(now))),
                          S3_W <- c(window(S3, start=c(2011,1), end=c(now))),
                          uzimt_E_W_l3 <- c(window(stats::lag(uzimt_E,-3), start=c(2011,1), end=c(now))),
                          uzimt_E_W <- c(window(uzimt_E, start=c(2011,1), end=c(now))),
                          interval = "confidence"))


Ef=ts(Ef, start=2011, frequency=4)
plot(E_W, lwd=2)
lines(Ef, col="blue")

#### 6. F - STATYBA ####

uzimt_F_W <- window(uzimt_F, start=c(2011,1), end(BVP))
S3_W <- window(S3, start=c(2011,1),end(BVP))
S2_W <- window(S2, start=c(2011,1), end(BVP))
pastatai_W_l1 <- window(stats::lag(pastatai,-1), start=c(2011,1), end(BVP))
F_W <- window(TM_F, start=c(2011,1),end(BVP))
atlikti_statiniai_W <- window(atlikti_statiniai, start=c(2011,1), end(BVP))

mod_F <- lm(F_W ~ atlikti_statiniai_W + pastatai_W_l1 + S2_W + S3_W + uzimt_F_W )
summary(mod_F)
e_F <- resid(mod_F)
shapiro.test(e_F)
MAPE_F <- mean(abs(e_F)/F_W*100)
MAPE_F
vif(mod_F)
dwtest(mod_F)
Ff <- predict(mod_F, list(atlikti_statiniai_W <- c(window(atlikti_statiniai, start=c(2011,1), end=c(now))),
                             S2_W <- c(window(S2, start=c(2011,1), end=c(now))),
                             S3_W <- c(window(S3, start=c(2011,1), end=c(now))),
                           pastatai_W_l1 <- c(window(stats::lag(pastatai,-1), start=c(2011,1), end=c(now))),
                           uzimt_F_W <- c(window(uzimt_F, start=c(2011,1), end=c(now))),
                           interval = "confidence"))
Ff=ts(Ff, start=2011, frequency=4)
plot(F_W, lwd=2)
lines(Ff, col="blue")

#### 7. G - DIDMENINĖ IR MAŽMENINĖ PREKYBA; VARIKLINIŲ TRANSPORTO PRIEMONIŲ IR MOTOCIKLŲ REMONTAS ####

G_W <- window(TM_G, start=c(2011,1),end(BVP))
G_ap_W <- window(G_ap, start=c(2011,1), end(BVP))
S1_W <- window(S1, start=c(2011,1), end(BVP))
mat_inv_G_W_l1 <- window(stats::lag(mat_inv_G,-1), start=c(2011,1), end(BVP))
S3_W <- window(S3, start=c(2011,1),end(BVP))

mod_G <- lm(G_W ~ G_ap_W + S1_W + S3_W + mat_inv_G_W_l1)
summary(mod_G)
e_G <- resid(mod_G)
shapiro.test(e_G)
MAPE_G <- mean(abs(e_G)/G_W*100)
MAPE_G
vif(mod_G)
dwtest(mod_G)
Gf <- predict(mod_G, list(G_ap_W <- c(window(G_ap, start=c(2011,1), end=c(now))),
                          S1_W <- c(window(S1, start=c(2011,1), end=c(now))),
                          S3_W <- c(window(S3, start=c(2011,1), end=c(now))),
                          mat_inv_G_W_l1 <- c(window(stats::lag(mat_inv_G,-1), start=c(2011,1), end=c(now))),
                          interval = "confidence"))
Gf=ts(Gf, start=2011, frequency=4)
plot(TM_G, lwd=2)
lines(Gf, col="blue")

#### 8. H - TRANSPORTAS IR SAUGOJIMAS ####

H_W <- window(TM_H, start=c(2011,1),end(BVP))
H_W_l4 <- window(stats::lag(TM_H,-4), start=c(2011,1),end(BVP))
salyg_d_sk_H_W <- window(salyg_d_sk_H, start=c(2011,1), end(BVP))
PVM_H_W <- window(PVM_H, start=c(2011,1),end(BVP))
S3_W <- window(S3, start=c(2011,1),end(BVP))

mod_H <- lm(H_W ~ S3_W + H_W_l4 + salyg_d_sk_H_W + PVM_H_W)
summary(mod_H)
e_H <- resid(mod_H)
shapiro.test(e_H)
MAPE_H <- mean(abs(e_H)/H_W*100)
MAPE_H
vif(mod_H)
dwtest(mod_H)

Hf <- predict(mod_H, list(PVM_H_W <- c(window(PVM_H, start=c(2011,1), end=c(now))),
                        H_W_l4 <- c(window(stats::lag(TM_H,-4), start=c(2011,1), end=c(now))),
                        S3_W <- c(window(S3, start=c(2011,1), end=c(now))),  
                        salyg_d_sk_H_W <- c(window(salyg_d_sk_H, start=c(2011,1), end=c(now))), 
                        interval = "confidence"))


Hf=ts(Hf, start=2011, frequency=4)
plot(H_W, lwd=2)
lines(Hf, col="blue")

#### I - APGYVENDINIMO IR MAITINIMO PASLAUGŲ VEIKLA ####

I_W <- window(TM_I, start=c(2011,1),end(BVP))
Pasl_I_W <- window(Pasl_I,  start=c(2011,1), end(BVP))
S1_W <- window(S1, start=c(2011,1), end(BVP))
I56_Mait_ap_W <- window(I56_Mait_ap, start=c(2011,1), end(BVP))
S2_W <- window(S2, start=c(2011,1), end(BVP))

mod_I <- lm(I_W ~ Pasl_I_W + S1_W + I56_Mait_ap_W + S2_W )
summary(mod_I)
e_I <- resid(mod_I)
shapiro.test(e_I)
MAPE_I <- mean(abs(e_I)/I_W*100)
MAPE_I
vif(mod_I)
dwtest(mod_I)

If <- predict(mod_I, list(Pasl_I_W <- c(window(Pasl_I, start=c(2011,1), end=c(now))),
                          S1_W <- c(window(S1, start=c(2011,1), end=c(now))),  
                          S2_W <- c(window(S2, start=c(2011,1), end=c(now))),  
                          I56_Mait_ap_W <- c(window(I56_Mait_ap, start=c(2011,1), end=c(now))), 
                          interval = "confidence"))
If=ts(If, start=2011, frequency=4)
plot(I_W, lwd=2)
lines(If, col="blue")

#### J - INFORMACIJA IR RYŠIAI ####


J_W <- window(TM_J, start=c(2011,1),end(BVP))
PVM_J_W <- window(PVM_J, start=c(2011,1),end(BVP))
J_W_l1 <- window(stats::lag(TM_J,-1), start=c(2011,1),end(BVP))
DS_J_W_l1 <- window(stats::lag(DS_J,-1), start=c(2011,1),end(BVP))
S3_W <- window(S3, start=c(2011,1),end(BVP))
S2_W <- window(S2, start=c(2011,1), end(BVP))

mod_J <- lm(J_W ~ PVM_J_W + J_W_l1 + DS_J_W_l1 + S2_W + S3_W)
summary(mod_J)
e_J <- resid(mod_J)
shapiro.test(e_J)
MAPE_J <- mean(abs(e_J)/J_W*100)
MAPE_J
vif(mod_J)
dwtest(mod_J)

Jf <- predict(mod_J, list(
                                        PVM_J_W <- c(window(PVM_J, start=c(2011,1), end=c(now))),
                                        DS_J_W_l1 <- c(window(stats::lag(DS_J,-1), start=c(2011,1), end=c(now))), 
                                        J_W_l1 <- c(window(stats::lag(TM_J,-1), start=c(2011,1), end=c(now))), 
                                        S2_W <- c(window(S2, start=c(2011,1), end=c(now))),  
                                        S3_W <- c(window(S3, start=c(2011,1), end=c(now))),  
                          interval = "confidence"))
Jf=ts(Jf, start=2011, frequency=4)
plot(J_W, lwd=2)
lines(Jf, col="blue")

#### K - FINANSINĖ IR DRAUDIMO VEIKLA ####

K_W <- window(TM_K, start=c(2011,1),end(BVP))
PVM_K_W_l1 <- window(stats::lag(PVM_K,-1), start=c(2011,1),end(BVP))
DU_K_W <- window(DU_K, start=c(2011,1),end(BVP))
S4_W <- window(S4, start=c(2011,1), end(BVP))
K_W_l1 <- window(stats::lag(TM_K,-1), start=c(2011,1),end(BVP))

mod_K <- lm(K_W ~ PVM_K_W_l1 + DU_K_W + S4_W + K_W_l1)
summary(mod_K)
e_K <- resid(mod_K)
shapiro.test(e_J)
MAPE_K <- mean(abs(e_K)/K_W*100)
MAPE_K
vif(mod_K)
dwtest(mod_K)

Kf <- predict(mod_K, list(
                                  DU_K_W<- c(window(DU_K, start=c(2011,1),end=c(now))), 
                                  S4_W <- c(window(S4, start=c(2011,1), end=c(now))),  
                                  PVM_K_W_l1 <- c(window(stats::lag(PVM_K,-1), start=c(2011,1),end=c(now))),
                                  K_W_l1 <- c(window(stats::lag(TM_K,-1), start=c(2011,1),end=c(now))),
                                  interval = "confidence"))

Kf=ts(Kf, start=2011, frequency=4)
plot(K_W, lwd=2)
lines(Kf, col="blue")

#### 7. L – Nekilnojamo turto operacijos ####

L_W <- window(TM_L, start=c(2011,1),end(BVP))
visi_statyb_darbai_W_l4 <- window(stats::lag(visi_statyb_darbai,-4), start=c(2011,1), end(BVP))
PVM_L_W <- window(PVM_L, start=c(2011,1),end(BVP))
L_W_l1 <- window(stats::lag(TM_L,-1), start=c(2011,1),end(BVP))
visi_statyb_darbai_W_l3 <- window(stats::lag(visi_statyb_darbai,-3), start=c(2011,1), end(BVP))
leidimai_nauj_gyv_W_l2 <- window(stats::lag(leidimai_nauj_gyv,-2), start=c(2011,1), end(BVP))

mod_L <- lm( L_W ~ visi_statyb_darbai_W_l4 + PVM_L_W + L_W_l1 + visi_statyb_darbai_W_l3 + leidimai_nauj_gyv_W_l2 )
summary(mod_L)
e_L <- resid(mod_L)
shapiro.test(e_L)
MAPE_L <- mean(abs(e_L)/L_W*100)
MAPE_L
vif(mod_L)
dwtest(mod_L)


Lf <- predict(mod_L, list(
        visi_statyb_darbai_W_l4 <- c(window(stats::lag(visi_statyb_darbai,-4), start=c(2011,1),end=c(now))),
        PVM_L_W <- c(window(PVM_L, start=c(2011,1),end=c(now))), 
        L_W_l1 <- c(window(stats::lag(TM_L,-1), start=c(2011,1),end=c(now))),
        visi_statyb_darbai_W_l3 <- c(window(stats::lag(visi_statyb_darbai,-3), start=c(2011,1),end=c(now))),
        leidimai_nauj_gyv_W_l2 <- c(window(stats::lag(leidimai_nauj_gyv,-2), start=c(2011,1),end=c(now))),
        interval = "confidence"))


Lf=ts(Lf, start=2011, frequency=4)
plot(L_W, lwd=2)
lines(Lf, col="blue")

#### M - PROFESINĖ, MOKSLINĖ IR TECHNINĖ VEIKLA ####

M_W <- window(TM_M, start=c(2011,1),end(BVP))
Pasl_M_W <- window(Pasl_M,  start=c(2011,1), end(BVP))
PVM_M_W_l1 <- window(stats::lag(PVM_M,-1), start=c(2011,1),end(BVP))
S1_W <- window(S1, start=c(2011,1), end(BVP))
TUI_M_W_l1 <- window(stats::lag(TUI_M,-1), start=c(2011,1), end(BVP))

mod_M <- lm(M_W ~ Pasl_M_W + PVM_M_W_l1 + S1_W + TUI_M_W_l1)
summary(mod_M)
e_M <- resid(mod_M)
shapiro.test(e_M)
MAPE_M <- mean(abs(e_M)/M_W*100)
MAPE_M
vif(mod_M)
dwtest(mod_M)

Mf <- predict(mod_M, list(
        Pasl_M_W<- c(window(Pasl_M, start=c(2011,1),end=c(now))), 
        S1_W <- c(window(S1, start=c(2011,1), end=c(now))),  
        PVM_M_W_l1 <- c(window(stats::lag(PVM_M,-1), start=c(2011,1),end=c(now))),
        TUI_M_W_l1 <- c(window(stats::lag(TUI_M,-1), start=c(2011,1),end=c(now))),
        interval = "confidence"))


Mf=ts(Mf, start=2011, frequency=4)
plot(M_W, lwd=2)
lines(Mf, col="blue")

#### N -  ADMINISTRACINĖ IR APTARNAVIMO VEIKLA ####

N_W <- window(TM_N, start=c(2011,1),end(BVP))
Pasl_N_W <- window(Pasl_N,  start=c(2011,1), end(BVP))
im_sk_N_W <- window(im_sk_N, start=c(2011,1), end(BVP))
DS_N_W <- window(DS_N, start=c(2011,1),end(BVP))

mod_N <- lm(N_W ~ Pasl_N_W + im_sk_N_W + DS_N_W)
summary(mod_N)

e_N <- resid(mod_N)
shapiro.test(e_N)
MAPE_N <- mean(abs(e_N)/N_W*100)
MAPE_N
vif(mod_N)
dwtest(mod_N)

Nf <- predict(mod_N, list(
        Pasl_N_W<- c(window(Pasl_N, start=c(2011,1),end=c(now))), 
        DS_N_W<- c(window(DS_N, start=c(2011,1),end=c(now))),
        im_sk_N_W<- c(window(im_sk_N, start=c(2011,1),end=c(now))),
        interval = "confidence"))


Nf=ts(Nf, start=2011, frequency=4)
plot(N_W, lwd=2)
lines(Nf, col="blue")

#### O - VIEŠASIS VALDYMAS IR GYNYBA; PRIVALOMASIS SOCIALINIS DRAUDIMAS ####

O_W <- window(TM_O, start=c(2011,1),end(BVP))
DU_O_W <- window(DU_O, start=c(2011,1),end(BVP))
O_W_l4 <- window(stats::lag(TM_O,-4), start=c(2011,1),end(BVP))
OTE_W <- window(OTE, start=c(2011,1), end(BVP))

mod_O <- lm(O_W ~ DU_O_W + O_W_l4 + OTE_W)

summary(mod_O)

e_O <- resid(mod_O)
shapiro.test(e_O)
MAPE_O <- mean(abs(e_O)/O_W*100)
MAPE_O
vif(mod_O)
dwtest(mod_O)

Of <- predict(mod_O, list(
        DU_O_W<- c(window(DU_O, start=c(2011,1),end=c(now))), 
        OTE_W<- c(window(OTE, start=c(2011,1),end=c(now))),
        S1_W<- c(window(S1, start=c(2011,1),end=c(now))),
        O_W_l4 <- c(window(stats::lag(TM_O,-4), start=c(2011,1),end=c(now))),
        interval = "confidence"))


Of=ts(Of, start=2011, frequency=4)
plot(O_W, lwd=2)
lines(Of, col="blue")

#### P - ŠVIETIMAS ####
P_W <- window(TM_P, start=c(2011,1),end(BVP))
DU_P_W <- window(DU_P, start=c(2011,1),end(BVP))
S3_W <- window(S3, start=c(2011,1),end(BVP))
S2_W <- window(S2, start=c(2011,1), end(BVP))
im_sk_P_W <- window(im_sk_P, start=c(2011,1), end(BVP))
Pasl_P_W <- window(Pasl_P,  start=c(2011,1), end(BVP))
TUI_P_W_l2 <- window(stats::lag(TUI_P,-2), start=c(2011,1), end(BVP))

mod_P <- lm(P_W ~ DU_P_W + S3_W + S2_W + im_sk_P_W + Pasl_P_W + TUI_P_W_l2) 
summary(mod_P)
e_P <- resid(mod_P)
shapiro.test(e_P)
MAPE_P <- mean(abs(e_P)/P_W*100)
MAPE_P
vif(mod_P)
dwtest(mod_P)

Pf <- predict(mod_P, list(
        DU_P_W<- c(window(DU_P, start=c(2011,1),end=c(now))), 
        S3_W<- c(window(S3, start=c(2011,1),end=c(now))), 
        S2_W<- c(window(S2, start=c(2011,1),end=c(now))), 
        im_sk_P_W<- c(window(im_sk_P, start=c(2011,1),end=c(now))), 
        Pasl_P_W<- c(window(Pasl_P, start=c(2011,1),end=c(now))), 
        TUI_P_W_l2 <- c(window(stats::lag(TUI_P,-2), start=c(2011,1),end=c(now))),
        interval = "confidence"))


Pf=ts(Pf, start=2011, frequency=4)
plot(P_W, lwd=2)
lines(Pf, col="blue")

#### Q - ŽMONIŲ SVEIKATOS PRIEŽIŪRA IR SOCIALINIS DARBAS ####
Q_W <- window(TM_Q, start=c(2016,1),end(BVP))
PVM_Q_W <- window(PVM_Q, start=c(2016,1),end(BVP))
Q_W_l4 <- window(stats::lag(TM_Q,-4), start=c(2016,1),end(BVP))
mat_inv_Q_W_l3 <- window(stats::lag(mat_inv_Q,-3), start=c(2016,1), end(BVP))

mod_Q <- lm (Q_W ~ PVM_Q_W + Q_W_l4 + mat_inv_Q_W_l3)
summary(mod_Q)
e_Q <- resid(mod_Q)
shapiro.test(e_Q)
MAPE_Q <- mean(abs(e_Q)/Q_W*100)
MAPE_Q
vif(mod_Q)
dwtest(mod_Q)

Qf <- predict(mod_Q, list(
        PVM_Q_W<- c(window(PVM_Q, start=c(2011,1),end=c(now))), 
        Q_W_l4 <- c(window(stats::lag(TM_Q,-4), start=c(2011,1),end=c(now))),
        mat_inv_Q_W_l3 <- c(window(stats::lag(mat_inv_Q,-3), start=c(2011,1),end=c(now))),
        interval = "confidence"))


Qf=ts(Qf, start=2011, frequency=4)
plot(Q_W, lwd=2)
lines(Qf, col="blue")

## MENINĖ, PRAMOGINĖ IR POILSIO ORGANIZAVIMO VEIKLA
R_W <- window(TM_R, start=c(2014,1),end(BVP))
Pasl_R_W <- window(Pasl_R,  start=c(2014,1), end(BVP))
DU_R_W <- window(DU_R, start=c(2014,1),end(BVP))
mat_inv_R_W_l2 <- window(stats::lag(mat_inv_R,-2), start=c(2014,1), end(BVP))

mod_R <- lm (R_W ~ Pasl_R_W + DU_R_W + mat_inv_R_W_l2)
summary(mod_R)
e_R <- resid(mod_R)
shapiro.test(e_R)
MAPE_R <- mean(abs(e_R)/R_W*100)
MAPE_R
vif(mod_R)
dwtest(mod_R)

Rf <- predict(mod_R, list(
        Pasl_R_W<- c(window(Pasl_R, start=c(2014,1),end=c(now))), 
        DU_R_W<- c(window(DU_R, start=c(2014,1),end=c(now))), 
        mat_inv_R_W_l2 <- c(window(stats::lag(mat_inv_R,-2), start=c(2014,1),end=c(now))),
        interval = "confidence"))


Rf=ts(Rf, start=2014, frequency=4)
plot(R_W, lwd=2)
lines(Rf, col="blue")

#### S - KITA APTARNAVIMO VEIKLA ####

S_W <- window(TM_S, start=c(2014,1),end(BVP))
S_W_l4 <- window(stats::lag(TM_S,-4), start=c(2014,1),end(BVP))
DS_S_val_W <- window(DS_S_valand, start=c(2014,1),end(BVP))
im_sk_S_W <- window(im_sk_S, start=c(2014,1), end(BVP))
im_sk_S_W_l1 <- window(stats::lag(im_sk_S,-1), start=c(2014,1), end(BVP))
mat_inv_S_W_l1 <- window(stats::lag(mat_inv_S,-1), start=c(2014,1), end(BVP))

mod_S <- lm(S_W ~ S_W_l4 + DS_S_val_W + im_sk_S_W + im_sk_S_W_l1 + mat_inv_S_W_l1)
summary(mod_S)
e_S <- resid(mod_S)
shapiro.test(e_S)
MAPE_S <- mean(abs(e_S)/R_W*100)
MAPE_S
vif(mod_S)
dwtest(mod_S)

Sf <- predict(mod_S, list(
        im_sk_S_W<- c(window(im_sk_S, start=c(2014,1),end=c(now))), 
        DS_S_val_W<- c(window(DS_S_valand, start=c(2014,1),end=c(now))), 
        S_W_l4 <- c(window(stats::lag(TM_S,-4), start=c(2014,1),end=c(now))),
        im_sk_S_W_l1 <- c(window(stats::lag(im_sk_S,-1), start=c(2014,1),end=c(now))),
        mat_inv_S_W_l1 <- c(window(stats::lag(mat_inv_S,-1), start=c(2014,1),end=c(now))),
        interval = "confidence"))

Sf=ts(Sf, start=2014, frequency=4)
plot(S_W, lwd=2)
lines(Sf, col="blue")


#### D21 – Mokesčiai produktams ###
D21_W <- window(D21, start=c(2011,1),end(BVP))
PVMviso_W <- window(PVMviso, start=c(2011,1),end(BVP))
DS_Total_l1 <- window(stats::lag(DS_Total,-1), start=c(2011,1),end(BVP))
D21_W_l4 <- window(stats::lag(D21,-4), start=c(2011,1),end(BVP))
S2_W <- window(S2, start=c(2011,1), end(BVP))
DS_Total_W <- window(DS_Total, start=c(2011,1), end(BVP))
DU_Total_W <- window(DU_Total, start=c(2011,1), end(BVP))

mod_D21 <- lm(D21_W~PVMviso_W + D21_W_l4)
summary(mod_D21)
e_D21 <- resid(mod_D21)
shapiro.test(e_D21)
MAPE_D21 <- mean(abs(e_D21)/D21_W*100)
MAPE_D21
vif(mod_D21)
dwtest(mod_D21)

D21f <- predict(mod_D21, list(
        PVMviso_W<- c(window(PVMviso, start=c(2011,1),end=c(now))), 
        D21_W_l4 <- c(window(stats::lag(D21,-4), start=c(2011,1),end=c(now))),
        interval = "confidence"))

D21f <- ts(D21f, start=2011, frequency=4)

graphics.off()
plot(D21, lwd=2)
lines(D21f, col="blue")

## D31 Subsidijos produktams
D31_W <- window(D31, start=c(2011,2),end(BVP))

auto.arima(D31_W)
mod_D31_ARIMA <- arima(D31_W, order=c(0,0,1), seasonal=c(0,1,0))
summary(mod_D31_ARIMA)
coeftest(mod_D31_ARIMA)
tsdiag(mod_D31_ARIMA)
D31_ARIMA <- fitted(mod_D31_ARIMA)
e_D31_ARIMA <- resid(mod_D31_ARIMA)
D31f <- forecast(mod_D31_ARIMA, h = 1)
D31_lef <- ts(D31f$mean, start=c(now),frequency = 4)
D31lef <- ts(c(D31_ARIMA,D31_lef), start(D31_ARIMA), frequency = 4)

graphics.off()
plot(D31_W, lwd=2)
lines(D31lef, col="blue")

## REZULTATAS

PV <- Af + Bf + Cf + Df + Ef + Ff + Gf + Hf + If + Jf + Kf + Lf + Mf + Nf + Of + Pf + Qf + Rf + Sf
PV_W <- window(PV, start=c(2014,1))
D21_D31_f <- D21f - D31lef
BVPf <- PV_W +  D21_D31_f
MAPE_BVP <- mean(abs(BVP_W-BVPf)/BVP_W*100)
MAPE_BVP

plot(BVP,lwd=2)
lines(BVPf,col="red")


