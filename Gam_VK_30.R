if(!require("readxl")) install.packages("readxl"); library("readxl")
if(!require("xlsx")) install.packages("xlsx"); library("xlsx")
if(!require("astsa")) install.packages("astsa"); library("astsa")
if(!require("car")) install.packages("car"); library("car")
if(!require("forecast")) install.packages("forecast"); library("forecast")
if(!require("lmtest")) install.packages("lmtest"); library("lmtest")
if(!require("tseries")) install.packages("tseries"); library("tseries")
if(!require("xts")) install.packages("xts"); library("xts")
setwd("C:/Users/PC/Desktop/StatDep/")

BVP_gamvk=read_excel("BVP.xlsx", sheet = "Gamybos_VK", range = NULL, col_names = FALSE, skip=21)
names(BVP_gamvk)=c("Data","A","B_E","C","F","G_T","G_I", "J", "K", "L", "M_N", "O_Q", "R_T", "A_T","D21","D31", "D21-D31","BVP")

regr=read_excel("BVP.xlsx", sheet = "Regresoriai", range = NULL, col_names = TRUE)

regr_men=read_excel("BVP.xlsx", sheet = "Regresoriai_men", range = NULL, col_names = FALSE, skip=1)
names(regr_men)=c("Data", "MazmP","Mait","CVSko",
                  "PP_B_E_VK","PP_B_VK","PP_C_VK","PP_D_VK", "PP_E_VK","PP_B_E_PK","PP_B_PK","PP_C_PK", "PP_D_PK", "PP_E_PK", 
                  "G46_DP_ap", "G_ap", "G45_V_ap","G47_MP_ap", "I56_Mait_ap",
                  "G46_DP_ind", "G_ind", "G45_V_ind","G47_MP_ind", "I56_Mait_ind",
                  "NB_turt", "FI_turt", "NB_sand","FI_sand")


########Gamybos VK metodas
A=ts(BVP_gamvk$A, start=2010, frequency = 4)#Žemės ūkis, miškininkystė ir žuvininkystė
B_E=ts(BVP_gamvk$B_E, start=2010, frequency = 4) #Kasyba ir karjerų eksploatavimas; apdirbamoji gamyba; elektros, dujų, garo tiekimas ir oro kondicionavimas; vandens tiekimas; nuotekų valymas, atliekų tvarkymas ir regeneravimas
C=ts(BVP_gamvk$C, start=2010, frequency = 4) #Apdirbamoji gamyba
F=ts(BVP_gamvk$F, start=2010, frequency = 4) #Statyba
G_I=ts(BVP_gamvk$G_I, start=2010, frequency = 4)#Didmeninė ir mažmeninė prekyba; variklinių transporto priemonių ir motociklų remontas; transportas ir saugojimas; apgyvendinimo ir maitinimo paslaugų veikla
J=ts(BVP_gamvk$J, start=2010, frequency = 4)#Informacija ir ryšiai
K=ts(BVP_gamvk$K, start=2010, frequency = 4)#Finansinė ir draudimo veikla
L=ts(BVP_gamvk$L, start=2010, frequency = 4)# Nekilnojamo turto operacijos
M_N=ts(BVP_gamvk$M_N, start=2010, frequency = 4)#Profesinė, mokslinė ir techninė veikla; administracinė ir aptarnavimo veikla
O_Q=ts(BVP_gamvk$O_Q, start=2010, frequency = 4)# Viešasis valdymas ir gynyba; privalomasis socialinis draudimas; švietimas; žmonių sveikatos priežiūra ir socialinis darbas
R_T=ts(BVP_gamvk$R_T, start=2010, frequency = 4)# Meninė, pramoginė ir poilsio organizavimo veikla, namų ūkio reikmenų remontas ir kitos paslaugos
D21=ts(BVP_gamvk$D21, start=2010, frequency = 4)#Mokesčiai produktams
D31=ts(BVP_gamvk$D31, start=2010, frequency = 4)#Subsidijos produktams
BVP=ts(BVP_gamvk$BVP, start=2010, frequency = 4) #BVP

##Papildomi kintamieji
now=end(BVP)+c(0,1)#1 keisti pagal prognozuojamo ketvirčio numerį nuo l.e. pabaigos


## BVP komponentų lag
A_l1=window(lag(A,-1),start=c(2011,1),end(BVP))
A_l4=window(lag(A,-4),start=c(2011,1),end(BVP))
B_E_l1=window(lag(B_E,-1),start=c(2011,1),end(BVP))
B_E_l4=window(lag(B_E,-4),start=c(2011,1),end(BVP))
C_l1=window(lag(C,-1),start=c(2011,1),end(BVP))
C_l4=window(lag(C,-4),start=c(2011,1),end(BVP))
F_l1=window(lag(F,-1),start=c(2011,1),end(BVP))
F_l4=window(lag(F,-4),start=c(2011,1),end(BVP))
G_I_l1=window(lag(G_I,-1),start=c(2011,1),end(BVP))
G_I_l4=window(lag(G_I,-4),start=c(2011,1),end(BVP))
J_l1=window(lag(J,-1),start=c(2011,1),end(BVP))
#J_l2=window(lag(J,-2),start=c(2011,1),end(BVP))
J_l4=window(lag(J,-4),start=c(2011,1),end(BVP))
K_l1=window(lag(K,-1),start=c(2011,1),end(BVP))
K_l4=window(lag(K,-4),start=c(2011,1),end(BVP))
L_l1=window(lag(L,-1),start=c(2011,1),end(BVP))
L_l4=window(lag(L,-4),start=c(2011,1),end(BVP))
M_N_l1=window(lag(M_N,-1),start=c(2011,1),end(BVP))
M_N_l4=window(lag(M_N,-4),start=c(2011,1),end(BVP))
O_Q_l1=window(lag(O_Q,-1),start=c(2011,1),end(BVP))
O_Q_l4=window(lag(O_Q,-4),start=c(2011,1),end(BVP))
R_T_l1=window(lag(R_T,-1),start=c(2011,1),end(BVP))
R_T_l4=window(lag(R_T,-4),start=c(2011,1),end(BVP))
D21_l1=window(lag(D21,-1),start=c(2011,1),end(BVP))
D21_l4=window(lag(D21,-4),start=c(2011,1),end(BVP))
D31_l1=window(lag(D31,-1),start=c(2011,1),end(BVP))
D31_l4=window(lag(D31,-4),start=c(2011,1),end(BVP))
BVP_l1=window(lag(BVP,-1),start=c(2011,1),end(BVP))
BVP_l4=window(lag(BVP,-4),start=c(2011,1),end(BVP))


## BVP komponentai nuo 2011 m.
A_w=window(A,start=c(2011,1),end(BVP))
B_E_w=window(B_E,start=c(2011,1),end(BVP))
C_w=window(C,start=c(2011,1),end(BVP))
F_w=window(F,start=c(2011,1),end(BVP))
G_I_w=window(G_I,start=c(2011,1),end(BVP))
J_w=window(J,start=c(2011,1),end(BVP))
K_w=window(K,start=c(2011,1),end(BVP))
L_w=window(L,start=c(2011,1),end(BVP))
M_N_w=window(M_N,start=c(2011,1),end(BVP))
O_Q_w=window(O_Q,start=c(2011,1),end(BVP))
R_T_w=window(R_T,start=c(2011,1),end(BVP))
D21_w=window(D21,start=c(2011,1),end(BVP))
D31_w=window(D31,start=c(2011,1),end(BVP))
BVP_w=window(BVP,start=c(2011,1),end(BVP))


## Regresoriai gamybos VK metodui
PVMviso=ts(regr$pvm_viso, start=2010, frequency = 4)
PVM_A=ts(rowSums(regr[,c(3:5)]), start=2010, frequency = 4)
PVM_B=ts(rowSums(regr[,c(6:7)]), start=2010, frequency = 4) # B veiklos PVM
PVM_C=ts(rowSums(regr[,c(8:31)]), start=2010, frequency = 4)# C veiklos PVM
PVM_D=ts(regr$pvm_35, start=2010, frequency = 4) # D veiklos PVM
pvm_39=ifelse(is.na(regr$pvm_39)==TRUE, 0,regr$pvm_39)#praleistų reikšmių pavertimas į 0
PVM_E=ts(rowSums(regr[,c(33:35)])+pvm_39, start=2010, frequency = 4) # E veiklos PVM
PVM_B_E=PVM_B+PVM_B+PVM_C+PVM_D+PVM_E  # B+C+D+E veiklų PVM
PVM_F=ts(rowSums(regr[,c(37:39)]), start=2010, frequency = 4)
PVM_G_I=ts(rowSums(regr[,c(40:49)]), start=2010, frequency = 4)
PVM_J=ts(rowSums(regr[,c(50:55)]), start=2010, frequency = 4)
PVM_K=ts(rowSums(regr[,c(56:58)]), start=2010, frequency = 4)
PVM_L=ts(regr$pvm_68, start=2010, frequency = 4)
PVM_M_N=ts(rowSums(regr[,c(60:72)]), start=2010, frequency = 4)
PVM_O_Q=ts(rowSums(regr[,c(63:77)]), start=2010, frequency = 4)
PVM_R_T=ts(rowSums(regr[,c(78:84)]), start=2010, frequency = 4)

MazPr=ts(regr$G47_mazmenine, start=2010, frequency = 4)
Maitin=ts(regr$I56_maitinimas, start=2010, frequency = 4)
DU_Total=ts(regr$DU_Total, start=2010, frequency = 4)
DU_A=ts(regr$DU_A, start=2010, frequency = 4)
DU_B=ts(regr$DU_B, start=2010, frequency = 4)
DU_C=ts(regr$DU_C, start=2010, frequency = 4)
DU_D=ts(regr$DU_D, start=2010, frequency = 4)
DU_E=ts(regr$DU_E, start=2010, frequency = 4)
DU_F=ts(regr$DU_F, start=2010, frequency = 4)
DU_G=ts(regr$DU_G, start=2010, frequency = 4)
DU_H=ts(regr$DU_H, start=2010, frequency = 4)
DU_I=ts(regr$DU_I, start=2010, frequency = 4)
DU_J=ts(regr$DU_J, start=2010, frequency = 4)
DU_K=ts(regr$DU_K, start=2010, frequency = 4)
DU_L=ts(regr$DU_L, start=2010, frequency = 4)
DU_M=ts(regr$DU_M, start=2010, frequency = 4)
DU_N=ts(regr$DU_N, start=2010, frequency = 4)
DU_O=ts(regr$DU_O, start=2010, frequency = 4)
DU_P=ts(regr$DU_P, start=2010, frequency = 4)
DU_Q=ts(regr$DU_Q, start=2010, frequency = 4)
DU_R=ts(regr$DU_R, start=2010, frequency = 4)
DU_S=ts(regr$DU_S, start=2010, frequency = 4)


DS_Total=ts(regr$DS_Total, start=2010, frequency = 4)
DS_A=ts(regr$DS_A, start=2010, frequency = 4)
DS_B=ts(regr$DS_B, start=2010, frequency = 4)
DS_C=ts(regr$DS_C, start=2010, frequency = 4)
DS_D=ts(regr$DS_D, start=2010, frequency = 4)
DS_E=ts(regr$DS_E, start=2010, frequency = 4)
DS_F=ts(regr$DS_F, start=2010, frequency = 4)
DS_G=ts(regr$DS_G, start=2010, frequency = 4)
DS_H=ts(regr$DS_H, start=2010, frequency = 4)
DS_I=ts(regr$DS_I, start=2010, frequency = 4)
DS_G_I=DS_G+DS_H+DS_I
DS_J=ts(regr$DS_J, start=2010, frequency = 4)
DS_K=ts(regr$DS_K, start=2010, frequency = 4)
DS_L=ts(regr$DS_L, start=2010, frequency = 4)
DS_M=ts(regr$DS_M, start=2010, frequency = 4)
DS_N=ts(regr$DS_N, start=2010, frequency = 4)
DS_M_N=DS_M+DS_N
DS_O=ts(regr$DS_O, start=2010, frequency = 4)
DS_P=ts(regr$DS_P, start=2010, frequency = 4)
DS_Q=ts(regr$DS_Q, start=2010, frequency = 4)
DS_O_Q=DS_O+DS_P+DS_Q
DS_R=ts(regr$DS_R, start=2010, frequency = 4)
DS_S=ts(regr$DS_S, start=2010, frequency = 4)
DS_R_S=DS_R+DS_S


SD=ts(regr$Statybos_darbai_Visi_statiniai,start=2010, frequency = 4)
SL_NP=ts(regr$Statybos_leidimai_Negyvenamieji_pastatai,start=2010, frequency = 4)
SL_GP=ts(regr$Statybos_leidimai_Gyvenamieji_pastatai,start=2010, frequency = 4)
SL=SL_NP+SL_GP

Kel_ap=ts(regr$Keleiviu_apyvarta,start=2010, frequency = 4)
Kro_ap=ts(regr$Kroviniu_apyvarta,start=2010, frequency = 4)

H_PP=ts(regr$H_paslaugu_pajamos,start=2010, frequency = 4)
I55_PP=ts(regr$I55_paslaugu_pajamos,start=2010, frequency = 4)
J_PP=ts(regr$J_paslaugu_pajamos,start=2010, frequency = 4)
L68_PP=ts(regr$L68_paslaugu_pajamos,start=2010, frequency = 4)
M_PP=ts(regr$M_NOT_M72_paslaugu_pajamos,start=2010, frequency = 4)
N_PP=ts(regr$N_paslaugu_pajamos,start=2010, frequency = 4)
M_N_PP=M_PP+N_PP
P_PP=ts(regr$P_paslaugu_pajamos,start=2010, frequency = 4)
Q_PP=ts(regr$Q_paslaugu_pajamos,start=2010, frequency = 4)
O_Q_PP=P_PP+Q_PP
R_PP=ts(regr$R_paslaugu_pajamos,start=2010, frequency = 4)
S_PP=ts(regr$S_NOT_S94_paslaugu_pajamos,start=2010, frequency = 4)
R_S_PP=R_PP+S_PP


Skola=ts(regr$Valdzios_s_skola, start=2010,frequency = 4)
Gal_v_isl=ts(regr$Galutinio_vartojimo_islaidos_Valdzios_sektoriaus, start=2010,frequency = 4)
VS_isl=ts(regr$Valdzios_sektoriaus_islaidos, start=2010,frequency = 4)
VS_paj=ts(regr$Valdzios_sektoriaus_pajamos, start=2010,frequency = 4)
t=1:(length(BVP)+1)
#LS_2018_1=ts(ifelse(t<=32,0,1), start=2010, frequency = 4)
S1=ts(rep(c(1,0,0,0), length.out=length(BVP)+1), start=2010, frequency = 4)
S2=ts(rep(c(0,1,0,0), length.out=length(BVP)+1), start=2010, frequency = 4)
S3=ts(rep(c(0,0,1,0), length.out=length(BVP)+1), start=2010, frequency = 4)
S4=ts(rep(c(0,0,0,1), length.out=length(BVP)+1), start=2010, frequency = 4)

ZUP=ts(regr$Zemes_uk_produkcija, start=2010,frequency = 4)


#Mėnesinių kintamųjų agregavimas
PP_B_E_VK=aggregate(ts(regr_men$PP_B_E_VK, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
PP_B_VK=aggregate(ts(regr_men$PP_B_VK, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
PP_C_VK=aggregate(ts(regr_men$PP_C_VK, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
PP_D_VK=aggregate(ts(regr_men$PP_D_VK, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
PP_E_VK=aggregate(ts(regr_men$PP_E_VK, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
PP_B_E_PK=aggregate(ts(regr_men$PP_B_E_PK, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
PP_B_PK=aggregate(ts(regr_men$PP_B_PK, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
PP_C_PK=aggregate(ts(regr_men$PP_C_PK, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
PP_D_PK=aggregate(ts(regr_men$PP_D_PK, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
PP_E_PK=aggregate(ts(regr_men$PP_E_PK, start=2010, frequency = 12), nfrequency = 4, FUN = sum)


G46_DP_ap=aggregate(ts(regr_men$G46_DP_ap, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
G_ap=aggregate(ts(regr_men$G_ap, start=2010, frequency = 12), nfrequency = 4, FUN = sum) 
G45_V_ap=aggregate(ts(regr_men$G45_V_ap, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
G47_MP_ap=aggregate(ts(regr_men$G47_MP_ap, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
I56_Mait_ap=aggregate(ts(regr_men$I56_Mait_ap, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
G46_DP_ind=aggregate(ts(regr_men$G46_DP_ind, start=2010, frequency = 12), nfrequency = 4, FUN = mean)
G_ind=aggregate(ts(regr_men$G_ind, start=2010, frequency = 12), nfrequency = 4, FUN = mean)
G45_V_ind=aggregate(ts(regr_men$G45_V_ind, start=2010, frequency = 12), nfrequency = 4, FUN = mean)
G47_MP_ind=aggregate(ts(regr_men$G47_MP_ind, start=2010, frequency = 12), nfrequency = 4, FUN = mean)
I56_Mait_ind=aggregate(ts(regr_men$I56_Mait_ind, start=2010, frequency = 12), nfrequency = 4, FUN = mean)

NB_turt=aggregate(ts(regr_men$NB_turt, start=2010, frequency = 12), nfrequency = 4, FUN = mean)
FI_turt=aggregate(ts(regr_men$FI_turt, start=2010, frequency = 12), nfrequency = 4, FUN = mean)
NB_sand=aggregate(ts(regr_men$NB_turt, start=2010, frequency = 12), nfrequency = 4, FUN = sum)
FI_sand=aggregate(ts(regr_men$FI_turt, start=2010, frequency = 12), nfrequency = 4, FUN = sum)


##su lagu

DU_Total_l1=window(lag(DU_Total,-1),start(BVP_w),end(BVP_w))
DU_A_l1=window(lag(DU_A,-1),start(BVP_w),end(BVP_w))
DU_A_l2=window(lag(DU_A,-2),start(BVP_w),end(BVP_w))
DU_A_l4=window(lag(DU_A,-4),start(BVP_w),end(BVP_w))
DU_B_l1=window(lag(DU_B,-1),start(BVP_w),end(BVP_w))
DU_C_l1=window(lag(DU_C,-1),start(BVP_w),end(BVP_w))
DU_D_l1=window(lag(DU_D,-1),start(BVP_w),end(BVP_w))
DU_E_l1=window(lag(DU_E,-1),start(BVP_w),end(BVP_w))
DU_F_l1=window(lag(DU_F,-1),start(BVP_w),end(BVP_w))
DU_G_l1=window(lag(DU_G,-1),start(BVP_w),end(BVP_w))
DU_H_l1=window(lag(DU_H,-1),start(BVP_w),end(BVP_w))
DU_I_l1=window(lag(DU_I,-1),start(BVP_w),end(BVP_w))
DU_J_l1=window(lag(DU_J,-1),start(BVP_w),end(BVP_w))
DU_K_l1=window(lag(DU_K,-1),start(BVP_w),end(BVP_w))
DU_L_l1=window(lag(DU_L,-1),start(BVP_w),end(BVP_w))
DU_M_l1=window(lag(DU_M,-1),start(BVP_w),end(BVP_w))
DU_N_l1=window(lag(DU_N,-1),start(BVP_w),end(BVP_w))
DU_O_l1=window(lag(DU_O,-1),start(BVP_w),end(BVP_w))
DU_P_l1=window(lag(DU_P,-1),start(BVP_w),end(BVP_w))
DU_Q_l1=window(lag(DU_Q,-1),start(BVP_w),end(BVP_w))
DU_S_l1=window(lag(DU_S,-1),start(BVP_w),end(BVP_w))


DS_Total_l1=window(lag(DS_Total,-1),start(BVP_w),end(BVP_w))
DS_A_l1=window(lag(DS_A,-1),start(BVP_w),end(BVP_w))
DS_A_l2=window(lag(DS_A,-2),start(BVP_w),end(BVP_w))
DS_A_l4=window(lag(DS_A,-4),start(BVP_w),end(BVP_w))
DS_B_l1=window(lag(DS_B,-1),start(BVP_w),end(BVP_w))
DS_C_l1=window(lag(DS_C,-1),start(BVP_w),end(BVP_w))
DS_D_l1=window(lag(DS_D,-1),start(BVP_w),end(BVP_w))
DS_E_l1=window(lag(DS_E,-1),start(BVP_w),end(BVP_w))
DS_F_l1=window(lag(DS_F,-1),start(BVP_w),end(BVP_w))
DS_G_l1=window(lag(DS_G,-1),start(BVP_w),end(BVP_w))
DS_H_l1=window(lag(DS_H,-1),start(BVP_w),end(BVP_w))
DS_I_l1=window(lag(DS_I,-1),start(BVP_w),end(BVP_w))
DS_G_I_l1=window(lag(DS_G_I,-1),start(BVP_w),end(BVP_w))
DS_J_l1=window(lag(DS_J,-1),start(BVP_w),end(BVP_w))
DS_K_l1=window(lag(DS_K,-1),start(BVP_w),end(BVP_w))
DS_L_l1=window(lag(DS_L,-1),start(BVP_w),end(BVP_w))
DS_M_l1=window(lag(DS_M,-1),start(BVP_w),end(BVP_w))
DS_N_l1=window(lag(DS_N,-1),start(BVP_w),end(BVP_w))
DS_M_N_l1=window(lag(DS_M_N,-1),start(BVP_w),end(BVP_w))
DS_O_l1=window(lag(DS_O,-1),start(BVP_w),end(BVP_w))
DS_P_l1=window(lag(DS_P,-1),start(BVP_w),end(BVP_w))
DS_Q_l1=window(lag(DS_Q,-1),start(BVP_w),end(BVP_w))
DS_O_Q_l1=window(lag(DS_O_Q,-1),start(BVP_w),end(BVP_w))
DS_R_l1=window(lag(DS_R,-1),start(BVP_w),end(BVP_w))
DS_S_l1=window(lag(DS_S,-1),start(BVP_w),end(BVP_w))
DS_R_S_l1=window(lag(DS_R_S,-1),start(BVP_w),end(BVP_w))

SD_l1=window(lag(SD,-1),start(BVP_w),end(BVP_w))
SD_l2=window(lag(SD,-2),start(BVP_w),end(BVP_w))
SD_l3=window(lag(SD,-3),start(BVP_w),end(BVP_w))
SD_l4=window(lag(SD,-4),start(BVP_w),end(BVP_w))
SL_NP_l1=window(lag(SL_NP,-1),start(BVP_w),end(BVP_w))
SL_NP_l2=window(lag(SL_NP,-2),start(BVP_w),end(BVP_w))
SL_NP_l3=window(lag(SL_NP,-3),start(BVP_w),end(BVP_w))
SL_NP_l4=window(lag(SL_NP,-4),start(BVP_w),end(BVP_w))
SL_GP_l1=window(lag(SL_GP,-1),start(BVP_w),end(BVP_w))
SL_GP_l2=window(lag(SL_GP,-2),start(BVP_w),end(BVP_w))
SL_GP_l3=window(lag(SL_GP,-3),start(BVP_w),end(BVP_w))
SL_GP_l4=window(lag(SL_GP,-4),start(BVP_w),end(BVP_w))
SL_l1=window(lag(SL,-1),start(BVP_w),end(BVP_w))
SL_l2=window(lag(SL,-2),start(BVP_w),end(BVP_w))
SL_l4=window(lag(SL,-4),start(BVP_w),end(BVP_w))

G_ap_l1=window(lag(G_ap,-1),start(BVP_w),end(BVP_w))
G_ap_l2=window(lag(G_ap,-2),start(BVP_w),end(BVP_w))
G46_DP_ap_l1=window(lag(G46_DP_ap,-1),start(BVP_w),end(BVP_w))
G46_DP_ap_l2=window(lag(G46_DP_ap,-2),start(BVP_w),end(BVP_w))
G46_DP_ind_l1=window(lag(G46_DP_ind,-1),start(BVP_w),end(BVP_w))
G46_DP_ind_l2=window(lag(G46_DP_ind,-2),start(BVP_w),end(BVP_w))

Kel_ap_l1=window(lag(Kel_ap,-1),start(BVP_w),end(BVP_w))
Kro_ap_l1=window(lag(Kro_ap,-1),start(BVP_w),end(BVP_w))

H_PP_l1=window(lag(H_PP,-1),start(BVP_w),end(BVP_w))
I55_PP_l1=window(lag(I55_PP,-1),start(BVP_w),end(BVP_w))
J_PP_l1=window(lag(J_PP,-1),start(BVP_w),end(BVP_w))
L68_PP_l1=window(lag(L68_PP,-1),start(BVP_w),end(BVP_w))
M_PP_l1=window(lag(M_PP,-1),start(BVP_w),end(BVP_w))
N_PP_l1=window(lag(N_PP,-1),start(BVP_w),end(BVP_w))
M_N_PP_l1=window(lag(M_N_PP,-1),start(BVP_w),end(BVP_w))
P_PP_l1=window(lag(P_PP,-1),start(BVP_w),end(BVP_w))
Q_PP_l1=window(lag(Q_PP,-1),start(BVP_w),end(BVP_w))
O_Q_PP_l1=window(lag(O_Q_PP,-1),start(BVP_w),end(BVP_w))
R_PP_l1=window(lag(R_PP,-1),start(BVP_w),end(BVP_w))
S_PP_l1=window(lag(S_PP,-1),start(BVP_w),end(BVP_w))
R_S_PP_l1=window(lag(R_S_PP,-1),start(BVP_w),end(BVP_w))

NB_turt_l1=window(lag(NB_turt,-1),start(BVP_w),end(BVP_w))
FI_turt_l1=window(lag(FI_turt,-1),start(BVP_w),end(BVP_w))
NB_sand_l1=window(lag(NB_sand,-1),start(BVP_w),end(BVP_w))
FI_sand_l1=window(lag(FI_sand,-1),start(BVP_w),end(BVP_w))

Gal_v_isl_l1=window(lag(Gal_v_isl,-1),start(BVP_w),end(BVP_w))
Gal_v_isl_l4=window(lag(Gal_v_isl,-4),start(BVP_w),end(BVP_w))
VS_isl_l1=window(lag(VS_isl,-1),start(BVP_w),end(BVP_w))
VS_isl_l4=window(lag(VS_isl,-4),start(BVP_w),end(BVP_w))
VS_paj_l1=window(lag(VS_paj,-1),start(BVP_w),end(BVP_w))
VS_paj_l4=window(lag(VS_paj,-4),start(BVP_w),end(BVP_w))
Skola_l1=window(lag(Skola,-1),start(BVP_w),end(BVP_w))

ZUP_l1=window(lag(ZUP,-1),start(BVP_w),end(BVP_w))
ZUP_l2=window(lag(ZUP,-2),start(BVP_w),end(BVP_w))
ZUP_l4=window(lag(ZUP,-4),start(BVP_w),end(BVP_w))


## regresorių ilgio suvienodinimas
S1_w=window(S1,start(BVP_w),end(BVP_w))
S2_w=window(S2,start(BVP_w),end(BVP_w))
S3_w=window(S3,start(BVP_w),end(BVP_w))
S4_w=window(S4,start(BVP_w),end(BVP_w))
PVMviso_w=window(PVMviso,start(BVP_w),end(BVP_w))
PVM_A_w=window(PVM_A,start(BVP_w),end(BVP_w))
PVM_B_E_w=window(PVM_B_E,start(BVP_w),end(BVP_w))
PVM_B_w=window(PVM_B,start(BVP_w),end(BVP_w))
PVM_C_w=window(PVM_C,start(BVP_w),end(BVP_w))
PVM_D_w=window(PVM_D,start(BVP_w),end(BVP_w))
PVM_E_w=window(PVM_E,start(BVP_w),end(BVP_w))
PVM_F_w=window(PVM_F,start(BVP_w),end(BVP_w))
PVM_G_I_w=window(PVM_G_I,start(BVP_w),end(BVP_w))
PVM_J_w=window(PVM_J,start(BVP_w),end(BVP_w))
PVM_K_w=window(PVM_K,start(BVP_w),end(BVP_w))
PVM_L_w=window(PVM_L,start(BVP_w),end(BVP_w))
PVM_M_N_w=window(PVM_M_N,start(BVP_w),end(BVP_w))
PVM_O_Q_w=window(PVM_O_Q,start(BVP_w),end(BVP_w))
PVM_R_T_w=window(PVM_R_T,start(BVP_w),end(BVP_w))



PP_B_E_VK_w=window(PP_B_E_VK,start(BVP_w),end(BVP_w))
PP_B_VK_w=window(PP_B_VK,start(BVP_w),end(BVP_w))
PP_C_VK_w=window(PP_C_VK,start(BVP_w),end(BVP_w))
PP_D_VK_w=window(PP_D_VK,start(BVP_w),end(BVP_w))
PP_E_VK_w=window(PP_E_VK,start(BVP_w),end(BVP_w))
PP_B_E_PK_w=window(PP_B_E_PK,start(BVP_w),end(BVP_w))
PP_B_PK_w=window(PP_B_PK,start(BVP_w),end(BVP_w))
PP_C_PK_w=window(PP_C_PK,start(BVP_w),end(BVP_w))
PP_D_PK_w=window(PP_D_PK,start(BVP_w),end(BVP_w))
PP_E_PK_w=window(PP_E_PK,start(BVP_w),end(BVP_w))

SD_w=window(SD,start(BVP_w),end(BVP_w))
SL_NP_w=window(SL_NP,start(BVP_w),end(BVP_w))
SL_GP_w=window(SL_GP,start(BVP_w),end(BVP_w))
SL_w=window(SL,start(BVP_w),end(BVP_w))

G46_DP_ap_w=window(G46_DP_ap,start(BVP_w),end(BVP_w))
G_ap_w=window(G_ap,start(BVP_w),end(BVP_w))
G45_V_ap_w=window(G45_V_ap,start(BVP_w),end(BVP_w))
G47_MP_ap_w=window(G47_MP_ap,start(BVP_w),end(BVP_w))
I56_Mait_ap_w=window(I56_Mait_ap,start(BVP_w),end(BVP_w))
G46_DP_ind_w=window(G46_DP_ind,start(BVP_w),end(BVP_w))
G_ind_w=window(G_ind,start(BVP_w),end(BVP_w))
G45_V_ind_w=window(G45_V_ind,start(BVP_w),end(BVP_w))
G47_MP_ind_w=window(G47_MP_ind,start(BVP_w),end(BVP_w))
I56_Mait_ind_w=window(I56_Mait_ind,start(BVP_w),end(BVP_w))

ZUP_w=window(ZUP,start(BVP_w),end(BVP_w))

#### 1.A – Žemės ūkis, miškininkystė ir žuvininkystė
mod_A=lm(A_w~ZUP_w+DS_A_l2+S2_w)
summary(mod_A)
e_A=resid(mod_A)
shapiro.test(e_A)
MAPE_A=mean(abs(e_A)/A_w*100)
MAPE_A
vif(mod_A)
dwtest(mod_A)


## prognozė
Af=predict(mod_A, list(ZUP_w=c(window(ZUP, start=c(2011,1), end=c(now))), 
                                   DS_A_l2=c(window(lag(DS_A,-2), start=c(2011,1),end=c(now))),
                                   S2_w=c(window(S2, start=c(2011,1),end=c(now)))), interval = "confidence")
Af=ts(Af[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(A_w, lwd=2)
lines(Af, col="blue")


#### 2. 'B+C+D+E – Kasyba ir karjerų eksploatavimas; apdirbamoji gamyba; elektros, dujų, garo tiekimas ir oro kondicionavimas; vandens tiekimas; nuotekų valymas, atliekų tvarkymas ir regeneravimas


mod_B_E=lm(B_E_w~PVM_B_w+PVM_E_w+S2_w+PP_C_VK_w+DU_Total_l1)
summary(mod_B_E)
e_B_E=resid(mod_B_E)
shapiro.test(e_B_E)
MAPE_B_E=mean(abs(e_B_E)/B_E_w*100)
MAPE_B_E
vif(mod_B_E)
dwtest(mod_B_E)

## prognozė
B_Ef=predict(mod_B_E, 
                  list(PVM_B_w=c(window(PVM_B, start=c(2011,1), end=c(now))),
                       PVM_E_w=c(window(PVM_E, start=c(2011,1), end=c(now))),
                       S2_w=c(window(S2, start=c(2011,1), end=c(now))),
                       PP_C_VK_w=c(window(PP_C_VK, start=c(2011,1), end=c(now))), 
                       DU_Total_l1=c(window(lag(DU_Total,-1), start=c(2011,1), end=c(now)))), interval = "confidence")
B_Ef=ts(B_Ef[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(B_E_w, lwd=2)
lines(B_Ef, col="blue")


#### 3. C – Apdirbamoji gamyba


mod_C=lm(C_w~PVM_C_w+S2_w+PP_C_VK_w+DU_C_l1+DS_C_l1)
summary(mod_C)
e_C=resid(mod_C)
shapiro.test(e_C)
MAPE_C=mean(abs(e_C)/C_w*100)
MAPE_C
vif(mod_C)
dwtest(mod_C)

## prognozė
Cf=predict(mod_C, 
             list(PVM_C_w=c(window(PVM_C, start=c(2011,1), end=c(now))),
                  S2_w=c(window(S2, start=c(2011,1), end=c(now))),
                  PP_C_VK_w=c(window(PP_C_VK, start=c(2011,1), end=c(now))), 
                  DU_C_l1=c(window(lag(DU_C,-1), start=c(2011,1), end=c(now))),
                  DS_C_l1=c(window(lag(DS_C,-1), start=c(2011,1), end=c(now)))), interval = "confidence")
Cf=ts(Cf[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(C_w, lwd=2)
lines(Cf, col="blue")


#### 3. Statyba

mod_F=lm(F_w~PVM_F_w+SD_l1+DS_F_l1+S2_w+S3_w)
summary(mod_F)
e_F=resid(mod_F)
shapiro.test(e_F)
MAPE_F=mean(abs(e_F)/F_w*100)
MAPE_F
vif(mod_F)
dwtest(mod_F)


## prognozė
Ff=predict(mod_F, 
           list(PVM_F_w=c(window(PVM_F, start=c(2011,1), end=c(now))),
                SD_l1=c(window(lag(SD,-1), start=c(2011,1), end=c(now))),
                DS_F_l1=c(window(lag(DS_F,-1), start=c(2011,1), end=c(now))),
                S2_w=c(window(S2, start=c(2011,1), end=c(now))),
                S3_w=c(window(S3, start=c(2011,1), end=c(now)))), interval = "confidence")
Ff=ts(Ff[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(F_w, lwd=2)
lines(Ff, col="blue")

#### 4. G+H+I – Didmeninė ir mažmeninė prekyba; variklinių transporto priemonių ir motociklų remontas; transportas ir saugojimas; apgyvendinimo ir maitinimo paslaugų veikla

mod_G_I=lm(G_I_w~G47_MP_ap_w+Kel_ap_l1+S1_w+S3_w)
summary(mod_G_I)
e_G_I=resid(mod_G_I)
shapiro.test(e_G_I)
MAPE_G_I=mean(abs(e_G_I)/G_I_w*100)
MAPE_G_I
vif(mod_G_I)
dwtest(mod_G_I)


## prognozė
G_If=predict(mod_G_I, 
           list(G47_MP_ap_w=c(window(G47_MP_ap, start=c(2011,1), end=c(now))),
                Kel_ap_l1=c(window(lag(Kel_ap,-1), start=c(2011,1), end=c(now))),
                S1_w=c(window(S1, start=c(2011,1), end=c(now))),
                S3_w=c(window(S3, start=c(2011,1), end=c(now)))), interval = "confidence")
G_If=ts(G_If[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(G_I_w, lwd=2)
lines(G_If, col="blue")


#### 5. J – Informacija ir ryšiai

mod_J=lm(J_w~PVM_J_w+J_l1+DS_J_l1+S2_w+S3_w)
summary(mod_J)
e_J=resid(mod_J)
shapiro.test(e_J)
MAPE_J=mean(abs(e_J)/J_w*100)
MAPE_J
vif(mod_J)
dwtest(mod_J)

## prognozė
Jf=predict(mod_J, 
             list(PVM_J_w=c(window(PVM_J, start=c(2011,1), end=c(now))),
                  J_l1=c(window(lag(J,-1), start=c(2011,1), end=c(now))),
                  DS_J_l1=c(window(lag(DS_J,-1), start=c(2011,1), end=c(now))),
                  S2_w=c(window(S2, start=c(2011,1), end=c(now))),
                  S3_w=c(window(S3, start=c(2011,1), end=c(now)))), interval = "confidence")
Jf=ts(Jf[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(J_w, lwd=2)
lines(Jf, col="blue")


#### 6. K – Finansinė ir draudimo veikla

mod_K=lm(K_w~PVM_K_w+K_l1+DU_K_l1+S1_w+S2_w)
summary(mod_K)
e_K=resid(mod_K)
shapiro.test(e_K)
MAPE_K=mean(abs(e_K)/K_w*100)
MAPE_K
vif(mod_K)
dwtest(mod_K)

## prognozė
Kf=predict(mod_K, 
           list(PVM_K_w=c(window(PVM_K, start=c(2011,1), end=c(now))),
                K_l1=c(window(lag(K,-1), start=c(2011,1), end=c(now))),
                DU_K_l1=c(window(lag(DU_K,-1), start=c(2011,1), end=c(now))),
                S1_w=c(window(S1, start=c(2011,1), end=c(now))),
                S2_w=c(window(S2, start=c(2011,1), end=c(now)))), interval = "confidence")
Kf=ts(Kf[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(K_w, lwd=2)
lines(Kf, col="blue")


#### 7. L – Nekilnojamo turto operacijos

mod_L=lm(L_w~PVM_L_w+L_l1+SD_l3+SD_l4+SL_GP_l2)
summary(mod_L)
e_L=resid(mod_L)
shapiro.test(e_L)
MAPE_L=mean(abs(e_L)/L_w*100)
MAPE_L
vif(mod_L)
dwtest(mod_L)

## prognozė
Lf=predict(mod_L, 
           list(PVM_L_w=c(window(PVM_L, start=c(2011,1), end=c(now))),
                L_l1=c(window(lag(L,-1), start=c(2011,1), end=c(now))),
                SD_l3=c(window(lag(SD,-3), start=c(2011,1), end=c(now))),
                SD_l4=c(window(lag(SD,-4), start=c(2011,1), end=c(now))),
                SL_GP_l2=c(window(lag(SL_GP,-2), start=c(2011,1), end=c(now)))), interval = "confidence")
Lf=ts(Lf[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(L_w, lwd=2)
lines(Lf, col="blue")


#### 8. M+N – Profesinė, mokslinė ir techninė veikla; administracinė ir aptarnavimo veikla


mod_M_N=lm(M_N_w~PVM_M_N_w+M_N_PP_l1+DS_M_N_l1+S1_w)
summary(mod_M_N)
e_M_N=resid(mod_M_N)
shapiro.test(e_M_N)
MAPE_M_N=mean(abs(e_M_N)/M_N_w*100)
MAPE_M_N
vif(mod_M_N)
dwtest(mod_M_N)

## prognozė
M_Nf=predict(mod_M_N, 
           list(PVM_M_N_w=c(window(PVM_M_N, start=c(2011,1), end=c(now))),
                M_N_PP_l1=c(window(lag(M_N_PP,-1), start=c(2011,1), end=c(now))),
                DS_M_N_l1=c(window(lag(DS_M_N,-1), start=c(2011,1), end=c(now))),
                S1_w=c(window(S1, start=c(2011,1), end=c(now)))), interval = "confidence")
M_Nf=ts(M_Nf[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(M_N_w, lwd=2)
lines(M_Nf, col="blue")


#### 9. O+P+Q – Viešasis valdymas ir gynyba; privalomasis socialinis draudimas; švietimas; žmonių sveikatos priežiūra ir socialinis darbas

mod_O_Q=lm(O_Q_w~PVM_O_Q_w+O_Q_PP_l1+S2_w+S3_w)
summary(mod_O_Q)
e_O_Q=resid(mod_O_Q)
shapiro.test(e_O_Q)
MAPE_O_Q=mean(abs(e_O_Q)/O_Q_w*100)
MAPE_O_Q
vif(mod_O_Q)
dwtest(mod_O_Q)

## prognozė
O_Qf=predict(mod_O_Q, 
             list(PVM_O_Q_w=c(window(PVM_O_Q, start=c(2011,1), end=c(now))),
                  O_Q_PP_l1=c(window(lag(O_Q_PP,-1), start=c(2011,1), end=c(now))),
                  S2_w=c(window(S2, start=c(2011,1), end=c(now))),
                  S3_w=c(window(S3, start=c(2011,1), end=c(now)))), interval = "confidence")
O_Qf=ts(O_Qf[,1], start=2011, frequency=4)


##grafikas
graphics.off()
plot(O_Q_w, lwd=2)
lines(O_Qf, col="blue")

#### 10.R+S+T – Meninė, pramoginė ir poilsio organizavimo veikla, namų ūkio reikmenų remontas ir kitos paslaugos


mod_R_T=lm(R_T_w~PVM_R_T_w+R_S_PP_l1+S3_w+S1_w)
summary(mod_R_T)
e_R_T=resid(mod_R_T)
shapiro.test(e_R_T)
MAPE_R_T=mean(abs(e_R_T)/R_T_w*100)
MAPE_R_T
vif(mod_R_T)
dwtest(mod_R_T)

## prognozė
R_Tf=predict(mod_R_T, 
             list(PVM_R_T_w=c(window(PVM_R_T, start=c(2011,1), end=c(now))),
                  R_S_PP_l1=c(window(lag(R_S_PP,-1), start=c(2011,1), end=c(now))),
                  S1_w=c(window(S1, start=c(2011,1), end=c(now))),
                  S3_w=c(window(S3, start=c(2011,1), end=c(now)))), interval = "confidence")
R_Tf=ts(R_Tf[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(R_T_w, lwd=2)
lines(R_Tf, col="blue")


## 11. D21 – Mokesčiai produktams

mod_D21=lm(D21_w~PVMviso_w+DS_Total_l1+S2_w)
summary(mod_D21)
e_D21=resid(mod_D21)
shapiro.test(e_D21)
MAPE_D21=mean(abs(e_D21)/D21_w*100)
MAPE_D21
vif(mod_D21)
dwtest(mod_D21)

## prognozė
D21f=predict(mod_D21, 
             list(PVMviso_w=c(window(PVMviso, start=c(2011,1), end=c(now))),
                  DS_Total_l1=c(window(lag(DS_Total,-1), start=c(2011,1), end=c(now))),
                  S2_w=c(window(S2, start=c(2011,1), end=c(now)))), interval = "confidence")
D21f=ts(D21f[,1], start=2011, frequency=4)

##grafikas
graphics.off()
plot(D21_w, lwd=2)
lines(D21f, col="blue")



## 12. D31– Subsidijos produktams


auto.arima(D31_w)
mod_D31_ARIMA= arima(D31_w, order=c(0,0,1), seasonal=c(0,1,0))
summary(mod_D31_ARIMA)
coeftest(mod_D31_ARIMA)
tsdiag(mod_D31_ARIMA)
D31_ARIMA=fitted(mod_D31_ARIMA)
e_D31_ARIMA=resid(mod_D31_ARIMA)
D31f= forecast(mod_D31_ARIMA, h = 1)
D31_lef=ts(D31f$mean, start=c(now),frequency = 4)
D31lef=ts(c(D31_ARIMA,D31_lef), start(D31_ARIMA), frequency = 4)



##grafikas
graphics.off()
plot(D31_w, lwd=2)
lines(D31lef, col="blue")




########Suvestinė

G_Tf=G_If+Jf+Kf+Lf+M_Nf+O_Qf+R_Tf
A_Tf= Af+B_Ef+Ff+G_Tf
D21_D31f=D21f-D31lef
BVPf=A_Tf+D21_D31f
MAPE_BVP=mean(abs(BVP_w-BVPf)/BVP_w*100)
MAPE_BVP
  
BVP_VK_30=data.frame(Af, B_Ef, Cf, Ff, G_Tf, G_If, Jf, Kf, Lf, M_Nf, O_Qf, R_Tf,A_Tf, D21f, D31lef, D21_D31f, BVPf, 
              stringsAsFactors = FALSE)
names(BVP_VK_30)=c("A", "B+C+D+E","C","F","G+...+T", "G+H+I", "J", "K","L","M+N", "O+P+Q", "R+S+T","A+...+T",
            "D21", "D31", "D21-D31","BVP")
plot(BVP_w,lwd=2)
lines(BVPf,col="red")

write.xlsx2(BVP_VK_30, file="BVP_prognoze_GVK_30.xlsx", sheetName = "Gamybos_VK_30",
            col.names = TRUE, row.names = FALSE, append = FALSE)


