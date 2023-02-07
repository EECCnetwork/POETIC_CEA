######ISARIC DATASET######
#Author - Hiral A Shah
#Date - 02/08/2021
##########################
library(RColorBrewer)
library(matrixStats)
library(data.table)
library(ggplot2)
library(reshape2)
library(data.table) 
library(dplyr)
library(usdm)
library(tidyverse)
library(MultinomialCI)
library(msm)
library(DescTools)
library(markovchain)
library(EnvStats)

#starting parameters

set.seed(123)

nyear <- 1000

isaric_critical_costs <- c(rep(450,nyear),rep(500,nyear),rep(550,nyear),rep(600,nyear),
                          rep(650,nyear),rep(700,nyear),rep(750,nyear),rep(800,nyear),
                          rep(850,nyear),rep(900,nyear),rep(950,nyear),rep(1000,nyear),
                         rep(1050,nyear),rep(1100,nyear),rep(1150,nyear),rep(1200,nyear),
                         rep(1250,nyear),rep(1300,nyear),
                         rep(1350,nyear),rep(1400,nyear))

sens_n <- length(isaric_critical_costs)


triage_severe <- rtri(n = sens_n, mod = 0.8563, min = 0.7563, max = 0.9563)
triage_critical <- rtri(n = sens_n, mod = 0.1436, min = 0.0436, max = 0.2436)
pop <- 100000
timehorizon <- 28

YLD_severe <- rtri(n = sens_n, mod = 0.133, min = 0.088, max = 0.190)
YLD_critical <- rtri(n = sens_n, mod = 0.655, min = 0.579, max = 0.727) 

#Demographic parameters

ending_popn_age <- 70
age_strt_mort1_18to45 <- rtri(n = sens_n, mod = 0.204, min = 0.104, max = 0.304)
age_strt_mort2_46to56 <- rtri(n = sens_n, mod = 0.214, min = 0.114, max = 0.314)
age_strt_mort3_57to67 <- rtri(n = sens_n, mod = 0.276, min = 0.176, max = 0.376)
age_strt_mort4_68to100 <- rtri(n = sens_n, mod = 0.305, min = 0.205, max = 0.405)
age_threshold1 <- 18
age_threshold2 <- 46
age_threshold3 <- 57
age_threshold4 <- 68


#Clinical Effectiveness - triangular distributions

eff_eecc_severe_to_critical <- rtri(n = sens_n, mod = 0.19, min = 0, max = 1)
eff_eecc_critical_to_severe <- rtri(n = sens_n, mod = 0.25, min = 0, max = 1)
eff_aos_critical_to_death <- rtri(n = sens_n, mod = 0.17, min = 0, max = 1)

#ISARIC transition parameters


isaric_critical_to_death <-  rtri(n = sens_n, mod = 0.142857142857143, min = 0.0408163265306122, max = 0.273807473614838)
isaric_critical_to_discharge <-  rtri(n = sens_n, mod = 0.0816326530612245, min = 0, max = 0.21258298381892)
isaric_critical_to_severe <-  rtri(n = sens_n, mod = 0.06122449, min = 0, max = 0.192174821)
isaric_critical_to_critical <-  1 - (isaric_critical_to_death + isaric_critical_to_discharge + isaric_critical_to_severe)
isaric_severe_to_critical <- rtri(n = sens_n, mod = 0.415309446254072, min = 0.396742671009772, max = 0.433958001522844)
isaric_severe_to_death <-  rep(0,sens_n)
isaric_severe_to_discharge <-  rtri(n = sens_n, mod = 0.042671009771987, min = 0.0241042345276873, max = 0.0613195650407591)
isaric_severe_to_severe <- 1 - (isaric_severe_to_critical + isaric_severe_to_death + isaric_severe_to_discharge) 


#EECC transition parameters

eecc_critical_to_critical <- isaric_critical_to_critical 
eecc_critical_to_death <- isaric_critical_to_death 
eecc_critical_to_discharge <- isaric_critical_to_discharge 
eecc_critical_to_severe <- isaric_critical_to_severe * (1 + eff_eecc_critical_to_severe)
eecc_severe_to_critical <- isaric_severe_to_critical * (1 - eff_eecc_severe_to_critical)
eecc_severe_to_death <- isaric_severe_to_death 
eecc_severe_to_discharge <- isaric_severe_to_discharge
eecc_severe_to_severe <- isaric_severe_to_severe

#EECC+AOS transition parameters

eeccaos_critical_to_critical <- isaric_critical_to_critical
eeccaos_critical_to_death <- isaric_critical_to_death * (1 - eff_aos_critical_to_death)
eeccaos_critical_to_discharge <- isaric_critical_to_discharge
eeccaos_critical_to_severe <- isaric_critical_to_severe * (1 + eff_eecc_critical_to_severe)
eeccaos_severe_to_critical <- isaric_severe_to_critical * (1 - eff_eecc_severe_to_critical)
eeccaos_severe_to_death <- isaric_severe_to_death
eeccaos_severe_to_discharge <- isaric_severe_to_discharge
eeccaos_severe_to_severe <- isaric_severe_to_severe


#ISARIC cost parameters


isaric_severe_costs <- rtri(n = sens_n, mod = 202.03, min =184.05, max = 474.35)
isaric_death_costs <- rep(0,sens_n)
isaric_discharge_costs <- rep(0,sens_n)


#EECC cost parameters

eecc_severe_costs <- rtri(n = sens_n, mod = 15.17, min = 11.90, max = 18.25)
eecc_critical_costs <- rtri(n = sens_n, mod = 40.81, min =28.62, max = 52.00)
eecc_death_costs <- rep(0,sens_n)
eecc_discharge_costs <- rep(0,sens_n)

#EECC + AOS cost parameters

eeccaos_severe_costs <- rtri(n = sens_n, mod = 15.17, min = 11.90, max = 18.25)
eeccaos_critical_costs <- rtri(n = sens_n, mod = 315.32, min = 83.63, max = 779.71)
eeccaos_death_costs <- rep(0,sens_n)
eeccaos_discharge_costs <- rep(0,sens_n)


