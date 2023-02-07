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

age_strt_mort1_18to45 <- c(rep(0.10,nyear),rep(0.12,nyear),rep(0.14,nyear),
                           rep(0.16,nyear),rep(0.18,nyear),rep(0.20,nyear),rep(0.22,nyear),rep(0.24,nyear),
                           rep(0.26,nyear),rep(0.28,nyear),rep(0.30,nyear),rep(0.32,nyear))

sens_n <- length(age_strt_mort1_18to45)
triage_severe <- rtri(n = sens_n, mod = 0.8563, min = 0.7563, max = 0.9563)
triage_critical <- rtri(n = sens_n, mod = 0.1436, min = 0.0436, max = 0.2436)
pop <- 100000
timehorizon <- 28


YLD_severe <- rtri(n = sens_n, mod = 0.133, min = 0.088, max = 0.190)
YLD_critical <- rtri(n = sens_n, mod = 0.655, min = 0.579, max = 0.727) 


#Demographic parameters

ending_popn_age <- 70

age_strt_mort2_46to56 <- rtri(n = sens_n, mod = 0.214, min = 0.114, max = 0.314)
age_strt_mort3_57to67 <- rtri(n = sens_n, mod = 0.276, min = 0.176, max = 0.376)
age_strt_mort4_68to100 <- rtri(n = sens_n, mod = 0.305, min = 0.205, max = 0.405)
age_threshold1 <- 18
age_threshold2 <- 46
age_threshold3 <- 57
age_threshold4 <- 68

#Clinical Effectiveness - triangular distributions

eff_eecc_severe_to_critical <- rtri(n = sens_n, mod = 0.375, min = 0, max = 1)
eff_eecc_critical_to_severe <- rtri(n = sens_n, mod = 0.5, min = 0, max = 1)
eff_aos_critical_to_death <- rtri(n = sens_n, mod = 0.34, min = 0, max = 1)

#Tanzania District Critical Care transition parameters

tanzania_critical_to_death <- rtri(n = sens_n, mod = 0.53, min = 0.5, max = 0.7)
tanzania_critical_to_discharge <- rep(0,sens_n)
tanzania_critical_to_severe <- rtri(n = sens_n, mod = 0.07, min = 0.05, max = 0.10)
tanzania_critical_to_critical <- 1-(tanzania_critical_to_death + tanzania_critical_to_discharge + tanzania_critical_to_severe)
tanzania_severe_to_critical <- rtri(n = sens_n, mod = 0.30, min = 0.25, max = 0.40)
tanzania_severe_to_death <- rep(0,sens_n)
tanzania_severe_to_discharge <- rtri(n = sens_n, mod = 0.25, min = 0.12, max = 0.30)
tanzania_severe_to_severe <- 1-(tanzania_severe_to_death + tanzania_severe_to_discharge + tanzania_severe_to_critical)


#EECC transition parameters

eecc_critical_to_critical <- tanzania_critical_to_critical 
eecc_critical_to_death <- tanzania_critical_to_death 
eecc_critical_to_discharge <- tanzania_critical_to_discharge 
eecc_critical_to_severe <- tanzania_critical_to_severe * (1 + eff_eecc_critical_to_severe)
eecc_severe_to_critical <- tanzania_severe_to_critical * (1 - eff_eecc_severe_to_critical)
eecc_severe_to_death <- tanzania_severe_to_death 
eecc_severe_to_discharge <- tanzania_severe_to_discharge
eecc_severe_to_severe <- tanzania_severe_to_severe

#EECC+AOS transition parameters

eeccaos_critical_to_critical <- tanzania_critical_to_critical
eeccaos_critical_to_death <- tanzania_critical_to_death * (1 - eff_aos_critical_to_death)
eeccaos_critical_to_discharge <- tanzania_critical_to_discharge
eeccaos_critical_to_severe <- tanzania_critical_to_severe * (1 + eff_eecc_critical_to_severe)
eeccaos_severe_to_critical <- tanzania_severe_to_critical * (1 - eff_eecc_severe_to_critical)
eeccaos_severe_to_death <- tanzania_severe_to_death 
eeccaos_severe_to_discharge <- tanzania_severe_to_discharge 
eeccaos_severe_to_severe <- tanzania_severe_to_severe

#Tanzania cost parameters

tanzania_severe_costs <- rtri(n = sens_n, mod = 26.78, min =3.68, max = 56.65)
tanzania_critical_costs <- rtri(n = sens_n, mod = 26.78, min =3.68, max = 56.65)
tanzania_death_costs <- rep(0,sens_n)
tanzania_discharge_costs <- rep(0,sens_n)

#EECC cost parameters

eecc_severe_costs <- rtri(n = sens_n, mod = 28.65, min =3.94, max = 60.6)
eecc_critical_costs <- rtri(n = sens_n, mod = 50.66, min =6.96, max = 107.16)
eecc_death_costs <- rep(0,sens_n)
eecc_discharge_costs <- rep(0,sens_n)

#EECC + AOS cost parameters

eeccaos_severe_costs <- rtri(n = sens_n, mod = 30.93, min =4.25, max = 65.42)
eeccaos_critical_costs <- rtri(n = sens_n, mod = 315.12, min = 43.29, max = 666.66)
eeccaos_death_costs <- rep(0,sens_n)
eeccaos_discharge_costs <- rep(0,sens_n)





