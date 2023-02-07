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

#which parameter are you changing

nyear <- 1000
triage_severe <- c(rep(0.64,nyear),rep(0.65,nyear),rep(0.66,nyear),rep(0.67,nyear),
                   rep(0.68,nyear),rep(0.69,nyear),rep(0.70,nyear),rep(0.71,nyear),
                   rep(0.72,nyear),rep(0.73,nyear),rep(0.74,nyear),rep(0.75,nyear),
                   rep(0.76,nyear),rep(0.77,nyear),rep(0.78,nyear),rep(0.79,nyear),
                   rep(0.80,nyear))
triage_critical <- 1 - triage_severe

#starting parameters

set.seed(123)
sens_n <- length(triage_severe)

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

eff_eecc_severe_to_critical <- rtri(n = sens_n, mod = 0.375, min = 0, max = 1)
eff_eecc_critical_to_severe <- rtri(n = sens_n, mod = 0.5, min = 0, max = 1)
eff_aos_critical_to_death <- rtri(n = sens_n, mod = 0.34, min = 0, max = 1)

#No Critical Care transition parameters


nothing_critical_to_death <- rtri(n = sens_n, mod = 0.70, min = 0.5, max = 0.8)
nothing_critical_to_discharge <- rep(0,sens_n)
nothing_critical_to_severe <- rtri(n = sens_n, mod = 0.05, min = 0.00, max = 0.15)
nothing_critical_to_critical <- 1-(nothing_critical_to_death + nothing_critical_to_discharge + nothing_critical_to_severe)
nothing_severe_to_critical <- rtri(n = sens_n, mod = 0.40, min = 0.10, max = 0.70)
nothing_severe_to_death <- rep(0,sens_n)
nothing_severe_to_discharge <- rtri(n = sens_n, mod = 0.20, min = 0.10, max = 0.25)
nothing_severe_to_severe <- 1-(nothing_severe_to_death + nothing_severe_to_discharge + nothing_severe_to_critical)
#EECC transition parameters

eecc_critical_to_critical <- nothing_critical_to_critical 
eecc_critical_to_death <- nothing_critical_to_death 
eecc_critical_to_discharge <- nothing_critical_to_discharge 
eecc_critical_to_severe <- nothing_critical_to_severe * (1 + eff_eecc_critical_to_severe)
eecc_severe_to_critical <- nothing_severe_to_critical * (1 - eff_eecc_severe_to_critical)
eecc_severe_to_death <- nothing_severe_to_death 
eecc_severe_to_discharge <- nothing_severe_to_discharge 
eecc_severe_to_severe <- nothing_severe_to_severe

#EECC+AOS transition parameters

eeccaos_critical_to_critical <- nothing_critical_to_critical
eeccaos_critical_to_death <- nothing_critical_to_death * (1 - eff_aos_critical_to_death)
eeccaos_critical_to_discharge <- nothing_critical_to_discharge
eeccaos_critical_to_severe <- nothing_critical_to_severe * (1 + eff_eecc_critical_to_severe)
eeccaos_severe_to_critical <- nothing_severe_to_critical * (1 - eff_eecc_severe_to_critical)
eeccaos_severe_to_death <- nothing_severe_to_death 
eeccaos_severe_to_discharge <- nothing_severe_to_discharge 
eeccaos_severe_to_severe <- nothing_severe_to_severe

#Nothing cost parameters

nothing_severe_costs <- rtri(n = sens_n, mod = 25.57, min =2.45, max = 37.68)
nothing_critical_costs <- rtri(n = sens_n, mod = 25.57, min =2.45, max = 37.68)
nothing_death_costs <- rep(0,sens_n)
nothing_discharge_costs <- rep(0,sens_n)

#EECC cost parameters

eecc_severe_costs <- rtri(n = sens_n, mod = 36.4, min =11.19, max = 59.8)
eecc_critical_costs <- rtri(n = sens_n, mod = 58.41, min =30.37, max = 111.6)
eecc_death_costs <- rep(0,sens_n)
eecc_discharge_costs <- rep(0,sens_n)

#EECC + AOS cost parameters

eeccaos_severe_costs <- rtri(n = sens_n, mod = 38.68, min =13.27, max = 63.33)
eeccaos_critical_costs <- rtri(n = sens_n, mod = 322.87, min = 227.10, max = 409.80)
eeccaos_death_costs <- rep(0,sens_n)
eeccaos_discharge_costs <- rep(0,sens_n)
