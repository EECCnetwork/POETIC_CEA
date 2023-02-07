######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This is the parameters page for the "No Critical Care" base case scenario.
#Parameters can be changed below for each of the differing probabilities. 
#All parameters within the base case scenarios use a triangular distribution

#Load the below packages. Ensure they are installed before running any other script. 
library(RColorBrewer)
library(matrixStats)
library(data.table)
library(ggplot2)
library(dplyr)
library(MultinomialCI)
library(msm)
library(DescTools)
library(markovchain)
library(EnvStats)

#Please ensure that you load the "model functions" script using the code in the line below

source("Markov_Model/probabilistic_analysis/model_functions_probabilistic.R")

#Set the seed R's random number generator, 
#This is useful for creating simulations or random objects that can be reproduced.

set.seed(123)

#Determine the number of samples taken from each distribution
sens_n <- 10000

#Triage probabilities - parameters to specify the proportion of severe and critical patients that enter a hospital

triage_severe <- runif(n = sens_n, min = 0.7563, max = 0.9563)
triage_critical <- runif(n = sens_n, min = 0.0436, max = 0.2436)

#Cohort size - parameter to determine the cohort size
pop <- 10000

#Time horizon
timehorizon <- 28

#DALY weights for the years of life lived with disability for severe and critical COVID19

YLD_severe <- runif(n = sens_n, min = 0.088, max = 0.190)
YLD_critical <- runif(n = sens_n, min = 0.579, max = 0.727) 

#Life expectancy

ending_popn_age <- 70

#Proportion of the population in each age strata (18-45, 46-56, 57-67 and 68-100)
age_strt_mort1_18to45 <- runif(n = sens_n, min = 0.104, max = 0.304)
age_strt_mort2_46to56 <- runif(n = sens_n, min = 0.114, max = 0.314)
age_strt_mort3_57to67 <- runif(n = sens_n, min = 0.176, max = 0.376)
age_strt_mort4_68to100 <- runif(n = sens_n, min = 0.205, max = 0.405)

#Threshold age for each age strata
age_threshold1 <- 18
age_threshold2 <- 46
age_threshold3 <- 57
age_threshold4 <- 68

#Clinical Effectiveness probabilities

eff_eecc_severe_to_critical <- runif(n = sens_n, min = 0, max = 1)
eff_eecc_critical_to_severe <- runif(n = sens_n, min = 0, max = 1)
eff_aos_critical_to_death <- runif(n = sens_n, min = 0, max = 1)

#No Critical Care transition probabilities


nothing_critical_to_death <- runif(n = sens_n, min = 0.5, max = 0.8)
nothing_critical_to_discharge <- rep(0,sens_n)
nothing_critical_to_severe <- runif(n = sens_n, min = 0.00, max = 0.15)
nothing_critical_to_critical <- 1-(nothing_critical_to_death + nothing_critical_to_discharge + nothing_critical_to_severe)
nothing_severe_to_critical <- runif(n = sens_n, min = 0.10, max = 0.70)
nothing_severe_to_death <- rep(0,sens_n)
nothing_severe_to_discharge <- runif(n = sens_n, min = 0.10, max = 0.25)
nothing_severe_to_severe <- 1-(nothing_severe_to_death + nothing_severe_to_discharge + nothing_severe_to_critical)

#Impact of EECC on No Critical Care which takes into account EECC effectiveness

eecc_critical_to_critical <- nothing_critical_to_critical 
eecc_critical_to_death <- nothing_critical_to_death 
eecc_critical_to_discharge <- nothing_critical_to_discharge 
eecc_critical_to_severe <- nothing_critical_to_severe * (1 + eff_eecc_critical_to_severe)
eecc_severe_to_critical <- nothing_severe_to_critical * (1 - eff_eecc_severe_to_critical)
eecc_severe_to_death <- nothing_severe_to_death 
eecc_severe_to_discharge <- nothing_severe_to_discharge 
eecc_severe_to_severe <- nothing_severe_to_severe

#Impact of ACC on No Critical Care which takes into account ACC effectiveness

eeccaos_critical_to_critical <- nothing_critical_to_critical
eeccaos_critical_to_death <- nothing_critical_to_death * (1 - eff_aos_critical_to_death)
eeccaos_critical_to_discharge <- nothing_critical_to_discharge
eeccaos_critical_to_severe <- nothing_critical_to_severe * (1 + eff_eecc_critical_to_severe)
eeccaos_severe_to_critical <- nothing_severe_to_critical * (1 - eff_eecc_severe_to_critical)
eeccaos_severe_to_death <- nothing_severe_to_death 
eeccaos_severe_to_discharge <- nothing_severe_to_discharge 
eeccaos_severe_to_severe <- nothing_severe_to_severe

#Cost of No Critical Care in each state

nothing_severe_costs <- runif(n = sens_n, min =2.45, max = 37.68)
nothing_critical_costs <- runif(n = sens_n, min =2.45, max = 37.68)
nothing_death_costs <- rep(0,sens_n)
nothing_discharge_costs <- rep(0,sens_n)

#Cost of EECC in each state

eecc_severe_costs <- runif(n = sens_n, min =11.19, max = 59.8)
eecc_critical_costs <- runif(n = sens_n, min =30.37, max = 111.6)
eecc_death_costs <- rep(0,sens_n)
eecc_discharge_costs <- rep(0,sens_n)

#Cost of ACC in each state

eeccaos_severe_costs <- runif(n = sens_n, min =13.27, max = 63.33)
eeccaos_critical_costs <- runif(n = sens_n, min = 227.10, max = 409.80)
eeccaos_death_costs <- rep(0,sens_n)
eeccaos_discharge_costs <- rep(0,sens_n)
