######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This is the markov model simulations page for the "District Hospital Critical Care" base case scenario.

#load the parameters page for the "District Hospital Critical Care" Base Case
#The parameters page will also load the model functions script
#The parameters page will also load any libraries that need loading.
#Please ensure you have installed the correct packages as per the parameters script below.

source("Markov_Model/probabilistic_analysis/probabilistic_parameters_page_tanzaniabaseline.R")

#Below is the code to simulate the markov model for a District Hospital Critical Care Scenario
#The markov model will be run using the parameters specified in the parameters page above
#The markov model uses the markov model function from the model functions script

tanzania_sims <- as.data.frame(do.call(rbind, sapply(1:length(triage_critical), function(i){
  pop_n = pop
  time_horizon = timehorizon
  triage_sev = triage_severe[i]
  triage_crit = triage_critical[i]
  crit2crit = tanzania_critical_to_critical[i]
  crit2death = tanzania_critical_to_death[i]
  crit2discharge = tanzania_critical_to_discharge[i]
  crit2severe = tanzania_critical_to_severe[i]
  severe2crit = tanzania_severe_to_critical[i]
  severe2death = tanzania_severe_to_death[i]
  severe2discharge = tanzania_severe_to_discharge[i]
  severe2severe = tanzania_severe_to_severe[i]
  output <- markov_function(pop_n, time_horizon, triage_sev, triage_crit, 
                            crit2crit, crit2death,crit2discharge,crit2severe,
                            severe2crit, severe2death, severe2discharge, severe2severe)
  matrix(c(output), nrow = 1)}, simplify = FALSE)))

#Below is the code to calculate the number of DALYs lost in a District Hospital Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

tanzania_DALY <- DALY_calc(DEATHS = unlist(tanzania_sims[,3]),
                          age_mortprop1 = age_strt_mort1_18to45,
                          age_mortprop2 = age_strt_mort2_46to56,
                          age_mortprop3 = age_strt_mort3_57to67,
                          age_mortprop4 = age_strt_mort4_68to100,
                          age_entry1 = age_threshold1,
                          age_entry2 = age_threshold2,
                          age_entry3 = age_threshold3,
                          age_entry4 = age_threshold4,
                          life_expectancy = ending_popn_age,
                          pop_n = pop,
                          severepercent = triage_severe,
                          criticalpercent = triage_critical,
                          severeDALY = YLD_severe, 
                          criticalDALY = YLD_critical, 
                          time_horizon = timehorizon)

#Below is the code to calculate the cost of severe and critical COVID19 in a District Hospital Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

tanzania_costs <- costing_calc(outcome_dataset = tanzania_sims,
                               severe_cost = tanzania_severe_costs,
                               critical_cost = tanzania_critical_costs,
                               death_cost = tanzania_death_costs,
                               discharge_cost = tanzania_discharge_costs)

#Below is the code to simulate the impact of EECC in a District Hospital Critical Care Scenario
#The markov model will be run using the parameters specified in the parameters page above
#The markov model uses the markov model function from the model functions script


tz_eecc_sims <- as.data.frame(do.call(rbind, sapply(1:length(triage_critical), function(i){
  pop_n = pop
  time_horizon = timehorizon
  triage_sev = triage_severe[i]
  triage_crit = triage_critical[i]
  crit2crit = eecc_critical_to_critical[i]
  crit2death = eecc_critical_to_death[i]
  crit2discharge = eecc_critical_to_discharge[i]
  crit2severe = eecc_critical_to_severe[i]
  severe2crit = eecc_severe_to_critical[i]
  severe2death = eecc_severe_to_death[i]
  severe2discharge = eecc_severe_to_discharge[i]
  severe2severe = eecc_severe_to_severe[i]
  output <- markov_function(pop_n, time_horizon, triage_sev, triage_crit, 
                            crit2crit, crit2death,crit2discharge,crit2severe,
                            severe2crit, severe2death, severe2discharge, severe2severe)
  matrix(c(output), nrow = 1)}, simplify = FALSE)))

#Below is the code to quantify the impact of EECC on the number of DALYs lost in a District Hospital Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

tz_eecc_DALY <- DALY_calc(DEATHS = unlist(tz_eecc_sims[,(3)]),
                          age_mortprop1 = age_strt_mort1_18to45,
                          age_mortprop2 = age_strt_mort2_46to56,
                          age_mortprop3 = age_strt_mort3_57to67,
                          age_mortprop4 = age_strt_mort4_68to100,
                          age_entry1 = age_threshold1,
                          age_entry2 = age_threshold2,
                          age_entry3 = age_threshold3,
                          age_entry4 = age_threshold4,
                          life_expectancy = ending_popn_age,
                          pop_n = pop,
                          severepercent = triage_severe,
                          criticalpercent = triage_critical,
                          severeDALY = YLD_severe, 
                          criticalDALY = YLD_critical, 
                          time_horizon = timehorizon)


#Below is the code to quantify the impact of EECC on the cost of severe and critical COVID19 in a District Hospital Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

tz_eecc_costs <- costing_calc(outcome_dataset = tz_eecc_sims,
                              severe_cost = eecc_severe_costs,
                              critical_cost = eecc_critical_costs,
                              death_cost = eecc_death_costs,
                              discharge_cost = eecc_discharge_costs)


#Below is the code to simulate the impact of ACC in a District Hospital Critical Care Scenario
#The markov model will be run using the parameters specified in the parameters page above
#The markov model uses the markov model function from the model functions script

tz_eeccaos_sims <- as.data.frame(do.call(rbind, sapply(1:length(triage_critical), function(i){
  pop_n = pop
  time_horizon = timehorizon
  triage_sev = triage_severe[i]
  triage_crit = triage_critical[i]
  crit2crit = eeccaos_critical_to_critical[i]
  crit2death = eeccaos_critical_to_death[i]
  crit2discharge = eeccaos_critical_to_discharge[i]
  crit2severe = eeccaos_critical_to_severe[i]
  severe2crit = eeccaos_severe_to_critical[i]
  severe2death = eeccaos_severe_to_death[i]
  severe2discharge = eeccaos_severe_to_discharge[i]
  severe2severe = eeccaos_severe_to_severe[i]
  output <- markov_function(pop_n, time_horizon, triage_sev, triage_crit, 
                            crit2crit, crit2death,crit2discharge,crit2severe,
                            severe2crit, severe2death, severe2discharge, severe2severe)
  matrix(c(output), nrow = 1)}, simplify = FALSE)))

#Below is the code to quantify the impact of ACC on the number of DALYs lost in a District Hospital Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

tz_eeccaos_DALY <- DALY_calc(DEATHS = unlist(tz_eeccaos_sims[,(3)]),
                          age_mortprop1 = age_strt_mort1_18to45,
                          age_mortprop2 = age_strt_mort2_46to56,
                          age_mortprop3 = age_strt_mort3_57to67,
                          age_mortprop4 = age_strt_mort4_68to100,
                          age_entry1 = age_threshold1,
                          age_entry2 = age_threshold2,
                          age_entry3 = age_threshold3,
                          age_entry4 = age_threshold4,
                          life_expectancy = ending_popn_age,
                          pop_n = pop,
                          severepercent = triage_severe,
                          criticalpercent = triage_critical,
                          severeDALY = YLD_severe, 
                          criticalDALY = YLD_critical, 
                          time_horizon = timehorizon)

#Below is the code to quantify the impact of ACC on the cost of severe and critical COVID19 in a District Hospital Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

tz_eeccaos_costs <- costing_calc(outcome_dataset = tz_eeccaos_sims,
                              severe_cost = eeccaos_severe_costs,
                              critical_cost = eeccaos_critical_costs,
                              death_cost = eeccaos_death_costs,
                              discharge_cost = eeccaos_discharge_costs)

#The below code restructures DALY results for a District Hospital Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

tanzania_dat <- data.frame(tanzania_DALY)
tanzania_DALY_dat <- tanzania_dat$DALY
tanzania_outcome_results <- results_func(tanzania_DALY_dat)
tanzania_outcome_results_final <- data.frame(DALYMean = tanzania_outcome_results$mean_yes,
                                             DALYMin = tanzania_outcome_results$range_low,
                                             DALYMax = tanzania_outcome_results$range_high,
                                             DALYMedian = tanzania_outcome_results$median_yes)

#The below code restructures Cost results for a District Hospital Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

tanzania_costdat <- data.frame(tanzania_costs)
tanzania_totalcost_dat <- tanzania_costdat$totalcost
tanzania_cost_results <- results_func(tanzania_totalcost_dat)
tanzania_cost_results_final <- data.frame(CostMean = tanzania_cost_results$mean_yes,
                                          CostMin = tanzania_cost_results$range_low,
                                         CostMax = tanzania_cost_results$range_high,
                                         CostMedian = tanzania_cost_results$median_yes)

#The below code restructures DALY results for the impact of EECC in a District Hospital Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

tz_eecc_dat <- data.frame(tz_eecc_DALY)
tz_eecc_DALY_dat <- tz_eecc_dat$DALY
tz_eecc_outcome_results <- results_func(tz_eecc_DALY_dat)
tz_eecc_outcome_results_final <- data.frame(DALYMean = tz_eecc_outcome_results$mean_yes,
                                            DALYMin = tz_eecc_outcome_results$range_low,
                                            DALYMax = tz_eecc_outcome_results$range_high,
                                            DALYMedian = tz_eecc_outcome_results$median_yes)

#The below code restructures Cost results for the impact of EECC in a District Hospital Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

tz_eecc_costdat <- data.frame(tz_eecc_costs)
tz_eecc_totalcost_dat <- tz_eecc_costdat$totalcost
tz_eecc_cost_results <- results_func(tz_eecc_totalcost_dat)
tz_eecc_cost_results_final <- data.frame(CostMean = tz_eecc_cost_results$mean_yes,
                                         CostMin = tz_eecc_cost_results$range_low,
                                         CostMax = tz_eecc_cost_results$range_high,
                                         CostMedian = tz_eecc_cost_results$median_yes)

#The below code restructures DALY results for the impact of ACC in a District Hospital Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

tz_eeccaos_dat <- data.frame(tz_eeccaos_DALY)
tz_eeccaos_DALY_dat <- tz_eeccaos_dat$DALY
tz_eeccaos_outcome_results <- results_func(tz_eeccaos_DALY_dat)
tz_eeccaos_outcome_results_final <- data.frame(DALYMean = tz_eeccaos_outcome_results$mean_yes,
                                               DALYMin = tz_eeccaos_outcome_results$range_low,
                                               DALYMax = tz_eeccaos_outcome_results$range_high,
                                               DALYMedian = tz_eeccaos_outcome_results$median_yes)
#The below code restructures Cost results for the impact of ACC in a District Hospital Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

tz_eeccaos_costdat <- data.frame(tz_eeccaos_costs)
tz_eeccaos_totalcost_dat <- tz_eeccaos_costdat$totalcost
tz_eeccaos_cost_results <- results_func(tz_eeccaos_totalcost_dat)
tz_eeccaos_cost_results_final <- data.frame(CostMean = tz_eeccaos_cost_results$mean_yes,
                                            CostMin = tz_eeccaos_cost_results$range_low,
                                            CostMax = tz_eeccaos_cost_results$range_high,
                                            CostMedian = tz_eeccaos_cost_results$median_yes)

#The below script calculates the incremental costs of EECC and ACC compared to District Hospital Critical Care

tanzania_vs_tz_eecc_inc_costs <- (tz_eecc_cost_results_final - tanzania_cost_results_final)
tanzania_vs_tz_eeccaos_inc_costs <- (tz_eeccaos_cost_results_final - tanzania_cost_results_final)
tz_eecc_vs_tz_eeccaos_inc_costs <- (tz_eeccaos_cost_results_final - tz_eecc_cost_results_final)

#The below script calculates the incremental DALYS of EECC and ACC compared to District Hospital Critical Care


tanzania_vs_tz_eecc_inc_outcomes <- (tanzania_outcome_results_final - tz_eecc_outcome_results_final)
tanzania_vs_tz_eeccaos_inc_outcomes <- (tanzania_outcome_results_final - tz_eeccaos_outcome_results_final)
tz_eecc_vs_tz_eeccaos_inc_outcomes <- (tz_eecc_outcome_results_final - tz_eeccaos_outcome_results_final)

#The below script calculates the ICER Values of EECC and ACC compared to District Hospital Critical Care


tanzania_vs_tz_eecc_ICER <- tanzania_vs_tz_eecc_inc_costs/
  tanzania_vs_tz_eecc_inc_outcomes

tanzania_vs_tz_eeccaos_ICER <- tanzania_vs_tz_eeccaos_inc_costs/
  tanzania_vs_tz_eeccaos_inc_outcomes

tz_eecc_vs_tz_eeccaos_ICER <- tz_eecc_vs_tz_eeccaos_inc_costs/
  tz_eecc_vs_tz_eeccaos_inc_outcomes


#The below code calculates the incremental costs, incremental  DALYs and ICER values for EECC compared to District Hospital Critical Care 

TZ_INC_DALY_tanzaniaVSEECC <- tanzania_DALY_dat - tz_eecc_DALY_dat
RES_TZ_INC_DALY_tanzaniaVSEECC <- results_func(TZ_INC_DALY_tanzaniaVSEECC)
RES_TZ_INC_DALY_tanzaniaVSEECC1 <- data.frame(DALYMean = RES_TZ_INC_DALY_tanzaniaVSEECC$mean_yes,
                                             DALYMin = RES_TZ_INC_DALY_tanzaniaVSEECC$range_low,
                                             DALYMax = RES_TZ_INC_DALY_tanzaniaVSEECC$range_high,
                                             DALYMedian = RES_TZ_INC_DALY_tanzaniaVSEECC$median_yes)

TZ_INC_COSTS_tanzaniaVSEECC <- tz_eecc_totalcost_dat - tanzania_totalcost_dat
RES_TZ_INC_COSTS_tanzaniaVSEECC <- results_func(TZ_INC_COSTS_tanzaniaVSEECC)
RES_TZ_INC_COSTS_tanzaniaVSEECC1 <- data.frame(CostMean = RES_TZ_INC_COSTS_tanzaniaVSEECC$mean_yes,
                                               CostMin = RES_TZ_INC_COSTS_tanzaniaVSEECC$range_low,
                                               CostMax = RES_TZ_INC_COSTS_tanzaniaVSEECC$range_high,
                                               CostMedian = RES_TZ_INC_COSTS_tanzaniaVSEECC$median_yes)

TZ_ICER_tanzaniaVSEECC <- TZ_INC_COSTS_tanzaniaVSEECC/TZ_INC_DALY_tanzaniaVSEECC
TZ_ICER_tanzaniaVSEECC_RESULTS <- results_func(TZ_ICER_tanzaniaVSEECC)
TZ_ICER_tanzaniaVSEECC_RESULTS1 <- data.frame(ICERMean = TZ_ICER_tanzaniaVSEECC_RESULTS$mean_yes,
                                              ICERMin = TZ_ICER_tanzaniaVSEECC_RESULTS$range_low,
                                              ICERMax = TZ_ICER_tanzaniaVSEECC_RESULTS$range_high,
                                              ICERMedian = TZ_ICER_tanzaniaVSEECC_RESULTS$median_yes)

#The below code calculates the incremental costs, incremental  DALYs and ICER values for ACC compared to District Hospital Critical Care 

TZ_INC_DALY_tanzaniaVSEECCAOS <- tanzania_DALY_dat - tz_eeccaos_DALY_dat
RES_TZ_INC_DALY_tanzaniaVSEECCAOS <- results_func(TZ_INC_DALY_tanzaniaVSEECCAOS)
RES_TZ_INC_DALY_tanzaniaVSEECCAOS1 <- data.frame(DALYMean = RES_TZ_INC_DALY_tanzaniaVSEECCAOS$mean_yes,
                                                DALYMin = RES_TZ_INC_DALY_tanzaniaVSEECCAOS$range_low,
                                                DALYMax = RES_TZ_INC_DALY_tanzaniaVSEECCAOS$range_high,
                                                DALYMedian = RES_TZ_INC_DALY_tanzaniaVSEECCAOS$median_yes)


TZ_INC_COSTS_tanzaniaVSEECCAOS <- tz_eeccaos_totalcost_dat - tanzania_totalcost_dat
RES_TZ_INC_COSTS_tanzaniaVSEECCAOS <- results_func(TZ_INC_COSTS_tanzaniaVSEECCAOS)
RES_TZ_INC_COSTS_tanzaniaVSEECCAOS1 <- data.frame(CostMean = RES_TZ_INC_COSTS_tanzaniaVSEECCAOS$mean_yes,
                                                  CostMin = RES_TZ_INC_COSTS_tanzaniaVSEECCAOS$range_low,
                                                  CostMax = RES_TZ_INC_COSTS_tanzaniaVSEECCAOS$range_high,
                                                  CostMedian = RES_TZ_INC_COSTS_tanzaniaVSEECCAOS$median_yes)


TZ_ICER_tanzaniaVSEECCAOS <- TZ_INC_COSTS_tanzaniaVSEECCAOS/TZ_INC_DALY_tanzaniaVSEECCAOS
TZ_ICER_tanzaniaVSEECCAOS_RESULTS <- results_func(TZ_ICER_tanzaniaVSEECCAOS)
TZ_ICER_tanzaniaVSEECCAOS_RESULTS1 <- data.frame(ICERMean = TZ_ICER_tanzaniaVSEECCAOS_RESULTS$mean_yes,
                                                 ICERMin = TZ_ICER_tanzaniaVSEECCAOS_RESULTS$range_low,
                                                 ICERMax = TZ_ICER_tanzaniaVSEECCAOS_RESULTS$range_high,
                                                 ICERMedian = TZ_ICER_tanzaniaVSEECCAOS_RESULTS$median_yes)

