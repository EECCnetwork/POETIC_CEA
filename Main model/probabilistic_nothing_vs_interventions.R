######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This is the markov model simulations page for the "District Hospital Critical Care" base case scenario.

#load the parameters page for the "District Hospital Critical Care" Base Case
#The parameters page will also load the model functions script
#The parameters page will also load any libraries that need loading.
#Please ensure you have installed the correct packages as per the parameters script below.

source("Markov_Model/probabilistic_analysis/probabilistic_parameters_page_nothingbaseline.R")

#Below is the code to simulate the markov model for a No Critical Care Scenario
#The markov model will be run using the parameters specified in the parameters page above
#The markov model uses the markov model function from the model functions script

nothing_sims <- as.data.frame(do.call(rbind, sapply(1:length(triage_critical), function(i){
  pop_n = pop
  time_horizon = timehorizon
  triage_sev = triage_severe[i]
  triage_crit = triage_critical[i]
  crit2crit = nothing_critical_to_critical[i]
  crit2death = nothing_critical_to_death[i]
  crit2discharge = nothing_critical_to_discharge[i]
  crit2severe = nothing_critical_to_severe[i]
  severe2crit = nothing_severe_to_critical[i]
  severe2death = nothing_severe_to_death[i]
  severe2discharge = nothing_severe_to_discharge[i]
  severe2severe = nothing_severe_to_severe[i]
  output <- markov_function(pop_n, time_horizon, triage_sev, triage_crit, 
                            crit2crit, crit2death,crit2discharge,crit2severe,
                            severe2crit, severe2death, severe2discharge, severe2severe)
  matrix(c(output), nrow = 1)}, simplify = FALSE)))

#Below is the code to calculate the number of DALYs lost in a No Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

nothing_DALY <- DALY_calc(DEATHS = unlist(nothing_sims[,3]),
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
#Below is the code to calculate the cost of severe and critical COVID19 in a No Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

nothing_costs <- costing_calc(outcome_dataset = nothing_sims,
                               severe_cost = nothing_severe_costs,
                               critical_cost = nothing_critical_costs,
                               death_cost = nothing_death_costs,
                               discharge_cost = nothing_discharge_costs)

#Below is the code to simulate the impact of EECC in a No Critical Care Scenario
#The markov model will be run using the parameters specified in the parameters page above
#The markov model uses the markov model function from the model functions script

no_eecc_sims <- as.data.frame(do.call(rbind, sapply(1:length(triage_critical), function(i){
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

#Below is the code to quantify the impact of EECC on the number of DALYs lost in a No Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

no_eecc_DALY <- DALY_calc(DEATHS = unlist(no_eecc_sims[,(3)]),
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

#Below is the code to quantify the impact of EECC on the cost of severe and critical COVID19 in a No Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

no_eecc_costs <- costing_calc(outcome_dataset = no_eecc_sims,
                              severe_cost = eecc_severe_costs,
                              critical_cost = eecc_critical_costs,
                              death_cost = eecc_death_costs,
                              discharge_cost = eecc_discharge_costs)



#Below is the code to simulate the impact of ACC in a No Critical Care Scenario
#The markov model will be run using the parameters specified in the parameters page above
#The markov model uses the markov model function from the model functions script
no_eeccaos_sims <- as.data.frame(do.call(rbind, sapply(1:length(triage_critical), function(i){
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

#Below is the code to quantify the impact of ACC on the number of DALYs lost in a No Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script
no_eeccaos_DALY <- DALY_calc(DEATHS = unlist(no_eeccaos_sims[,(3)]),
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


#Below is the code to quantify the impact of ACC on the cost of severe and critical COVID19 in a No Critical Care Scenario
#This code uses results from the markov simulation above based on the parameters specified in the parameters page above
#This code uses the model functions script

no_eeccaos_costs <- costing_calc(outcome_dataset = no_eeccaos_sims,
                              severe_cost = eeccaos_severe_costs,
                              critical_cost = eeccaos_critical_costs,
                              death_cost = eeccaos_death_costs,
                              discharge_cost = eeccaos_discharge_costs)

#The below code restructures DALY results for a No Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

nothing_dat <- data.frame(nothing_DALY)
nothing_DALY_dat <- nothing_dat$DALY
nothing_outcome_results <- results_func(nothing_DALY_dat)
nothing_outcome_results_final <- data.frame(DALYMean = nothing_outcome_results$mean_yes,
                                            DALYMin = nothing_outcome_results$range_low,
                                            DALYMax = nothing_outcome_results$range_high,
                                            DALYMedian = nothing_outcome_results$median_yes)
#The below code restructures Cost results for a No Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

nothing_costdat <- data.frame(nothing_costs)
nothing_totalcost_dat <- nothing_costdat$totalcost
nothing_cost_results <- results_func(nothing_totalcost_dat)
nothing_cost_results_final <- data.frame(CostMean = nothing_cost_results$mean_yes,
                                         CostMin = nothing_cost_results$range_low,
                                         CostMax = nothing_cost_results$range_high,
                                         CostMedian = nothing_cost_results$median_yes)
#The below code restructures DALY results for the impact of EECC in a No Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

no_eecc_dat <- data.frame(no_eecc_DALY)
no_eecc_DALY_dat <- no_eecc_dat$DALY
no_eecc_outcome_results <- results_func(no_eecc_DALY_dat)
no_eecc_outcome_results_final <- data.frame(DALYMean = no_eecc_outcome_results$mean_yes,
                                            DALYMin = no_eecc_outcome_results$range_low,
                                            DALYMax = no_eecc_outcome_results$range_high,
                                            DALYMedian = no_eecc_outcome_results$median_yes)
#The below code restructures Cost results for the impact of EECC in a No Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum
no_eecc_costdat <- data.frame(no_eecc_costs)
no_eecc_totalcost_dat <- no_eecc_costdat$totalcost
no_eecc_cost_results <- results_func(no_eecc_totalcost_dat)
no_eecc_cost_results_final <- data.frame(CostMean = no_eecc_cost_results$mean_yes,
                                         CostMin = no_eecc_cost_results$range_low,
                                         CostMax = no_eecc_cost_results$range_high,
                                         CostMedian = no_eecc_cost_results$median_yes)

#The below code restructures DALY results for the impact of ACC in a No Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

no_eeccaos_dat <- data.frame(no_eeccaos_DALY)
no_eeccaos_DALY_dat <- no_eeccaos_dat$DALY
no_eeccaos_outcome_results <- results_func(no_eeccaos_DALY_dat)
no_eeccaos_outcome_results_final <- data.frame(DALYMean = no_eeccaos_outcome_results$mean_yes,
                                               DALYMin = no_eeccaos_outcome_results$range_low,
                                               DALYMax = no_eeccaos_outcome_results$range_high,
                                               DALYMedian = no_eeccaos_outcome_results$median_yes)

#The below code restructures Cost results for the impact of ACC in a No Critical Care Scenario to make them more interpretable
#This is done by creating a dataframe which includes a mean, median, minimum and maximum

no_eeccaos_costdat <- data.frame(no_eeccaos_costs)
no_eeccaos_totalcost_dat <- no_eeccaos_costdat$totalcost
no_eeccaos_cost_results <- results_func(no_eeccaos_totalcost_dat)
no_eeccaos_cost_results_final <- data.frame(CostMean = no_eeccaos_cost_results$mean_yes,
                                            CostMin = no_eeccaos_cost_results$range_low,
                                         CostMax = no_eeccaos_cost_results$range_high,
                                         CostMedian = no_eeccaos_cost_results$median_yes)

#The below script calculates the incremental costs of EECC and ACC compared to No Critical Care

nothing_vs_no_eecc_inc_costs <- (no_eecc_cost_results_final - nothing_cost_results_final)
nothing_vs_no_eeccaos_inc_costs <- (no_eeccaos_cost_results_final - nothing_cost_results_final)
no_eecc_vs_no_eeccaos_inc_costs <- (no_eeccaos_cost_results_final - no_eecc_cost_results_final)

#The below script calculates the incremental DALYS of EECC and ACC compared to No Critical Care


nothing_vs_no_eecc_inc_outcomes <- (nothing_outcome_results_final - no_eecc_outcome_results_final)
nothing_vs_no_eeccaos_inc_outcomes <- (nothing_outcome_results_final - no_eeccaos_outcome_results_final)
no_eecc_vs_no_eeccaos_inc_outcomes <- (no_eecc_outcome_results_final - no_eeccaos_outcome_results_final)

#The below script calculates the ICER Values of EECC and ACC compared to No Critical Care

nothing_vs_no_eecc_ICER <- nothing_vs_no_eecc_inc_costs/
  nothing_vs_no_eecc_inc_outcomes

nothing_vs_no_eeccaos_ICER <- nothing_vs_no_eeccaos_inc_costs/
  nothing_vs_no_eeccaos_inc_outcomes

no_eecc_vs_no_eeccaos_ICER <- no_eecc_vs_no_eeccaos_inc_costs/
  no_eecc_vs_no_eeccaos_inc_outcomes

#The below code calculates the incremental costs, incremental  DALYs and ICER values for EECC compared to No Critical Care 

NO_INC_DALY_NOTHINGVSEECC <- nothing_DALY_dat - no_eecc_DALY_dat
RES_NO_INC_DALY_NOTHINGVSEECC <- results_func(NO_INC_DALY_NOTHINGVSEECC)
RES_NO_INC_DALY_NOTHINGVSEECC1 <- data.frame(DALYMean = RES_NO_INC_DALY_NOTHINGVSEECC$mean_yes,
                                             DALYMin = RES_NO_INC_DALY_NOTHINGVSEECC$range_low,
                                             DALYMax = RES_NO_INC_DALY_NOTHINGVSEECC$range_high,
                                             DALYMedian = RES_NO_INC_DALY_NOTHINGVSEECC$median_yes)

NO_INC_COSTS_NOTHINGVSEECC <- no_eecc_totalcost_dat - nothing_totalcost_dat
RES_NO_INC_COSTS_NOTHINGVSEECC <- results_func(NO_INC_COSTS_NOTHINGVSEECC)
RES_NO_INC_COSTS_NOTHINGVSEECC1 <- data.frame(CostMean = RES_NO_INC_COSTS_NOTHINGVSEECC$mean_yes,
                                              CostMin = RES_NO_INC_COSTS_NOTHINGVSEECC$range_low,
                                              CostMax = RES_NO_INC_COSTS_NOTHINGVSEECC$range_high,
                                              CostMedian = RES_NO_INC_COSTS_NOTHINGVSEECC$median_yes)

NO_ICER_NOTHINGVSEECC <- NO_INC_COSTS_NOTHINGVSEECC/NO_INC_DALY_NOTHINGVSEECC
NO_ICER_NOTHINGVSEECC_RESULTS <- results_func(NO_ICER_NOTHINGVSEECC)
NO_ICER_NOTHINGVSEECC_RESULTS1 <- data.frame(ICERMean = NO_ICER_NOTHINGVSEECC_RESULTS$mean_yes,
                                             ICERMin = NO_ICER_NOTHINGVSEECC_RESULTS$range_low,
                                             ICERMax = NO_ICER_NOTHINGVSEECC_RESULTS$range_high,
                                             ICERMedian = NO_ICER_NOTHINGVSEECC_RESULTS$median_yes)

#The below code calculates the incremental costs, incremental  DALYs and ICER values for ACC compared to No Critical Care


NO_INC_DALY_NOTHINGVSEECCAOS <- nothing_DALY_dat - no_eeccaos_DALY_dat
RES_NO_INC_DALY_NOTHINGVSEECCAOS <- results_func(NO_INC_DALY_NOTHINGVSEECCAOS)
RES_NO_INC_DALY_NOTHINGVSEECCAOS1 <- data.frame(DALYMean = RES_NO_INC_DALY_NOTHINGVSEECCAOS$mean_yes,
                                             DALYMin = RES_NO_INC_DALY_NOTHINGVSEECCAOS$range_low,
                                             DALYMax = RES_NO_INC_DALY_NOTHINGVSEECCAOS$range_high,
                                             DALYMedian = RES_NO_INC_DALY_NOTHINGVSEECCAOS$median_yes)


NO_INC_COSTS_NOTHINGVSEECCAOS <- no_eeccaos_totalcost_dat - nothing_totalcost_dat
RES_NO_INC_COSTS_NOTHINGVSEECCAOS <- results_func(NO_INC_COSTS_NOTHINGVSEECCAOS)
RES_NO_INC_COSTS_NOTHINGVSEECCAOS1 <- data.frame(CostMean = RES_NO_INC_COSTS_NOTHINGVSEECCAOS$mean_yes,
                                                 CostMin = RES_NO_INC_COSTS_NOTHINGVSEECCAOS$range_low,
                                                 CostMax = RES_NO_INC_COSTS_NOTHINGVSEECCAOS$range_high,
                                                 CostMedian = RES_NO_INC_COSTS_NOTHINGVSEECCAOS$median_yes)


NO_ICER_NOTHINGVSEECCAOS <- NO_INC_COSTS_NOTHINGVSEECCAOS/NO_INC_DALY_NOTHINGVSEECCAOS
NO_ICER_NOTHINGVSEECCAOS_RESULTS <- results_func(NO_ICER_NOTHINGVSEECCAOS)
NO_ICER_NOTHINGVSEECCAOS_RESULTS1 <- data.frame(ICERMean = NO_ICER_NOTHINGVSEECCAOS_RESULTS$mean_yes,
                                                ICERMin = NO_ICER_NOTHINGVSEECCAOS_RESULTS$range_low,
                                                ICERMax = NO_ICER_NOTHINGVSEECCAOS_RESULTS$range_high,
                                                ICERMedian = NO_ICER_NOTHINGVSEECCAOS_RESULTS$median_yes)

