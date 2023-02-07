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


#load data

source("Markov_Model/probabilistic_analysis/model_functions_probabilistic.R")

source("Markov_Model/probabilistic_analysis/probabilistic_parameters_page_isaricbaseline.R")

timehorizon = 60

#isaric simulations

isaric_sims <- as.data.frame(do.call(rbind, sapply(1:length(triage_critical), function(i){
  pop_n = pop
  time_horizon = timehorizon
  triage_sev = triage_severe[i]
  triage_crit = triage_critical[i]
  crit2crit = isaric_critical_to_critical[i]
  crit2death = isaric_critical_to_death[i]
  crit2discharge = isaric_critical_to_discharge[i]
  crit2severe = isaric_critical_to_severe[i]
  severe2crit = isaric_severe_to_critical[i]
  severe2death = isaric_severe_to_death[i]
  severe2discharge = isaric_severe_to_discharge[i]
  severe2severe = isaric_severe_to_severe[i]
  output <- markov_function(pop_n, time_horizon, triage_sev, triage_crit, 
                            crit2crit, crit2death,crit2discharge,crit2severe,
                            severe2crit, severe2death, severe2discharge, severe2severe)
  matrix(c(output), nrow = 1)}, simplify = FALSE)))


isaric_DALY <- DALY_calc(DEATHS = unlist(isaric_sims[,3]),
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

isaric_costs <- costing_calc(outcome_dataset = isaric_sims,
                               severe_cost = isaric_severe_costs,
                               critical_cost = isaric_critical_costs,
                               death_cost = isaric_death_costs,
                               discharge_cost = isaric_discharge_costs)

#only EECC

is_eecc_sims <- as.data.frame(do.call(rbind, sapply(1:length(triage_critical), function(i){
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


is_eecc_DALY <- DALY_calc(DEATHS = unlist(is_eecc_sims[,(3)]),
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


is_eecc_costs <- costing_calc(outcome_dataset = is_eecc_sims,
                              severe_cost = eecc_severe_costs,
                              critical_cost = eecc_critical_costs,
                              death_cost = eecc_death_costs,
                              discharge_cost = eecc_discharge_costs)



#EECC + AOS
is_eeccaos_sims <- as.data.frame(do.call(rbind, sapply(1:length(triage_critical), function(i){
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


is_eeccaos_DALY <- DALY_calc(DEATHS = unlist(is_eeccaos_sims[,(3)]),
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


is_eeccaos_costs <- costing_calc(outcome_dataset = is_eeccaos_sims,
                              severe_cost = eeccaos_severe_costs,
                              critical_cost = eeccaos_critical_costs,
                              death_cost = eeccaos_death_costs,
                              discharge_cost = eeccaos_discharge_costs)

#probabilistic mean, ci low, ci high, ranges

isaric_dat <- data.frame(isaric_DALY)
isaric_DALY_dat <- isaric_dat$DALY
isaric_outcome_results <- results_func(isaric_DALY_dat)
isaric_outcome_results_final <- data.frame(DALYMean = isaric_outcome_results$mean_yes,
                                           DALYMin = isaric_outcome_results$range_low,
                                           DALYMax = isaric_outcome_results$range_high,
                                           DALYMedian = isaric_outcome_results$median_yes)

isaric_costdat <- data.frame(isaric_costs)
isaric_totalcost_dat <- isaric_costdat$totalcost
isaric_cost_results <- results_func(isaric_totalcost_dat)
isaric_cost_results_final <- data.frame(CostMean = isaric_cost_results$mean_yes,
                                        CostMin = isaric_cost_results$range_low,
                                        CostMax = isaric_cost_results$range_high,
                                        CostMedian = isaric_cost_results$median_yes)

is_eecc_dat <- data.frame(is_eecc_DALY)
is_eecc_DALY_dat <- is_eecc_dat$DALY
is_eecc_outcome_results <- results_func(is_eecc_DALY_dat)
is_eecc_outcome_results_final <- data.frame(DALYMean = is_eecc_outcome_results$mean_yes,
                                            DALYMin = is_eecc_outcome_results$range_low,
                                            DALYMax = is_eecc_outcome_results$range_high,
                                            DALYMedian = is_eecc_outcome_results$median_yes)

is_eecc_costdat <- data.frame(is_eecc_costs)
is_eecc_totalcost_dat <- is_eecc_costdat$totalcost
is_eecc_cost_results <- results_func(is_eecc_totalcost_dat)
is_eecc_cost_results_final <- data.frame(CostMean = is_eecc_cost_results$mean_yes,
                                         CostMin = is_eecc_cost_results$range_low,
                                         CostMax = is_eecc_cost_results$range_high,
                                         CostMedian = is_eecc_cost_results$median_yes)


is_eeccaos_dat <- data.frame(is_eeccaos_DALY)
is_eeccaos_DALY_dat <- is_eeccaos_dat$DALY
is_eeccaos_outcome_results <- results_func(is_eeccaos_DALY_dat)
is_eeccaos_outcome_results_final <- data.frame(DALYMean = is_eeccaos_outcome_results$mean_yes,
                                               DALYMin = is_eeccaos_outcome_results$range_low,
                                               DALYMax = is_eeccaos_outcome_results$range_high,
                                               DALYMedian = is_eeccaos_outcome_results$median_yes)

is_eeccaos_costdat <- data.frame(is_eeccaos_costs)
is_eeccaos_totalcost_dat <- is_eeccaos_costdat$totalcost
is_eeccaos_cost_results <- results_func(is_eeccaos_totalcost_dat)
is_eeccaos_cost_results_final <- data.frame(CostMean = is_eeccaos_cost_results$mean_yes,
                                            CostMin = is_eeccaos_cost_results$range_low,
                                            CostMax = is_eeccaos_cost_results$range_high,
                                            CostMedian = is_eeccaos_cost_results$median_yes)


#incremental costs

isaric_vs_is_eecc_inc_costs <- (is_eecc_cost_results_final - isaric_cost_results_final)
isaric_vs_is_eeccaos_inc_costs <- (is_eeccaos_cost_results_final - isaric_cost_results_final)
is_eecc_vs_is_eeccaos_inc_costs <- (is_eeccaos_cost_results_final - is_eecc_cost_results_final)

#incremental dalys

isaric_vs_is_eecc_inc_outcomes <- (isaric_outcome_results_final - is_eecc_outcome_results_final)
isaric_vs_is_eeccaos_inc_outcomes <- (isaric_outcome_results_final - is_eeccaos_outcome_results_final)
is_eecc_vs_is_eeccaos_inc_outcomes <- (is_eecc_outcome_results_final - is_eeccaos_outcome_results_final)

#ICERS

isaric_vs_is_eecc_ICER <- isaric_vs_is_eecc_inc_costs/
  isaric_vs_is_eecc_inc_outcomes

isaric_vs_is_eeccaos_ICER <- isaric_vs_is_eeccaos_inc_costs/
  isaric_vs_is_eeccaos_inc_outcomes

is_eecc_vs_is_eeccaos_ICER <- is_eecc_vs_is_eeccaos_inc_costs/
  is_eecc_vs_is_eeccaos_inc_outcomes


#one way sense results

isaric_oneway_60 <- one_way_sense(outcome_dat = isaric_DALY_dat, 
                               cost_dat = isaric_costdat, 
                               parameter = timehorizon)
isaric_eecc_oneway_60 <- one_way_sense(outcome_dat = is_eecc_DALY_dat, 
                                    cost_dat = is_eecc_costdat, 
                                    parameter = timehorizon)
isaric_eeccaos_oneway_60 <- one_way_sense(outcome_dat = is_eeccaos_DALY_dat, 
                                       cost_dat = is_eeccaos_costdat, 
                                       parameter = timehorizon)


isaric_EECC_NHB_60 <- one_way_nhb(parameter = isaric_oneway_60$params,
                               intervention_mean_daly = isaric_oneway_60$meanDALYs, 
                               intervention_mean_cost = isaric_oneway_60$meanTotal_Cost,
                               intervention_min_daly = isaric_oneway_60$minDALYs, 
                               intervention_min_cost = isaric_oneway_60$minTotal_Cost,
                               intervention_max_daly = isaric_oneway_60$maxDALYs, 
                               intervention_max_cost = isaric_oneway_60$maxTotal_Cost,
                               comparator_mean_daly = isaric_eecc_oneway_60$meanDALYs, 
                               comparator_mean_cost = isaric_eecc_oneway_60$meanTotal_Cost,
                               comparator_min_daly = isaric_eecc_oneway_60$minDALYs, 
                               comparator_min_cost = isaric_eecc_oneway_60$minTotal_Cost,
                               comparator_max_daly = isaric_eecc_oneway_60$maxDALYs, 
                               comparator_max_cost = isaric_eecc_oneway_60$maxTotal_Cost,
                               wtp = 101,
                               intname = "Only EECC",
                               compname = "Regional or Referral Critical Care")

isaric_EECCAOS_NHB_60 <- one_way_nhb(parameter = isaric_oneway_60$params,
                                  intervention_mean_daly = isaric_oneway_60$meanDALYs, 
                                  intervention_mean_cost = isaric_oneway_60$meanTotal_Cost,
                                  intervention_min_daly = isaric_oneway_60$minDALYs, 
                                  intervention_min_cost = isaric_oneway_60$minTotal_Cost,
                                  intervention_max_daly = isaric_oneway_60$maxDALYs, 
                                  intervention_max_cost = isaric_oneway_60$maxTotal_Cost,
                                  comparator_mean_daly = isaric_eeccaos_oneway_60$meanDALYs, 
                                  comparator_mean_cost = isaric_eeccaos_oneway_60$meanTotal_Cost,
                                  comparator_min_daly = isaric_eeccaos_oneway_60$minDALYs, 
                                  comparator_min_cost = isaric_eeccaos_oneway_60$minTotal_Cost,
                                  comparator_max_daly = isaric_eeccaos_oneway_60$maxDALYs, 
                                  comparator_max_cost = isaric_eeccaos_oneway_60$maxTotal_Cost,
                                  wtp = 101,
                                  intname = "EECC & AOS",
                                  compname = "Regional or Referral Critical Care")

is_oneway_60_dat <- rbind(isaric_EECC_NHB_60,isaric_EECCAOS_NHB_60)

