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

source("Markov_Model/probabilistic_analysis/Results/one_way/eecc_effectiveness/pars_crittosev_nocriticalcare_ecc_eff.R")

#nothing simulations

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

nothing_costs <- costing_calc(outcome_dataset = nothing_sims,
                               severe_cost = nothing_severe_costs,
                               critical_cost = nothing_critical_costs,
                               death_cost = nothing_death_costs,
                               discharge_cost = nothing_discharge_costs)

#only EECC

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


no_eecc_costs <- costing_calc(outcome_dataset = no_eecc_sims,
                              severe_cost = eecc_severe_costs,
                              critical_cost = eecc_critical_costs,
                              death_cost = eecc_death_costs,
                              discharge_cost = eecc_discharge_costs)



#EECC + AOS
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


no_eeccaos_costs <- costing_calc(outcome_dataset = no_eeccaos_sims,
                              severe_cost = eeccaos_severe_costs,
                              critical_cost = eeccaos_critical_costs,
                              death_cost = eeccaos_death_costs,
                              discharge_cost = eeccaos_discharge_costs)

#probabilistic mean, ci low, ci high, ranges

nothing_dat <- data.frame(nothing_DALY)
nothing_DALY_dat <- nothing_dat$DALY
nothing_outcome_results <- results_func(nothing_DALY_dat)
nothing_outcome_results_final <- data.frame(DALYMean = nothing_outcome_results$mean_yes,
                                            DALYMin = nothing_outcome_results$range_low,
                                            DALYMax = nothing_outcome_results$range_high,
                                            DALYMedian = nothing_outcome_results$median_yes)

nothing_costdat <- data.frame(nothing_costs)
nothing_totalcost_dat <- nothing_costdat$totalcost
nothing_cost_results <- results_func(nothing_totalcost_dat)
nothing_cost_results_final <- data.frame(CostMean = nothing_cost_results$mean_yes,
                                         CostMin = nothing_cost_results$range_low,
                                         CostMax = nothing_cost_results$range_high,
                                         CostMedian = nothing_cost_results$median_yes)


no_eecc_dat <- data.frame(no_eecc_DALY)
no_eecc_DALY_dat <- no_eecc_dat$DALY
no_eecc_outcome_results <- results_func(no_eecc_DALY_dat)
no_eecc_outcome_results_final <- data.frame(DALYMean = no_eecc_outcome_results$mean_yes,
                                            DALYMin = no_eecc_outcome_results$range_low,
                                            DALYMax = no_eecc_outcome_results$range_high,
                                            DALYMedian = no_eecc_outcome_results$median_yes)

no_eecc_costdat <- data.frame(no_eecc_costs)
no_eecc_totalcost_dat <- no_eecc_costdat$totalcost
no_eecc_cost_results <- results_func(no_eecc_totalcost_dat)
no_eecc_cost_results_final <- data.frame(CostMean = no_eecc_cost_results$mean_yes,
                                         CostMin = no_eecc_cost_results$range_low,
                                         CostMax = no_eecc_cost_results$range_high,
                                         CostMedian = no_eecc_cost_results$median_yes)


no_eeccaos_dat <- data.frame(no_eeccaos_DALY)
no_eeccaos_DALY_dat <- no_eeccaos_dat$DALY
no_eeccaos_outcome_results <- results_func(no_eeccaos_DALY_dat)
no_eeccaos_outcome_results_final <- data.frame(DALYMean = no_eeccaos_outcome_results$mean_yes,
                                               DALYMin = no_eeccaos_outcome_results$range_low,
                                               DALYMax = no_eeccaos_outcome_results$range_high,
                                               DALYMedian = no_eeccaos_outcome_results$median_yes)


no_eeccaos_costdat <- data.frame(no_eeccaos_costs)
no_eeccaos_totalcost_dat <- no_eeccaos_costdat$totalcost
no_eeccaos_cost_results <- results_func(no_eeccaos_totalcost_dat)
no_eeccaos_cost_results_final <- data.frame(CostMean = no_eeccaos_cost_results$mean_yes,
                                            CostMin = no_eeccaos_cost_results$range_low,
                                         CostMax = no_eeccaos_cost_results$range_high,
                                         CostMedian = no_eeccaos_cost_results$median_yes)


#incremental costs

nothing_vs_no_eecc_inc_costs <- (no_eecc_cost_results_final - nothing_cost_results_final)
nothing_vs_no_eeccaos_inc_costs <- (no_eeccaos_cost_results_final - nothing_cost_results_final)
no_eecc_vs_no_eeccaos_inc_costs <- (no_eeccaos_cost_results_final - no_eecc_cost_results_final)

#incremental dalys

nothing_vs_no_eecc_inc_outcomes <- (nothing_outcome_results_final - no_eecc_outcome_results_final)
nothing_vs_no_eeccaos_inc_outcomes <- (nothing_outcome_results_final - no_eeccaos_outcome_results_final)
no_eecc_vs_no_eeccaos_inc_outcomes <- (no_eecc_outcome_results_final - no_eeccaos_outcome_results_final)

#ICERS

nothing_vs_no_eecc_ICER <- nothing_vs_no_eecc_inc_costs/
  nothing_vs_no_eecc_inc_outcomes

nothing_vs_no_eeccaos_ICER <- nothing_vs_no_eeccaos_inc_costs/
  nothing_vs_no_eeccaos_inc_outcomes

no_eecc_vs_no_eeccaos_ICER <- no_eecc_vs_no_eeccaos_inc_costs/
  no_eecc_vs_no_eeccaos_inc_outcomes

#one way sense results

nothing_oneway <- one_way_sense(outcome_dat = nothing_DALY_dat, 
                                 cost_dat = nothing_costdat, 
                                 parameter = eff_eecc_critical_to_severe)
nothing_eecc_oneway <- one_way_sense(outcome_dat = no_eecc_DALY_dat, 
                              cost_dat = no_eecc_costdat, 
                              parameter = eff_eecc_critical_to_severe)
nothing_eeccaos_oneway <- one_way_sense(outcome_dat = no_eeccaos_DALY_dat, 
                                 cost_dat = no_eeccaos_costdat, 
                                 parameter = eff_eecc_critical_to_severe)


nothing_EECC_NHB <- one_way_nhb(parameter = nothing_oneway$params,
                             intervention_mean_daly = nothing_oneway$meanDALYs, 
                             intervention_mean_cost = nothing_oneway$meanTotal_Cost,
                             intervention_min_daly = nothing_oneway$minDALYs, 
                             intervention_min_cost = nothing_oneway$minTotal_Cost,
                             intervention_max_daly = nothing_oneway$maxDALYs, 
                             intervention_max_cost = nothing_oneway$maxTotal_Cost,
                             comparator_mean_daly = nothing_eecc_oneway$meanDALYs, 
                             comparator_mean_cost = nothing_eecc_oneway$meanTotal_Cost,
                             comparator_min_daly = nothing_eecc_oneway$minDALYs, 
                             comparator_min_cost = nothing_eecc_oneway$minTotal_Cost,
                             comparator_max_daly = nothing_eecc_oneway$maxDALYs, 
                             comparator_max_cost = nothing_eecc_oneway$maxTotal_Cost,
                             wtp = 101,
                             intname = "Only EECC",
                             compname = "No Critical Care")

nothing_EECCAOS_NHB <- one_way_nhb(parameter = nothing_oneway$params,
                                intervention_mean_daly = nothing_oneway$meanDALYs, 
                                intervention_mean_cost = nothing_oneway$meanTotal_Cost,
                                intervention_min_daly = nothing_oneway$minDALYs, 
                                intervention_min_cost = nothing_oneway$minTotal_Cost,
                                intervention_max_daly = nothing_oneway$maxDALYs, 
                                intervention_max_cost = nothing_oneway$maxTotal_Cost,
                                comparator_mean_daly = nothing_eeccaos_oneway$meanDALYs, 
                                comparator_mean_cost = nothing_eeccaos_oneway$meanTotal_Cost,
                                comparator_min_daly = nothing_eeccaos_oneway$minDALYs, 
                                comparator_min_cost = nothing_eeccaos_oneway$minTotal_Cost,
                                comparator_max_daly = nothing_eeccaos_oneway$maxDALYs, 
                                comparator_max_cost = nothing_eeccaos_oneway$maxTotal_Cost,
                                wtp = 101,
                                intname = "EECC & AOS",
                                compname = "No Critical Care")

no_oneway_dat <- rbind(nothing_EECC_NHB,nothing_EECCAOS_NHB)



triage_plot_severe <-ggplot(no_oneway_dat, aes(x = Pars,y = NHB_MEAN, colour=Int)) + 
  geom_point(size = 3) +
  scale_colour_manual(values = c("Only EECC" = "#39568CFF", 
                                 "EECC & AOS" = "#73D055FF")) +
  geom_hline(yintercept=0, size = 0.5, linetype="dotted") +
  scale_x_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
  #scale_y_continuous(limits = c(-3000, 11000), breaks = c(seq(-3000, 11000, by = 1000))) +
  theme_minimal() +
  ggtitle("Triage Probabilities") +
  ylab("Conditional Expected Net Health Benefits") +
  xlab("Probability of a critically ill patient with a\nCOVID-19 diagnosis being classified as “severe”") +
  theme(legend.position = "right",
        axis.text.x=element_text(size = 15),
        axis.text.y=element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) + labs(colour = "Scenario")
