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

source("Markov_Model/probabilistic_analysis/Results/one_way/severe_probabilities/pars_sevtocrit_tzcriticalcare.R")

#tanzania simulations

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

tanzania_costs <- costing_calc(outcome_dataset = tanzania_sims,
                               severe_cost = tanzania_severe_costs,
                               critical_cost = tanzania_critical_costs,
                               death_cost = tanzania_death_costs,
                               discharge_cost = tanzania_discharge_costs)

#only EECC

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


tz_eecc_costs <- costing_calc(outcome_dataset = tz_eecc_sims,
                              severe_cost = eecc_severe_costs,
                              critical_cost = eecc_critical_costs,
                              death_cost = eecc_death_costs,
                              discharge_cost = eecc_discharge_costs)



#EECC + AOS
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


tz_eeccaos_costs <- costing_calc(outcome_dataset = tz_eeccaos_sims,
                              severe_cost = eeccaos_severe_costs,
                              critical_cost = eeccaos_critical_costs,
                              death_cost = eeccaos_death_costs,
                              discharge_cost = eeccaos_discharge_costs)

#probabilistic mean, ci low, ci high, ranges

tanzania_dat <- data.frame(tanzania_DALY)
tanzania_DALY_dat <- tanzania_dat$DALY
tanzania_outcome_results <- results_func(tanzania_DALY_dat)
tanzania_outcome_results_final <- data.frame(DALYMean = tanzania_outcome_results$mean_yes,
                                            DALYMin = tanzania_outcome_results$range_low,
                                            DALYMax = tanzania_outcome_results$range_high,
                                            DALYMedian = tanzania_outcome_results$median_yes)

tanzania_costdat <- data.frame(tanzania_costs)
tanzania_totalcost_dat <- tanzania_costdat$totalcost
tanzania_cost_results <- results_func(tanzania_totalcost_dat)
tanzania_cost_results_final <- data.frame(CostMean = tanzania_cost_results$mean_yes,
                                         CostMin = tanzania_cost_results$range_low,
                                         CostMax = tanzania_cost_results$range_high,
                                         CostMedian = tanzania_cost_results$median_yes)


tz_eecc_dat <- data.frame(tz_eecc_DALY)
tz_eecc_DALY_dat <- tz_eecc_dat$DALY
tz_eecc_outcome_results <- results_func(tz_eecc_DALY_dat)
tz_eecc_outcome_results_final <- data.frame(DALYMean = tz_eecc_outcome_results$mean_yes,
                                            DALYMin = tz_eecc_outcome_results$range_low,
                                            DALYMax = tz_eecc_outcome_results$range_high,
                                            DALYMedian = tz_eecc_outcome_results$median_yes)

tz_eecc_costdat <- data.frame(tz_eecc_costs)
tz_eecc_totalcost_dat <- tz_eecc_costdat$totalcost
tz_eecc_cost_results <- results_func(tz_eecc_totalcost_dat)
tz_eecc_cost_results_final <- data.frame(CostMean = tz_eecc_cost_results$mean_yes,
                                         CostMin = tz_eecc_cost_results$range_low,
                                         CostMax = tz_eecc_cost_results$range_high,
                                         CostMedian = tz_eecc_cost_results$median_yes)


tz_eeccaos_dat <- data.frame(tz_eeccaos_DALY)
tz_eeccaos_DALY_dat <- tz_eeccaos_dat$DALY
tz_eeccaos_outcome_results <- results_func(tz_eeccaos_DALY_dat)
tz_eeccaos_outcome_results_final <- data.frame(DALYMean = tz_eeccaos_outcome_results$mean_yes,
                                               DALYMin = tz_eeccaos_outcome_results$range_low,
                                               DALYMax = tz_eeccaos_outcome_results$range_high,
                                               DALYMedian = tz_eeccaos_outcome_results$median_yes)


tz_eeccaos_costdat <- data.frame(tz_eeccaos_costs)
tz_eeccaos_totalcost_dat <- tz_eeccaos_costdat$totalcost
tz_eeccaos_cost_results <- results_func(tz_eeccaos_totalcost_dat)
tz_eeccaos_cost_results_final <- data.frame(CostMean = tz_eeccaos_cost_results$mean_yes,
                                            CostMin = tz_eeccaos_cost_results$range_low,
                                         CostMax = tz_eeccaos_cost_results$range_high,
                                         CostMedian = tz_eeccaos_cost_results$median_yes)


#incremental costs

tanzania_vs_tz_eecc_inc_costs <- (tz_eecc_cost_results_final - tanzania_cost_results_final)
tanzania_vs_tz_eeccaos_inc_costs <- (tz_eeccaos_cost_results_final - tanzania_cost_results_final)
tz_eecc_vs_tz_eeccaos_inc_costs <- (tz_eeccaos_cost_results_final - tz_eecc_cost_results_final)

#incremental dalys

tanzania_vs_tz_eecc_inc_outcomes <- (tanzania_outcome_results_final - tz_eecc_outcome_results_final)
tanzania_vs_tz_eeccaos_inc_outcomes <- (tanzania_outcome_results_final - tz_eeccaos_outcome_results_final)
tz_eecc_vs_tz_eeccaos_inc_outcomes <- (tz_eecc_outcome_results_final - tz_eeccaos_outcome_results_final)

#ICERS

tanzania_vs_tz_eecc_ICER <- tanzania_vs_tz_eecc_inc_costs/
  tanzania_vs_tz_eecc_inc_outcomes

tanzania_vs_tz_eeccaos_ICER <- tanzania_vs_tz_eeccaos_inc_costs/
  tanzania_vs_tz_eeccaos_inc_outcomes

tz_eecc_vs_tz_eeccaos_ICER <- tz_eecc_vs_tz_eeccaos_inc_costs/
  tz_eecc_vs_tz_eeccaos_inc_outcomes

#one way sense results

tanzania_oneway <- one_way_sense(outcome_dat = tanzania_DALY_dat, 
                                 cost_dat = tanzania_costdat, 
                                 parameter = tanzania_severe_to_critical)
tanzania_eecc_oneway <- one_way_sense(outcome_dat = tz_eecc_DALY_dat, 
                              cost_dat = tz_eecc_costdat, 
                              parameter = tanzania_severe_to_critical)
tanzania_eeccaos_oneway <- one_way_sense(outcome_dat = tz_eeccaos_DALY_dat, 
                                 cost_dat = tz_eeccaos_costdat, 
                                 parameter = tanzania_severe_to_critical)


tanzania_EECC_NHB <- one_way_nhb(parameter = tanzania_oneway$params,
                             intervention_mean_daly = tanzania_oneway$meanDALYs, 
                             intervention_mean_cost = tanzania_oneway$meanTotal_Cost,
                             intervention_min_daly = tanzania_oneway$minDALYs, 
                             intervention_min_cost = tanzania_oneway$minTotal_Cost,
                             intervention_max_daly = tanzania_oneway$maxDALYs, 
                             intervention_max_cost = tanzania_oneway$maxTotal_Cost,
                             comparator_mean_daly = tanzania_eecc_oneway$meanDALYs, 
                             comparator_mean_cost = tanzania_eecc_oneway$meanTotal_Cost,
                             comparator_min_daly = tanzania_eecc_oneway$minDALYs, 
                             comparator_min_cost = tanzania_eecc_oneway$minTotal_Cost,
                             comparator_max_daly = tanzania_eecc_oneway$maxDALYs, 
                             comparator_max_cost = tanzania_eecc_oneway$maxTotal_Cost,
                             wtp = 101,
                             intname = "Only EECC",
                             compname = "District Level Critical Care")

tanzania_EECCAOS_NHB <- one_way_nhb(parameter = tanzania_oneway$params,
                                intervention_mean_daly = tanzania_oneway$meanDALYs, 
                                intervention_mean_cost = tanzania_oneway$meanTotal_Cost,
                                intervention_min_daly = tanzania_oneway$minDALYs, 
                                intervention_min_cost = tanzania_oneway$minTotal_Cost,
                                intervention_max_daly = tanzania_oneway$maxDALYs, 
                                intervention_max_cost = tanzania_oneway$maxTotal_Cost,
                                comparator_mean_daly = tanzania_eeccaos_oneway$meanDALYs, 
                                comparator_mean_cost = tanzania_eeccaos_oneway$meanTotal_Cost,
                                comparator_min_daly = tanzania_eeccaos_oneway$minDALYs, 
                                comparator_min_cost = tanzania_eeccaos_oneway$minTotal_Cost,
                                comparator_max_daly = tanzania_eeccaos_oneway$maxDALYs, 
                                comparator_max_cost = tanzania_eeccaos_oneway$maxTotal_Cost,
                                wtp = 101,
                                intname = "EECC & AOS",
                                compname = "District Level Critical Care")

tz_oneway_dat <- rbind(tanzania_EECC_NHB,tanzania_EECCAOS_NHB)



triage_plot_severe <-ggplot(tz_oneway_dat, aes(x = Pars,y = NHB_MEAN, colour=Int)) + 
  geom_point(size = 3) +
  scale_colour_manual(values = c("Only EECC" = "#39568CFF", 
                                 "EECC & AOS" = "#73D055FF")) +
  geom_hline(yintercept=0, size = 0.5, linetype="dotted") +
  scale_x_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
  #scale_y_continuous(limits = c(-3000, 11000), breaks = c(seq(-3000, 11000, by = 1000))) +
  theme_minimal() +
  ggtitle("Triage Probabilities") +
  ylab("Conditional Expected Net Health Benefits") +
  xlab("Probability of a critically ill patient with a\nCOVID-19 diagtzsis being classified as “severe”") +
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
