######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This script takes all results of the base case analysis and creates detailed tables which can be used for manuscripts

#Load the modelling scripts


source("Markov_Model/probabilistic_analysis/probabilistic_nothing_vs_interventions.R")

source("Markov_Model/probabilistic_analysis/probabilistic_tanzania_vs_interventions.R")


#create dataframe of results

comparator <- c((rep("No Critical Care",2)),
                (rep("Tanzania District Critical Care",2)))

intervention <- rep(c("Only EECC","EECC & AOS"),2)


comparatorcosts <- rbind(nothing_cost_results_final,nothing_cost_results_final,
                     tanzania_cost_results_final, tanzania_cost_results_final)

interventioncosts <- rbind(no_eecc_cost_results_final,no_eeccaos_cost_results_final,
                         tz_eecc_cost_results_final, tz_eeccaos_cost_results_final)


comparatorDALYs <- rbind(nothing_outcome_results_final,nothing_outcome_results_final,
                         tanzania_outcome_results_final, tanzania_outcome_results_final)

interventionDALYs <- rbind(no_eecc_outcome_results_final,no_eeccaos_outcome_results_final,
                           tz_eecc_outcome_results_final, tz_eeccaos_outcome_results_final)


costs_results_table <- data.frame(comparator, intervention, comparatorcosts,interventioncosts)
colnames(costs_results_table) <- c("Comparator", "Intervention", 
                                   "CompCostMean", "CompCostMin", "CompCostMax", "CompCostMed",
                                   "IntCostMean", "IntCostMin", "IntCostMax", "IntCostMed")
DALY_results_table <- data.frame(comparator, intervention, comparatorDALYs,interventionDALYs)
colnames(DALY_results_table) <- c("Comparator", "Intervention", 
                                   "CompDALYMean", "CompDALYMin", "CompDALYMax", "CompDALYMed",
                                   "IntDALYMean", "IntDALYMin", "IntDALYMax", "IntDALYMed")

incremental_labels <- c("No Critical Care vs Only EECC", "No Critical Care vs EECC + AOS",
                        "Tanzania District Critical Care vs Only EECC", "Tanzania District Critical Care vs EECC + AOS")

incremental_costs <- rbind(nothing_vs_no_eecc_inc_costs, nothing_vs_no_eeccaos_inc_costs, 
                       tanzania_vs_tz_eecc_inc_costs, tanzania_vs_tz_eeccaos_inc_costs)

incremental_outcomes <- rbind(nothing_vs_no_eecc_inc_outcomes, nothing_vs_no_eeccaos_inc_outcomes, 
                              tanzania_vs_tz_eecc_inc_outcomes, tanzania_vs_tz_eeccaos_inc_outcomes)

ICER_results <- rbind(nothing_vs_no_eecc_ICER, nothing_vs_no_eeccaos_ICER, 
                  tanzania_vs_tz_eecc_ICER, tanzania_vs_tz_eeccaos_ICER)

ICER_table <- data.frame(incremental_labels, incremental_costs, incremental_outcomes, ICER_results)
colnames(ICER_table) <- c("Scenario", "IncMeanCosts", "IncMinCosts", "IncMaxCosts", "IncMedCosts",
                          "IncMeanDALY", "IncMinDALY", "IncMaxDALY", "IncMedDALY", 
                          "ICERMean", "ICERMin", "ICERMax", "ICERMed")

#Create your results table

comparator1 <- c(rep("No Critical Care", 3), rep("District Level Critical Care", 3))
intervention1 <- c(rep(c("No Intervention", "Only EECC", "EECC & AOS"),3))


dalys1 <- rbind(nothing_outcome_results_final, no_eecc_outcome_results_final,
            tanzania_outcome_results_final,tz_eecc_outcome_results_final)

costs1 <- rbind(nothing_cost_results_final, no_eecc_cost_results_final, 
            tanzania_cost_results_final,tz_eecc_cost_results_final)



incremental_outcomes1 <- rbind(0, RES_NO_INC_DALY_NOTHINGVSEECC1, RES_NO_INC_DALY_NOTHINGVSEECCAOS1,
                              0, RES_TZ_INC_DALY_tanzaniaVSEECC1, RES_TZ_INC_DALY_tanzaniaVSEECCAOS1)

incremental_costs1 <- rbind(0, RES_NO_INC_COSTS_NOTHINGVSEECC1, RES_NO_INC_COSTS_NOTHINGVSEECCAOS1,
                       0, RES_TZ_INC_COSTS_tanzaniaVSEECC1, RES_TZ_INC_COSTS_tanzaniaVSEECCAOS1)


ICER_results1 <- rbind(0, NO_ICER_NOTHINGVSEECC_RESULTS1, NO_ICER_NOTHINGVSEECCAOS_RESULTS1, 
                      0, TZ_ICER_tanzaniaVSEECC_RESULTS1, TZ_ICER_tanzaniaVSEECCAOS_RESULTS1)

