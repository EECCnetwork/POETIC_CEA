######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This is the script to generate cost-effectiveness planes for the "No Critical Care" base case scenario.

#load the model simulations page for base case no critical care script 

####load data

source("Markov_Model/probabilistic_analysis/probabilistic_nothing_vs_interventions.R")

##Transform the data to suit ggplot2 format

no_critical_care_total_cost <- nothing_totalcost_dat
no_critical_care_total_DALYs <- nothing_DALY_dat

no_eecc_total_cost <- no_eecc_totalcost_dat
no_eecc_total_DALYs <- no_eecc_DALY_dat

no_eeccaos_total_cost <- no_eeccaos_totalcost_dat
no_eeccaos_total_DALYs <- no_eeccaos_DALY_dat

no_scenario1_incDALYs <- (no_critical_care_total_DALYs - no_eecc_total_DALYs)
no_scenario1_incCOSTs <- (no_eecc_total_cost - no_critical_care_total_cost)

no_scenario1_increments <- data.frame(inc_COSTS = no_scenario1_incCOSTs,
                                   inc_DALYs = no_scenario1_incDALYs,
                                   Intervention = "EECC")


no_scenario2_incDALYs <- (no_critical_care_total_DALYs - no_eeccaos_total_DALYs)
no_scenario2_incCOSTs <- (no_eeccaos_total_cost - no_critical_care_total_cost)

no_scenario2_increments <- data.frame(inc_COSTS = no_scenario2_incCOSTs,
                                   inc_DALYs = no_scenario2_incDALYs,
                                   Intervention = "ACC")

no_scenario3_incDALYs <- (no_eecc_total_DALYs - no_eeccaos_total_DALYs)
no_scenario3_incCOSTs <- (no_eeccaos_total_cost - no_eecc_total_cost)

no_all_scenario_dat_nothing <- rbind(no_scenario1_increments, no_scenario2_increments)

#Plot the plane

plane_no_critical_care <- ggplot(data = no_all_scenario_dat_nothing, aes(x = inc_DALYs, y = inc_COSTS, col = Intervention)) + 
  geom_point(size=1) + 
  ggtitle("Comparator = No Critical Care") +
  scale_colour_manual(values = c("EECC" = "#39568CFF", 
                                 "ACC" = "#73D055FF")) +
  geom_hline(yintercept = 0, linetype="dashed") + 
  geom_vline(xintercept = 0, linetype="dashed") +
  theme_bw() + 
  labs(colour = "Scenario") +
  ylab('Incremental Costs') +
  xlab("Incremental DALYs") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust =0.5),
        legend.position = "none") 

