######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This is the script to generate cost-effectiveness planes for the "District Hospital Critical Care" base case scenario.

#load the model simulations page for base case no critical care script 

####load data

source("Markov_Model/probabilistic_analysis/probabilistic_tanzania_vs_interventions.R")

##Transform the data to suit ggplot2 format

tz_critical_care_total_cost <- tanzania_totalcost_dat
tz_critical_care_total_DALYs <- tanzania_DALY_dat

tz_eecc_total_cost <- tz_eecc_totalcost_dat
tz_eecc_total_DALYs <- tz_eecc_DALY_dat

tz_eeccaos_total_cost <- tz_eeccaos_totalcost_dat
tz_eeccaos_total_DALYs <- tz_eeccaos_DALY_dat

tz_scenario1_incDALYs <- (tz_critical_care_total_DALYs - tz_eecc_total_DALYs)
tz_scenario1_incCOSTs <- (tz_eecc_total_cost - tz_critical_care_total_cost)

tz_scenario1_increments <- data.frame(inc_COSTS = tz_scenario1_incCOSTs,
                                   inc_DALYs = tz_scenario1_incDALYs,
                                   Intervention = "EECC")


tz_scenario2_incDALYs <- (tz_critical_care_total_DALYs - tz_eeccaos_total_DALYs)
tz_scenario2_incCOSTs <- (tz_eeccaos_total_cost - tz_critical_care_total_cost)

tz_scenario2_increments <- data.frame(inc_COSTS = tz_scenario2_incCOSTs,
                                   inc_DALYs = tz_scenario2_incDALYs,
                                   Intervention = "ACC")

tz_scenario3_incDALYs <- (tz_eecc_total_DALYs - tz_eeccaos_total_DALYs)
tz_scenario3_incCOSTs <- (tz_eeccaos_total_cost - tz_eecc_total_cost)


tz_all_scenario_dat <- rbind(tz_scenario1_increments, tz_scenario2_increments)

#Plot the plane

plane_tz_critical_care <- ggplot(data = tz_all_scenario_dat, aes(x = inc_DALYs, y = inc_COSTS, col = Intervention)) + 
  geom_point(size=1)+
  ggtitle("Comparator = District Level Critical Care") +
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

