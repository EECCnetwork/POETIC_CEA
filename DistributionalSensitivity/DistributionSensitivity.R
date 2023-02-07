######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This is the distributional sensitivity simulations page for the "No Critical Care" base case scenario.

#load the model simulations page for base case no critical care script with triangular distributions

source("Markov_Model/probabilistic_analysis/probabilistic_nothing_vs_interventions.R")


#load the model simulations page for base case no critical care script with uniform distributions

source("Markov_Model/probabilistic_analysis/Results/distributional_sensitivity/probabilistic_nothing_vs_interventions_uniform.R")

#create a dataframe of the uniform ICERs and triangular icers for each scenario and interventions

tri_eecc_dist_results <- data.frame(Distribution = "Triangular",
                               ICER_Value = NO_ICER_NOTHINGVSEECC,
                               Scenario = "EECC")

tri_eeccaos_dist_results <- data.frame(Distribution = "Triangular",
                                    ICER_Value = NO_ICER_NOTHINGVSEECCAOS,
                                    Scenario = "ACC")

unif_eecc_dist_results <- data.frame(Distribution = "Uniform",
                                    ICER_Value = unif_NO_ICER_NOTHINGVSEECC,
                                    Scenario = "EECC")

unif_eeccaos_dist_results <- data.frame(Distribution = "Uniform",
                                       ICER_Value = unif_NO_ICER_NOTHINGVSEECCAOS,
                                       Scenario = "ACC")

#cbind all results into a long dataframe

dist_dat <- rbind(tri_eecc_dist_results,
                  tri_eeccaos_dist_results,
                  unif_eecc_dist_results,
                  unif_eeccaos_dist_results)


#create boxplots

boxplots <- ggplot(data = dist_dat, aes(x=Scenario, y=ICER_Value, fill=Distribution)) + 
  geom_boxplot(outlier.shape = NA) +
  ylim(0,750)+
  ggtitle("Sensitivity Analysis of Distributions Used")+
  theme_bw() +
  ylab('Incremental Cost-Effectiveness Ratio') +
  xlab("Intervention") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right")

#To create the CEACs

rm(list=ls())

#Load the base case scenario results with triangular distributions
source("Markov_Model/probabilistic_analysis/Results/ceac_graphs/no_critical_care_ceacs.R")
#Load the base case scenario results with uniform distributions
source("Markov_Model/probabilistic_analysis/Results/distributional_sensitivity/no_critical_care_ceacs_uniform.R")

#bring together the results into a dataframe

triang_dat_ceac <- dat_ceac_all_no
triang_dat_ceac$Distribution <- "Triangular"

unif_dat_ceac <- unif_dat_ceac_all_no
unif_dat_ceac$Distribution <- "Uniform"

dat_ceac <- rbind(triang_dat_ceac,unif_dat_ceac)


ceac_no_critical_care <- ggplot(data = dat_ceac, aes(x = WTP, y = val, 
                                                            col = label)) + 
  geom_line(aes(linetype = Distribution),size = 1) +
  scale_colour_manual(values = c("EECC" = "#39568CFF", 
                                 "ACC" = "#73D055FF")) +
  ggtitle("Comparator = No Critical Care")+
  geom_vline(xintercept=c(101), linetype="dotted") + 
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
  theme_bw() + 
  labs(colour = "Intervention") +
  ylab('Probability of being cost-effective (%)') +
  xlab("Willingness-to-pay (USD per DALY)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "right")
