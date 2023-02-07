######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This is the distributional sensitivity simulations page for the "No Critical Care" base case scenario.


####load data

source("Markov_Model/probabilistic_analysis/Results/distributional_sensitivity/probabilistic_nothing_vs_interventions_uniform.R")
##net health benefits approach

##ceac for no critical care vs only EECC

WTP_vec <- seq(0,2000,by = 50)
unif_nothing_vs_eecc_prob <- rep(NA, times = length(WTP_vec))

for (wtp in 1:length(WTP_vec))
{
  unif_DALY_vec1 <- unif_nothing_DALY_dat - unif_no_eecc_DALY_dat ###1000
  unif_cost_vec1 <- (unif_no_eecc_totalcost_dat - unif_nothing_totalcost_dat)/WTP_vec[wtp] ###5000 col, 1000 row
  unif_PSA_vec <- unif_DALY_vec1- unif_cost_vec1
  unif_nothing_vs_eecc_prob[wtp] <- sum(unif_PSA_vec >= 0)/length(unif_DALY_vec1)
}

plot(WTP_vec, unif_nothing_vs_eecc_prob)

##ceac for no critical care vs EECC + AOS

WTP_vec <- seq(0,2000,by = 50)
unif_nothing_vs_eeccaos_prob <- rep(NA, times = length(WTP_vec))

for (wtp in 1:length(WTP_vec))
{
  unif_DALY_vec1 <- unif_nothing_DALY_dat - unif_no_eeccaos_DALY_dat ###1000
  unif_cost_vec1 <- (unif_no_eeccaos_totalcost_dat - unif_nothing_totalcost_dat)/WTP_vec[wtp] ###5000 col, 1000 row
  unif_PSA_vec <- unif_DALY_vec1- unif_cost_vec1
  unif_nothing_vs_eeccaos_prob[wtp] <- sum(unif_PSA_vec >= 0)/length(unif_DALY_vec1)
}

plot(WTP_vec, unif_nothing_vs_eeccaos_prob)


###CEAC Graph

unif_dat_ceac_all_no = data.frame(label = c(rep("EECC",length(WTP_vec)), rep("ACC", length(WTP_vec))),
                          val = c(unif_nothing_vs_eecc_prob, unif_nothing_vs_eeccaos_prob),
                          WTP = rep(WTP_vec,2))

ceac_no_critical_care <- ggplot(data = unif_dat_ceac_all_no, aes(x = WTP, y = val, col = label)) + 
  scale_colour_manual(values = c("EECC" = "#39568CFF", 
                                 "ACC" = "#73D055FF")) +
  geom_line(size = 2) +
  ggtitle("Comparator = No Critical Care")+
  #geom_rect(aes(xmin=19, xmax=101, ymin=0, ymax=Inf),col = "grey", alpha = 0.005) +
  geom_vline(xintercept=c(101), linetype="dotted") + 
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
  theme_bw() + 
  labs(colour = "Intervention") +
  ylab('Probability of being cost-effective (%)') +
  xlab("Willingness-to-pay (USD per DALY)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")

