######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This is the script to generate cost-effectiveness acceptability curves for the "No Critical Care" base case scenario.

#load the model simulations page for base case no critical care script 

####load data

source("Markov_Model/probabilistic_analysis/probabilistic_nothing_vs_interventions.R")
##Quantify the net health benefits for the different scenarios

##This scenario looks at EECC vs No Critical Care

#Create a willingness to pay vector
WTP_vec <- seq(0,2000,by = 50)

#create an empty vector to store results
nothing_vs_eecc_prob <- rep(NA, times = length(WTP_vec))

#calculate the net health benefits of EECC compared to No Critical Care at each willingness to pay threshold in the WTP_Vec

for (wtp in 1:length(WTP_vec))
{
  DALY_vec1 <- nothing_DALY_dat - no_eecc_DALY_dat ###1000
  cost_vec1 <- (no_eecc_totalcost_dat - nothing_totalcost_dat)/WTP_vec[wtp] ###5000 col, 1000 row
  PSA_vec <- DALY_vec1- cost_vec1
  nothing_vs_eecc_prob[wtp] <- sum(PSA_vec >= 0)/length(DALY_vec1)
}
#Plot the CEAC to quick show

plot(WTP_vec, nothing_vs_eecc_prob)

##This scenario looks at ACC vs No Critical Care
#Create a willingness to pay vector

WTP_vec <- seq(0,2000,by = 50)

#create an empty vector to store results
nothing_vs_eeccaos_prob <- rep(NA, times = length(WTP_vec))


#calculate the net health benefits of ACC compared to No Critical Care at each willingness to pay threshold in the WTP_Vec

for (wtp in 1:length(WTP_vec))
{
  DALY_vec1 <- nothing_DALY_dat - no_eeccaos_DALY_dat ###1000
  cost_vec1 <- (no_eeccaos_totalcost_dat - nothing_totalcost_dat)/WTP_vec[wtp] ###5000 col, 1000 row
  PSA_vec <- DALY_vec1- cost_vec1
  nothing_vs_eeccaos_prob[wtp] <- sum(PSA_vec >= 0)/length(DALY_vec1)
}
#Plot the CEAC to quick show
plot(WTP_vec, nothing_vs_eeccaos_prob)
###Plot the CEAC Graph using ggplot2

dat_ceac_all_no = data.frame(label = c(rep("EECC",length(WTP_vec)), rep("ACC", length(WTP_vec))),
                          val = c(nothing_vs_eecc_prob, nothing_vs_eeccaos_prob),
                          WTP = rep(WTP_vec,2))

ceac_no_critical_care <- ggplot(data = dat_ceac_all_no, aes(x = WTP, y = val, col = label)) + 
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

