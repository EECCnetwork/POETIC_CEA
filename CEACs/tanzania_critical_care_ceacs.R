######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This is the script to generate cost-effectiveness acceptability curves for the "District Hospital Critical Care" base case scenario.

####load data

source("Markov_Model/probabilistic_analysis/probabilistic_tanzania_vs_interventions.R")

##Quantify the net health benefits for the different scenarios

##This scenario looks at EECC vs District Level Critical Care

#Create a willingness to pay vector

WTP_vec <- seq(0,2000,by = 50)

#create an empty vector to store results

tanzania_vs_eecc_prob <- rep(NA, times = length(WTP_vec))
#calculate the net health benefits of EECC compared to District Level Critical Care at each willingness to pay threshold in the WTP_Vec
for (wtp in 1:length(WTP_vec))
{
  DALY_vec1 <- tanzania_DALY_dat - tz_eecc_DALY_dat ###1000
  cost_vec1 <- (tz_eecc_totalcost_dat - tanzania_totalcost_dat)/WTP_vec[wtp] ###5000 col, 1000 row
  PSA_vec <- DALY_vec1- cost_vec1
  tanzania_vs_eecc_prob[wtp] <- sum(PSA_vec >= 0)/length(DALY_vec1)
}
#Plot the CEAC to quick show
plot(WTP_vec, tanzania_vs_eecc_prob)

#
##This scenario looks at ACC vs District Level Critical Care

#Create a willingness to pay vector
WTP_vec <- seq(0,2000,by = 50)

#create an empty vector to store results
tanzania_vs_eeccaos_prob <- rep(NA, times = length(WTP_vec))

#calculate the net health benefits of ACC compared to District Lvel Critical Care at each willingness to pay threshold in the WTP_Vec

for (wtp in 1:length(WTP_vec))
{
  DALY_vec1 <- tanzania_DALY_dat - tz_eeccaos_DALY_dat ###1000
  cost_vec1 <- (tz_eeccaos_totalcost_dat - tanzania_totalcost_dat)/WTP_vec[wtp] ###5000 col, 1000 row
  PSA_vec <- DALY_vec1- cost_vec1
  tanzania_vs_eeccaos_prob[wtp] <- sum(PSA_vec >= 0)/length(DALY_vec1)
}
#Plot the CEAC to quick show
plot(WTP_vec, tanzania_vs_eeccaos_prob)

###Plot the CEAC Graph using ggplot2

dat_ceac_all_tz = data.frame(label = c(rep("EECC",length(WTP_vec)), rep("ACC", length(WTP_vec))),
                          val = c(tanzania_vs_eecc_prob, tanzania_vs_eeccaos_prob),
                          WTP = rep(WTP_vec,2))

ceac_tanzania <- ggplot(data = dat_ceac_all_tz, aes(x = WTP, y = val, col = label)) + 
  scale_colour_manual(values = c("EECC" = "#39568CFF", 
                                 "ACC" = "#73D055FF")) +
  geom_line(size = 2) +
  ggtitle("Comparator = District Level Critical Care")+
  #geom_rect(aes(xmin=19, xmax=101, ymin=0, ymax=Inf),col = "grey", alpha = 0.005) +
  geom_vline(xintercept=c(101), linetype="dotted") + 
  scale_y_continuous(limits = c(0, 1), breaks = c(seq(0, 1, by = 0.1))) +
  theme_bw() + 
  labs(colour = "Scenario") +
  ylab('Probability of being cost-effective (%)') +
  xlab("Willingness-to-pay (USD per DALY)") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")

