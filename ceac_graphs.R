######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This script brings together the two cost-effectiveness acceptability curves for easy viewing

####load data

source("Markov_Model/probabilistic_analysis/Results/ceac_graphs/no_critical_care_ceacs.R")
source("Markov_Model/probabilistic_analysis/Results/ceac_graphs/tanzania_critical_care_ceacs.R")
#source("Markov_Model/probabilistic_analysis/Results/ceac_graphs/isaric_critical_care_ceacs.R")

grid.arrange(ceac_no_critical_care, ceac_tanzania, ncol=2, nrow = 1)

