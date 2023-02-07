######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This script brings together the two cost-effectiveness planes for easy viewing

####load data

source("Markov_Model/probabilistic_analysis/Results/ce_planes/no_critical_care_planes.R")
source("Markov_Model/probabilistic_analysis/Results/ce_planes/tz_critical_care_planes.R")

grid.arrange(plane_no_critical_care, plane_tz_critical_care, ncol=2, nrow = 1)