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


#load data for no critical care


source("Markov_Model/probabilistic_analysis/Results/one_way/int_costs/results_eeccaossevcosts_nocriticalcare.R")

#load data for tz critical care

source("Markov_Model/probabilistic_analysis/Results/one_way/int_costs/results_eeccaossevcosts_tzcriticalcare.R")


#load data for tz critical care

source("Markov_Model/probabilistic_analysis/Results/one_way/int_costs/results_eeccaossevcosts_iscriticalcare.R")

#rbind all three dataframes


grid.arrange(plot_eeccaosseverecosts_nothing, plot_eeccaosseverecosts_tanzania, plot_eeccaosseverecosts_isaric, ncol=3, nrow = 1)

