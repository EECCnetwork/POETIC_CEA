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


source("Markov_Model/probabilistic_analysis/Results/one_way/int_costs/results_eecccritcosts_nocriticalcare.R")

#load data for tz critical care

source("Markov_Model/probabilistic_analysis/Results/one_way/int_costs/results_eecccritcosts_tzcriticalcare.R")


#load data for tz critical care

source("Markov_Model/probabilistic_analysis/Results/one_way/int_costs/results_eecccritcosts_iscriticalcare.R")

#rbind all three dataframes


grid.arrange(plot_eecccriticalcosts_nothing, plot_eecccriticalcosts_tanzania, plot_eecccriticalcosts_isaric, ncol=3, nrow = 1)

