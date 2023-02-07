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


source("Markov_Model/probabilistic_analysis/Results/one_way/comp_costs/results_sevcosts_nocriticalcare.R")

#load data for tz critical care

source("Markov_Model/probabilistic_analysis/Results/one_way/comp_costs/results_sevcosts_tzcriticalcare.R")


#load data for tz critical care

#source("Markov_Model/probabilistic_analysis/Results/one_way/comp_costs/results_sevcosts_iscriticalcare.R")

#rbind all three dataframes


grid.arrange(plot_severecosts_nothing, plot_severecosts_tanzania, ncol=2, nrow = 1)

