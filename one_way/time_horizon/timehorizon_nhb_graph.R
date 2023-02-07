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


source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_nothing_vs_interventions_30.R")
source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_nothing_vs_interventions_60.R")
source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_nothing_vs_interventions_90.R")
source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_nothing_vs_interventions_120.R")
source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_nothing_vs_interventions_150.R")
source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_nothing_vs_interventions_180.R")


#load data for tz critical care


source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_tanzania_vs_interventions_30.R")
source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_tanzania_vs_interventions_60.R")
source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_tanzania_vs_interventions_90.R")
source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_tanzania_vs_interventions_120.R")
source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_tanzania_vs_interventions_150.R")
source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_tanzania_vs_interventions_180.R")


#load data for is critical care

#source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_isaric_vs_interventions_30.R")
#source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_isaric_vs_interventions_60.R")
#source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_isaric_vs_interventions_90.R")
#source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_isaric_vs_interventions_120.R")
#source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_isaric_vs_interventions_150.R")
#source("Markov_Model/probabilistic_analysis/Results/one_way/time_horizon/probabilistic_isaric_vs_interventions_180.R")

#rbind all three dataframes

final_dat <- rbind(no_oneway_30_dat, tz_oneway_30_dat, 
                   no_oneway_60_dat, tz_oneway_60_dat, 
                   no_oneway_90_dat, tz_oneway_90_dat, 
                   no_oneway_120_dat, tz_oneway_120_dat, 
                   no_oneway_150_dat, tz_oneway_150_dat, 
                   no_oneway_180_dat, tz_oneway_180_dat)

triage_plot_timehorizon <-ggplot(final_dat, aes(x = Pars,y = NHB_MEAN, colour=Int)) +
  geom_point(aes(shape = Comp), size = 5) + 
  scale_colour_manual(values = c("Only EECC" = "#39568CFF", 
                                 "EECC & AOS" = "#73D055FF")) +
  scale_shape_manual(values=c("No Critical Care" = 17, 
                              "District Level Critical Care" = 19,
                              "Regional or Referral Critical Care" = 9)) +
  geom_hline(yintercept=0, size = 0.5, linetype="dotted") +
  scale_x_continuous(limits = c(30, 180), breaks = c(seq(30, 180, by = 30))) +
  theme_bw() +
  ggtitle("Time Horizon") +
  ylab("Conditional Expected Net Health Benefits") +
  xlab("Time Horizon (Days)") +
  theme(legend.position = "none",
        axis.text.x=element_text(size = 15),
        axis.text.y=element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) + labs(colour = "Intervention", shape = "Comparator")



