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


source("Markov_Model/probabilistic_analysis/Results/one_way/daly_weights/results_YLDcritical_nocriticalcare.R")

#load data for tz critical care

source("Markov_Model/probabilistic_analysis/Results/one_way/daly_weights/results_YLDcritical_tzcriticalcare.R")


#load data for tz critical care

#source("Markov_Model/probabilistic_analysis/Results/one_way/daly_weights/results_YLDcritical_iscriticalcare.R")

#rbind all three dataframes

final_dat <- rbind(no_oneway_dat, tz_oneway_dat)

daly_critical_plot <-ggplot(final_dat, aes(x = Pars,y = NHB_MEAN, colour=Int)) +
  geom_point(aes(shape = Comp), size = 5) + 
  scale_colour_manual(values = c("Only EECC" = "#39568CFF", 
                                 "EECC & AOS" = "#73D055FF")) +
  scale_shape_manual(values=c("No Critical Care" = 17, 
                              "District Level Critical Care" = 19,
                              "Regional or Referral Critical Care" = 9)) +
  geom_hline(yintercept=0, size = 0.5, linetype="dotted") +
  scale_x_continuous(limits = c(0.6, 0.73), breaks = c(seq(0.6, 0.73, by = 0.01))) +
  theme_bw() +
  ggtitle("DALY weights") +
  ylab("Conditional Expected Net Health Benefits") +
  xlab("Disability weight for a critical care episode") +
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
