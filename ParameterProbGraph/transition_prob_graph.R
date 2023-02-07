######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This script brings plots the distributions of each parameter

#starting parameters

set.seed(123)
sens_n <- 10000

#No Critical Care


source("Markov_Model/probabilistic_analysis/probabilistic_parameters_page_nothingbaseline.R")

source("Markov_Model/probabilistic_analysis/probabilistic_parameters_page_tanzaniabaseline.R")

source("Markov_Model/probabilistic_analysis/probabilistic_parameters_page_isaricbaseline.R")

#create probability dataframes - severe

data_severe_to_critical <- data.frame(label = c( rep("No Critical Care", sens_n), rep("Tanzania District Critical Care", sens_n), rep("ISARIC Critical Care", sens_n) ),
                                      data = c(nothing_severe_to_critical, tanzania_severe_to_critical, isaric_severe_to_critical))

data_severe_to_severe <- data.frame(label = c( rep("No Critical Care", sens_n), rep("Tanzania District Critical Care", sens_n), rep("ISARIC Critical Care", sens_n) ),
                                      data = c(nothing_severe_to_severe, tanzania_severe_to_severe, isaric_severe_to_severe))

data_severe_to_death <- data.frame(label = c( rep("No Critical Care", sens_n), rep("Tanzania District Critical Care", sens_n), rep("ISARIC Critical Care", sens_n) ),
                                      data = c(nothing_severe_to_death, tanzania_severe_to_death, isaric_severe_to_death))

data_severe_to_improve <- data.frame(label = c( rep("No Critical Care", sens_n), rep("Tanzania District Critical Care", sens_n), rep("ISARIC Critical Care", sens_n) ),
                                   data = c(nothing_severe_to_discharge, tanzania_severe_to_discharge, isaric_severe_to_discharge))

#create probability dataframes - critical

data_critical_to_critical <- data.frame(label = c( rep("No Critical Care", sens_n), rep("Tanzania District Critical Care", sens_n), rep("ISARIC Critical Care", sens_n) ),
                                      data = c(nothing_critical_to_critical, tanzania_critical_to_critical, isaric_critical_to_critical))

data_critical_to_severe <- data.frame(label = c( rep("No Critical Care", sens_n), rep("Tanzania District Critical Care", sens_n), rep("ISARIC Critical Care", sens_n) ),
                                    data = c(nothing_critical_to_severe, tanzania_critical_to_severe, isaric_critical_to_severe))

data_critical_to_death <- data.frame(label = c( rep("No Critical Care", sens_n), rep("Tanzania District Critical Care", sens_n), rep("ISARIC Critical Care", sens_n) ),
                                   data = c(nothing_critical_to_death, tanzania_critical_to_death, isaric_critical_to_death))

data_critical_to_improve <- data.frame(label = c( rep("No Critical Care", sens_n), rep("Tanzania District Critical Care", sens_n), rep("ISARIC Critical Care", sens_n) ),
                                     data = c(nothing_critical_to_discharge, tanzania_critical_to_discharge, isaric_critical_to_discharge))

#severe plots

p_sev_to_crit <- ggplot(data = data_severe_to_critical, aes(x=data, fill=label)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', breaks = seq(from=0, to=1.0, by=0.01)) +
  scale_x_continuous() + xlab("Probability") +
  scale_fill_manual(values=c("green", "red", "blue"),
                      breaks=c("No Critical Care", "Tanzania District Critical Care", 
                               "ISARIC Critical Care")) + theme_bw() +
  ggtitle("Probability of patient in the severe state\nprogressing to the critical state") +
  theme(legend.position = "none")


p_sev_to_sev <- ggplot(data = data_severe_to_severe, aes(x=data, fill=label)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', breaks = seq(from=0, to=1.0, by=0.01)) +
  scale_x_continuous() + xlab("Probability") +
  scale_fill_manual(values=c("green", "red", "blue"),
                    breaks=c("No Critical Care", "Tanzania District Critical Care", 
                             "ISARIC Critical Care")) + theme_bw() +
  ggtitle("Probability of patient in the severe state\nremaining in the severe state") +
  theme(legend.position = "none")

p_sev_to_death <- ggplot(data = data_severe_to_death, aes(x=data, fill=label)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', breaks = seq(from=0, to=1.0, by=0.01)) +
  scale_x_continuous() + xlab("Probability") +
  scale_fill_manual(values=c("green", "red", "blue"),
                    breaks=c("No Critical Care", "Tanzania District Critical Care", 
                             "ISARIC Critical Care")) + theme_bw() +
  ggtitle("Probability of patient in the severe state\nprogressing to death") +
  theme(legend.position = "none")


p_sev_to_improve <- ggplot(data = data_severe_to_improve, aes(x=data, fill=label)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', breaks = seq(from=0, to=1.0, by=0.01)) +
  scale_x_continuous() + xlab("Probability") +
  scale_fill_manual(values=c("green", "red", "blue"),
                    breaks=c("No Critical Care", "Tanzania District Critical Care", 
                             "ISARIC Critical Care")) + theme_bw() +
  ggtitle("Probability of patient in the severe state\n progressing to the improve state") +
  theme(legend.position = "none")

#critical plots


p_crit_to_sev <- ggplot(data = data_critical_to_severe, aes(x=data, fill=label)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', breaks = seq(from=0, to=1.0, by=0.01)) +
  scale_x_continuous() + xlab("Probability") +
  scale_fill_manual(values=c("green", "red", "blue"),
                    breaks=c("No Critical Care", "Tanzania District Critical Care", 
                             "ISARIC Critical Care")) + theme_bw() +
  ggtitle("Probability of patient in the critical state\nprogressing to the severe state") +
  theme(legend.position = "none")


p_crit_to_crit <- ggplot(data = data_critical_to_critical, aes(x=data, fill=label)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', breaks = seq(from=0, to=1.0, by=0.01)) +
  scale_x_continuous() + xlab("Probability") +
  scale_fill_manual(values=c("green", "red", "blue"),
                    breaks=c("No Critical Care", "Tanzania District Critical Care", 
                             "ISARIC Critical Care")) + theme_bw() +
  ggtitle("Probability of patient in the critical state\nremaining in the critical state") +
  theme(legend.position = "none")

p_crit_to_death <- ggplot(data = data_critical_to_death, aes(x=data, fill=label)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', breaks = seq(from=0, to=1.0, by=0.01)) +
  scale_x_continuous() + xlab("Probability") +
  scale_fill_manual(values=c("green", "red", "blue"),
                    breaks=c("No Critical Care", "Tanzania District Critical Care", 
                             "ISARIC Critical Care")) + theme_bw() +
  ggtitle("Probability of patient in the critical state\nprogressing to death") +
  theme(legend.position = "none")


p_crit_to_improve <- ggplot(data = data_critical_to_improve, aes(x=data, fill=label)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', breaks = seq(from=0, to=1.0, by=0.01)) +
  scale_x_continuous() + xlab("Probability") +
  scale_fill_manual(values=c("green", "red", "blue"),
                    breaks=c("No Critical Care", "Tanzania District Critical Care", 
                             "ISARIC Critical Care")) + theme_bw() +
  ggtitle("Probability of patient in the critical state\n progressing to the improve state") +
  theme(legend.position = "none")

grid.arrange(p_sev_to_crit, p_sev_to_sev, p_sev_to_death, p_sev_to_improve, 
             p_crit_to_sev, p_crit_to_crit, p_crit_to_death, p_crit_to_improve, ncol=4, nrow = 2)
