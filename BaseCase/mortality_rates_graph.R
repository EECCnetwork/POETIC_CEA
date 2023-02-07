######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This script performs a sensitivity analysis to assess the trend between increasing mortality rates and the conditional net health benefit
#There, we aim to see if at high or low mortality rates, whether EECC or ACC would remain cost-effective.

#load the information from the results tables scripts


source("Markov_Model/probabilistic_analysis/Results/base_case/deaths_table.R")

source("Markov_Model/probabilistic_analysis/Results/base_case/basecaseanalysis_table.R")

#Create your mortality rates, and interpret DALYs and costs for the base case scenario

nothing_deaths_dat <- nothing_dat$DEATHS/pop
tanzania_deaths_dat <- tanzania_dat$DEATHS/pop
isaric_deaths_dat <- isaric_dat$DEATHS/pop

nothing_DALY_1 <- nothing_DALY[,4]
tanzania_DALY_1 <- tanzania_DALY[,4]
isaric_DALY_1 <- isaric_DALY[,4]

nothing_costs_1 <- nothing_costs$totalcost
tanzania_costs_1 <- tanzania_costs$totalcost
isaric_costs_1 <- isaric_costs$totalcost

#Create your mortality rates, and interpret DALYs and costs for the impact of EECC in each base case scenario

no_eecc_deaths_dat <- no_eecc_dat$DEATHS/pop
tz_eecc_deaths_dat <- tz_eecc_dat$DEATHS/pop
is_eecc_deaths_dat <- is_eecc_dat$DEATHS/pop

no_eecc_DALY_1 <- no_eecc_DALY[,4]
tz_eecc_DALY_1 <- tz_eecc_DALY[,4]
is_eecc_DALY_1 <- is_eecc_DALY[,4]

no_eecc_costs_1 <- no_eecc_costs$totalcost
tz_eecc_costs_1 <- tz_eecc_costs$totalcost
is_eecc_costs_1 <- is_eecc_costs$totalcost


#Create your mortality rates, and interpret DALYs and costs for the impact of ACC in each base case scenario


no_eeccaos_deaths_dat <- no_eeccaos_dat$DEATHS/pop
tz_eeccaos_deaths_dat <- tz_eeccaos_dat$DEATHS/pop
is_eeccaos_deaths_dat <- is_eeccaos_dat$DEATHS/pop

no_eeccaos_DALY_1 <- no_eeccaos_DALY[,4]
tz_eeccaos_DALY_1 <- tz_eeccaos_DALY[,4]
is_eeccaos_DALY_1 <- is_eeccaos_DALY[,4]

no_eeccaos_costs_1 <- no_eeccaos_costs$totalcost
tz_eeccaos_costs_1 <- tz_eeccaos_costs$totalcost
is_eeccaos_costs_1 <- is_eeccaos_costs$totalcost

#Calculate the net health benefit of EECC for each scenario 

no_eecc_NHB_dat <- (nothing_DALY_1 - no_eecc_DALY_1) - ((no_eecc_costs_1 - nothing_costs_1)/101)
tz_eecc_NHB_dat <- (tanzania_DALY_1 - tz_eecc_DALY_1) - ((tz_eecc_costs_1 - tanzania_costs_1)/101)
is_eecc_NHB_dat <- (isaric_DALY_1 - is_eecc_DALY_1) - ((is_eecc_costs_1 - isaric_costs_1)/101)


#Calculate the net health benefit of ACC for each scenario 

no_eeccaos_NHB_dat <- (nothing_DALY_1 - no_eeccaos_DALY_1) - ((no_eeccaos_costs_1 - nothing_costs_1)/101)
tz_eeccaos_NHB_dat <- (tanzania_DALY_1 - tz_eeccaos_DALY_1) - ((tz_eeccaos_costs_1 - tanzania_costs_1)/101)
is_eeccaos_NHB_dat <- (isaric_DALY_1 - is_eeccaos_DALY_1) - ((is_eeccaos_costs_1 - isaric_costs_1)/101)

#Reform the data so it can be graphed easily in GGPLOT2

no_eecc_graph_dat <- data.frame(Mort_Rate = nothing_deaths_dat*100,
                           NHB = no_eecc_NHB_dat,
                           label = "EECC")

no_eeccaos_graph_dat <- data.frame(Mort_Rate = nothing_deaths_dat*100,
                                NHB = no_eeccaos_NHB_dat,
                                label = "ACC")

tz_eecc_graph_dat <- data.frame(Mort_Rate = tanzania_deaths_dat*100,
                           NHB = tz_eecc_NHB_dat,
                           label = "EECC")

tz_eeccaos_graph_dat <- data.frame(Mort_Rate = tanzania_deaths_dat*100,
                                NHB = tz_eeccaos_NHB_dat,
                                label = "ACC")


is_eecc_graph_dat <- data.frame(Mort_Rate = isaric_deaths_dat*100,
                                NHB = is_eecc_NHB_dat,
                                label = "EECC")

is_eeccaos_graph_dat <- data.frame(Mort_Rate = isaric_deaths_dat*100,
                                NHB = is_eeccaos_NHB_dat,
                                label = "ACC")

#Create ggplots for EECC and ACC compared to No Critical Care

plot_nothing_eecc_mortrate_nhb <-ggplot(no_eecc_graph_dat, aes(x = Mort_Rate,y = NHB, colour=label)) +
  geom_point(size = 1) + 
  scale_colour_manual(values = c("EECC" = "#39568CFF", 
                                 "ACC" = "#73D055FF")) +
  geom_hline(yintercept=0, size = 0.5, linetype="dotted") +
  scale_x_continuous(limits = c(0, 100), breaks = c(seq(0, 100, by = 10))) +
  theme_minimal() +
  ggtitle("No Critical Care") +
  ylab("Conditional Expected Net Health Benefits") +
  xlab("Mortality Rate (%)") +
  theme(legend.position = "none",
        axis.text.x=element_text(size = 15, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y=element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) + labs(colour = "Intervention", shape = "Comparator")

plot_nothing_eeccaos_mortrate_nhb <-ggplot(no_eeccaos_graph_dat, aes(x = Mort_Rate,y = NHB, colour=label)) +
  geom_point(size = 1) + 
  scale_colour_manual(values = c("EECC" = "#39568CFF", 
                                 "ACC" = "#73D055FF")) +
  geom_hline(yintercept=0, size = 0.5, linetype="dotted") +
  scale_x_continuous(limits = c(0, 100), breaks = c(seq(0, 100, by = 10))) +
  theme_minimal() +
  ggtitle("No Critical Care") +
  ylab("Conditional Expected Net Health Benefits") +
  xlab("Mortality Rate (%)") +
  theme(legend.position = "none",
        axis.text.x=element_text(size = 15, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y=element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) + labs(colour = "Intervention", shape = "Comparator")

#Create ggplots for EECC and ACC compared to District Hospital Critical Care



plot_tanzania_eecc_mortrate_nhb <-ggplot(tz_eecc_graph_dat, aes(x = Mort_Rate,y = NHB, colour=label)) +
  geom_point(size = 1) + 
  scale_colour_manual(values = c("EECC" = "#39568CFF", 
                                 "ACC" = "#73D055FF")) +
  geom_hline(yintercept=0, size = 0.5, linetype="dotted") +
  scale_x_continuous(limits = c(0, 100), breaks = c(seq(0, 100, by = 10))) +
  theme_minimal() +
  ggtitle("District Level Critical Care") +
  ylab("Conditional Expected Net Health Benefits") +
  xlab("Mortality Rate (%)") +
  theme(legend.position = "none",
        axis.text.x=element_text(size = 15, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y=element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) + labs(colour = "Intervention", shape = "Comparator")

plot_tanzania_eeccaos_mortrate_nhb <-ggplot(tz_eeccaos_graph_dat, aes(x = Mort_Rate,y = NHB, colour=label)) +
  geom_point(size = 1) + 
  scale_colour_manual(values = c("EECC" = "#39568CFF", 
                                 "ACC" = "#73D055FF")) +
  geom_hline(yintercept=0, size = 0.5, linetype="dotted") +
  scale_x_continuous(limits = c(0, 100), breaks = c(seq(0, 100, by = 10))) +
  theme_minimal() +
  ggtitle("District Level Critical Care") +
  ylab("Conditional Expected Net Health Benefits") +
  xlab("Mortality Rate (%)") +
  theme(legend.position = "none",
        axis.text.x=element_text(size = 15, angle = 90, vjust = 0.5, hjust=1),
        axis.text.y=element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) + labs(colour = "Intervention", shape = "Comparator")

