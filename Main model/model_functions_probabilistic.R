######POETIC CEA######
#Author - Hiral A Shah
#Date - 21/12/2022
##########################

#This is the model functions script for all POETIC analysis.
#The following functions below are the engines for the POETIC Cost-effectiveness model 


#POETIC Markov Model Function - this function simulates the markov model

markov_function <- function(pop_n, time_horizon, triage_sev, triage_crit, 
                            crit2crit, crit2death,crit2discharge,crit2severe,
                            severe2crit, severe2death, severe2discharge, severe2severe){
  #calculate the number of people in each cohort that are severe
  severe_pop_triage <- pop_n * triage_sev
  #calculate the number of people in each cohort that are critical
  critical_pop_triage <- pop_n * triage_crit
  #combine a vector of 28 days, with the severe population on day 1
  pop_severe <- c(severe_pop_triage, rep(0, time_horizon-1))
  #combine a vector of 28 days, with the critical population on day 1
  pop_critical <- c(critical_pop_triage, rep(0, time_horizon-1))
  #combine a vector of 28 days, with the dead population on day 1
  pop_death <- rep(0, time_horizon)
  #combine a vector of 28 days, with the discharged/recover/improved population on day 1
  pop_discharge <- rep(0, time_horizon)
  #simulate the transition matrix for each ith iteration over the time horizon specified
  for(i in 2:time_horizon){
    pop_severe[i] <- (pop_severe[i - 1] + pop_critical[i - 1] * crit2severe) -  
                        (pop_severe[i - 1] * severe2crit) - 
                        (pop_severe[i - 1] * severe2death) - 
                        (pop_severe[i - 1] * severe2discharge)
    pop_critical[i] <- (pop_critical[i-1] + pop_severe[i - 1] * severe2crit) - 
                         (pop_critical[i-1] * crit2severe) - 
                          (pop_critical[i-1] * crit2death) -
                          (pop_critical[i-1] * crit2discharge)
    pop_death[i] <- (pop_severe[i - 1] * severe2death) + (pop_critical[i-1] * crit2death)
    pop_discharge[i] <- (pop_severe[i - 1] * severe2discharge) + (pop_critical[i-1] * crit2discharge)
  }
  #sum the population of severe, critical, dead and discharged over the time horizon at each time step
  popsev <- sum(pop_severe)
  popcrit <- sum(pop_critical)
  popdeath <- sum(pop_death)
  popdis <- sum(pop_discharge)
  #create a dataframe of the sums
  states_go <- data.frame(Severe = popsev,
                          Critical = popcrit,
                          Death = popdeath,
                          Discharge = popdis)
  states_go}

#DALY Calculation function

DALY_calc <-function(DEATHS, age_mortprop1, age_mortprop2, age_mortprop3,age_mortprop4, 
                     age_entry1, age_entry2, age_entry3,age_entry4, life_expectancy, pop_n, 
                     severepercent, criticalpercent, severeDALY, criticalDALY, time_horizon){
  #Specify the number of deaths
  Deaths <- DEATHS
  #Calculate the years of life lost based on life expectency relative to each age strata
  YLL <- ((life_expectancy - age_entry1)*(age_mortprop1*Deaths))+((life_expectancy - age_entry2)*(age_mortprop2*Deaths))+
    ((life_expectancy - age_entry3)*(age_mortprop3*Deaths))+((life_expectancy - age_entry4)*(age_mortprop4*Deaths))
  #calculate the years of life lived in disability
  YLD <-  ((pop_n *severepercent) * (severeDALY * time_horizon/365)) + 
    ((pop_n *criticalpercent) * (criticalDALY * time_horizon/365))
  #combine the years of life lost with the years of life lived with disability to quantify the DALY
  DALY <- YLL + YLD
  #Create a dataframe of results
  Final <- cbind(DEATHS, YLL, YLD, DALY)
}

#Costing Calculation Function

costing_calc <- function(outcome_dataset, severe_cost, critical_cost, death_cost, discharge_cost){
  #Unlist the outcomes dataset, and create a data frame of the number of people in severe, critical,death and discharge states at each time step
  severe_n <- data.frame(unlist(outcome_dataset[,1]))
  crit_n <- data.frame(unlist(outcome_dataset[,2]))
  death_n <- data.frame(unlist(outcome_dataset[,3]))
  discharge_n <- data.frame(unlist(outcome_dataset[,4]))
  #Attach costs per day to each state per time step
  severecost <- severe_n * severe_cost
  criticalcost <- crit_n * critical_cost
  deathcost <- death_n * death_cost
  dischargecost <- discharge_n * discharge_cost
  #calculate the total costs
  totalcost <- severecost + criticalcost + deathcost + dischargecost
  #create a dataframe of all cost results
  costs <- data.frame(severecost, 
                      criticalcost, 
                      deathcost, 
                      dischargecost, 
                      totalcost)
  #add column names to results
  colnames(costs) <- c("severecost","criticalcost","deathcost","dischargecost","totalcost")
  costs
}


#probabilistic mean, cilow and ci high

results_func <- function(data){
  #specify the sample size
  sample.n <- length(data)
  #specify the data being used
  dat <- data
  #calculate the mean of the data
  mean_yes <- mean(dat, na.rm = TRUE)
  #calculate the median of the data
  median_yes <- median(dat, na.rm = TRUE)
  #calculate the standard deviation and standard error of the data
  sd_yes <- sd(dat, na.rm = TRUE)
  se_yes <- sd_yes/sqrt(sample.n)
  #calculate the range of the data
  range_low <- min(dat, na.rm = TRUE)
  range_high <- max(dat, na.rm = TRUE)
  #calculate the confidence intervals of the data
  error_yes <- qnorm(0.975)* sd_yes/sqrt(sample.n)
  cilow_yes <- mean_yes - error_yes
  cihigh_yes <- mean_yes + error_yes
  #create a dataframe of all of the above results
  results <- data.frame(mean_yes, cilow_yes, cihigh_yes, median_yes, sd_yes, se_yes, range_low, range_high)
}


#These three functions generate the data needed to create conditional expected net health benefit curves
#All three functions are needed, and all thre perform different things. 


#This function aggregates the results any one way sensitivity analysis performed into a interpretable format
one_way_sense <- function(outcome_dat, cost_dat, parameter){
  #Load the outcomes and specific parameter that has been varied as part of the univariate sensitivity analysis
  #create a dataframe of the parameter that was varied, the outcome results, and cost results of varying that single parameter whilst keeping everything else constant
  scenario_df <- data.frame(parameter, outcome_dat, cost_dat)
  #Calculate the mean of the results
  scenario_group_means <- aggregate(x=scenario_df[,c(2,7)],
                                   by=list(scenario_df$parameter),
                                   FUN=mean)
  #Calculate the lower range of results
  scenario_group_min <- aggregate(x=scenario_df[,c(2,7)],
                                by=list(scenario_df$parameter),
                                FUN=min)
  #calculate the upper range of results
  scenario_group_max <- aggregate(x=scenario_df[,c(2,7)],
                                by=list(scenario_df$parameter),
                                FUN = max)
  #make the results interpretable into a dataframe
  mean_results <- data.frame(meanDALYs = scenario_group_means$outcome_dat,
                             meanTotal_Cost = scenario_group_means$totalcost)
  min_results <- data.frame(minDALYs = scenario_group_min$outcome_dat,
                             minTotal_Cost = scenario_group_min$totalcost)
  max_results <- data.frame(maxDALYs = scenario_group_max$outcome_dat,
                            maxTotal_Cost = scenario_group_max$totalcost)
  #create the final results dataframe
  final_results <- data.frame(params = scenario_group_means$Group.1, mean_results, min_results, max_results)
}


#This function calculates the net health benefits of any scenario within the univariate sensitivity analysis
one_way_nhb <- function(parameter,intervention_mean_daly, intervention_mean_cost,
                        intervention_min_daly, intervention_min_cost, 
                        intervention_max_daly, intervention_max_cost,
                        comparator_mean_daly, comparator_mean_cost,
                        comparator_min_daly, comparator_min_cost, 
                        comparator_max_daly, comparator_max_cost, wtp, intname, compname) {
 
  #calculate the mean net health benefits
  int_comp_NHB_mean <- (intervention_mean_daly - comparator_mean_daly) - 
    ((comparator_mean_cost - intervention_mean_cost)/101)
  #calculate the minimum net health benefits
  int_comp_NHB_min <- (intervention_min_daly - comparator_min_daly) - 
    ((comparator_min_cost - intervention_min_cost)/101)
  #calculate the maximum net health benefits
  int_comp_NHB_max <- (intervention_max_daly - comparator_max_daly) - 
    ((comparator_max_cost - intervention_max_cost)/101)
  #create a dataframe of results
  results <- data.frame(Pars = parameter,
                        NHB_MEAN = int_comp_NHB_mean,
                        NHB_MIN = int_comp_NHB_min,
                        NHB_MAX = int_comp_NHB_max,
                        Int = intname,
                        Comp = compname)
}

#This brings all the results together to enable ggplot graphing

one_way_new <- function(parameter, intervention_daly, intervention_cost,
                        comparator_daly, comparator_cost, wtp, intervention_name,
                        comparator_name){
  #calculate the net health benefits
  NHB <- (intervention_daly - comparator_daly) -  ((comparator_cost - intervention_cost)/wtp)
  #specify the overal results of net health benefits per univariate parameter
  overall_results <- data.frame(parameter, NHB)
  #calculate the mean
  scenario_group_means <- aggregate(x=overall_results[,c(1,2)],
                                    by=list(overall_results$parameter),
                                    FUN=mean)
  #calculate the min
  scenario_group_min <- aggregate(x=overall_results[,c(1,2)],
                                  by=list(overall_results$parameter),
                                  FUN=min)
  #calculate the max
  scenario_group_max <- aggregate(x=overall_results[,c(1,2)],
                                  by=list(overall_results$parameter),
                                  FUN = max)
  #present results in a dataframe
  results <- data.frame(Pars = parameter,
                        NHB_MEAN = scenario_group_means$NHB,
                        NHB_Min = scenario_group_min$NHB, 
                        NHB_Max = scenario_group_max$NHB,
                        Int = intervention_name,
                        Comp = comparator_name)
  
  
}
