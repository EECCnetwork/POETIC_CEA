# POETIC_CEA
POETIC-COVID CEA PROJECT

Provision of Essential Treatment in Critical Illness in COVID-19 (POETIC COVID) project is funded by Wellcome Trust and is a collaboration between the Center for Global Development (CGD), the London School of Hygiene and Tropical Medicine, Institute for Healthcare Improvement (IHI), Uppsala University and KEMRI-Wellcome Trust. 
One of the key aims of this project is to identify the most efficient pathway for providing critical care to cope with the COVID-19 pandemic in low resourced settings from a provider perspective.
This work included estimating the costs of providing critical care to COVID-19 patients in Kenya and Tanzania and estimating and comparing the cost-effectiveness of different critical care strategies for COVID-19 patients in the form of cost per death averted/life years saved.

This repository contains code used to simulate the cost-effectiveness of essential emergency and critical care (EECC) and advanced critical care (ACC) for treating critically ill patients with COVID-19 in Tanzania.

The model code is written in R and R Studio and results are saved as Excel data files or as plots based on ggplot2. Therefore R and R Studio will be required to run any code within the POETIC COVID CEA Project

INSTRUCTIONS TO USE

There are 5 main scripts in total. 

There is one main model functions script which holds all the model engines and main model functions needed for all analysis. 

For each base case scenario ("No Critical Care" & "District Hospital Critical Care"), there 2 additional main scripts. 

1) Parameters script - this script loads all of the parameters needed to run the markov model. The parameters page also loads the model functions script. 
	a) An example of the file name of a parameters script for No Critical Care is "probabilistic_parameters_page_nothingbaseline.R"
2) Modelling script - this script simulates the markov model based no the respective parameters for each individual base case scenario. 
	a) An example of the file name of a modelling script for No Critical Care is "probabilistic_nothing_vs_interventions.R"

The results folder further holds scripts for specific plots and sensitivity or uncertainty analysis. However, all use the same model functions. 

For some sensitivity and uncertainty analysis (i.e. univariate/one-way sensitivity), duplicate parameter scripts and modelling scripts have had to be created, with slight tweaks in parameter values. 
However, all follow the same logic and structure as the main 5 scripts. 
