# Unger_et_al_2025
This repository contains raw data and R code used in the manuscript:  
"Climate Change Resilience in Phallusia nigra: A Comparative Study of Native and Introduced Populations"

# Contents
- `data`: Raw data used for statistical analysis and figures. 
   Sheets include: juveniles, juveniles (%), blood flow, larvae.
	columns explanation of sheets 'juveniles', 'juveniles (%)':
		"number"- id of individuals in survival measurements 
		"temperature"- temperature of treatment (16/25/31)
		"salinity"- salinity of treatment (28/31/35/40/43)			
		"day"- day of death
		"event"- 1 if died, 0 if stayed alive till day 28 of experiment 		
		"origin"- population origin (Mediterranean Sea/Red Sea/Singapore)
		"aquaria"- aquaria number of individual, each treatment had its own aquaria
		"survival"- percentage of juveniles alive 
	columns explanation of sheet 'blood flow':
		"plate"- Petri dish the individual was on
		"number"- id of individuals measured
		"temperature"- temperature of treatment (16/25/31)
		"salinity"- salinity of treatment (28/31/35/40/43)			
		"day"- day of experiment (was measured weekly)
		"time"- seconds it took to reverse the blood flow
		"origin"- population origin (Mediterranean Sea/Red Sea/Singapore)
	columns explanation of sheet 'larvae':
		"Temperature"- temperature of treatment (16/25/31)
		"Salinity"- salinity of treatment (28/31/35/40/43)			
		"Origin"- population origin (Mediterranean Sea/Red Sea/Singapore)
		"Plate"- number of plate oocytes were in (6 per treatment) 
		"Success"- 0 for non-fertilized eggs, undeveloped larvae, and dead larvae. 1 for larvae reaching the fully developed or settled stage 


- `code`: Contains the R script for all figures, statistics and species distribution model. 
   sections include:
	Fig. 1
	Fig. 2
	Fig. 3
Stress experiment on P. nigra juveniles- 
	Kaplan-Meir survival analysis with log-rank tests
	General Linear Model (GLM) with negative binomial regresssion
	Poisson GLM for all populations blood flow 
	Linear Mixed-Effects Model (LMM) for Singapore 

Stress experiment on P. nigra's reproduction products-
	GLM for each population

Potential distribution of P. nigra under a global change scenario-
	Creation of maps with ocean conditions 
	Generalized Additive Models (gam) for survival predictions
	Fig. 4-6- maps with P. nigra distribution predictions for present and change in future

# How to Use
Open the `code.R` file in R. 
Modify file paths and names where needed. 
Run each section separately.

# Requirements
- R version 4.2.2
- R packages: readxl, ggplot2, tidyverse, dplyr, tidyr, scales, survival, MASS, performance, lme4, emmeans, lmerTest, car, stats, ResourceSelection, raster, lubridate, data.table, mgcv, maps, virdis. 

# Contact
Amit Unger - amitunger@mail.tau.ac.il
