## Description
This repository contains raw data and R code used in the manuscript:  
**"Climate Change Resilience in *Phallusia nigra*: A Comparative Study of Native and Introduced Populations"**

---

## Contents

### `data/`
Raw data used for statistical analysis and figures.  

#### Sheets include:
- **juveniles** / **juveniles (%)**  
  Columns:
  - `number`: ID of individuals in survival measurements  
  - `temperature`: Temperature of treatment (16 / 25 / 31 °C)  
  - `salinity`: Salinity of treatment (28 / 31 / 35 / 40 / 43 PSU)  
  - `day`: Day of death  
  - `event`: 1 if died, 0 if alive at day 28  
  - `origin`: Population origin (Mediterranean Sea / Red Sea / Singapore)  
  - `aquaria`: Aquaria number per treatment  
  - `survival`: Percentage of juveniles alive  

- **blood flow**  
  Columns:
  - `plate`: Petri dish ID  
  - `number`: ID of individuals measured  
  - `temperature`: Treatment temperature  
  - `salinity`: Treatment salinity  
  - `day`: Day of experiment (weekly measurements)  
  - `time`: Time in seconds to reverse blood flow  
  - `origin`: Population origin  

- **larvae**  
  Columns:
  - `Temperature`: Treatment temperature  
  - `Salinity`: Treatment salinity  
  - `Origin`: Population origin  
  - `Plate`: Plate ID (6 per treatment)  
  - `Success`: 0 = failed (unfertilized/dead); 1 = fully developed/settled  

---

### `code/`
R script for all figures, statistical models, and species distribution modeling.

#### Sections:

- **Figure 1**: Survival rates of juveniles  
- **Figure 2**: Blood flow stress response  
- **Figure 3**: Larval success rates  

**Stress experiments on juveniles**:
- Kaplan-Meier survival analysis with log-rank tests  
- Generalized Linear Model (GLM) with negative binomial regression  
- Poisson GLM for all populations (blood flow)  
- Linear Mixed-Effects Model (LMM) for Singapore population  

**Stress experiments on reproductive products**:
- GLM for each population  

**Species Distribution Modeling**:
- Mapping of ocean conditions  
- Generalized Additive Models (GAMs) for survival predictions  
- **Figures 4–6**: Predicted present and future distributions of *P. nigra*  

---

## How to Use

1. Open the `code.R` file in RStudio or R.
2. Modify file paths and filenames to match your local setup.
3. Run each section separately for corresponding results and figures.

---

## Requirements

- **R version**: 4.2.2  
- **R packages**:  
  `readxl`, `ggplot2`, `tidyverse`, `dplyr`, `tidyr`, `scales`, `survival`, `MASS`, `performance`, `lme4`, `emmeans`, `lmerTest`, `car`, `stats`, `ResourceSelection`, `raster`, `lubridate`, `data.table`, `mgcv`, `maps`, `viridis`

---

## Contact
Amit Unger - amitunger@mail.tau.ac.il
