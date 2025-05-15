## Description
This repository contains raw data and R code used in the manuscript:  
**"Climate Change Resilience in *Phallusia nigra*: A Comparative Study of Native and Introduced Populations"**

## Repository Structure

### `data/`  
This folder contains all raw and processed data files used for statistical analyses and figure generation.

| Filename | Description |
|----------|-------------|
| `all_yearly_mean_juvenile.csv` | Averaged yearly survival data of juveniles across treatments |
| `all_yearly_mean_larvae.csv` | Averaged yearly data on larval success across treatments |
| `blood_flow.csv` | Blood flow reversal time measurements across treatments |
| `larvae_figure.csv` | Data specifically prepared for plotting larval success figures |
| `larvae_results.csv` | Processed results of larval success analyses |
| `survival_percentage.csv` | Percent survival of juveniles across treatments |
| `survival_results.csv` | Processed results of juvenile survival analyses |

---

### `code/`  
This folder contains all R scripts for analyses and figure generation. The script numbering corresponds to the order of sections and figures in the manuscript.

| Filename | Description |
|----------|-------------|
| `1-kaplan-meir.R` | Kaplan-Meier survival analysis with log-rank tests (juveniles) |
| `2-survival statistics.R` | Generalized Linear Models (GLMs) on juvenile survival |
| `3-blood flow statistics.R` | Blood flow response analysis using Poisson GLMs and LMMs |
| `4-larvae statistics.R` | GLMs for larval success per population |
| `5-distribution maps.R` | Species distribution modeling using GAMs and environmental layers <br> (includes code for **Figures 4–6**) |
| `Figures_1_2.R` | Code for generating Figures 1 and 2 (juvenile survival and blood flow) |
| `Figure_3.R` | Code for generating Figure 3 (larval success) |

---

## How to Use

1. Open the `.R` scripts in RStudio or another R environment.
2. Modify file paths if necessary to point to your local `data/` and `code/` directories.
3. Run the scripts in the order listed to reproduce figures and analyses.

---

## Requirements

- **R version**: 4.2.2 or later  
- **R packages** (may vary slightly by script):  
  `ggplot2`, `tidyverse`, `dplyr`, `tidyr`, `scales`, `survival`,  
  `MASS`, `performance`, `lme4`, `emmeans`, `lmerTest`, `car`, `stats`,  
  `ResourceSelection`, `raster`, `lubridate`, `data.table`, `mgcv`, `maps`, `viridis`

---

## Figures

- **Figure 1–2**: See `Figures_1_2.R`  
- **Figure 3**: See `Figure_3.R`  
- **Figures 4–6**: See `5-distribution maps.R`

---

## Contact
Amit Unger - amitunger@mail.tau.ac.il
