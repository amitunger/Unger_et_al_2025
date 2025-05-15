## Description
This repository contains raw data and R code used in the manuscript:  
**"Climate Change Resilience in *Phallusia nigra*: A Comparative Study of Native and Introduced Populations"**

## Repository Structure
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
## `data/`

| File Name                     | Description                                                                 | Columns Description |
|------------------------------|-----------------------------------------------------------------------------|---------------------|
| `survival_results.csv`       | Raw survival data for juvenile *P. nigra* under stress conditions.          | - `number`: ID of individual<br>- `temperature`: Treatment temperature (16/25/31)<br>- `salinity`: Treatment salinity (28/31/35/40/43)<br>- `day`: Day of death<br>- `event`: 1 = died, 0 = survived until day 28<br>- `origin`: Population origin (Mediterranean Sea/Red Sea/Singapore)<br>- `aquaria`: Aquaria number per treatment |
| `survival_percentage.csv`    | Percent survival data by treatment and population.                          | - Same columns as above, plus:<br>- `survival`: Percentage of juveniles alive |
| `blood_flow.csv`             | Blood flow recovery time measurements under stress.                         | - `plate`: Petri dish ID<br>- `number`: ID of individual<br>- `temperature`: Treatment temperature (16/25/31)<br>- `salinity`: Treatment salinity (28/31/35/40/43)<br>- `day`: Experimental day (measured weekly)<br>- `time`: Time (seconds) to reverse blood flow<br>- `origin`: Population origin (Mediterranean Sea/Red Sea/Singapore) |
| `larvae_results.csv`         | Raw fertilization and development results of larvae.                        | - `Temperature`: Treatment temperature (16/25/31)<br>- `Salinity`: Treatment salinity (28/31/35/40/43)<br>- `Origin`: Population origin (Mediterranean Sea/Red Sea/Singapore)<br>- `Plate`: Plate ID (6 per treatment)<br>- `Success`: 0 = unsuccessful (not fertilized/undeveloped/dead), 1 = successful (fully developed/settled) |
| `larvae_figure.csv`          | Subset of larvae data used for figure generation.                           | - Same columns as `larvae_results.csv` |
| `all_yearly_mean_juvenile.csv` | Yearly mean survival per treatment and origin.                            | - Summarized version of juvenile survival data<br>- Likely includes `temperature`, `salinity`, `origin`, and mean survival values |
| `all_yearly_mean_larvae.csv`   | Yearly mean larval success per treatment and origin.                      | - Summarized version of larval data<br>- Likely includes `Temperature`, `Salinity`, `Origin`, and mean `Success` |


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
