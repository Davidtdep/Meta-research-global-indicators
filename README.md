# Meta-research-global-indicators

This repository contains the R code used for the analyses in the article:  
*"Meta-research in biomedical investigation: gaps and opportunities based on meta-research publications and global indicators in health, science and human development"* (currently under review in the journal *Publications*, MDPI).

## Purpose
This script demonstrates analyses of meta-research publications and their associations with global indicators such as the Human Development Index (HDI), Research and Development Expenditure (R&D), and completed clinical trials.

## Required R packages
The following R packages are necessary to execute the analyses:
- ggplot2
- readxl
- maps
- dplyr
- tidyr
- stringr
- reshape2
- circlize
- gridExtra
- ggstream
- scales
- babynames
- lmtest
- tseries
- urca

## Analyses included
This script performs the following analyses:
- **Data cleaning and preprocessing**: Standardizing country names, handling missing data, and aligning datasets.
- **Correlations and regressions**:
  - HDI vs. number of meta-research articles.
  - R&D expenditure vs. number of meta-research articles.
  - Completed clinical trials vs. number of meta-research articles.
- **Time series analysis**: Exploring trends and cointegration of articles with R&D expenditure and clinical trials.
- **Residual diagnostics**: Checking normality and homoscedasticity in regression models.

## How to use
1. Download or clone this repository.
2. Place the datasets (`data.xlsx`, `human-development-index.csv`, `Research and development expenditure.xls`, `clinicalTrials.csv`) in the `data` folder.
3. Open the script in RStudio (`main_analysis.R`).
4. Install the required R packages (see above).
5. Run the script to replicate the analyses.

## Data availability
The data files (`data.xlsx`, `human-development-index.csv`, `Research and development expenditure.xls`, `clinicalTrials.csv`) are not publicly available but can be provided upon reasonable request. The script demonstrates the workflow for the analyses without requiring access to the raw data.

## License
This repository is licensed under CC0 1.0 Universal (Public Domain). The code is free to use, modify, and distribute without restrictions.
