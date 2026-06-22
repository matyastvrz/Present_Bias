# Present Bias in Quasi-Hyperbolic Discounting: A Meta-Analysis Replication Package

## Overview
This is a replication package for my Bachelor's Thesis. The main `beta_code.R` file contains the entire analysis, to have as simple a structure as possible and to be compatible with the university information system. It follows the main text of the thesis, and consists of data preparation and cleaning, exploratory data analysis, linear, nonlinear, endogeneity-robust publication bias and p-hacking tests, and a heterogeneity analysis using Bayesian and frequentist model averaging. Lastly, it computes fitted best-practice estimates of present bias.


## Repository structure

- `beta_code.R` — main analysis script (single-file workflow)
- `beta_data_cleaned.xlsx` — cleaned dataset used by the analysis
- `stem_method.R` — script for the stem-based method by Furukawa (2021)
- `FMA/` — frequentist model averaging with clustered standard errors scripts (`fma_cluster.R`, `artma_run_fma_cluster`)
- `Present_Bias.pdf` — thesis PDF
- `README.md` — this file


## How to use

1. Install the required packages (see `beta_code.R` for the list of dependencies)
2. Open and run `beta_code.R` to execute the full analysis

