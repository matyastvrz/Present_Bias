# Present Bias in Quasi-Hyperbolic Discounting: A Meta-Analysis Replication Package

## Overview
This is a replication package for my Bachelor's Thesis. The main `beta_code.R` file contains the entire analysis, to have as simple a structure as possible and to be compatible with the university information system. It follows the main text of the thesis, and consists of data preparation and cleaning, exploratory data analysis, linear, nonlinear, endogeneity-robust publication bias and p-hacking tests, and a heterogeneity analysis using Bayesian and frequentist model averaging. Lastly, it computes fitted best-practice estimates of present bias.


## Repository structure

- `beta_code.R` — main analysis script (single-file workflow)
- `beta_data_cleaned.xlsx` — cleaned dataset used by the analysis
- `stem_method.R` — script for the stem-based method by Furukawa (2021)
- `FMA/` — frequentist model averaging utilities (`fma_cluster.R`, `artma_run_fma_cluster`, ...)
- `Present_Bias.pdf` — thesis PDF
- `renv/` — renv project metadata (do not commit library/)
- `renv.lock` — recorded package versions for the project
- `.Rprofile` — project R startup config
- `.gitignore` — repository ignore rules
- `README.md` — this file


## How to use

Open R or RStudio, set the working directory to the project root, then run:

```r
setwd("/path/to/Present_Bias")
source("renv/activate.R")   # enables project library paths
renv::restore()              # install packages from renv.lock (if needed)
source("beta_code.R")       # run the full analysis
```

Or in RStudio: open `Present_Bias.Rproj`, open `beta_code.R`, and click `Source`.

Notes:
- `renv.lock` captures package versions; restoring it gives the closest reproducible environment.
- If you prefer not to use `renv`, you can manually install packages used by the script before running it.


