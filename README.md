---
title: "README"
output: html_document
date: "2025-12-05"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


# Boston BlueBikes Analysis

## Overview

This R project analyzes **BlueBikes station distribution and daily ridership in Boston**, integrating spatial and temporal perspectives. The analysis includes:

1. **Spatial Analysis of Stations**

   * Locations of stations inside Boston neighborhoods
   * Income-based filtering of neighborhoods (high-income vs. low-income)
   * Station-to-resident ratios across neighborhoods
   * Visualization of station density and clustering

2. **Temporal & Statistical Analysis**

   * Daily ridership in 2021 across station income categories
   * Confidence intervals for mean rides per day
   * Summary statistics (mean, median, min, max, IQR)
   * Threshold and cross-section analyses
   * Regression models for ridership effects of neighborhood income and rush hour
   * Monte Carlo simulations for predicted ride counts

---

## Data Sources

* **Spatial Data**

  * `data/social_infra/bounds.geojson` – Boston boundary
  * `data/boston_social_infra/boston_census_data.csv` – neighborhood population & density
  * `data/boston_social_infra/boston_grid.geojson` – census grid geometries
  * `data/bluebikes/bgdataset.rds` – neighborhood-level station info
  * `data/bluebikes/stationbg_dataset.rds` – station-level spatial data

* **Ridership Data**

  * `data/bluebikes/bluebikes.sqlite` – daily trip counts between stations
  * Table: `tally_rush_edges`

---

## Packages

```r
dplyr, readr, ggplot2, tidyr, stringr, lubridate, RSQLite, broom, sf, ggspatial, viridis, GGally, gridExtra
```

---

## Analysis Workflow

### 1. Spatial Analysis

* Load Boston boundary and BlueBikes station data.
* Filter stations **within Boston** and identify **high-income neighborhoods** (pop earning > $60k).
* Visualize stations with **geom_sf()** and add scale bars.
* Compute **mean, median, sum, and station-to-resident rate per neighborhood**.
* Rank neighborhoods by station density and cluster stations using **k-means**.
* Plot clusters and station density per neighborhood.

---

### 2. Ridership Analysis

* Connect to SQLite database and filter `tally_rush_edges` for **2021 trips**.
* Join with station income classification (`maj_high_income`) to create `start_high_income` and `end_high_income`.
* Drop rows with missing data for clean analysis.

#### Summary and Visualization

* Compute mean rides, SE, and 95% confidence intervals per day by start neighborhood income.
* Visualize trends using **line plots with ribbons** for confidence intervals.
* Density plots for distributions of total rides per day.
* Summary statistics: min, max, mean, median, IQR.

---

### 3. Cross-Section & Threshold Analysis

* Count unique cross-sections per day.
* Calculate share of cross-sections exceeding thresholds (e.g., >2000 rides).
* Identify days with lowest and highest share exceeding threshold.

---

### 4. Regression Modeling

* Fit linear models to estimate effects on total rides:

  1. `total_rides ~ start_high_income`
  2. `total_rides ~ end_high_income`
  3. `total_rides ~ start_high_income + end_high_income + rush`
* Compare model performance using **R² and sigma**.

---

### 5. Monte Carlo Simulation

* Simulate 1000 daily ridership scenarios using model predictions and residual error (`sigma`).
* Calculate **mean effects, differences, percent change, and z-scores** between high- and low-income neighborhoods.
* Compute confidence intervals for simulated outcomes.

---

### 6. Key Outputs

* Spatial maps of stations and neighborhoods, highlighting high-income areas
* Station-to-resident ratios per neighborhood and cluster visualizations
* Daily ridership trends with confidence intervals
* Summary statistics of ride counts per income group
* Threshold and cross-section analyses
* Regression model results with income and rush-hour effects
* Monte Carlo simulations for predicted ridership and confidence intervals

---

## Requirements

* R ≥ 4.2
* Required packages: see above
* Data folders:

```
data/social_infra/
data/boston_social_infra/
data/bluebikes/
```

---

## How to Run

1. Place the `data/` folder in your working directory.
2. Run the R script sequentially to perform spatial, temporal, and simulation analyses.
3. Review visualizations and tables for insights into station distribution and ridership trends.

---

## Notes

* High-income neighborhoods: >50% population earns > $60,000/year
* Station density computed as: `# stations / (pop_density * total_area / 1000)`
* Confidence intervals calculated as `mean ± 1.96 × SE`
* Simulated outcomes assume normal residual error distribution

