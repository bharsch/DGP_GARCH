# GARCH Model Simulation Shiny App
## Overview
This Shiny app allows users to simulate GARCH return data using different GARCH variants and distribution models. Users can customize model parameters and visualize simulated return series and conditional variance.

## Features
* Supports sGARCH, EGARCH, and GJR-GARCH models.
* Allows selection of Normal, Student's t, and Skewed Student's t distributions.
* Interactive sliders for omega, alpha, beta, gamma (EGARCH/GJR-GARCH), shape (t-distribution), and skew (Skewed Student Distribution).
* Real-time plots for simulated return series and conditional variance distribution.
* Summary statistics displayed in a table.

## Installation
Ensure you have R and the necessary packages installed:
```bash
install.packages(c("shiny", "rugarch", "shinythemes", "bslib"))
```
Then, run the app using:
```bash
shiny::runApp("path/to/app_directory")
```
## Usage
* Select a GARCH model (sGARCH, EGARCH, or GJR-GARCH).
* Choose a distribution (Normal, Student’s t, Skewed Student Distribution).
* Adjust parameters using the sliders.
* View real-time updates of plots and summary statistics.

## Troubleshooting
* If the app crashes when selecting GJR-GARCH, ensure that stationarity condition holds.
* If sliders do not appear, check that the correct model and distribution settings are selected.

## Author
Developed by Benjamin Harsch. Contributions and feedback are welcome!
