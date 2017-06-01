# Mangrove forest structure package in R #

## Description ##
Set of tools to calculate mangrove forest structure using either the Point-Centered Quarter Method or fixed-area sampling methods. Outputs include density, diameter, basal area, height, as well as relative comparisons of density, dominance, frequency, and importance value (IV). Output also includes functions for common structural indices [e.g., Holdridge Complexity Index (Holdridge 1967) and Mean Stand Diameter (Cintrón and Schaeffer Novelli 1984)] and visual representations of relative values and canopy height.

----
## Installation ##

### Using devtools to install ###
Type the following into R
```
install.packages('devtools')
devtools::install_github('gshideler/mangroveStructure')
```
You can skip the first line if you already have devtools installed.

----

## Overview of package functions ##
A column with height measurements is required for canopy.plot(), for all others it is optional. If included, height-related metrics are displayed for each function.

Functions will not compute if missing values (NAs) are present. 

#### pcqm.method() ####
This function allows you to estimate mangrove forest structure based on sampling using the Point-Centered Quarter Method (PCQM) (Cottam and Curtis 1956). Following Cintrón and Schaeffer Novelli (1984): (1) an individual tree must be present in each quarter, and (2) a single tree should not be measured twice. 

#### pcqm.indices() ####
This function allows you to calculate the Holdridge Complexity Index (Holdridge 1967) and Mean Stand Diameter (Cintrón and Schaeffer Novelli 1984) based on sampling using the Point-Centered Quarter Method (PCQM). Following Cintrón and Schaeffer Novelli (1984): (1) an individual tree must be present in each quarter, and (2) a single tree should not be measured twice. Height is required for the Holdridge Complexity Index.

#### canopy.profile() ####
This function allows you to plot canopy height across distance from Point-Centered Quarter Method transect data. Following Cintrón and Schaeffer Novelli (1984): (1) an individual tree must be present in each quarter, and (2) a single tree should not be measured twice.

#### plot.method() ####
This function allows you to estimate mangrove forest structure based on fixed-area sampling (plot). 

#### plot.indices() ####
This function allows you to calculate the Holdridge Complexity Index (Holdridge 1967) and Mean Stand Diameter (Cintrón and Schaeffer Novelli 1984) using fixed-area sampling (plot). Height is required for the Holdridge Complexity Index.

#### iv.plot() ####
This function allows you to create a radar plot using data object created from either pcqm.method() or plot.method() using the fmsb package.

----

## Use examples ##
#### Load the necessary libraries ####
Note: the RCurl library is to allow for downloading of csv files from the github server, which are pulled for the examples below.
```
library(mangroveStructure)
library(RCurl)
```
#### Compute structure for point-centered transects (PCQM) ####
```
pcqm_data <- read.csv(text=getURL("https://raw.githubusercontent.com/gshideler/mangroveStructure/master/testdata/pcqm_data.csv"), header=TRUE)
 
r1 <- pcqm.method(pcqm_data)
iv.plot(r1)
pcqm.indices(pcqm_data, ageplot=TRUE)
canopy.profile(pcqm_data)
```
If your column names are different from the defaults, you can specify them using the appropriate argument. See package help files for more information and for all defaults. This applies to all functions in the package.

Example:
```
pcqm.method(pcqm_data, samplingpoint = "Sampling_Point", dbh = "Diameter")

```
#### Compute structure for fixed-area sampling plots ####
```
plot_data <- read.csv(text=getURL("https://raw.githubusercontent.com/gshideler/mangroveStructure/master/testdata/plot_data.csv"), header=TRUE)

r2 <- plot.method(plot_data)
iv.plot(r2)
plot.indices(plot_data, ageplot=TRUE)
```

----
## License ##
Use of <strong>mangroveStructure</strong> is covered under the open source "MIT License".

See <a href = "https://raw.githubusercontent.com/gshideler/mangroveStructure/master/LICENSE">License</a> page for more details.

----
## References ##
Cintrón G, Shaeffer Novelli Y. 1984. Methods for studying mangrove structure. In: Snedaker SC, Snedaker JG. (eds.) The mangrove ecosystem: research methods. Unesco. 251 p.

Cottam G, Curtis JT. 1956. The use of distance measures in phytosociological sampling. Ecology. 37:451–460.

Holdridge LR. 1967. Life zone ecology. Tropical Science Center. San José, Costa Rica. 206 p.
