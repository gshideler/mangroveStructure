# Mangrove forest structure package in R #

## Description ##
Set of tools to calculate mangrove forest structure using either the Point-Centered Quarter Method or fixed-area sampling methods. Outputs include density, diameter, basal area, height, as well as relative comparisons of density, dominance, frequency and importance value (IV). Output also includes functions for common structural indices [e.g., Holdridge Complexity Index (Holdridge 1967) and Mean Stand Diameter (Cintrón and Schaeffer Novelli 1984)] and visual representations of relative values and canopy height.

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

## Use examples ##
```
library(mangroveStructure)
library(RCurl) #This is a library to allow the downloading of csv files from github for use examples

## Compute structure for point-centered transects
pcqm_data <- read.csv(text=getURL("https://raw.githubusercontent.com/gshideler/mangroveStructure/master/testdata/pcqm_data.csv"), header=TRUE)
 
pcqm.method(pcqm_data, ivplot=TRUE)
pcqm.indices(pcqm_data, ageplot=TRUE)
canopy.profile(pcqm_data)

## Compute structure for fixed-area sampling plots
plot_data <- read.csv(text=getURL("https://raw.githubusercontent.com/gshideler/mangroveStructure/master/testdata/plot_data.csv"), header=TRUE)

plot.method(plot_data, ivplot=TRUE)
plot.indices(plot_data, ageplot=TRUE)
```
----

## References ##
Cintrón G, Shaeffer Novelli Y. 1984. Methods for studying mangrove structure. In: Snedaker SC, Snedaker JG. (eds.) The mangrove ecosystem: research methods. Unesco. 251 p.

Cottam G, Curtis JT. 1956. The use of distance measures in phytosociological sampling. Ecology. 37:451–460.

Holdridge LR. 1967. Life zone ecology. Tropical Science Center. San José, Costa Rica. 206 p.
