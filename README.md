# Mangrove forest structure package in R #

## Description ##
Set of tools to calculate mangrove forest structure using either the Point-Centered Quarter Method or fixed-area sampling methods. Outputs include density, diameter, basal area, height, as well as relative comparisons of density, dominance, frequency and importance value (IV). Output also includes functions for common structural indices [e.g., Holdridge Complexity Index (Holdridge 1967) and Mean Stand Diameter (Cintrón and Schaeffer Novelli 1984)] and visual representations of relative values and canopy height.

----
## Installation ##

### Using devtools ###
Type the following into R
```
install.packages('devtools')
devtools::install_github('gshideler/mangroveStructure')
```
You can skip the first line if you already have devtools installed.

----

### Use examples ###
```
library(mangroveStructure)
library(RCurl)

## Compute structure for point-centered transects
pcqm_temp <- getURL("https://github.com/gshideler/mangroveStructure/tree/master/testdata/pcqm_data.csv")
pcqm_data <- read.csv(text = pcqm_temp)

pcqm.method(pcqm_data, ivplot=TRUE)
pcqm.indices(pcqm_data, ageplot=TRUE)
canopy.profile(pcqm_data)

## Compute structure for fixed-area sampling plots
plot_temp <- getURL("https://github.com/gshideler/mangroveStructure/tree/master/testdata/plot_data.csv")
plot_data <- read.csv(text = plot_temp)

plot.method(plot_data, ivplot=TRUE)
plot.indices(plot_data, ageplot=TRUE)
