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

## Compute structure for point-centered transects
test_pcqm <-

## Compute structure for fixed-area sampling plots
test_plot <-
