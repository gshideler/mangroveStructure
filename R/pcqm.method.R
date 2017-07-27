#' Mangrove Point-Centered Quarter Method Analysis
#'
#' This function allows you to estimate mangrove forest structure based on sampling using the Point-Centered Quarter Method (PCQM). See Cottam and Curtis (1956) for informaiton on PCQM.
#' @param samplingpoint Column name in data frame for sampling points (numerical). Default name is "samplingpoint". First sampling point must be 1.
#' @param dist Column name in data frame for distance from tree to sampling point. Default name is "dist". Values must be in meters.
#' @param species Column name in data frame for species. Default name is "species".
#' @param dbh Column name in data frame for diameter at breast height. Default name is "dbh". Values must be in centimeters.
#' @param height Column name in data frame for height (Optional). Default name is "height". Values must be in meters. When included, additional height-related outputs are displayed.
#' @keywords mangrove structure, pcqm
#' @examples
#' pcqm.method(mangrove_data)
#' pcqm.method(mangrove_data, dbh = "Diameter", height = "Tree_Height", samplingpoint = "Sampling_Point")
#' @export

# Function to analyze forest data using the Point-Centered Quarter Method
pcqm.method <- function(x, 
                 samplingpoint = 'samplingpoint',
                 dist = 'dist',
                 species = 'species',
                 dbh = 'dbh',
                 height = 'height'){
  
   #Confirm it is a data frame
  x <- as.data.frame(x)
  
  #Load columns
  x$SamplingPoint <- x[,samplingpoint]
  x$Distance <- x[,dist]
  x$Species <- x[,species]
  x$dbh <- x[,dbh]
  if("height" %in% colnames(x)) x$height <- x[,height]
  
 
  
  # Get the summarize and transform functions from the plyr namespace
  summarize = get("summarize", asNamespace('plyr'))
  transform = get("transform", asNamespace('plyr'))
  
  # Get the radarchart function from the fmsb namespace
  radarchart = get("radarchart", asNamespace('fmsb'))
  
  #Check for NA cells
  if(any(is.na(x)) == TRUE) stop("Data frame cannot contain missing values (NAs).")
    
  # Get unique number of species
  spcount <- length(unique(x$Species))
  
  # Get height of trees, with mean, min, max; and of three tallest trees for canopy height
  if("height" %in% colnames(x)) species_height <- plyr::ddply(x, "Species", summarize, Number = length(height), Mean = round(mean(height), digits=2), SD = round(sd(height), digits=2), Minimum = min(height), Maximum= max(height))
  if("height" %in% colnames(x)) height_totals <- data.frame(Species = "Total", Number = length(x$height), Mean = round(mean(x$height), digits=2), SD = round(sd(x$height), digits=2), Minimum = min(x$height), Maximum = max(x$height))
  if("height" %in% colnames(x)) species_height_output <- rbind(species_height, height_totals) 
  
  # Canopy height
  if("height" %in% colnames(x)) x <- x[order(-x$height),]
  if("height" %in% colnames(x)) x2 <- head(x,3)
  if("height" %in% colnames(x)) canopy <- round(mean(x2$height), digits=2)
  
  # Get DBH of trees, with mean, min, max
  species_dbh <- plyr::ddply(x, "Species", summarize, Number = length(dbh), Mean = round(mean(dbh), digits=2), SD = round(sd(dbh), digits=2), Minimum = min(dbh), Maximum = max(dbh))                                                        
  dbh_totals <- data.frame(Species = "Total", Number = length(x$dbh), Mean = round(mean(x$dbh), digits=2), SD = round(sd(x$dbh), digits=2), Minimum = min(x$dbh), Maximum = max(x$dbh))
  species_dbh_output <- rbind(species_dbh, dbh_totals) 
  
            # Distance summaries
            Mdistance <- round(mean(x$Distance),digits=2)
            Tdistance <- round(sum(x$Distance), digits=2)
            x$count <- 1:nrow(x)
  
            rows <- max(x$count)
            points <- length(unique(x$SamplingPoint))
             
            # Print header outputs
            cat("\n ---------------------------------------------------")
            cat("\n Point-Centered Quarter Method data analysis summary")
            cat("\n ---------------------------------------------------\n\n")
            cat(paste(" Sampling points =", length(unique(x$SamplingPoint))))
            cat(paste("\n Number of species =", spcount))
            cat(paste(",   Total number of trees =", rows))
            cat(paste("\n Total distance =", Tdistance))
            cat(paste(",   Mean distance =", Mdistance))
            
            # Print height metrics
            if("height" %in% colnames(x)) cat("\n\n HEIGHT METRICS\n -----\n")
            if("height" %in% colnames(x)) print.noquote(species_height_output, row.names = FALSE)
            if("height" %in% colnames(x)) cat(paste("Forest canopy height =", canopy))
            
            # Print DBH metrics
            cat("\n\n DBH METRICS\n -----\n")
            print.noquote(species_dbh_output, row.names = FALSE)

            # Calculate and print density summaries
            cat("\n\n DENSITY COMPUTATION\n -----\n")
            x$static <- 1
            densum <- plyr::ddply(x, "static", transform, sum.n = max(count), meandist= mean(Distance), stemcalc=round((1/(mean(Distance)^2))*1000, digits=1))
            densum1 <- plyr::ddply(densum, "Species", summarize, Number = length(static), Proportion = round(length(static) / max(sum.n), digits=2), stemcalc=max(stemcalc))
            densum2 <- plyr::ddply(densum1, "Species", transform, Stems_Per_0.1_Ha = round(Proportion * stemcalc))
            myvarsden <- c("Species", "Number","Proportion", "Stems_Per_0.1_Ha")
            densum3 <- densum2[myvarsden]
            print.noquote(densum3, row.names = FALSE)
            cat(paste(" Total density per 0.1 ha =", sum(densum2$Stems_Per_0.1_Ha)))

            # Calculate and print basal area summaries
            cat("\n\n BASAL AREA COMPUTATION\n -----\n")
            densum$BA <- (3.14159265359 * (densum$dbh/2) ^ 2) / 10000
            densumBA <- plyr::ddply(densum, "Species", summarize, MeanBA = round(mean(BA), digits=4))
            densumBA <- plyr::join(densum2, densumBA, by="Species")
            densumBA$Basal_Area <- densumBA$Stems_Per_0.1_Ha * densumBA$MeanBA
            totbas <- sum(densumBA$Basal_Area)
            myvars <- c("Species", "MeanBA","Basal_Area")
            densumBA2 <- densumBA[myvars]
            densumBA2 <- densumBA2[order(-densumBA2$Basal_Area),]
            densumBA2$Rank <- 1:nrow(densumBA2)
            densumBA2 <- densumBA2[order(densumBA2$Species),]
            print.noquote(densumBA2, row.names = FALSE)
            cat(paste(" Total basal area in m^2 per 0.1 ha =", totbas))


            # Calculate and print absolute frequency
            cat("\n\n ABSOLUTE FREQUENCY COMPUTATION\n -----\n")
            freqsum <- plyr::ddply(x, "static", transform, sum.n = max(SamplingPoint))
            freqsum2 <- plyr::ddply(freqsum, "Species", summarize, Number = length(unique(SamplingPoint)), Frequency = length(unique(SamplingPoint)) / max(sum.n))
            freqsum2$Frequency <- round(freqsum2$Frequency*100, digits=2)
            totalfreq <-round(sum(freqsum2$Frequency), digits=2)
            freqsum2$Frequency <- paste(freqsum2$Frequency, "%")
            print.noquote(freqsum2, row.names = FALSE)
            cat(paste(" Total frequency =", totalfreq, "%"))


            # Relative comparisons
            cat("\n\n RELATIVE COMPUTATIONS\n -----\n")
            densum2$totspec <- sum(densum2$Stems_Per_0.1_Ha)
            densum2$Relative_Density <- round((densum2$Stems_Per_0.1_Ha / densum2$totspec) * 100, digits=1)
            compare <- plyr::join(densum2, densumBA2, by="Species")
            freqsum2 <- plyr::ddply(freqsum, "Species", summarize, Number = length(unique(SamplingPoint)), Frequency = length(unique(SamplingPoint)) / max(sum.n)*100)
            freqsum2$Relative_Frequency <- round(freqsum2$Frequency / totalfreq *100, digits=1)
            compare <- plyr::join(compare, freqsum2, by="Species")
            compare$Relative_Dominance <- compare$Basal_Area/totbas*100
            compare$Relative_Dominance <- round(compare$Relative_Dominance, digits=1)
            compare <- plyr::ddply(compare, "Species", transform, Importance_Value = sum(Relative_Density+Relative_Dominance+Relative_Frequency))
            compare <- compare[order(-compare$Importance_Value),]
            compare$Rank <- 1:nrow(compare)
            compare <- compare[order(compare$Species),]
            myvars <- c("Species", "Relative_Density", "Relative_Dominance", "Relative_Frequency", "Importance_Value", "Rank")
            relative <- compare[myvars]
            output <- relative
            relative$Relative_Density <- paste(relative$Relative_Density, "%")
            relative$Relative_Frequency <- paste(relative$Relative_Frequency, "%")
            relative$Relative_Dominance <- paste(relative$Relative_Dominance, "%")
            print.noquote(relative, row.names = FALSE)
                       
            #Warning if sampling points are missing quarters
            quartercheck <- round(rows/points, digits=5.0000)
            if(quartercheck != 4.00000) warning("Sampling points missing quarters. Results may not be reliable.")
            
            return(output)
}


