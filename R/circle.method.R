#' Mangrove Fixed-Radius Plot Method Analysis
#'
#' This function allows you to estimate mangrove forest structure based on sampling using fixed-radius plot sampling.
#' @param plotnumber Column name in data frame for plot number (numerical). Default name is "plotnumber". First plot number must be 1.
#' @param dbh Column name in data frame for diameter at breast height. Default name is "dbh". Values must be in centimeters.
#' @param species Column name in data frame for species. Default name is "species".
#' @param height Column name in data frame for height (Optional). Default name is "height". Values must be in meters. When included, additional height-related outputs are displayed.
#' @param plot.radius Plot radius measured in meters. Default is 5 meters.
#' @keywords mangrove structure, plot
#' @examples
#' circle.method(mangrove_data)
#' circle.method(mangrove_data, species="Species", plot.radius=5, plotnumber = "Plot", dbh="Diameter", height="Tree.Height")
#' @export

# Function to analyze forest data using the Fixed-Radius Plot Method
circle.method<-function(x,
                      plotnumber = 'plotnumber',
                      dbh = 'dbh',
                      species = 'species',
                      height = 'height',
                      plot.radius=5){
  
  #Confirm it is a data frame
  x <- as.data.frame(x)
  
  #Load columns
  x$PlotNumber <- x[,plotnumber]
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
  
  # Plot calculations
  radius = plot.radius
  area = round((3.14159265359 * radius ^ 2), digits=2)
  x$areasum = area*length(unique(x$PlotNumber))
  x$count <- 1:nrow(x)
  x$rows <- max(x$count)
  
  
  
  cat("\n ---------------------------------------------------")
  cat("\n Fixed-Radius Plot Method data analysis summary")
  cat("\n ---------------------------------------------------\n\n")
  cat(paste(" Number of plots =", length(unique(x$PlotNumber))))
  cat(paste(",   Total number of trees =", max(x$rows)))
  cat(paste("\n Area of each plot =", area, "m^2"))
  cat(paste(",   Sum of all plot areas =", max(x$areasum), "m^2"))
  
  # Print height metrics
  if("height" %in% colnames(x)) cat("\n\n HEIGHT METRICS\n -----\n")
  if("height" %in% colnames(x)) print.noquote(species_height_output, row.names = FALSE)
  if("height" %in% colnames(x)) cat(paste("Forest canopy height =", canopy))
            
  # Print DBH metrics
  cat("\n\n DBH METRICS\n -----\n")
  print.noquote(species_dbh_output, row.names = FALSE)
  
  # Density summaries
  cat("\n DENSITY COMPUTATION\n -----\n")
  x$static <- 1
  densum <- plyr::ddply(x, "Species", summarize, Number = length(count), Proportion = round(length(count)/max(rows), digits=2), areasum=max(areasum))
  densum1 <- plyr::ddply(densum, "Species", transform, Stems_Per_0.1_Ha = round((Number/areasum) * 1000))
  myvarsden <- c("Species", "Number","Proportion", "Stems_Per_0.1_Ha")
  densum2 <- densum1[myvarsden]
  print.noquote(densum2, row.names = FALSE)
  cat(paste(" Total density per 0.1 ha =", sum(densum1$Stems_Per_0.1_Ha)))
  
  
  # Basal area summaries
  cat("\n\n BASAL AREA COMPUTATION\n -----\n")
  x$BA <- (3.14159265359 * (x$dbh/2) ^ 2) / 10000
  densumBA <- plyr::ddply(x, "Species", summarize, Mean_BA = round(mean(BA), digits=4))
  densumBA <- plyr::join(densum2, densumBA, by="Species")
  densumBA$Total_BA <- densumBA$Stems_Per_0.1_Ha * densumBA$Mean_BA
  totbas <- sum(densumBA$Total_BA)
  myvars <- c("Species", "Mean_BA","Total_BA")
  densumBA2 <- densumBA[myvars]
  densumBA2 <- densumBA2[order(-densumBA2$Total_BA),]
  densumBA2$Rank <- 1:nrow(densumBA2)
  densumBA2 <- densumBA2[order(densumBA2$Species),]
  print.noquote(densumBA2, row.names = FALSE)
  cat(paste(" Total basal area in m^2 per 0.1 ha =", totbas))
  
  
  # Absolute frequency
  cat("\n\n ABSOLUTE FREQUENCY COMPUTATION\n -----\n")
  freqsum <- plyr::ddply(x, "static", transform, sum.n = length(unique(PlotNumber)))
  freqsum2 <- plyr::ddply(freqsum, "Species", summarize, Number = length(unique(PlotNumber)), Frequency = length(unique(PlotNumber)) / max(sum.n))
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
  freqsum2 <- plyr::ddply(freqsum, "Species", summarize, Number = length(unique(PlotNumber)), Frequency = length(unique(PlotNumber)) / max(sum.n)*100)
  freqsum2$Relative_Frequency <- round(freqsum2$Frequency / totalfreq *100, digits=1)
  compare <- plyr::join(compare, freqsum2, by="Species")
  compare$Relative_Dominance <- compare$Total_BA/totbas*100
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
  return(output)

}

