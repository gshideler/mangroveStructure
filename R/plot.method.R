#' Mangrove Plot Method Analysis
#'
#' This function allows you to estimate mangrove forest structure based on sampling using the Point-Centered Quarter Method (PCQM). See Cottam and Curtis (1956) for informaiton on PCQM.
#' @param plotnumber Column name in data frame for plot nymber (numerical). Default name is "plotnumber". First plot number must be 1.
#' @param dbh Column name in data frame for diameter at breast height. Default name is "dbh". Values must be in centimeters.
#' @param species Column name in data frame for species. Default name is "species".
#' @param height Column name in data frame for height (Optional). Default name is "height". Values must be in meters. When included, additional height-related outputs are displayed.
#' @param plot.width Plot width measured in meters. Default is 10 meters.
#' @param plot.length Plot length measured in meters. Default is 10 meters.
#' @param ivplot Logical argument for whether a radar plot of species importance values is displayed. If ivplot=TRUE, the top five species (ranked by importance values) will be plotted via radarchart() from package 'fmsb'.
#' @keywords mangrove structure, plot
#' @examples
#' plot.method(mangrove_data)
#' plot.method(mangrove_data, species="Species", plot.width=5, plotnumber = "Plot", dbh="Diameter", height="Tree.Height", ivplot=T)
#' @export

# Function to analyze forest data using the Plot Method
plot.method<-function(x,
                      plotnumber = 'plotnumber',
                      dbh = 'dbh',
                      species = 'species',
                      height = 'height',
                      plot.width=10,
                      plot.length=10,
                      ivplot = FALSE){
  
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
  if("height" %in% colnames(x)) avgheight <- round(mean(x$height), digits=2)
  if("height" %in% colnames(x)) sdheight <- round(sd(x$height), digits=2)
  if("height" %in% colnames(x)) heightmin <- min(x$height)
  if("height" %in% colnames(x)) heightmax <- max(x$height)
  
  # Canopy height
  if("height" %in% colnames(x)) x <- x[order(-x$height),]
  if("height" %in% colnames(x)) x2 <- head(x,3)
  if("height" %in% colnames(x)) canopy <- mean(x2$height)
  
  # Get DBH of trees, with mean, min, max
  avgdbh <- round(mean(x$dbh), digits=2)
  sddbh <- round(sd(x$dbh), digits=2)
  dbhmin <- min(x$dbh)
  dbhmax <- max(x$dbh)
  
  # Plot calculations
  width = plot.width
  length = plot.length
  area = plot.length*plot.width
  x$areasum = area*length(unique(x$PlotNumber))
  x$count <- 1:nrow(x)
  x$rows <- max(x$count)
  
  
  
  cat("\n ---------------------------------------------------")
  cat("\n Plot Method data analysis summary")
  cat("\n ---------------------------------------------------\n\n")
  cat(paste(" Number of plots =", length(unique(x$PlotNumber))))
  cat(paste(",   Total number of trees =", max(x$rows)))
  cat(paste("\n Area of each plot =", area, "m^2"))
  cat(paste(",   Sum of all plot areas =", max(x$areasum), "m^2"))
  
  # Height metrics
  if("height" %in% colnames(x)) cat(paste("\n\n Height metrics:"))
  if("height" %in% colnames(x)) cat(paste("\n Canopy height =", canopy))
  if("height" %in% colnames(x)) cat(paste(",   Mean tree height =", avgheight, "("))
  if("height" %in% colnames(x)) cat(paste("SD", sdheight))
  if("height" %in% colnames(x)) cat(paste(")"))
  if("height" %in% colnames(x)) cat(paste("\n Min height =", heightmin))
  if("height" %in% colnames(x)) cat(paste(",   Max height =", heightmax))
  
  # DBH metrics
  cat(paste("\n\n DBH metrics:"))
  cat(paste("\n Mean DBH =", avgdbh, "("))
  cat(paste("SD", sddbh))
  cat(paste(")"))
  cat(paste("\n Min DBH =", dbhmin))
  cat(paste(",   Max DBH =", dbhmax))
  cat("\n")
  
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
  densumBA <- plyr::ddply(x, "Species", summarize, MeanBA = round(mean(BA), digits=4))
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
  compare$Relative_Dominance <- compare$Basal_Area/totbas*100
  compare$Relative_Dominance <- round(compare$Relative_Dominance, digits=1)
  compare <- plyr::ddply(compare, "Species", transform, Importance_Value = sum(Relative_Density+Relative_Dominance+Relative_Frequency))
  compare <- compare[order(-compare$Importance_Value),]
  compare$Rank <- 1:nrow(compare)
  compare <- compare[order(compare$Species),]
  myvars <- c("Species", "Relative_Density", "Relative_Dominance", "Relative_Frequency", "Importance_Value", "Rank")
  relative <- compare[myvars]
  relative$Relative_Density <- paste(relative$Relative_Density, "%")
  relative$Relative_Frequency <- paste(relative$Relative_Frequency, "%")
  relative$Relative_Dominance <- paste(relative$Relative_Dominance, "%")
  print.noquote(relative, row.names = FALSE)
  
  if(ivplot == TRUE){
    # PLOT RELATIVE IMPORATNCE DATA
    compare2 <- compare[order(compare$Species),]
    compare2 <- subset(compare2, compare2$Rank<6)
    MX <- data.frame(100, 100, 100)
    names(MX) <- c("Rel_Dom", "Rel_Den", "Rel_Fre")
    MN <- data.frame(0, 0, 0)
    names(MN) <- c("Rel_Dom", "Rel_Den", "Rel_Fre")
    Rel_Dom <- data.frame(compare2$Relative_Dominance)
    names(Rel_Dom)<-"Rel_Dom"
    Rel_Den <- data.frame(compare2$Relative_Density)
    names(Rel_Den)<-"Rel_Den"
    Rel_Fre <- data.frame(compare2$Relative_Frequency)
    names(Rel_Fre) <- "Rel_Fre"
    
    plotdata <- cbind(Rel_Dom, Rel_Fre, Rel_Den)
    plotdata2 <- rbind(MX, MN, plotdata)
    
    colors_border=c( "firebrick", "black" , "dodgerblue", "goldenrod1", "lightgreen" )
    colors_in=c( "firebrick", "black" , "dodgerblue", "goldenrod1", "lightgreen" )
    speclist <- compare2$Species
    
    # Plot radarChart
    fmsb::radarchart(plotdata2, maxmin=TRUE,
               axistype=1 , centerzero = TRUE, vlabels=c("Relative\ndominance (%)", "Relative\ndensity (%)", "Relative\nfrequency (%)"),
               pcol=colors_border,  plwd=4 , plty=1, 
               cglcol="grey", cglty=1, axislabcol="black", seg=5, caxislabels=seq(0,100,20), cglwd=0.8)
    legend(.65, .8, legend = speclist, col=colors_in, title = "Species", seg.len = 2,  pch = 16, lwd=4,lty = 1, bty="n",box.col="grey")
  }
}


