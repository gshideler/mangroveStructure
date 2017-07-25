#' Mangrove Indices for Point-Centered Quarter Method
#'
#' This function allows you to calculate the Holdridge Complexity Index and Mean Stand Diameter for pcqm data.
#' @param samplingpoint Column name in data frame for sampling points (numerical). Default name is "samplingpoint". First sampling point must be 1.
#' @param dist Column name in data frame for distance from tree to sampling point. Default name is "dist". Values must be in meters.
#' @param species Column name in data frame for species. Default name is "species".
#' @param dbh Column name in data frame for diameter at breast height. Default name is "dbh". Values must be in centimeters.
#' @param height Column name in data frame for height (Optional). Default name is "height". Values must be in meters. When included, additional height-related outputs are displayed.
#' @param sizebin Logical argument for displaying size class plots (Default is FALSE). If sizebin=TRUE, plots of size class proportions are displayed for dbh (<5 cm dbh, 5–10 cm dbh, and >10 cm dbh) and height (<5 m, 5–10 m, and >10 m).
#' @keywords mangrove structure, pcqm, Holdridge Complexity Index, Mean Stand Diameter
#' @examples
#' pcqm.indices(mangrove_data)
#' pcqm.indices(mangrove_data, dbh = "Diameter", height = "Tree_Height", samplingpoint = "Sampling_Point", sizebin = T)
#' @export
pcqm.indices <- function(x, 
                         samplingpoint = 'samplingpoint',
                         dist = 'dist',
                         species = 'species',
                         dbh = 'dbh',
                         height = 'height',
                         sizebin = F){
  
  #Confirm it is a data frame
  x <- as.data.frame(x)
  
  #Load columns
  x$SamplingPoint <- x[,samplingpoint]
  x$Distance <- x[,dist]
  x$Species <- x[,species]
  x$dbh <- x[,dbh]
  if("height" %in% colnames(x)) x$height <- x[,height]
  
  ## Get the functions from the plyr namespace
  summarize = get("summarize", asNamespace('plyr'))
  transform = get("transform", asNamespace('plyr'))
  
  #Check for NA cells
  if(any(is.na(x)) == TRUE) stop("Data frame cannot contain missing values (NAs).")
  
  # Pre calculations for later use
  spp <- length(unique(x$Species))
  x$count <- 1:nrow(x)

  
  cat("\n ---------------------------------------------------")
  cat("\n Point-Centered Quarter Method Indices")
  cat("\n ---------------------------------------------------\n\n")

  # Calculate Density
  x$static <- 1
  densum <- plyr::ddply(x, "static", transform, sum.n = max(count), meandist= mean(Distance), stemcalc=((1/(mean(Distance)^2))*1000))
  densum1 <- plyr::ddply(densum, "Species", summarize, Number = length(static), Proportion = (length(static) / max(sum.n)), stemcalc=max(stemcalc))
  densum2 <- plyr::ddply(densum1, "Species", transform, Stems_Per_0.1_Ha = round(Proportion * stemcalc))
  totden <- sum(densum2$Stems_Per_0.1_Ha)
  
  # Calculate Basal Area
  densum$BA <- (3.14159265359 * (densum$dbh/2) ^ 2) / 10000
  densumBA <- plyr::ddply(densum, "Species", summarize, MeanBA = (mean(BA)))
  densumBA <- plyr::join(densum2, densumBA, by="Species")
  densumBA$Basal_Area <- densumBA$Stems_Per_0.1_Ha * densumBA$MeanBA
  totbas <- sum(densumBA$Basal_Area)
  
  # Get height of three tallest trees
  if("height" %in% colnames(x)) x <- x[order(-x$height),]
  if("height" %in% colnames(x)) x2 <- head(x,3)
  if("height" %in% colnames(x)) heightcalc <- mean(x2$height)
  
  # Calculate Holdridge Complexity Index
  if("height" %in% colnames(x)) CI <- round((heightcalc * totbas * totden * spp) / 1000, digits=2)
  
  # Print the Output from HCI
  if("height" %in% colnames(x)) cat(" Complexity Index\n -----\n")
  if("height" %in% colnames(x)) cat(paste(" ", CI))
  if("height" %in% colnames(x)) cat("\n\n")
  
  # Calculate Mean Stand Diameter
  MSD <- round(sqrt((totbas * 12732.39)/totden), digits=2)
  
  # Print output from MSD
  cat(" Mean Stand Diameter\n -----\n")
  cat(paste(" ", MSD,"\n\n"))
 
  if(sizebin==T){
    # Plot of dbh proportions
    x$size <- "<5 cm"
    x$size[x$dbh >=5 & x$dbh<=10] <- "5–10 cm"
    x$size[x$dbh > 10] <- ">10 cm"
    x$sizefac <- factor(x$size, levels= c("<5 cm", "5–10 cm", ">10 cm"))
    plotsize <- table(x$sizefac)
    par(ask = TRUE)
    barplot(plotsize / sum(plotsize), ylab="Proportion of forest trees", xlab="Tree diameter size class", cex.lab = 1.2, cex.names=1.1, cex.axis = 1.1, ylim=c(0,1), col = c("burlywood3", "tan3", "saddlebrown"))
   
    # Plot of height proportions
    x$size2 <- "<5 m"
    x$size2[x$height >=5 & x$height<=10] <- "5–10 m"
    x$size2[x$height > 10] <- ">10 cm"
    x$sizefac2 <- factor(x$size2, levels= c("<5 m", "5–10 m", ">10 m"))
    plotsize2 <- table(x$sizefac2)
    barplot(plotsize2 / sum(plotsize2), ylab="Proportion of forest trees", xlab="Tree height size class", cex.lab = 1.2, cex.names=1.1, cex.axis = 1.1, ylim=c(0,1), col = c("darkolivegreen3", "forestgreen", "darkgreen"))
  
    #Scatterplot of dbh vs height
    plot(x$dbh, x$height, pch=16, color=x$size, xlab="Tree DBH", ylab="Tree Height", cex.lab = 1.2, cex.names=1.1, cex.axis = 1.1, col = c("burlywood3", "tan3", "saddlebrown"))
  }
  }
  
  
