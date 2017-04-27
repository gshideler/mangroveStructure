#' Mangrove Indices for Point-Centered Quarter Method
#'
#' This function allows you to calculate the Holdridge Complexity Index and Mean Stand Diameter for pcqm data.
#' @param samplingpoint Column name in dataframe for sampling points (numerical). Default name is "samplingpoint". First sampling point must be 1.
#' @param dist Column name in dataframe for distance from tree to sampling point. Default name is "dist". Values must be in meters.
#' @param species Column name in dataframe for species. Default name is "species".
#' @param dbh Column name in dataframe for diameter at breast height. Default name is "dbh". Values must be in centimeters.
#' @param height Column name in dataframe for height. Default name is "height". Values must be in meters.
#' @param ageplot Logical argument for displaying size class plot (Default is FALSE). If ageplot=TRUE, plot of size class proportions is displayed: seedling (<5 cm dbh), sapling (5-10 cm dbh), and pole (>10 cm dbh).
#' @keywords mangrove structure, pcqm, Holdridge Complexity Index, Mean Stand Diameter
#' @examples
#' pcqm.indices(mangrove_data)
#' pcqm.indices(mangrove_data, dbh = "Diameter", height = "Tree_Height", samplingpoint = "Sampling_Point", ageplot = T)
#' @export
pcqm.indices <- function(x, 
                         samplingpoint = 'samplingpoint',
                         dist = 'dist',
                         species = 'species',
                         dbh = 'dbh',
                         height = 'height',
                         ageplot = F){
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
  if("height" %in% colnames(x)) cat(" Holdridge Complexity Index\n -----\n")
  if("height" %in% colnames(x)) cat(paste(" ", CI))
  if("height" %in% colnames(x)) cat("\n\n")
  
  # Calculate Mean Stand Diameter
  MSD <- round(sqrt((totbas * 12732.39)/totden), digits=2)
  
  # Print output from MSD
  cat(" Mean Stand Diameter\n -----\n")
  cat(paste(" ", MSD))
 
  if(ageplot==T){
    # Plot of forest size class proportions
    x$size <- "Seedling"
    x$size[x$dbh >=5 & x$dbh<=10] <- "Sapling"
    x$size[x$dbh > 10] <- "Pole"
    x$sizefac <- factor(x$size, levels= c("Seedling", "Sapling", "Pole"))
    plotsize <- table(x$sizefac)
    barplot(plotsize / sum(plotsize), ylab="Proportion of forest trees", xlab="Tree size class", cex.lab = 1.2, cex.names=1.1, cex.axis = 1.1, ylim=c(0,1), col = c("darkolivegreen3", "forestgreen", "darkgreen"))
  }
  }
  
  
