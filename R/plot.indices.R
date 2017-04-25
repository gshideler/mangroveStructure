#' Mangrove Indices for Plot Method
#'
#' This function allows you to calculate the Holdridge Complexity Index and Mean Stand Diameter for pcqm data.
#' @param plotnumber Column name in dataframe for plot nymber (numerical). Default name is "plotnumber". First plot number must be 1.
#' @param dbh Column name in dataframe for diameter at breast height. Default name is "dbh". Values must be in centimeters.
#' @param species Column name in dataframe for species. Default name is "species".
#' @param height Column name in dataframe for height. Default name is "height". Values must be in meters.
#' @param plot.width Plot width measured in meters. Default is 10 meters.
#' @param plot.length Plot length measured in meters. Default is 10 meters.
#' @param ageplot Logical argument for displaying size class plot (Default is FALSE). If ageplot=TRUE, plot of size class proportions is displayed: seedling (<5 cm dbh), sapling (5-10 cm dbh), and pole (>10 cm dbh).
#' @keywords mangrove structure, plot, Holdridge Complexity Index, Mean Stand Diameter
#' @examples
#' plot.indices(mangrove_data)
#' plot.indices(mangrove_data, plotnumber = "Plot", species = "Species", plot.width = 5, ageplot=T)
#' @export

# Function to obtain Plot Method Indices
plot.indices <- function(x, 
                         plotnumber = 'plotnumber',
                         dbh = 'dbh',
                         species = 'species',
                         height = 'height',
                         plot.width=10,
                         plot.length=10, 
                         ageplot=F){
                           
                           
                           x$PlotNumber <- x[,plotnumber]
                           x$Species <- x[,species]
                           x$dbh <- x[,dbh]
                           x$height <- x[,height]
                           
        # Pre calculations
        width = plot.width
        length = plot.length
        area = plot.length*plot.width
        x$areasum = area*length(unique(x$PlotNumber))
        x$count <- 1:nrow(x)
        x$rows <- max(x$count)
        spp <- length(unique(x$Species))
 
        # Get the summarize function from the plyr namespace
        summarize = get("summarize", asNamespace('plyr'))
        transform = get("transform", asNamespace('plyr'))
        
  
  cat("\n ---------------------------------------------------")
  cat("\n Plot Method Indices")
  cat("\n ---------------------------------------------------\n\n")
  
  # Calculate Density
  densum <- plyr::ddply(x, "Species", summarize, Number = length(count), Proportion = (length(count)/max(rows)), areasum=max(areasum))
  densum1 <- plyr::ddply(densum, "Species", transform, Stems_Per_0.1_Ha = round((Number/areasum) * 1000))
  totden <- sum(densum1$Stems_Per_0.1_Ha)
  
  # Calculate Basal Area
  x$BA <- (3.14159265359 * (x$dbh/2) ^ 2) / 10000
  densumBA <- plyr::ddply(x, "Species", summarize, MeanBA = (mean(BA)))
  densumBA <- plyr::join(densum1, densumBA, by="Species")
  densumBA$Basal_Area <- densumBA$Stems_Per_0.1_Ha * densumBA$MeanBA
  totbas <- sum(densumBA$Basal_Area)
  
  # Get height of three tallest trees
  x <- x[order(-x$height),]
  x2 <- head(x,3)
  heightcalc <- mean(x2$height)
  
  # Calculate Holdridge Complexity Index
  CI <- round((heightcalc * totbas * totden * spp) / 1000, digits=2)
  
  # Print the Output from HCI
  cat(" Holdridge Complexity Index\n -----\n")
  cat(paste(" ", CI))
  cat("\n\n")
  
  # Calculate Mean Stand Diameter
  MSD <- round(sqrt((totbas * 12732.39)/totden), digits=2)
  
  # Print output from MSD
  cat(" Mean Stand Diameter\n -----\n")
  cat(paste(" ", MSD))
  
  if(ageplot==T){
    # Plot of forest size class proportions
    x$size <- "Seedling"
    x$size[x$dbh >=2.5 & x$dbh<=10] <- "Sapling"
    x$size[x$dbh > 10] <- "Pole"
    x$sizefac <- factor(x$size, levels= c("Seedling", "Sapling", "Pole"))
    plotsize <- table(x$sizefac)
    barplot(plotsize / sum(plotsize), ylab="Proportion of forest trees", xlab="Tree size class", cex.lab = 1.2, cex.names=1.1, cex.axis = 1.1, ylim=c(0,1), col = c("darkolivegreen3", "forestgreen", "darkgreen"))
    
  }
}

