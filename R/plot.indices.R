#' Mangrove Indices for Plot Method
#'
#' This function allows you to calculate the Holdridge Complexity Index and Mean Stand Diameter for pcqm data.
#' @param plotnumber Column name in data frame for plot number (numerical). Default name is "plotnumber". First plot number must be 1.
#' @param dbh Column name in data frame for diameter at breast height. Default name is "dbh". Values must be in centimeters.
#' @param species Column name in data frame for species. Default name is "species".
#' @param height Column name in data frame for height (Optional). Default name is "height". Values must be in meters. When included, additional height-related outputs are displayed.
#' @param plot.width Plot width measured in meters. Default is 10 meters.
#' @param plot.length Plot length measured in meters. Default is 10 meters.
#' @param sizebin Logical argument for displaying size class plot (Default is FALSE). If sizebin=TRUE, plot of size class proportions is displayed: seedling (<5 cm dbh), sapling (5-10 cm dbh), and pole (>10 cm dbh).
#' @keywords mangrove structure, plot, Holdridge Complexity Index, Mean Stand Diameter
#' @examples
#' plot.indices(mangrove_data)
#' plot.indices(mangrove_data, plotnumber = "Plot", species = "Species", plot.width = 5, sizebin=T)
#' @export

# Function to obtain Plot Method Indices
plot.indices <- function(x, 
                         plotnumber = 'plotnumber',
                         dbh = 'dbh',
                         species = 'species',
                         height = 'height',
                         plot.width=10,
                         plot.length=10, 
                         sizebin=F){
                           
                         #Confirm it is a data frame
                         x <- as.data.frame(x)
  
                            #Load columns
                           x$PlotNumber <- x[,plotnumber]
                           x$Species <- x[,species]
                           x$dbh <- x[,dbh]
                           if("height" %in% colnames(x)) x$height <- x[,height]
  
    #Check for NA cells
    if(any(is.na(x)) == TRUE) stop("Data frame cannot contain missing values (NAs).")

                           
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
    if("height" %in% colnames(x)) x$size2 <- "<5 m"
    if("height" %in% colnames(x)) x$size2[x$height >=5 & x$height<=10] <- "5–10 m"
    if("height" %in% colnames(x)) x$size2[x$height > 10] <- ">10 m"
    if("height" %in% colnames(x)) x$sizefac2 <- factor(x$size2, levels= c("<5 m", "5–10 m", ">10 m"))
    if("height" %in% colnames(x)) plotsize2 <- table(x$sizefac2)
    if("height" %in% colnames(x)) barplot(plotsize2 / sum(plotsize2), ylab="Proportion of forest trees", xlab="Tree height size class", cex.lab = 1.2, cex.names=1.1, cex.axis = 1.1, ylim=c(0,1), col = c("darkolivegreen3", "forestgreen", "darkgreen"))
  
    #Scatterplot of dbh vs height
     if("height" %in% colnames(x)) plot(x$dbh, x$height, pch=21, xlab="Tree DBH (cm)", ylab="Tree Height (m)", cex.lab = 1.2, cex.axis = 1.1, col="black", bg = c("snow", "gray75", "black")[x$sizefac], ylim=c(0,max(x$height)+1), xlim=c(0,max(x$dbh)+1), yaxs="i", xaxs="i")
     if("height" %in% colnames(x)) legend("bottomright",legend=c("<5 cm dbh","5–10 cm dbh",">10 cm dbh"),pch=c(21,21,21),col = "black", pt.bg = c("snow", "gray75", "black"), cex=1.2, pt.cex=1.2, title="DBH Size Class")
  
  }
}

