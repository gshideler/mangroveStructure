#' Importance Value Radar Plot for Mangrove PCQM or Plot Data
#'
#' This function allows you to output a radar plot for objects created from pcqm.method() or plot.method().
#' @param x Data frame object output from the pcqm.method() or plot.method() functions.
#' @param colors Optional list of colors to be used in the radar plot. Maximum of five species will be plotted. If listed, one color must be provided for each species. If not specified, default colors will be used (red, black, blue, orange, green).
#' @examples
#' iv.plot(data_output)
#' iv.plot(data_output, colors = c("black" , "gray", "goldenrod1", "lightgreen", "red"))
#' @export
iv.plot <- function(x, colors = NULL,
                    Species = 'Species',
                    Relative_Density = 'Relative_Density',
                    Relative_Dominance = 'Relative_Dominance',
                    Relative_Frequency = 'Relative_Frequency',
                    Importance_Value = 'Importance_Value',
                    Rank = 'Rank'){
    x <- as.data.frame(x)

    x$Relative_Density <- x[,Relative_Density]
    x$Relative_Dominance <- x[,Relative_Dominance]
    x$Relative_Frequency <- x[,Relative_Frequency]
    x$Importance_Value <- x[,Importance_Value]
    x$Rank <- x[,Rank]

  # PLOT RELATIVE IMPORATNCE DATA
  compare <- x[order(x$Species),]
  MX <- data.frame(100, 100, 100)
  names(MX) <- c("Rel_Dom", "Rel_Den", "Rel_Fre")
  MN <- data.frame(0, 0, 0)
  names(MN) <- c("Rel_Dom", "Rel_Den", "Rel_Fre")
  Rel_Dom <- data.frame(compare$Relative_Dominance)
  names(Rel_Dom)<-"Rel_Dom"
  Rel_Den <- data.frame(compare$Relative_Density)
  names(Rel_Den)<-"Rel_Den"
  Rel_Fre <- data.frame(compare$Relative_Frequency)
  names(Rel_Fre) <- "Rel_Fre"
  
  plotdata <- cbind(Rel_Dom, Rel_Fre, Rel_Den)
  plotdata2 <- rbind(MX, MN, plotdata)
  
  if(is.null(colors)){
  colors <- c( "firebrick", "black" , "dodgerblue", "goldenrod1", "lightgreen" )
  }
  else{
    colors <- colors
    if(length(colors) != length(unique(compare$Species))) stop("If specifying colors, one color must be listed for each species (listed alphabetically)")

  }
  
  compare$legend <- paste(compare$Species, " (", compare$Importance_Value, ")")
  speclist <- compare$legend
  
  # Plot radarChart
  fmsb::radarchart(plotdata2, maxmin=TRUE,
                   axistype=1 , centerzero = TRUE, vlabels=c("Relative\ndominance (%)", "Relative\ndensity (%)", "Relative\nfrequency (%)"),
                   pcol=colors,  plwd=4 , plty=1, 
                   cglcol="grey", cglty=1, axislabcol="black", seg=5, caxislabels=seq(0,100,20), cglwd=0.8)
  legend(.65, .8, legend = speclist, col=colors, title = "Species", seg.len = 2,  pch = 16, lwd=4,lty = 1, bty="n",box.col="grey")
}

