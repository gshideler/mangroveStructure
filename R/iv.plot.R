iv.plot <- function(x, Species = 'Species',
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
  compare2 <- subset(compare, compare$Rank<6)
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
  
  colors_border=c( "firebrick", "black" , "dodgerblue", "goldenrod1", "lightgreen" )
  colors_in=c( "firebrick", "black" , "dodgerblue", "goldenrod1", "lightgreen" )
  speclist <- compare$Species
  
  # Plot radarChart
  fmsb::radarchart(plotdata2, maxmin=TRUE,
                   axistype=1 , centerzero = TRUE, vlabels=c("Relative\ndominance (%)", "Relative\ndensity (%)", "Relative\nfrequency (%)"),
                   pcol=colors_border,  plwd=4 , plty=1, 
                   cglcol="grey", cglty=1, axislabcol="black", seg=5, caxislabels=seq(0,100,20), cglwd=0.8)
  legend(.65, .8, legend = speclist, col=colors_in, title = "Species", seg.len = 2,  pch = 16, lwd=4,lty = 1, bty="n",box.col="grey")
}

