#' Mangrove Canopy Plot for Point-Centered Quarter Method
#'
#' This function allows you to calculate the Holdridge Complexity Index and Mean Stand Diameter for pcqm data.
#' @param samplingpoint Column name for sampling points (numerical). Default name is "samplingpoint". First sampling point must be 1.
#' @param height Column name for height. Default name is "height". Values must be in meters.
#' @param interval10 Logical argument for distance between PCQM sampling points. If interval10=TRUE (Default), the canopy plot assumes equidistant 10-meter spacing between all sampling points. If interval10=FALSE, a unique column must exist in the dataframe with distance from the previous sampling point.
#' @param interval Column name for the distances between sampling points (required if interval10=FALSE). Default name is "interval". The first row (samplingpoint 1) must be a value of 0. Each successive sampling point number represents the distance from the previous sampling point, NOT the additive distance from sampling point 1.
#' @param ymax Optional argument for specifying the maximum extent of the y axis. If not specified (Default), ymax is estimated based on dataframe values.
#' @keywords mangrove structure, pcqm, canopy profile
#' @examples
#' canopy.profile(mangrove_data)
#' canopy.profile(mangrove_data, ymax = 50, samplingpoint = "Sampling_Point", interval10=FALSE)
#' @export

# Function to plot canopy height across distance from Point-Centered Quarter Method transect
canopy.profile<-function(x, 
                      samplingpoint = 'samplingpoint',
                      height = 'height',
                      interval10 = TRUE,
                      interval = 'interval',
                      ymax = NULL){
  
    x$SamplingPoint <- x[,samplingpoint]
    x$height <- x[,height]


  #Preference for y axis maximum value display
    null_Max <- max(x$height)+mean(sd(x$height))
    Max <- if(is.null(ymax)) null_Max else ymax

  
  # Get the summarize and transform functions from the plyr namespace
  summarize = get("summarize", asNamespace('plyr'))

  # RUN if interval is standard 10 m for entire data set
  if(interval10==TRUE){
      if(min(x$SamplingPoint>1)){
      stop("Please check that Sampling Points begin at 1.")
            }
    else{
      plotcan <- plyr::ddply(x, "SamplingPoint", summarize, Avg_Height = mean(height), SD = sd(height))
      plotcan$SamplingPoint <- (plotcan$SamplingPoint*10)-10
      
      plot(plotcan$SamplingPoint, plotcan$Avg_Height, pch=20, ylab="Mean canopy height (SD)", xlab="Distance from starting point",ylim=range((c(0, Max))))
      lines(plotcan$SamplingPoint, plotcan$Avg_Height)
      arrows(plotcan$SamplingPoint, plotcan$Avg_Height-plotcan$SD, plotcan$SamplingPoint, plotcan$Avg_Height+plotcan$SD, length=0.05, angle=90, code=3)
    }
  }
  
  # RUN if interval has unique distances
  else{
    x$Interval <- x[,interval]
    plotcan <- plyr::ddply(x, "SamplingPoint", summarize, Avg_Height = mean(height), SD = sd(height), Interval=max(Interval))
    holder=-1
    for (i in 1:length(plotcan)+1) {
      holder <- plotcan$Interval[i] + holder
      plotcan$SamplingPoint[i] <- holder
    }

    plot(plotcan$SamplingPoint, plotcan$Avg_Height, pch=20, ylab="Mean canopy height (SD)", xlab="Distance from starting point", ylim=range((c(0, Max))))
    lines(plotcan$SamplingPoint, plotcan$Avg_Height)
    arrows(plotcan$SamplingPoint, plotcan$Avg_Height-plotcan$SD, plotcan$SamplingPoint, plotcan$Avg_Height+plotcan$SD, length=0.05, angle=90, code=3)
   
  }
  
  }
