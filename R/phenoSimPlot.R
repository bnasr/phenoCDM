#' Plot Simulated Phenology Data
#'
#' This function plots the time-series data described with a connectivity matrix.
#' @param z A vector of time-series data [n x 1]
#' @param connect The connectivity matrix for the z vector [n x 2]. Each row contains the last and next elements of the time-series. NA values means not connected.
#' @param add logical value indicating whether the plot should be overlaid on the current panel.
#' @param col The color variable as charachter
#' @param ylim Range of the y axis
#' @param pch pch value for the symbols
#' @param lwd lwd value for line tickness
#' @keywords  Plot Simulated Phenology Data
#' @export
#' @import graphics
#' @import stats
#' @examples
#'
#' #Simulate Phenology Data
#' ssSim <- phenoSim(nSites = 2, #number of sites
#'                   nTSet = 30, #number of time steps
#'                   beta = c(1, 2), #beta coefficients
#'                   sig = .01, #process error
#'                   tau = .1, #observation error
#'                   plotFlag = TRUE, #whether plot the data or not
#'                   miss = 0.05, #fraction of missing data
#'                   ymax = c(6, 3) #maximum of saturation trajectory
#' )
#'
#' #Plot Simulated Data
#' phenoSimPlot(ssSim$z, ssSim$connect)
#'
phenoSimPlot <- function(z, connect, add=FALSE, col='blue', ylim = range(z, na.rm = TRUE), pch=1, lwd=1){
  if(add) par(new=TRUE)
  plot(z, ylim=ylim, col=col, pch=pch, xlab = 'Index', ylab = 'Response')
  ix <- 1:length(z)
  ww1 <- which(is.na(connect[,1]))
  ww2 <- which(is.na(connect[,2]))
  for(i in 1:length(ww1))lines(ix[ww1[i]:ww2[i]], z[ww1[i]:ww2[i]], col=col, lwd=lwd)
  abline(v=ix[ww1], col='grey')

}
