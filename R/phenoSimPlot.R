#' Plot Simulated Phenology Data
#'
#' This function plots the time-series data described with a connectivity matrix.
#' @param z A vector of time-series data [n x 1]
#' @param connect The connectivity matrix for the z vector [n x 2]. Each row contains last and next element of the time-series. NA values means not connected.
#' @param add A logical variable whether the plot should be overlaid on the current panel.
#' @param col The color variable 
#' @param ylim Range of the y axis
#' @param pch pch value for the points
#' @param lwd lwd value for line tickness
#' @keywords  Plot Simulated Phenology Data
#' @export
#' @examples
#'
#' #Plot Simulated Phenology Data
#' ssSim <- phenoSim(nSites = 4, #number of sites
#'                   nTSet = 20, #number of Time steps
#'                   beta = c(1, 2), #beta coefficients
#'                   sig = .01, #process error
#'                   tau = .2, #observation error
#'                   plotFlag = T, #whether plot the data or not
#'                   miss = 0.1, #fraction of missing data
#'                   ymax = c(9,5,7, 3) #maximum of saturation trajectory
#' )
#' 
#' ssOut <- fitCDM(x = ssSim$x, #predictors  
#'                 nGibbs = 2000,
#'                 nBurnin = 1000,
#'                 z = ssSim$z,#response
#'                 connect = ssSim$connect, #connectivity of time data
#'                 quiet=T)
#' 
#' summ <- getGibbsSummary(ssOut, burnin = 1000, sigmaPerSeason = F)
#' 
#' colMeans(summ$ymax)
#' colMeans(summ$betas)
#' colMeans(summ$tau)
#' colMeans(summ$sigma)
#'
phenoSimPlot <- function(z, connect, add=F, col='blue', ylim = range(z, na.rm = T), pch=1, lwd=1){
  if(add) par(new=T)
  plot(z, ylim=ylim, col=col, pch=pch, xlab = '', ylab = '')
  ix <- 1:length(z)
  ww1 <- which(is.na(connect[,1]))
  ww2 <- which(is.na(connect[,2]))
  for(i in 1:length(ww1))lines(ix[ww1[i]:ww2[i]], z[ww1[i]:ww2[i]], col=col, lwd=lwd)
  abline(v=ix[ww1], col='grey')
  
}
