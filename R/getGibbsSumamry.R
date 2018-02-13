#' Summarize Output of the CDM Model
#'
#' This function return a summary of the output from the Gibbs-Sampling of the CDM model.
#' @param ssOut CDM output list.
#' @param burnin Number of burnin itterations .
#' @param colNames vector of charachters includes names of each variable in the output.
#' @param sigmaPerSeason logical value indicating whether each site/season has a separate process error
#' @keywords  Gibbs Sampling Output Sammary
#' @export
#' @import stats
#' @examples
#'
#' #Summarize CDM Model Ouput
#'
#' ssSim <- phenoSim(nSites = 2, #number of sites
#'                   nTSet = 30, #number of Time steps
#'                   beta = c(1, 2), #beta coefficients
#'                   sig = .01, #process error
#'                   tau = .1, #observation error
#'                   plotFlag = TRUE, #whether plot the data or not
#'                   miss = 0.05, #fraction of missing data
#'                   ymax = c(6, 3) #maximum of saturation trajectory
#' )
#'
#' ssOut <- fitCDM(x = ssSim$x, #predictors
#'                 nGibbs = 600,
#'                 nBurnin = 300,
#'                 z = ssSim$z,#response
#'                 connect = ssSim$connect, #connectivity of time data
#'                 quiet=TRUE)
#'
#' summ <- getGibbsSummary(ssOut, burnin = 300, sigmaPerSeason = FALSE)
#'
#' colMeans(summ$ymax)
#' colMeans(summ$betas)
#' colMeans(summ$tau)
#' colMeans(summ$sigma)
#'
getGibbsSummary <- function(ssOut, burnin = NULL , colNames = NULL, sigmaPerSeason=TRUE){
  p <- ncol(ssOut$data$x)
  if(!is.null(colNames))colnames(ssOut$chains)[1:p] <- colNames
  ng <- nrow(ssOut$chains)
  if(is.null(burnin)) burnin <- floor(ng/2)
  burnt <- (1:burnin)
  n <- ncol(ssOut$chains)

  cutIX <- n-1
  if(sigmaPerSeason) cutIX <- p + (n-p-1)/2 +1

  ssGibbs.betas <- ssOut$chains[-burnt,1:p]
  ssGibbs.ymax <- ssOut$chains[-burnt,(p+1):(cutIX-1)]
  ssGibbs.sigma <- as.matrix(ssOut$chains[-burnt,cutIX:(n-1)])
  ssGibbs.tau <- as.matrix(ssOut$chains[-burnt,n])
  # ymaxChains <- as.matrix(ssOut$rawsamples$ymax)

  list(betas = ssGibbs.betas,
       sigma = ssGibbs.sigma,
       tau = ssGibbs.tau,
       ymax = ssGibbs.ymax)
}
