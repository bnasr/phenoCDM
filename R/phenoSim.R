#' Simulate Green-up Phenology Data
#'
#' This function return a set of simulated data for multiple green-up phenology time-series.
#' @param nSites Number of sites/seasons
#' @param nTSet A vector of integer values. Length of each time-series will be randomly sampled from this vector.
#' @param p Number of predictors in th emodel.
#' @param beta Beta coefficients
#' @param sig Process error.
#' @param tau Observation error.
#' @param miss Fraction of missing data.
#' @param plotFlag Whether to plot the resulted time-series.
#' @param ymax Asymptotic maximum values.
#' @param trend -1:decreasing, +1:increasing, 0: not constrained
#' @keywords  Simulate Phenology Data
#' @export
#' @examples
#'
#' #Simulate Phenology Data
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
phenoSim <- function(nSites=1000,
                     nTSet=c(3:6),
                     p=2,
                     beta =NULL,
                     sig= .1,
                     tau=.01,
                     miss=0,
                     plotFlag = F,
                     ymax=1,
                     trend = +1 # -1:decreasing, +1:increasing, 0: not constrained
){
  if(trend==-1)
    t <- c(1, 0, 0)
  else if(trend==0)
    t <- c(0, 1, 0)
  else if(trend==1)
    t <- c(0, 0, 1)
  else
    stop('trend should be -1:decreasing, 0: not constrained or +1:increasing')

  nSamples.Site <- sample(nTSet, nSites, replace = T)

  if(length(nTSet)==1) nSamples.Site <- sample(c(nTSet, nTSet), nSites, replace = T)

  if(length(ymax)==1) ymax <- rep(ymax, nSites)

  N <- sum(nSamples.Site)

  if(!is.null(beta)) {
    p <- length(beta)
  }else {
    beta <- matrix(2*runif(p)-1)
  }

  x <- matrix(runif(p*N), ncol=p, nrow=N)

  y <- rep(0, N)

  sampleSiteNo <- c(0, cumsum(nSamples.Site))
  for(i in 1:nSites){
    y[sampleSiteNo[i]+1] <- runif(1, .2, .3)

    for(j in 2:nSamples.Site[i]){
      dy <- (x[sampleSiteNo[i]+j-1,]%*%beta)*(1-(y[sampleSiteNo[i]+j-1]/ymax[i]))
      dy <- t[1]*min(0, rnorm(1, dy, sig )) + t[2]*rnorm(1, dy, sig ) + t[3]*max(0, rnorm(1, dy, sig ))
      y[sampleSiteNo[i]+j] <- y[sampleSiteNo[i]+j-1] + dy
    }
  }
  z <- rnorm(N, y, tau)

  connect <- matrix(NA, nrow = N, ncol = 2)
  colnames(connect) <- c('Back','Fore')

  for(i in 1:nSites){
    connect[(sampleSiteNo[i]+1):(sampleSiteNo[i+1]-1),2] <-
      (sampleSiteNo[i]+2):(sampleSiteNo[i+1])

    connect[(sampleSiteNo[i]+2):(sampleSiteNo[i+1]),1] <-
      (sampleSiteNo[i]+1):(sampleSiteNo[i+1]-1)
  }

  wNA <- sample(1:N, floor(miss*N) )
  z[wNA] <- NA
  if (plotFlag)
  {
    phenoSimPlot(z, connect)
  }

  list(x=x, z=z, y= y, connect=connect, trend = trend,
       miss =miss,
       tau = tau, sig=sig, beta=beta, ymax=ymax,
       startPoints = which(is.na(connect[,1])),
       n=nSamples.Site, wNA = wNA)
}
