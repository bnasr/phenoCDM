#' Fit a CDM Model
#'
#' This function fits a CDM model on the input data as it is described by the phenoSim function.
#' @param x Matrix of predictors [N x p].
#' @param z Vector of reponse values [N x 1].
#' @param connect The connectivity matrix for the z vector [n x 2]. Each row contains the last and next elements of the time-series. NA values means not connected.
#' @param nGibbs Number of Gibbs itterations
#' @param nBurnin Number of burn-in itterations.
#' @param n.adapt Number of itterations for adaptive sampling
#' @param n.chains Number of Gibbs sampling chains
#' @param quiet whether to report the progress
#' @param calcLatentGibbs Whether to calculate the latent states
#' @keywords  CDM Model Fit
#' @export
#' @examples
#'
#' #Summarize CDM Model Ouput
#'
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
fitCDM <- function(x, z, connect=NULL,
                   # HeadTail =NULL,
                   nGibbs=1000,
                   nBurnin = 1,
                   n.adapt=100,
                   n.chains=4,
                   quiet=F,
                   calcLatentGibbs=F

){
  require(rjags)

  if(!is.null(connect)){
    Head <- which(is.na(connect[,1]))
    Tail <- which(is.na(connect[,2]))
    Body <- which(!rowSums(is.na(connect)))
    # }else if(!is.null(HeadTail)){
    # Head <- which(HeadTail[,1])
    # Tail <- which(HeadTail[,2])
    # Body <- which(!(HeadTail[,1]|HeadTail[,2]))
  }else
  {
    stop('One of connect or HeadTail arguments should be defined.')
  }

  model <- textConnection('model
                          {
                          for (i in 1:N)
                          {
                            z[i] ~ dnorm(y[i], tau2)
                          }

                          tau2 <- pow(tau, -2)
                          tau ~ dunif(0, 100)

                          for (i in connectHead)
                          {
                            y[i] ~ dnorm(0, sigma2)
                          }

                          for (i in connectBody)
                          {
                            dy[i] ~ dnorm(x[i-1,]%*%beta*(1-y[i-1]/ymax[blocks[i]]), sigma2)
                            y[i] <- y[i-1] + max(0, dy[i])
                          }

                          for (i in connectTail)
                          {
                            dy[i] ~ dnorm(x[i-1,]%*%beta*(1 - y[i-1]/ymax[blocks[i]]), sigma2)
                            y[i] <- y[i-1] + max(0, dy[i])
                          }

                          for (i in 1:nblocks)
                          {
                            ymax[i] ~ dnorm(0, .001)T(0,10000)
                          }
                          for (i in 1:p)
                          {
                            beta[i] ~ dnorm(0, .0001)
                          }

                          for (i in 1:N)
                          {
                            zpred[i] ~ dnorm(y[i], tau2)
                          }

                          sigma2 <- pow(sigma, -2)
                          sigma ~ dunif(0.01, 0.03)
                          }')

  ssModel <- jags.model(model,
                        quiet = quiet,
                        data = list('x' = x,
                                    'z' = z,
                                    'N' = nrow(x),
                                    'p'= ncol(x),
                                    'blocks'=cumsum(is.na(connect[,1])*1),
                                    'nblocks'=sum(is.na(connect[,1])),
                                    connectHead=Head,
                                    connectBody=Body,
                                    connectTail=Tail),
                        n.chains = n.chains,
                        n.adapt = n.adapt)

  update(ssModel, nBurnin)

  ssSamples <- jags.samples(ssModel,c('y','beta', 'sigma', 'tau', 'ymax', 'zpred'), nGibbs )


  # print(ssSamples)


  ssGibbs.jags <- data.frame(beta=t(apply(ssSamples$beta, c(1,2), mean)),
                             ymax=t(apply(ssSamples$ymax, c(1,2), mean)),
                             sigma=t(apply(ssSamples$sigma, c(1,2), mean)),
                             tau=t(apply(ssSamples$tau, c(1,2), mean))
  )
  latentGibbs <- NULL
  if(calcLatentGibbs) latentGibbs <- t(apply(ssSamples$y, c(1,2), mean))
  zpred <- t(apply(ssSamples$zpred, c(1,2), mean))

  ww <- grep(pattern = 'beta', colnames(ssGibbs.jags))
  if(!is.null(colnames(x))) colnames(ssGibbs.jags)[ww] <- colnames(x)
  return(list(model=ssModel,
              chains=ssGibbs.jags,
              nBurnin= nBurnin,
              nGibbs=nGibbs,
              latentGibbs = latentGibbs,
              zpred = zpred,
              rawsamples = ssSamples,
              data =list(x = x,
                         z = z,
                         connect = connect,
                         # HeadTail = HeadTail,
                         nBurnin = nBurnin,
                         nGibbs = nGibbs,
                         n.chains=n.chains,
                         n.adapt = n.adapt)))
}

