#' Plot Observed vs Predicted
#'
#' This function plot posterior distributions of the parameters.
#' @param o Observed vector
#' @param p Predicted Gibbs samples
#' @param nburnin numbe of burn-in itterations
#' @param xlim x-axis range
#' @param ylim y-axis range
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param colSet vector of colors for points, bars and the 1:1 line
#' @param cex cex value for size
#' @param lwd line width
#' @param pch pch value for symbols
#' @keywords  Plot Observed vs Predicted
#' @export
#' @import graphics
#' @import stats
#' @examples
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
#'                 nGibbs = 200,
#'                 nBurnin = 100,
#'                 z = ssSim$z,#response
#'                 connect = ssSim$connect, #connectivity of time data
#'                 quiet=TRUE)
#'
#' summ <- getGibbsSummary(ssOut, burnin = 100, sigmaPerSeason = FALSE)
#'
#' colMeans(summ$ymax)
#' colMeans(summ$betas)
#' colMeans(summ$tau)
#' colMeans(summ$sigma)
#'
#' par(mfrow = c(1,3), oma = c(1,1,3,1), mar=c(2,2,0,1), font.axis=2)
#'
#' plotPost(chains = ssOut$chains[,c("beta.1", "beta.2")], trueValues = ssSim$beta)
#' plotPost(chains = ssOut$chains[,c("ymax.1", "ymax.2")], trueValues = ssSim$ymax)
#' plotPost(chains = ssOut$chains[,c("sigma", "tau")], trueValues = c(ssSim$sig, ssSim$tau))
#'
#' mtext('Posterior distributions of the parameters', side = 3, outer = TRUE, line = 1, font = 2)
#' legend('topleft', legend = c('posterior', 'true value'),
#'      col = c('black', 'red'), lty = 1, bty = 'n', cex=1.5, lwd =2)
#'
#'
#' yGibbs <- ssOut$latentGibbs
#' zGibbs <- ssOut$zpred
#' o <- ssOut$data$z
#' p <- apply(ssOut$rawsamples$y, 1, mean)
#' R2 <- cor(na.omit(cbind(o, p)))[1,2]^2
#' #Plot Observed vs Predicted
#' par( mar=c(4,4,1,1), font.axis=2)
#' plotPOGibbs(o = o , p = zGibbs,
#'             xlim = c(0,10), ylim=c(0,10),
#'             cex = .7, nburnin = 1000)
#'             points(o, p, pch = 3)
#'
#' mtext(paste0('R² = ', signif(R2, 3)), line = -1, cex = 2, font = 2, side = 1, adj = .9)
#' legend('topleft', legend = c('mean', '95th percentile', '1:1 line', 'latent states'),
#'       col = c('#fb8072','#80b1d3','black', 'black'),
#'       bty = 'n', cex=1.5,
#'       lty = c(NA, 1, 2, NA), lwd =c(NA, 2, 2, 2), pch = c(16, NA, NA, 3))
#'
plotPOGibbs <- function(o,
                        p,
                        nburnin = NULL,
                        xlim =range(o, na.rm=TRUE),
                        ylim=range(p,na.rm=TRUE),
                        xlab='Observed',
                        ylab='Predicted',
                        colSet= c('#fb8072','#80b1d3','black'),
                        cex=1,
                        lwd=2,
                        pch=19){
  if(length(lwd)==1) lwd=rep(lwd,2)
  if(!is.null(nburnin)) p <- p[-(1:nburnin),]
  #o  - length n vector of obs or true values
  #p - ng by n matrix of estimates

  n <- length(o)
  y <- apply(p,2,quantile,c(.5,.005,.995))
  # y <- apply(p,2,quantile,c(.5,.25,.75))

  plot(o,y[1,],ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab,col=colSet[1],bty='n' ,pch=pch, cex=cex)
  #for(j in 1:n)lines(c(o[j],o[j]),y[2:3,j],col=colors[j])
  segments(o,t(y[2,]),o,t(y[3,]),col=colSet[2],lwd = lwd[1])
  points(o,y[1,],ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab,col=colSet[1],bty='n' ,pch=pch, cex=cex)
  abline(0,1,lty=2, col=colSet[3], lwd=lwd[2])
  invisible(y)
}
