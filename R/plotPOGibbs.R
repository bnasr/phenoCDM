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
#' #Plot Observed vs Predicted
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
