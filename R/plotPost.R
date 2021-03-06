#' Plot Posterior Distributions
#'
#' This function plot posterior distributions of the parameters.
#' @param chains Gibbs sampling chains
#' @param trueValues numeric vector of true values
#' @param outline logical value whether showing outliers
#' @keywords  Plot Posterior Distributions
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
#' legend('topleft', legend = c('posterior', 'true value'), col = c('black', 'red'),
#'          lty = 1, bty = 'n', cex=1.5, lwd =2)
#'
plotPost <- function(chains, trueValues = NULL, outline = FALSE){
  myboxplot(chains, outline = outline, ylim = range(range(chains, na.rm = TRUE), trueValues, na.rm = TRUE), statsParam = c(.005,.25,.5,.75,.995))
  if(!is.null(trueValues)){
    xs <- 1:length(trueValues)
    segments(x0 = xs - 0.35, y0 = trueValues, x1 = xs + 0.35, y1 = trueValues, col = 'red', lwd=2)
  }
}

#This function replaces boxplot.stats --- it uses the middle 50% and
#middle 90% instead of middle 50%(IQR) and 1.5*IQR.
myboxplot.stats <-  function (x, coef = NULL,
                              do.conf = TRUE, do.out =TRUE,
                              statsParam = c(.005,.25,.5,.75,.995))
{
  if(length(statsParam)!=5)
  {
    warning('statsParams was replaced by c(.025,.25,.5,.75,.975)!')
    statsParam <- c(.025,.25,.5,.75,.975)
  }

  nna <- !is.na(x)
  n <- sum(nna)
  # stats <- quantile(x, c(.05,.25,.5,.75,.95), na.rm = TRUE)
  # stats <- quantile(x, c(.025,.25,.5,.75,.975), na.rm = TRUE)
  stats <- quantile(x, statsParam, na.rm = TRUE)
  # iqr <- diff(stats[c(2, 4)])
  out <- x < stats[1] | x > stats[5]
  conf <- if (do.conf)
    stats[3] + c(-1.58, 1.58) * diff(stats[c(2, 4)])/sqrt(n)
  list(stats = stats, n = n, conf = conf, out = x[out & nna])
}

myboxplot <- function (x, ..., range = 1.5, width = NULL, varwidth = FALSE,
                             statsParam = c(.025,.25,.5,.75,.975),
                             notch = FALSE, outline = TRUE, names, plot = TRUE, border = par("fg"),
                             col = NULL, log = "", pars = list(boxwex = 0.8, staplewex = 0.5,
                                                               outwex = 0.5), horizontal = FALSE, add = FALSE, at = NULL)
{
  args <- list(x, ...)
  namedargs <- if (!is.null(attributes(args)$names))
    attributes(args)$names != ""
  else rep_len(FALSE, length(args))
  groups <- if (is.list(x))
    x
  else args[!namedargs]
  if (0L == (n <- length(groups)))
    stop("invalid first argument")
  if (length(class(groups)))
    groups <- unclass(groups)
  if (!missing(names))
    attr(groups, "names") <- names
  else {
    if (is.null(attr(groups, "names")))
      attr(groups, "names") <- 1L:n
    names <- attr(groups, "names")
  }
  cls <- sapply(groups, function(x) class(x)[1L])
  cl <- if (all(cls == cls[1L]))
    cls[1L]
  else NULL
  for (i in 1L:n) groups[i] <- list(myboxplot.stats(unclass(groups[[i]]),
                                                    range,statsParam = statsParam))
  stats <- matrix(0, nrow = 5L, ncol = n)
  conf <- matrix(0, nrow = 2L, ncol = n)
  ng <- out <- group <- numeric(0L)
  ct <- 1
  for (i in groups) {
    stats[, ct] <- i$stats
    conf[, ct] <- i$conf
    ng <- c(ng, i$n)
    if ((lo <- length(i$out))) {
      out <- c(out, i$out)
      group <- c(group, rep.int(ct, lo))
    }
    ct <- ct + 1
  }
  if (length(cl) && cl != "numeric")
    oldClass(stats) <- cl
  z <- list(stats = stats, n = ng, conf = conf, out = out,
            group = group, names = names)
  if (plot) {
    if (is.null(pars$boxfill) && is.null(args$boxfill))
      pars$boxfill <- col
    do.call("bxp", c(list(z, notch = notch, width = width,
                          varwidth = varwidth, log = log, border = border,
                          pars = pars, outline = outline, horizontal = horizontal,
                          add = add, at = at), args[namedargs]))
    invisible(z)
  }
  else z
}
