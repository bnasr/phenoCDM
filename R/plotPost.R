#' Plot Posterior Distributions
#'
#' This function plot posterior distributions of the parameters.
#' @param chains Gibbs sampling chains
#' @param trueValues numeric vector of true values
#' @keywords  Plot Posterior Distributions
#' @export
#' @examples
#'
#' #Summarize CDM Model Ouput
#'
plotPost <- function(chains, trueValues = NULL, outline = F){
  boxplot(chains, outline = outline, ylim = range(range(chains, na.rm = T), trueValues, na.rm = T))
  if(!is.null(trueValues)){
    xs <- 1:length(trueValues)
    segments(x0 = xs - 0.35, y0 = trueValues, x1 = xs + 0.35, y1 = trueValues, col = 'red', lwd=2)
  }
}

  