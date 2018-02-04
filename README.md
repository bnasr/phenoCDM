# phenoCDM
Continuous development models for incremental time-series analysis: applications on leaf phenology



## Installation
The phenoCDM package can be directly installed from the Github source:

```{r, echo=TRUE}

install_github("bnasr/phenoCDM")

```

## Example
```{r, echo=TRUE}

library(phenoCDM)

ssSim <- phenoSim(nSites = 4, #number of sites
                 nTSet = 20, #number of time steps
                 beta = c(1, 2), #beta coefficients
                 sig = .01, #process error
                 tau = .2, #observation error
                 plotFlag = T, #whether plot the data or not
                 miss = 0.1, #fraction of missing data
                 ymax = c(9,5,7, 3) #maximum of saturation trajectory
)

ssOut <- fitCDM(x = ssSim$x, #predictors  
                nGibbs = 2000,
                nBurnin = 1000,
                z = ssSim$z,#response
                connect = ssSim$connect, #connectivity of time data
                quiet=T)

summ <- getGibbsSummary(ssOut, burnin = 1000, sigmaPerSeason = F)

colMeans(summ$ymax)
colMeans(summ$betas)
colMeans(summ$tau)
colMeans(summ$sigma)


boxplot(ssOut$chains[,c("beta.1", "beta.2")], outline = F)
boxplot(ssOut$chains[,c("ymax.1", "ymax.2", "ymax.3", "ymax.4")], outline = F)
boxplot(ssOut$chains[,c("sigma", "tau")], outline = F)


```
