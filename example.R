library(phenoCDM)

ssSim <- phenoSim(nSites = 3, #number of sites
                  nTSet = 30, #number of time steps
                  beta = c(1, 2), #beta coefficients
                  sig = 0.01, #process error
                  tau = 0.1, #observation error
                  plotFlag = F, #whether plot the data or not
                  miss = 0.1, #fraction of missing data
                  ymax = c(9,5, 12) #maximum of saturation trajectory
)

ww1 <- which(is.na( ssSim$connect[,1]))
ww2 <- which(is.na( ssSim$connect[,2]))
png('fig1.png', width = 6, height = 3, units = 'in', res = 300)
par(mfrow = c(1,3), oma = c(3,2,1,1), mar=c(2,2,0,1))
for(i in 1:length(ww1))  {
  z <- ssSim$z[ww1[i]:ww2[i]]
  ymax <- ssSim$ymax[i]
  plot(z, xlab = 'Index', ylab = '', type = 'b', ylim = range(c(0, ymax, z), na.rm = T))
  mtext(paste('Set', i), side = 1, line = -2, col = 'blue', font=2)
  abline(h = ymax, col='red')
}
mtext(text = 'Response (z)', side = 2, line = 0.5, outer = T, font = 2)
mtext(text = 'Index', side = 1, line = 0.5, outer = T, font = 2)
dev.off()

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



source('~/Projects/procVisData/dataViz.R')

png('fig2.png', width = 8, height = 3, units = 'in', res = 300)
par(mfrow = c(1,3), oma = c(1,1,1,1), mar=c(2,2,0,1), font.axis=2)

plotPost(chains = ssOut$chains[,c("beta.1", "beta.2")], trueValues = ssSim$beta)
plotPost(chains = ssOut$chains[,c("ymax.1", "ymax.2", "ymax.3")], trueValues = ssSim$ymax)
plotPost(chains = ssOut$chains[,c("sigma", "tau")], trueValues = c(ssSim$sig, ssSim$tau))

dev.off()

